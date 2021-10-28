#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Run a stacked ensemble model for antibiotic consumption in LMICs #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(data.table)
library(gbm)
library(xgboost)
library(caret)
library(mgcv)
library(dplyr)
library(glmnet)
library(matrixStats)
library(quadprog)
library(foreign)
library(Cubist, lib = '/ihme/homes/annieb6/temp_packages')
library(nnet, lib = '/ihme/homes/annieb6/temp_packages')
library(randomForest, lib = '/ihme/homes/annieb6/temp_packages')

#~~~~~~~~~~~~~#
# i. Setup ####
#~~~~~~~~~~~~~#
#set output directory
model_date = format(Sys.Date(), "%Y_%m_%d")
outputdir <-  paste0('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/', model_date, '/')
dir.create(outputdir, showWarnings = F, recursive = T)

#Load data
mydata <- data.table(read.csv('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/input_data/J01_DDD_2000_2018_05_10_20.csv', stringsAsFactors = F))
names(mydata)[names(mydata) == 'loc_id'] <- 'location_id'
names(mydata)[names(mydata) == 'year'] <- 'year_id'

#specify child models to include
# can be xgboost (BRT), gam, ridge, lasso, enet, nnet (neural nets), rf (random forest), cubist
child_models <- c('xgboost', 'gam','ridge', 'nnet', 'cubist')

#specify the stacker you want to use out of CWM (constrained weighted mean, from quadratic programming), 
# RWM (weighted mean based on R-sqr) GBM, GLM, nnet
stacker <- 'CWM'

#specify the family you are modelling (currently can use binomial or gaussian)
family <- 'gaussian'

#specify transformation of the data to do - 'logit', 'log' or NULL
#nb if using logit this is only compatable for xgboost, random forest, cubist and neural networks
#child models with binomial data as these are using the probability, GAM and penalised regressions are using n and d
transformation <- 'log'

#Centre scale covariates?
centre_scale <- FALSE

#Include year in your models?
include_year <-  TRUE

#load covariates - either centre-scaled or standard
covs <- read.csv('/ihme/homes/annieb6/covariates/cleaned_covs.csv', stringsAsFactors = F)

#specify holdout method, currently can use random or country
holdout_method <- 'country'

#specify covariates you want to include in the model
covs_to_include <-  c("cv_anc4_coverage_prop",
                      "cv_hospital_beds_per1000",
                      "cv_mean_temperature",
                      "cv_pollution_outdoor_pm25",
                      "cv_sanitation_prop",
                      "cv_physicians_pc",
                      "cv_abx_prop",
                      "cv_haqi"
                      )


#specify what you columns are
p <- NULL           #the proportion of your indicator successes
n <- 'ddd_per_1000'  #the number of your indicator successes
d <- NULL            #the denoinator (sample size)
w <- NULL            #the weights to use

#Specify which years you are modelling for
min_year <- 2000
max_year <- 2018

#rename some colums to avoid confusion
colnames(mydata)[colnames(mydata)==d] <- 'd' 
if(!is.null(p)) {colnames(mydata)[colnames(mydata)==p] <- 'p'} 
if(!is.null(n)) {colnames(mydata)[colnames(mydata)==n] <- 'n'} 

#if you dont have n but have p and d
if(is.null(n) &!is.null(p)&!is.null(d)){mydata$n <- mydata$p*mydata$d}

#if you havent specified a weights column set to 1
if(is.null(w)){
  mydata$w <- 1
} else {
  colnames(mydata)[colnames(mydata)==w] <- 'w' 
}

#perform transformations as specified
if(is.null(transformation)){
} else if(transformation == 'log'){
  if(family == 'binomial'){
    mydata$n <- log(mydata$n)
    mydata$d <- log(mydata$d)
    mydata$p <- log(mydata$p)
  } else if(family == 'gaussian')
    mydata$n <- log(mydata$n)
} else if(transformation == 'logit'){  #ln(p/1-p)
  if(family == 'binomial'){
    mydata$p <- log(mydata$p/(1-mydata$p))
  } else if(family == 'gaussian'){
    message('should not be using logit transformation with gaussian data')
  }
}

#restrict covs to those included
covs <- covs[colnames(covs) %in% covs_to_include | colnames(covs)=='location_id' | colnames(covs) =='year_id']
covs <- data.table(covs)
covs <- na.omit(covs)

# transform covs - put any transformations desired here
# covs$cv_hospital_beds_per1000 <- log(covs$cv_hospital_beds_per1000)

#centre scale the covariates if desired
if(centre_scale == TRUE){
  covs <- data.frame(covs)
  covs[colnames(covs) %in% covs_to_include] <- data.frame(scale(covs[colnames(covs) %in% covs_to_include]))
  covs$year <- scale(covs$year_id)
  covs <-  data.table(covs)
}

#merge covs onto data
mydata <- merge(mydata, covs, by = c('location_id', 'year_id'))
mydata <- data.table(mydata)

## remove NAs
if(family == 'binomial'){
  mydata    <- na.omit(mydata, c('n', 'd', 'p', names(covs)))
}

if(family == 'gaussian'){
  mydata    <- na.omit(mydata, c('n', names(covs)))
}

## shuffle the data into five random folds
if(holdout_method == 'random'){
  mydata <- mydata[sample(nrow(mydata)),]
  mydata[,fold_id := cut(seq(1,nrow(mydata)),breaks=5,labels=FALSE)]
}

if(holdout_method == 'country'){
  country <- unique(mydata[, country])
  country <- country[sample(length(country))]
  fold_id <- cut(seq(1,length(country)),breaks=5,labels=FALSE)
  folds <- as.data.table(cbind(country, fold_id))
  
  mydata <- merge(mydata, folds, by = c('country'))
  mydata$fold_id <- as.numeric(mydata$fold_id)
  rm(country, fold_id)
}

# Limit to required years
mydata <- mydata[mydata$year_id >= min_year & mydata$year_id <= max_year,]
covs <- covs[covs$year_id >= min_year & covs$year_id <= max_year,]

## add a row id column
mydata[, a_rowid := seq(1:nrow(mydata))]

# Add year to the covariate list if desired
if(include_year == TRUE){
  covs_to_include <- c('year_id', covs_to_include)
}

#~~~~~~~~~~~~~~~~~~~~~#
# Fit child models ####
#~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~#
# 1. XGBoost ####
#~~~~~~~~~~~~~~~#

if('xgboost' %in% child_models){
  dir.create(paste0(outputdir, '/xgboost'), showWarnings = F)
  
  # Create model formula
  if(family == 'binomial'){
    form <- as.formula(paste0('p ~ ', paste(covs_to_include, collapse = " + ")))
  } else if(family == 'gaussian'){
    form <- as.formula(paste0('n ~ ', paste(covs_to_include, collapse = " + ")))
  }
  

  #tune the XGBoost
  # message("Model tuning xgboost")
  # #set the options for parameters to look at whilst tuning
  xg_grid <- expand.grid(nrounds = c(50, 100, 200),
                         max_depth = c(4, 6, 8, 10, 12),
                         eta = (3:8) / 100,
                         colsample_bytree = .5,
                         min_child_weight = 1,
                         subsample = 1,
                         gamma = 0)


  # Set cross validation options, default to 5 times repeated 5-fold cross validation
  # Selection function is "oneSE" to pick simplest model within one standard error of minimum
  # then imput this into the training model
  train_control <- trainControl(selectionFunction = "oneSE",
                                method = "repeatedcv",
                                number = 5,
                                repeats = 5,
                                index = list(mydata$a_rowid[mydata$fold_id!=1],
                                             mydata$a_rowid[mydata$fold_id!=2],
                                             mydata$a_rowid[mydata$fold_id!=3],
                                             mydata$a_rowid[mydata$fold_id!=4],
                                             mydata$a_rowid[mydata$fold_id!=5]),
                                indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                               mydata$a_rowid[mydata$fold_id==2],
                                               mydata$a_rowid[mydata$fold_id==3],
                                               mydata$a_rowid[mydata$fold_id==4],
                                               mydata$a_rowid[mydata$fold_id==5]))

  # Fit model
  xg_fit <- train(form,
                  data = mydata,
                  trControl = train_control,
                  verbose = F,
                  tuneGrid = xg_grid,
                  metric = "RMSE",
                  method = "xgbTree",
                  objective = if(family == 'binomial'){"reg:logistic"}else if(family == 'gaussian'){"reg:linear"}else{message('Family of model not compatiable')},
                  weights = mydata$w)

  # Save model fit object
  saveRDS(xg_fit, paste0(outputdir, "/xgboost/xg_fit.RDS"))

  # Save the best parameters to csv file
  write.csv(xg_fit$bestTune, paste0(outputdir, 'xgboost/xgboost_best_tune_.csv'))
  xg_best_tune <- xg_fit$bestTune
  
  #set up final parameters based on the model tuning
  xg_grid_final <- expand.grid(nrounds = xg_best_tune$nrounds,
                               max_depth = xg_best_tune$max_depth,
                               eta = xg_best_tune$eta,
                               colsample_bytree = .5,
                               min_child_weight = 1,
                               subsample = 1,
                               gamma = 0)
  
  train_control_final <- trainControl(method = "cv",
                                      number = 5,
                                      savePredictions = "final",
                                      index = list(mydata$a_rowid[mydata$fold_id!=1],
                                                   mydata$a_rowid[mydata$fold_id!=2],
                                                   mydata$a_rowid[mydata$fold_id!=3],
                                                   mydata$a_rowid[mydata$fold_id!=4],
                                                   mydata$a_rowid[mydata$fold_id!=5]),
                                      indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                                     mydata$a_rowid[mydata$fold_id==2],
                                                     mydata$a_rowid[mydata$fold_id==3],
                                                     mydata$a_rowid[mydata$fold_id==4],
                                                     mydata$a_rowid[mydata$fold_id==5]))
  
  #Fit final model
  message("Fitting xgboost on final tuned hyperparameters")
  xg_fit_final <- train(form,
                        data = mydata,
                        trControl = train_control_final,
                        verbose = F,
                        tuneGrid = xg_grid_final,
                        metric = "RMSE",
                        method = "xgbTree",
                        objective = if(family == 'binomial'){"reg:logistic"}else if(family == 'gaussian'){"reg:linear"}else{message('Family of model not compatiable')},
                        weights = mydata$w)
  
  # Plot the covariate importance of final model
  cov_plot <-
    ggplot(varImp(xg_fit_final, scale = FALSE)) +
    labs(x = "Covariate", y = "Relative Importance") +
    theme_bw()
  ggsave(filename = paste0(outputdir, '/xgboost/_covariate_importance.png'),
         plot = cov_plot)
  
  # Extract out of sample and in sample predictions
  mydata[, 'xgboost_cv_pred'   := arrange(xg_fit_final$pred, rowIndex)[,"pred"]]
  mydata[, 'xgboost_full_pred' := predict(xg_fit_final, mydata)]

  #save model fit
  xg_fit_final$model_name <- "xgboost"
  saveRDS(xg_fit_final, paste0(outputdir, '/xgboost/full_xgboost.RDS'))
  
  #predict out for all locations
  covs[, 'xgboost' := predict(xg_fit_final, covs)]
  
  rm(form, cov_plot, train_control, train_control_final, xg_best_tune, xg_fit, xg_fit_final, xg_grid, xg_grid_final)
}

#~~~~~~~~~~~#
# 2. GAM ####
#~~~~~~~~~~~#
if('gam' %in% child_models){
  dir.create(paste0(outputdir, '/gam/'), showWarnings = F)
  
  #If there are any binary covariates then remove them from the cov list and add as additional terms
  
  #set response variable
  if(family == 'binomial'){
    response <- cbind(sucesses = mydata$n, 
                      failures = mydata$d - mydata$n)
  } else if (family == 'gaussian'){
    response <- mydata$n
  }
  
  #build the GAM formula:
  #response ~ 1 + s(covariates, spline arguments)
  #TO DO: Look at tuning this model with different splines
  gam_formula <- paste0('response ~ 1+ s(', paste(covs_to_include, collapse = ", bs = 'ts', k = 3) + s("), ", bs = 'ts', k = 3)")
  gam_formula <- as.formula(gam_formula)
  
  # Fit full model
  #has some sort of parrallelisation inbuilt - set using this 
  full_gam = mgcv::gam(gam_formula, 
                       data = mydata, 
                       family = if(family =='binomial'){'quasibinomial'}else if(family == 'gaussian'){'gaussian'}, 
                       weights = mydata$w, 
                       control = list(nthreads = 2))
  full_gam$model_name = 'GAM'
  
  #predict using full model fit earlier
  mydata[,'gam_full_pred' := predict(full_gam, mydata, type = 'response')]
  
  #fit the model on the holdouts
  for(i in 1:5){
    if(family == 'binomial'){
      response <- cbind(successes = mydata$n[mydata$fold_id!=i], 
                      failures = mydata$d[mydata$fold_id!=i] - mydata$n[mydata$fold_id!=i])
    } else if (family == 'gaussian'){
      response <- mydata$n[mydata$fold_id!=i] 
    }      
     
     baby_gam = mgcv::gam(gam_formula, 
                          data = mydata[mydata$fold_id!=i], 
                          family = if(family =='binomial'){'quasibinomial'}else if(family == 'gaussian'){'gaussian'}, 
                          weights = mydata$weight[mydata$fold_id!=i], 
                          control = list(nthreads = 2))
    
    #fill in the data
    mydata[fold_id==i, 'gam_cv_pred' := predict(baby_gam, mydata[fold_id==i,],type = 'response')] 
      
  }
  
  #save full model fit
  saveRDS(full_gam, paste0(outputdir, '/gam/full_gam.RDS'))
  
  #predict out for all locations
  covs[,'gam' := predict(full_gam, covs, type = 'response')]
  
  #plot out GAM results to analyse
  pdf(paste0(outputdir, '/gam/plots.pdf'))
  gam.check(full_gam)
  dev.off()
  
  rm(baby_gam, full_gam, gam_formula, response)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Penalised regression (E-net/Ridge/Lasso) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#alpha 0 = Ridge, alpha 1 = Lasso, inbetween = e-net
if('enet' %in% child_models | 'ridge' %in% child_models | 'lasso' %in% child_models){
  
  dir.create(paste0(outputdir, '/glmnet'),showWarnings = F)
  
  #define the response to be modeled (2 variable matrix)
  if(family == 'binomial'){
    response <- cbind(failures   = mydata$d - mydata$n, 
                    successes = mydata$n)
  }else if(family == 'gaussian'){
      response <- mydata$n
  }
  
  #define variables to include (as a matrix)
  vars <- as.matrix(mydata[, covs_to_include, with = F])
  colnames(vars) <- covs_to_include
  
  #train model to select lambda and alpha,
  #no function to select alpha in glmnet so just run with a few options and compare MSE
  #let the function select its own ranges of lambda (can select manually of desired)
  #use 5 fold CV for this (?? could change to 10 ??)
  cv_lambda0 = cv.glmnet(x = vars , y= response, family = family, alpha = 0, weights = mydata$w, nfolds = 5, foldid = mydata$fold_id)
  cv_lambda0.25 = cv.glmnet(x = vars , y= response, family = family, alpha = 0.25, weights = mydata$w, nfolds = 5, foldid = mydata$fold_id)
  cv_lambda0.5 = cv.glmnet(x = vars , y= response, family = family, alpha = 0.5, weights = mydata$w, nfolds = 5, foldid = mydata$fold_id)
  cv_lambda0.75 = cv.glmnet(x = vars , y= response, family = family, alpha = 0.75, weights = mydata$w, nfolds = 5, foldid = mydata$fold_id)
  cv_lambda1 = cv.glmnet(x = vars , y= response, family = family, alpha = 1, weights = mydata$w, nfolds = 5, foldid = mydata$fold_id)
  
  #plot out the lambda and alpha options
  ##LOOK AT THESE PLOTS to select your prefered penalised regression model (cannot use multiple as they will be correlated)
  pdf(paste0(outputdir, '/glmnet/parameter_selection.pdf'))
    par(mfrow=c(3,2))
    plot(cv_lambda0)
    plot(cv_lambda0.25)
    plot(cv_lambda0.5)
    plot(cv_lambda0.75)
    plot(cv_lambda1)
    plot(log(cv_lambda0$lambda),cv_lambda0$cvm,pch=19,col="red",xlab="log(Lambda)",ylab=cv_lambda0$name)
    points(log(cv_lambda0.25$lambda),cv_lambda0.25$cvm,pch=19,col="pink")
    points(log(cv_lambda0.5$lambda),cv_lambda0.5$cvm,pch=19,col="blue")
    points(log(cv_lambda0.75$lambda),cv_lambda0.75$cvm,pch=19,col="yellow")
    points(log(cv_lambda1$lambda),cv_lambda1$cvm,pch=19,col="green")
    legend("bottomright",legend=c("alpha= 1","alpha= .75", "alpha= .5", "alpha= .25","alpha 0"),pch=19,col=c("green","yellow","blue","pink","red"))
  dev.off()
  
  #fit the full model using selected lambda and alpha
  if('ridge' %in% child_models){full_ridge = glmnet(x = vars , y= response, family = family, alpha = 0, weights = mydata$w)}
  if('enet' %in% child_models){full_enet = glmnet(x = vars , y= response, family = family, alpha = 0.5, weights = mydata$w)}
  if('lasso' %in% child_models){full_lasso = glmnet(x = vars , y= response, family = family, alpha = 1, weights = mydata$w)}
  
  #predict full model results (requires matrix)
  # used 'response' which gives the same results as inverse logit (link)
  if('ridge' %in% child_models){mydata[,'ridge_full_pred' := predict(full_ridge,newx = vars, s = cv_lambda0$lambda.1se, type = 'response')]}
  if('lasso' %in% child_models){mydata[,'lasso_full_pred' := predict(full_lasso,newx = vars, s = cv_lambda1$lambda.1se, type = 'response')]}
  if('enet' %in% child_models){mydata[,'enet_full_pred' := predict(full_enet,newx = vars, s = cv_lambda0.5$lambda.1se, type = 'response')]}
  
  #fit the model on the holdouts
  for(i in 1:5){
    if(family == 'binomial'){
    response <- cbind(failures = mydata$d[mydata$fold_id!=i] - mydata$n[mydata$fold_id!=i], 
                      successes = mydata$n[mydata$fold_id!=i])
    } else if(family == 'gaussian'){
      response <- mydata$n[mydata$fold_id!=i] 
    }
    
    vars <- as.matrix(mydata[fold_id != i, covs_to_include, with = F])
    colnames(vars) <- covs_to_include
    
    if('ridge' %in% child_models){baby_ridge = glmnet(x = vars , y= response, family = family, lambda = cv_lambda0$lambda.1se, alpha = 0, weights = mydata$w[mydata$fold_id!=i])}
    if('lasso' %in% child_models){baby_lasso = glmnet(x = vars , y= response, family = family, lambda = cv_lambda1$lambda.1se, alpha = 1, weights = mydata$w[mydata$fold_id!=i])}
    if('enet' %in% child_models){baby_enet = glmnet(x = vars , y= response, family = family, lambda = cv_lambda0.5$lambda.1se, alpha = 0.5, weights = mydata$w[mydata$fold_id!=i])}
    
    new_vars <- as.matrix(mydata[fold_id == i, covs_to_include, with = F])
    
    #fill in the data
    if('ridge' %in% child_models){mydata[fold_id==i,'ridge_cv_pred' := predict(baby_ridge,newx = new_vars, s = cv_lambda0$lambda.1se, type = 'response')]}
    if('lasso' %in% child_models){mydata[fold_id==i,'lasso_cv_pred' := predict(baby_lasso,newx = new_vars, s = cv_lambda1$lambda.1se, type = 'response')]}
    if('enet' %in% child_models){mydata[fold_id==i,'enet_cv_pred' := predict(baby_enet,newx = new_vars, s = cv_lambda0.5$lambda.1se, type = 'response')]}
  }
  
  #save the model and relevent coefficients
  if('ridge' %in% child_models){saveRDS(cv_lambda0, paste0(outputdir, '/glmnet/full_ridge.rds'))}
  if('enet' %in% child_models){saveRDS(cv_lambda0.5, paste0(outputdir, '/glmnet/full_enet.rds'))}
  if('lasso' %in% child_models){saveRDS(cv_lambda1, paste0(outputdir, '/glmnet/full_lasso.rds'))}
  
  #predict out for all locations
  all_names <- names(covs) 
  new_covs <- as.matrix(covs)
  names(new_covs) <- all_names
  if('ridge' %in% child_models){covs[,'ridge' := predict(full_ridge,newx = new_covs[,rownames(full_ridge$beta)], s = cv_lambda0$lambda.1se, type = 'response')]}
  if('enet' %in% child_models){covs[,'enet' := predict(full_enet,newx = new_covs[,rownames(full_enet$beta)], s = cv_lambda0.5$lambda.1se, type = 'response')]}
  if('lasso' %in% child_models){covs[,'lasso' := predict(full_lasso,newx = new_covs[,rownames(full_lasso$beta)], s = cv_lambda1$lambda.1se, type = 'response')]}
  
  rm(cv_lambda1, cv_lambda0.5, cv_lambda0, cv_lambda0.25, cv_lambda0.75, full_lasso, full_enet, full_ridge, baby_lasso, baby_ridge, baby_enet, new_vars, response, vars, i, new_covs, all_names)    
}

#~~~~~~~~~~~~~~~~~~~~~#
# 4. Random forest ####
#~~~~~~~~~~~~~~~~~~~~~#
if('rf' %in% child_models){
  dir.create(paste0(outputdir, '/rf'), showWarnings = F)
  
  # Create model formula
  if(family == 'binomial'){
    form <- as.formula(paste0('p ~ ', paste(covs_to_include, collapse = " + ")))
  } else if(family == 'gaussian'){
    form <- as.formula(paste0('n ~ ', paste(covs_to_include, collapse = " + ")))
  }
  
  train_control <- trainControl(selectionFunction = "best",
                                method = "repeatedcv",
                                number = 5,
                                repeats = 5,
                                index = list(mydata$a_rowid[mydata$fold_id!=1],
                                             mydata$a_rowid[mydata$fold_id!=2],
                                             mydata$a_rowid[mydata$fold_id!=3],
                                             mydata$a_rowid[mydata$fold_id!=4],
                                             mydata$a_rowid[mydata$fold_id!=5]),
                                indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                               mydata$a_rowid[mydata$fold_id==2],
                                               mydata$a_rowid[mydata$fold_id==3],
                                               mydata$a_rowid[mydata$fold_id==4],
                                               mydata$a_rowid[mydata$fold_id==5]),
                                search = 'grid')
  
  tunegrid <- expand.grid(.mtry=c(1:length(covs_to_include)))
  
  # Fit model
  rf_fit <- train(form,
                  data = mydata,
                  trControl = train_control,
                  verbose = T,
                  tuneGrid = tunegrid,
                  metric = "RMSE",
                  method = "rf",
                  weights = mydata$w)
  
  # Save model fit object 
  saveRDS(rf_fit, paste0(outputdir, "/rf/rf_fit.RDS"))
  png(paste0(outputdir, '/rf/rf_fit.png'))
  plot(rf_fit)  
  dev.off()
  
  #specify the parameters
  mtry_tune <- rf_fit$bestTune$mtry
  write.csv(mtry_tune, paste0(outputdir, '/rf/rf_params.csv'), row.names = F)
  tunegrid_final <- expand.grid(.mtry=mtry_tune)
  
  #specify the folds in the train control section
  train_control_final <- trainControl(method = "cv",
                                      number = 5,
                                      savePredictions = "final",
                                      index = list(mydata$a_rowid[mydata$fold_id!=1],
                                                   mydata$a_rowid[mydata$fold_id!=2],
                                                   mydata$a_rowid[mydata$fold_id!=3],
                                                   mydata$a_rowid[mydata$fold_id!=4],
                                                   mydata$a_rowid[mydata$fold_id!=5]),
                                      indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                                     mydata$a_rowid[mydata$fold_id==2],
                                                     mydata$a_rowid[mydata$fold_id==3],
                                                     mydata$a_rowid[mydata$fold_id==4],
                                                     mydata$a_rowid[mydata$fold_id==5]))
  
  
  #fit the final model
  rf_fit_final <- train(form,
                        data = mydata,
                        trControl = train_control_final,
                        verbose = T,
                        tuneGrid = tunegrid_final,
                        metric = "RMSE",
                        method = "rf",
                        importance=T,
                        weights = mydata$w)
  
  # Extract out of sample and in sample predictions
  mydata[, 'rf_cv_pred'   := arrange(rf_fit_final$pred, rowIndex)[,"pred"]]
  mydata[, 'rf_full_pred' := predict(rf_fit_final, mydata)]
  
  #save model fit
  rf_fit_final$model_name <- "rf"
  saveRDS(rf_fit_final, paste0(outputdir, '/rf/full_rf.RDS'))
  
  cov_plot <-
    ggplot(varImp(rf_fit_final, scale = FALSE)) +
    labs(x = "Covariate", y = "Relative Importance") +
    theme_bw()
  ggsave(filename = paste0(outputdir, '/rf/rf_covariate_importance.png'),
         plot = cov_plot)
  
  #predict out for all locations
  covs[, 'rf' := predict(rf_fit_final, covs)]
  
  rm(form, cov_plot, train_control, train_control_final, rf_fit, rf_fit_final, tunegrid, tunegrid_final)
}

#~~~~~~~~~~~~~~~~~~~~~~~#
# 5. Neural networks ####
#~~~~~~~~~~~~~~~~~~~~~~~#
if('nnet' %in% child_models){
  dir.create(paste0(outputdir, '/nnet'), showWarnings = F)
  # Create model formula
  if(family == 'binomial'){
    form <- as.formula(paste0('p ~ ', paste(covs_to_include, collapse = " + ")))
  } else if(family == 'gaussian'){
    form <- as.formula(paste0('n ~ ', paste(covs_to_include, collapse = " + ")))
  }
  
  train_control <- trainControl(selectionFunction = "best",
                                method = "repeatedcv",
                                number = 5,
                                repeats = 5,
                                index = list(mydata$a_rowid[mydata$fold_id!=1],
                                             mydata$a_rowid[mydata$fold_id!=2],
                                             mydata$a_rowid[mydata$fold_id!=3],
                                             mydata$a_rowid[mydata$fold_id!=4],
                                             mydata$a_rowid[mydata$fold_id!=5]),
                                indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                               mydata$a_rowid[mydata$fold_id==2],
                                               mydata$a_rowid[mydata$fold_id==3],
                                               mydata$a_rowid[mydata$fold_id==4],
                                               mydata$a_rowid[mydata$fold_id==5]),
                                search = 'grid')
  
  tunegrid <- expand.grid(.decay = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001), .size = c(4, 5, 6, 7, 8, 9))
  
  # Fit model
  nn_fit <- train(form,
                  data = mydata,
                  trControl = train_control,
                  verbose = T,
                  tuneGrid = tunegrid,
                  metric = "RMSE",
                  method = "nnet",
                  linout = TRUE,
                  maxit = 1000,
                  weights = mydata$w)
  
  # Save model fit object 
  saveRDS(nn_fit, paste0(outputdir, "/nnet/nn_fit.RDS"))
  png(paste0(outputdir, '/nnet/nn_fit.png'))
  plot(nn_fit)  
  dev.off()
  
  # Save the best parameters to csv file
  write.csv(nn_fit$bestTune, paste0(outputdir, '/nnet/nnet_best_tune.csv'))
  
  #specify the parameters
  tunegrid_final <- expand.grid(.decay=nn_fit$bestTune$decay, .size=nn_fit$bestTune$size)
  
  #specify the folds in the train control section
  train_control_final <- trainControl(method = "cv",
                                      number = 5,
                                      savePredictions = "final",
                                      index = list(mydata$a_rowid[mydata$fold_id!=1],
                                                   mydata$a_rowid[mydata$fold_id!=2],
                                                   mydata$a_rowid[mydata$fold_id!=3],
                                                   mydata$a_rowid[mydata$fold_id!=4],
                                                   mydata$a_rowid[mydata$fold_id!=5]),
                                      indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                                     mydata$a_rowid[mydata$fold_id==2],
                                                     mydata$a_rowid[mydata$fold_id==3],
                                                     mydata$a_rowid[mydata$fold_id==4],
                                                     mydata$a_rowid[mydata$fold_id==5]))
  
  
  #fit the final model
  nn_fit_final <- train(form,
                        data = mydata,
                        trControl = train_control_final,
                        verbose = T,
                        tuneGrid = tunegrid_final,
                        metric = "RMSE",
                        method = "nnet",
                        linout = TRUE,
                        maxit = 1000,
                        weights = mydata$w)
  
  # Extract out of sample and in sample predictions
  mydata[, 'nnet_cv_pred'   := arrange(nn_fit_final$pred, rowIndex)[,"pred"]]
  mydata[, 'nnet_full_pred' := predict(nn_fit_final, mydata)]
  
  #save model fit
  nn_fit_final$model_name <- "nn"
  saveRDS(nn_fit_final, paste0(outputdir, '/nnet/full_nn.RDS'))
  
  cov_plot <-
    ggplot(varImp(nn_fit_final, scale = FALSE)) +
    labs(x = "Covariate", y = "Relative Importance") +
    theme_bw()
  ggsave(filename = paste0(outputdir, '/nnet/nn_covariate_importance.png'),
         plot = cov_plot)
  
  #predict out for all locations
  covs[, 'nnet' := predict(nn_fit_final, covs)]
  
  rm(nn_fit, nn_fit_final, train_control, train_control_final, tunegrid, tunegrid_final, cov_plot)
}

#~~~~~~~~~~~~~~~~~~~~#
# 6. Cubist model ####
#~~~~~~~~~~~~~~~~~~~~#
if('cubist' %in% child_models){
  dir.create(paste0(outputdir, '/cubist'), showWarnings = F)
  
  # Create model formula
  if(family == 'binomial'){
    form <- as.formula(paste0('p ~ ', paste(covs_to_include, collapse = " + ")))
  } else if(family == 'gaussian'){
    form <- as.formula(paste0('n ~ ', paste(covs_to_include, collapse = " + ")))
  }
  
  train_control <- trainControl(selectionFunction = "best",
                                method = "repeatedcv",
                                number = 5,
                                repeats = 5,
                                index = list(mydata$a_rowid[mydata$fold_id!=1],
                                             mydata$a_rowid[mydata$fold_id!=2],
                                             mydata$a_rowid[mydata$fold_id!=3],
                                             mydata$a_rowid[mydata$fold_id!=4],
                                             mydata$a_rowid[mydata$fold_id!=5]),
                                indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                               mydata$a_rowid[mydata$fold_id==2],
                                               mydata$a_rowid[mydata$fold_id==3],
                                               mydata$a_rowid[mydata$fold_id==4],
                                               mydata$a_rowid[mydata$fold_id==5]),
                                search = 'grid')
  
  tunegrid <- expand.grid(.committees = c(seq(1, 40, 5)), 
                          .neighbors  = c(0, 3, 6, 9))
  
  # Fit model
  cubist_fit <- train(form,
                      data = mydata,
                      trControl = train_control,
                      verbose = T,
                      tuneGrid = tunegrid,
                      metric = "RMSE",
                      method = "cubist",
                      control = Cubist::cubistControl(),
                      weights = mydata$w)
  
  # Save model fit object 
  saveRDS(cubist_fit, paste0(outputdir, "/cubist/cubist_fit.RDS"))
  png(paste0(outputdir, '/cubist/cubist_fit.png'))
  plot(cubist_fit)  
  dev.off()
  
  # Save the best parameters to csv file
  write.csv(cubist_fit$bestTune, paste0(outputdir, '/cubist/cubist_best_tune.csv'))
  
  #specify the parameters
  tunegrid_final <- expand.grid(.committees=cubist_fit$bestTune$committees, .neighbors=cubist_fit$bestTune$neighbors)
  
  #specify the folds in the train control section
  train_control_final <- trainControl(method = "cv",
                                      number = 5,
                                      savePredictions = "final",
                                      index = list(mydata$a_rowid[mydata$fold_id!=1],
                                                   mydata$a_rowid[mydata$fold_id!=2],
                                                   mydata$a_rowid[mydata$fold_id!=3],
                                                   mydata$a_rowid[mydata$fold_id!=4],
                                                   mydata$a_rowid[mydata$fold_id!=5]),
                                      indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                                     mydata$a_rowid[mydata$fold_id==2],
                                                     mydata$a_rowid[mydata$fold_id==3],
                                                     mydata$a_rowid[mydata$fold_id==4],
                                                     mydata$a_rowid[mydata$fold_id==5]))
  
  
  #fit the final model
  cubist_fit_final <- train(form,
                            data = mydata,
                            trControl = train_control_final,
                            verbose = T,
                            tuneGrid = tunegrid_final,
                            metric = "RMSE",
                            method = "cubist",
                            weights = mydata$w)
  
  # Extract out of sample and in sample predictions
  mydata[, 'cubist_cv_pred'   := arrange(cubist_fit_final$pred, rowIndex)[,"pred"]]
  mydata[, 'cubist_full_pred' := predict(cubist_fit_final, mydata)]
  
  #save model fit
  cubist_fit_final$model_name <- "cubist"
  saveRDS(cubist_fit_final, paste0(outputdir, '/cubist/full_cubist.RDS'))
  
  cov_plot <-
    ggplot(varImp(cubist_fit_final, scale = FALSE)) +
    labs(x = "Covariate", y = "Relative Importance") +
    theme_bw()
  ggsave(filename = paste0(outputdir, '/cubist/cubist_covariate_importance.png'),
         plot = cov_plot)
  
  #predict out for all locations
  covs[, 'cubist' := predict(cubist_fit_final, covs)]

  rm(form, cov_plot, train_control, train_control_final, cubist_fit, cubist_fit_final, tunegrid, tunegrid_final)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Check the correlation of the stackers ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#if any of the preds are highly corelted the remove one of the correlated models

for(i in 1:length(child_models)){
  for(j in 1:length(child_models)){
    if(i==j){
    } else{
      if(cor(mydata[,get(paste0(child_models[i], '_cv_pred'))],mydata[,get(paste0(child_models[j], '_cv_pred'))])^2>0.8){message(paste0(child_models[i],  ' and ', child_models[j], ' correlated, remove one'))}
    }
  }
}
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Print out correlations between data and predictions ####
# this is to aid selection of child models               #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

for(i in 1:length(child_models)){
  if(family == 'binomial'){
    message(paste0(child_models[i], ' correlation: ', round(cor(mydata$p, mydata[,get(paste0(child_models[i], '_cv_pred'))])^2,2)))
  }
  if(family == 'gaussian'){
    message(paste0(child_models[i], ' correlation: ', round(cor(mydata$n, mydata[,get(paste0(child_models[i], '_cv_pred'))])^2,2)))
  }
}
 
#save the temporary files
if(centre_scale == TRUE){
  mydata$year <-  NULL
  covs$year <- NULL
}

write.csv(mydata, paste0(outputdir, '/fitted_child_models.csv'), row.names = F)
write.csv(covs, paste0(outputdir, '/child_model_preds.csv'), row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Combined the child model estimates ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# what methods to use to create the stackers
# use either a GPR or constrained weighted mean or other model
# ensure beta coefficients are contrained to sum to 1

# specify child models to use (if need to remove some due to correlation), want max 5ish, remove others from dataframe
# child_models <- child_models[child_models!='xgboost']
# child_models <- child_models[child_models!='gam']
# child_models <- child_models[child_models!='lasso']
# child_models <- child_models[child_models!='ridge']
# child_models <- child_models[child_models!='enet']
# child_models <- child_models[child_models!='rf']
# child_models <- child_models[child_models!='nnet']
# child_models <- child_models[child_models!='cubist']

#remove unwanted child models from the data frame to calculate child model weights
stackers <- data.table(mydata)
# stackers[, (colnames(stackers)[grep('xgboost', colnames(stackers))]) := NULL]
# stackers[, (colnames(stackers)[grep('gam', colnames(stackers))]) := NULL]
# stackers[, (colnames(stackers)[grep('lasso', colnames(stackers))]) := NULL]
# stackers[, (colnames(stackers)[grep('ridge', colnames(stackers))]) := NULL]
# stackers[, (colnames(stackers)[grep('enet', colnames(stackers))]) := NULL]
# stackers[, (colnames(stackers)[grep('rf', colnames(stackers))]) := NULL]
# stackers[, (colnames(stackers)[grep('nnet', colnames(stackers))]) := NULL]
# stackers[, (colnames(stackers)[grep('cubist', colnames(stackers))]) := NULL]

stackers <- data.frame(stackers)
X <- as.matrix(stackers[colnames(stackers)[(grep('cv_pred', colnames(stackers)))]])
Y = if(family == 'binomial'){stackers$p}else if(family == 'gaussian'){stackers$n}

#stack the predictions for all locations
C <- data.frame(covs)
C <- as.matrix(C[c(child_models)])

#Select which stacker you are using and stack the estimates

if(stacker == 'CWM'){
  # The following code is from http://zoonek.free.fr/blosxom/R/2012-06-01_Optimization.html
  # Calculate coefficients (child stacker weights)
  # Coefficients must sum to 1 and >=0
  s <- solve.QP( 
    t(X) %*% X, t(Y) %*% X, 
    cbind(  # One constraint per COLUMN
      matrix(1, nr=length(child_models), nc=1),
      diag(length(child_models)),
      -diag(length(child_models))
    ),
    c(1, 
      rep(0.000001, length(child_models)),
      rep(-1, length(child_models))), 
    meq = 1 # Only the first constraint is an equality if meq = 1 so set to 0, the others are >=
  )
  
  #calculate weighted stackers
  #for the fits with data
  mydata$stacked_preds <- rowWeightedMeans(X, w = s$solution)   # or can use: mydata$crossprod <- crossprod(t(X), s$solution)
  
  #Calculate the stacked predictions
  covs$cv_custom_stage_1 <- rowWeightedMeans(C, w = s$solution)
}

if(stacker == 'RWM'){
  r2 <- rep(NA, length(child_models))
  for(i in 1:length(child_models)){
    if(family == 'binomial'){
      r2[i] <- round(cor(mydata$p, mydata[,get(paste0(child_models[i], '_cv_pred'))])^2,2)
    }
    if(family == 'gaussian'){
      r2[i] <- round(cor(mydata$n, mydata[,get(paste0(child_models[i], '_cv_pred'))])^2,2)
    }
  }
  
  total <-  sum(r2)
  weights <- r2/total
  mydata$stacked_preds <- rowWeightedMeans(X, w = weights)   
  covs$cv_custom_stage_1 <- rowWeightedMeans(C, w = weights)
}

if(stacker == 'GBM'){
  if(family == 'gaussian'){
    form <- as.formula(paste0('n ~ ', paste(colnames(stackers)[(grep('cv_pred', colnames(stackers)))], collapse = " + ")))
  }
  if(family == 'binomial'){
    form <- as.formula(paste0('p ~ ', paste(colnames(stackers)[(grep('cv_pred', colnames(stackers)))], collapse = " + ")))
  }  
  train_control_final <- trainControl(method = "cv",
                                      number = 5,
                                      savePredictions = "final",
                                      index = list(mydata$a_rowid[mydata$fold_id!=1],
                                                   mydata$a_rowid[mydata$fold_id!=2],
                                                   mydata$a_rowid[mydata$fold_id!=3],
                                                   mydata$a_rowid[mydata$fold_id!=4],
                                                   mydata$a_rowid[mydata$fold_id!=5]),
                                      indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                                     mydata$a_rowid[mydata$fold_id==2],
                                                     mydata$a_rowid[mydata$fold_id==3],
                                                     mydata$a_rowid[mydata$fold_id==4],
                                                     mydata$a_rowid[mydata$fold_id==5]))
  
  model_gbm<- 
    train(form, data = mydata, method='gbm', trControl=train_control_final, tuneLength=3)
  
  # mydata[, 'stacked_preds'   := arrange(model_gbm$pred, rowIndex)[,"pred"]]
  mydata[, 'stacked_preds'   := predict(model_gbm, mydata)]
  
  covs <- data.frame(covs)
  colnames(covs)[colnames(covs) %in% child_models] <- colnames(stackers)[(grep('cv_pred', colnames(stackers)))]
  covs$cv_custom_stage_1 <- predict(model_gbm, covs[colnames(covs)[(grep('cv_pred', colnames(covs)))]]) 
  covs <- data.table(covs)
  
  #save the covariate importance plot
  jpeg(paste0(outputdir, 'stacker_cov_imporatance.jpeg'),
       height = 10, width = 10, units = 'cm', res = 150)
  ggplot(varImp(model_gbm, scale = FALSE)) +
  labs(x = "Covariate", y = "Relative Importance") +
  theme_bw()
  
  dev.off()
}
  
if(stacker == 'GLM'){
  if(family == 'gaussian'){
    form <- as.formula(paste0('n ~ ', paste(colnames(stackers)[(grep('cv_pred', colnames(stackers)))], collapse = " + ")))
  }
  if(family == 'binomial'){
    form <- as.formula(paste0('p ~ ', paste(colnames(stackers)[(grep('cv_pred', colnames(stackers)))], collapse = " + ")))
  }  
  train_control_final <- trainControl(method = "cv",
                                      number = 5,
                                      savePredictions = "final",
                                      index = list(mydata$a_rowid[mydata$fold_id!=1],
                                                   mydata$a_rowid[mydata$fold_id!=2],
                                                   mydata$a_rowid[mydata$fold_id!=3],
                                                   mydata$a_rowid[mydata$fold_id!=4],
                                                   mydata$a_rowid[mydata$fold_id!=5]),
                                      indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                                     mydata$a_rowid[mydata$fold_id==2],
                                                     mydata$a_rowid[mydata$fold_id==3],
                                                     mydata$a_rowid[mydata$fold_id==4],
                                                     mydata$a_rowid[mydata$fold_id==5]))
  
  model_glm<- 
    train(form, data = mydata, method='glm', trControl=train_control_final, tuneLength=3)
 
    # mydata[, 'stacked_preds'   := arrange(model_glm$pred, rowIndex)[,"pred"]]
    mydata[, 'stacked_preds'   := predict(model_glm, mydata)]
    
  covs <- data.frame(covs)
  colnames(covs)[colnames(covs) %in% child_models] <- colnames(stackers)[(grep('cv_pred', colnames(stackers)))]
  covs$cv_custom_stage_1 <- predict(model_gbm, covs[colnames(covs)[(grep('cv_pred', colnames(covs)))]]) 
  covs <- data.table(covs)
}

if(stacker == 'nnet'){
  if(family == 'gaussian'){
    form <- as.formula(paste0('n ~ ', paste(colnames(stackers)[(grep('cv_pred', colnames(stackers)))], collapse = " + ")))
  }
  if(family == 'binomial'){
    form <- as.formula(paste0('p ~ ', paste(colnames(stackers)[(grep('cv_pred', colnames(stackers)))], collapse = " + ")))
  }  
  train_control_final <- trainControl(method = "cv",
                                      number = 5,
                                      savePredictions = "final",
                                      index = list(mydata$a_rowid[mydata$fold_id!=1],
                                                   mydata$a_rowid[mydata$fold_id!=2],
                                                   mydata$a_rowid[mydata$fold_id!=3],
                                                   mydata$a_rowid[mydata$fold_id!=4],
                                                   mydata$a_rowid[mydata$fold_id!=5]),
                                      indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                                     mydata$a_rowid[mydata$fold_id==2],
                                                     mydata$a_rowid[mydata$fold_id==3],
                                                     mydata$a_rowid[mydata$fold_id==4],
                                                     mydata$a_rowid[mydata$fold_id==5]))
  
  model_nnet<- 
    train(form, data = mydata, method='nnet', trControl=train_control_final, tuneLength=3)
  
  # mydata[, 'stacked_preds'   := arrange(model_nnet$pred, rowIndex)[,"pred"]]
  mydata[, 'stacked_preds'   := predict(model_nnet, mydata)]
  
  covs <- data.frame(covs)
  colnames(covs)[colnames(covs) %in% child_models] <- colnames(stackers)[(grep('cv_pred', colnames(stackers)))]
  covs$cv_custom_stage_1 <- predict(model_gbm, covs[colnames(covs)[(grep('cv_pred', colnames(covs)))]]) 
  covs <- data.table(covs)
}

#clean up the covs dataset
stg1 <-  covs[, .(location_id, year_id,
                  age_group_id = rep(22, length(covs$location_id)),
                  sex_id = rep(3, length(covs$location_id)),
                  cv_custom_stage_1 = cv_custom_stage_1)]

#remove covariates from the dataset
mydata[, colnames(mydata)[grep('^cv_', colnames(mydata))] := NULL]

#save prediction
write.csv(mydata, paste0(outputdir, '/fitted_stackers.csv'), row.names = F)
write.csv(stg1, paste0(outputdir, '/custom_stage1_df.csv'), row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot out the is and oos preds ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#out of sample plots
for(stack in c(child_models, 'stacked_preds')){
  png(paste0(outputdir, '/', stack, '_oos.png'),
      height = 12, width = 12, res = 350, unit = 'in')
  print(
    ggplot(mydata)+
      geom_point(aes(x = if(family == 'binomial'){p}else if(family == 'gaussian'){n},
                     y=if(stack == 'stacked_preds'){stacked_preds} else{get(paste0(stack, '_cv_pred'))}))+
      geom_abline(slope = 1, intercept = 0, colour = 'red')+
      theme_bw() +
      theme(strip.background = element_rect(fill = "white")) +
      labs(
        x = "Data Estimate",
        y = "Mean Prediction",
        size = "Weight",
        title = (paste0("Validation Plot for ", stack)),
        subtitle = "OOS: TRUE")
  )
  dev.off()
}

#in sample plots
for(stack in child_models){
  png(paste0(outputdir, '/', stack, '_is.png'),
      height = 12, width = 12, res = 350, unit = 'in')
  print(
    ggplot(mydata)+
      geom_point(aes(x = if(family == 'binomial'){p}else if(family == 'gaussian'){n},
                     y=get(paste0(stack, '_full_pred'))))+
      geom_abline(slope = 1, intercept = 0, colour = 'red')+
      theme_bw() +
      theme(strip.background = element_rect(fill = "white")) +
      labs(
        x = "Data Estimate",
        y = "Mean Prediction",
        size = "Weight",
        title = (paste0("Validation Plot for ", stack)),
        subtitle = "OOS: FALSE")
  )
  dev.off()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Calculate metrics for all models ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#get the RMSE and R2 of each model into a data frame and save it
child_model_metrics <- data.frame(child_models) 
child_model_metrics$rmse <- NA
child_model_metrics$r_sq <- NA

mydata <- data.frame(mydata)
for(i in 1:length(child_models)){
  cm <- paste0(child_models, '_cv_pred')[i]
  pred <- mydata[c(cm)]
  pred <-  unlist(pred)
  child_model_metrics$rmse[child_model_metrics$child_models == child_models[i]] <- round(RMSE(pred, if(family == 'binomial'){mydata$p}else if(family == 'gaussian'){mydata$n}),4)
  child_model_metrics$r_sq[child_model_metrics$child_models == child_models[i]] <- round(cor(pred, if(family == 'binomial'){mydata$p}else if(family == 'gaussian'){mydata$n})^2,2)
}

child_model_metrics$child_models <- as.character(child_model_metrics$child_models)
child_model_metrics <- rbind(child_model_metrics, c('Stackers', NA, NA) )
child_model_metrics$rmse[child_model_metrics$child_models=='Stackers'] <- round(RMSE(mydata$stacked_preds, if(family == 'binomial'){mydata$p}else if(family == 'gaussian'){mydata$n}),4)
child_model_metrics$r_sq[child_model_metrics$child_models=='Stackers'] <- round(cor(mydata$stacked_preds, if(family == 'binomial'){mydata$p}else if(family == 'gaussian'){mydata$n})^2,2)

write.csv(child_model_metrics, paste0(outputdir, '/national_stacker_metrics.csv'), row.names = F)

#~~~~~~~~~~~~~~~~~~~~~#
# Plot the results ####
#~~~~~~~~~~~~~~~~~~~~~#
locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')
covs <- data.table(covs)

if(transformation == 'log'){
  covs[, gam := exp(gam)]
  covs[, ridge := exp(ridge)]
  # covs[, rf := exp(rf)]
  covs[, nnet := exp(nnet)]
  covs[, cubist := exp(cubist)]
  covs[, xgboost := exp(xgboost)]
  covs[, cv_custom_stage_1 := exp(cv_custom_stage_1)]
}

if(transformation == 'logit'){
  covs[, gam := inv.logit(gam)]
  covs[, enet := inv.logit(enet)]
  covs[, rf := inv.logit(rf)]
  covs[, nnet := inv.logit(nnet)]
  covs[, cubist := inv.logit(cubist)]
  covs[, xgboost := inv.logit(xgboost)]
  covs[, cv_custom_stage_1 := inv.logit(cv_custom_stage_1)]
}


#merge on the data points
mydata <- data.frame(mydata)
input <- mydata[c('location_id', 'year_id', 'n')]
covs <- merge(covs, input, by = c('location_id', 'year_id'), all.x = T, all.y = T, allow.cartesian = T)

#reshape long
covs <- melt(covs, id.vars = c('location_id', 'year_id', 'n'),
             measure.vars = c(child_models, 'cv_custom_stage_1'))

#merge on locations
covs <- merge(covs, locs, by.x = 'location_id', by.y = 'loc_id', all.x = T, all.y = F)
covs <- covs[covs$level == 3,]

# Plot out the stg1s data and check
pdf(paste0(outputdir, '/stacker_results.pdf'),
    height = 8.3, width = 11.7)

#plot out a page for each region
for(i in 1:length(unique(covs$region_id))){
  subset <- covs[covs$region_id == unique(covs$region_id)[i],]
  print(
    ggplot(subset)+
      geom_line(aes(x=year_id, y = value, group = variable, colour = variable))+
      geom_point(aes(x=year_id, y = exp(n)))+
      facet_wrap(~ihme_lc_id, nrow = ceiling(sqrt(length(unique(subset$location_id)))))+
      ylim(0, max(covs$value))
  )
}
dev.off()

#~~~~~#
# END #
#~~~~~#