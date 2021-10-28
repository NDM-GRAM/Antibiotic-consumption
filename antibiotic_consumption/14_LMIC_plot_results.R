#~~~~~~~~~~~~~~~~~~~~~~~#
# Plot STGPR results ####
#~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(ini, lib = '/share/homes/annieb6/temp_packages/')
library(foreign)
library(ggplot2)
library(raster)
library(gridExtra)
library(latticeExtra)
library(RColorBrewer)

source('/share/code/st_gpr/central/stgpr/r_functions/utilities/utility.r')

# Run details
run_id           <- 164387
indicator        <- 'abx_ddd'
min_year         <-  2000
max_year         <-  2018
family           <- 'gaussian'    #set up for binomial or gaussian data
transformation   <-  'log'        # set up for logit, log or NULL

#read in a file with the locations you are modelling over
# locs             <- read.csv('/ihme/homes/annieb6/AMR/misc/LMICs.csv')
locs             <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')

# Read in models results
gpr <- model_load(run_id, 'gpr')
st <- model_load(run_id, 'st')
lm <- model_load(run_id, 'stage1')
max_val <- ceiling(max(gpr$gpr_mean))
  
# back transform the estimates
if(transformation == 'logit'){
  st$st <- inv.logit(st$st)
  lm$stage1 <- inv.logit(lm$stage1)
}

if(transformation == 'log'){
  st$st <- exp(st$st)
  lm$stage1 <- exp(lm$stage1)
}

# Join the data together
mydata <- merge(gpr, st, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'))
mydata <- merge(mydata, lm, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'))

# Read in input data
params <- model_load(run_id, 'parameters')
input_data <- read.csv(params$path_to_data)
rm(params, lm, st)

# Merge the locations onto the data
locs  <- locs[c("loc_id", "loc_name", "spr_reg_id", "region_id", 'level', 'ihme_lc_id')]
mydata <- merge(mydata, locs, by.x = 'location_id', by.y = 'loc_id')
gpr <- merge(gpr, locs, by.x = 'location_id', by.y = 'loc_id')

# Calculate confidence intervals for the input data and bound to sensible values
if(family == 'binomial'){
  input_data$upper_ci <- input_data$val+(1.96*sqrt(input_data$var))
  input_data$lower_ci <- input_data$val-(1.96*sqrt(input_data$var))
  input_data$lower_ci[input_data$lower_ci<0] <- 0
  input_data$upper_ci[input_data$upper_ci>1] <- 1
}

if(family == 'gaussian'){
  input_data$upper_ci <- input_data$val+(1.96*sqrt(input_data$val))
  input_data$lower_ci <- input_data$val-(1.96*sqrt(input_data$val))
}
  
input_data <- input_data[c("location_id", "year_id", 'age_group_id', 'sex_id', "val", "upper_ci", "lower_ci")] 

#merge input data onto output data
mydata <- merge(input_data, mydata, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T, all.y = T)

#order data by alphabetical country and region
mydata <- mydata[order(mydata$spr_reg_id, mydata$region_id, mydata$ihme_lc_id),]
mydata <- mydata[!is.na(mydata$gpr_mean),]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot the data as scatterplots ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#split into national and subnational plots
national <- mydata[mydata$level == 3,]

pdf(paste0('/ihme/covariates/ubcov/model/output/', run_id, '/National_GPR_time_series.pdf'),
    height = 8.3, width = 11.7)

#plot out a page for each region
for(i in 1:length(unique(national$region_id))){
  subset <- national[national$region_id == unique(national$region_id)[i],]
  print(
    ggplot(subset)+
      geom_line(aes(x=year_id, y = gpr_mean),color = 'green')+
      geom_ribbon(aes(ymin = gpr_lower, ymax=gpr_upper, x = year_id), alpha = 0.1, fill = 'green') +
      geom_line(aes(x=year_id, y = st), color = 'blue')+
      geom_line(aes(x=year_id, y = stage1), color = 'red')+
      geom_pointrange(aes(x=year_id, y = val, ymin = lower_ci, ymax = upper_ci, fill = 'grey', alpha = 0.5)) +
      scale_x_continuous("Year", 
                         breaks = seq(min_year, max_year, 5),
                         labels = seq(min_year, max_year, 5))+
      ylim(0,max(national$gpr_upper))+
      ylab(indicator)+
      theme_bw()+
      scale_colour_manual(name = 'Model', values=vars)+
      theme(legend.position = "bottom")+
      ggtitle(unique(subset$region_id))+      
      facet_wrap(~ihme_lc_id, nrow = ceiling(sqrt(length(unique(subset$location_id)))))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(plot.title = element_text(hjust = 0.5))
  )
}
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot the in sample correlation ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
max(mydata$gpr_mean)
max(mydata$val, na.rm = T)
is_rmse <- RMSE(mydata$val[!is.na(mydata$val)], mydata$gpr_mean[!is.na(mydata$val)])
is_r2 <- cor(mydata$val[!is.na(mydata$val)], mydata$gpr_mean[!is.na(mydata$val)])^2

png(paste0('/ihme/covariates/ubcov/model/output/', run_id, '/LMIC_is_correlation.png'),
    height = 12, width = 12, res = 350, unit = 'in')
print(
  ggplot(mydata)+
    geom_point(aes(x = val, y=gpr_mean))+
    xlim(1000, 22000)+
    ylim(1000, 22000)+
    geom_abline(slope = 1, intercept = 0, colour = 'red')+
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    labs(
      x = "Data Estimate",
      y = "Mean Prediction",
      size = "Weight",
      title = ("IS Validation Plot for Stacked Ensemble-STGPR"),
      subtitle = paste0("RMSE = ", round(is_rmse,0), ";    R2 = ", round(is_r2,2)))

) 
dev.off()

#~~~~~~~~~~~~~~~~~~~~~#
# Save the results ####
#~~~~~~~~~~~~~~~~~~~~~#
mydata <- data.table(mydata)
mydata <- mydata[,.(location_id, year_id, age_group_id, sex_id, gpr_mean, gpr_lower, gpr_upper)]
write.csv(mydata, paste0('/ihme/covariates/ubcov/model/output/', run_id, '/model_estimates.csv'), row.names = F)



