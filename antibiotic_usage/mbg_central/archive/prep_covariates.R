# run gam, save as tranformed

library(raster, lib.loc = package_lib)
library(data.table, lib.loc = package_lib)
library(seegSDM, lib.loc = package_lib)
library(seegMBG, lib.loc = package_lib)

message('Loading raw covariate maps')

input_cov_dir <- paste0(root,'WORK/11_geospatial/01_covariates/08_processed_Africa_covariates') # Lucas says this is where he's keeping all the working layers for now.

# temporally-varying covariates
evi             <- brick(paste0(root,'/temp/learl/U5M/covs_20160617/EVI_stack.tif'))
lights_new      <- brick(paste0(root,'/temp/learl/U5M/covs_20160617/NTL_stack.tif'))
LST_day         <- brick(paste0(root,'/temp/learl/U5M/covs_20160617/LST_stack.tif'))
total_pop       <- brick(paste0(root,'/temp/learl/U5M/covs_20160617/pop_stack.tif'))
# temporally-nonvarying covariates
access          <- brick(paste0(input_cov_dir, '/synoptic_stack.tif'))$synoptic_stack.1
irrigation      <- brick(paste0(input_cov_dir, '/synoptic_stack.tif'))$synoptic_stack.2

# bring in the processed data for the GAM
df = fread(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/input_data.csv'))

# Add names to layers
names(access) <- "access"
names(irrigation) <- "irrigation"
for(c in c('evi','lights_new','LST_day','total_pop')){
  tmp=get(c)
  names(tmp)=rep(paste0(c,'.',1:4))
  assign(c,tmp)
}

# Construct list of covariates to GAM and use in model from fixed_effects parameter equation.
selected_covs <- strsplit(fixed_effects," ")
selected_covs <- selected_covs[[1]][selected_covs[[1]] != "+"]
num_covs <- length(selected_covs)
lcovs <- list()
for(i in 1:num_covs) {
  this_cov <- selected_covs[i]
  if(this_cov %in% c('access','irrigation')) { # Add if it is from the temporally non-varying list.
    lcovs[[i]] <- get(this_cov)
  }
  if(this_cov %in% c('evi','lights_new','LST_day','total_pop')) { # Add if it is from the temporally varying list.
    lcovs[[i]] <- get(this_cov)
  }
  names(lcovs)[i] <- this_cov
}

# crop and mask out to only area you are modeling for
template <- raster(paste0(template_dir,'/template5kdata'))
for(l in 1:length(lcovs)) {
  lcovs[[l]]  <- setExtent( lcovs[[l]], template)
  lcovs[[l]]  <- resample(lcovs[[l]],template)
  lcovs[[l]]  <- mask(crop( lcovs[[l]], template),template)
}
  


# ~~~~~~~~~~~~
# get required columns: named like U5m for ease of use

coords <- df[, c('longitude', 'latitude'), with=FALSE]
coords$lat=as.numeric(coords$latitude)
coords$long=as.numeric(coords$longitude)
coords <- coords[, c('long', 'lat'), with=FALSE]

response <- cbind(died = df[, get(indicator)],
                 lived = df[, N] - df[, get(indicator)])


extra_data <- data.frame(year = df$year)
# ~~~~~~~~~~~


# ~~~~~~~~~~~
# fit gam 

# using my own function for now that can take temporally varying covariates, 
# TODO: will need to send pull request to seegMBG of github
source('mbg_central/seegMBG_transform_functions.R')


# This should take a few minutes
system.time(trans <- gamTrans(coords=coords,
                              response=response,
                              covs=lcovs,
                              family = binomial,
                              extra_terms = ~ year,
                              extra_data = extra_data,
                              bam = TRUE,
                              predict = TRUE,
                              condition = NULL,
                              condition_covs = NULL,
                              s_args = list(bs = 'ts', k = 3),
                              samfrac = 0.1,
                              use.chol = TRUE))





# first, save gam output
m <- trans$model
cov_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/covs')
save(m,
     file = paste0(cov_dir,'/gam_covs.RData'))


### IF trans$trans IS RETURNED AS A LIST, IT IS SPLIT INTO TEMPORAL AND NON-TEMPORAL COVARIATES
if(class(trans$trans)=='list') {
  temporal=TRUE
} else {
  temporal=FALSE
}


message("CLAMP AND SAVE")
if(!temporal){ 
  # THEY ALL PASS
  # use chi-squared stats to determine covariate usefulness
  keep <- which(summary(trans$model)$chi.sq > 0.1)
  trans_ras <- trans$trans[[keep]]
  
  
  # clamp covariates
  # find most extreme vaaues of transofmred covariates that were observed
  vals <- extract(trans_ras, coords[idx_fit, ])
  sry <- apply(vals, 2, range, na.rm = TRUE)
  
  
  # clamp the covariates to these values
  for (i in 1:nlayers(trans_ras)) {
    range <- sry[, colnames(sry) == names(trans_ras)[i]]
    trans_ras[[i]][trans_ras[[i]] < range[1]] <- range[1]
    trans_ras[[i]][trans_ras[[i]] > range[2]] <- range[2]
  }
  
  
  # save these
  writeRaster(trans_ras,
              file = paste0(cov_dir, '/covs_transformed'),
              overwrite = TRUE)
  
  
}



# temporally varying covariates are present, save them all separately
# non varying ones will be save in the same covs_transformed location as before
if(temporal){
  
  
  # first clamp and save non temporally varying
  message('time invariant covariates')
  trans_ras=trans$trans$nT_vals_trans
  # clamp covariates
  # find most extreme vaaues of transofmred covariates that were observed
  vals <- extract(trans_ras, coords)
  sry <- apply(vals, 2, range, na.rm = TRUE)
  
  
  # clamp the covariates to these values
for (i in 1:nlayers(trans_ras)) {
  range <- sry[, colnames(sry) == names(trans_ras)[i]]
  trans_ras[[i]][trans_ras[[i]] < range[1]] <- range[1]
  trans_ras[[i]][trans_ras[[i]] > range[2]] <- range[2]
}
  
  # If you only specify one non-varying term, it simply gets name "layer" in the GAM function. Rather than fixing it in there 
  # I'm just going to check if that's the case and rename it here.
  if(length(names(trans_ras))==1) {
    for(cov in c('access','irrigation')) {
      if(cov %in% fixed_effects) names(trans_ras) <- cov 
    }
  }
  
  # save these
  writeRaster(trans_ras,
              file = paste0(cov_dir, '/covs_transformed'),
              overwrite = TRUE)
  
  
  # Now, this same process for the individual temporally varying covariates
  for(n in names(trans$trans$T_vals_trans)){
    message(n)
    
    trans_ras=trans$trans$T_vals_trans[[n]]
    
    # clamp covariates
    # find most extreme vaaues of transformed covariates that were observed
    vals <- extract(trans_ras, coords)
    sry <- apply(vals, 2, range, na.rm = TRUE)
    
    
    # clamp the covariates to these values
    for (i in 1:nlayers(trans_ras)) {
      range <- sry[, colnames(sry) == names(trans_ras)[i]]
      trans_ras[[i]][trans_ras[[i]] < range[1]] <- range[1]
      trans_ras[[i]][trans_ras[[i]] > range[2]] <- range[2]
    }
    
    
    # save these
    writeRaster(trans_ras,
                file = paste0(cov_dir,'/time_varying_covs_transformed_',n),
                overwrite = TRUE)
    
  }
  
  
}




















