# Run MBG for Africa
# Master script to reproduce full analysis (runs scripts in the right order)

root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")

# Load miscellaneous project functions.
package_lib <- paste0(root,'/temp/geospatial/packages') # Library for all MBG versioned packages. Ensures that none of this code is dependent on the machine where the user runs the code.
.libPaths(package_lib) # Ensures packages look for dependencies here when called with library(). Necessary for seeg libraries.
source('mbg_central/functions.R') # Other MBG-related functions.

library(data.table, lib.loc = package_lib)

# Call all folders/files we want to time-stamp "new_model", and then run a script at the very end of the process to go back and time-stamp all relevant folders/files. This way we can use hours/minutes in our time-stamp without risk of them changing during the time it takes everything to run and causing some path to break.
run_date <- 'new_model' 

# Read config file
setwd(repo)
config <- fread(paste0(indicator_group, '/config.csv'), header=FALSE)

# ~~~~~~~~

# Process parameters from config file
for(param in config[, V1]) {
  assign(param, config[V1==param, V2])
}
# Update later
target_country <- 'Africa'

# Set up folder structure
indicator_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator)
for(dir in c('templates','covs','output','model_image_history')) {
  dir.create(paste0(indicator_dir,'/',dir), showWarnings = FALSE)
}
template_dir <- paste0(indicator_dir,'/templates')
cov_dir <- paste0(indicator_dir,'/covs')

# 1. Initialize folder structure for shapefiles. Make all necessary template raster/shapefiles for meshes/predicting/plotting.
source('mbg_central/make_template_raster.R')

# 2. Build meshes for the GMRF approximation.
source('mbg_central/build_meshes.R')

# 3. Process covariates (must run GAMs by indicator).
source('mbg_central/prep_covariates.R')

# 4. Run MBG model
source('mbg_central/run_MBG.R')
  # Wait for model to finish running before moving on to next steps:
  message("Waiting for model to complete...")
  while(!file.exists(paste0('/share/geospatial/mbg/',indicator_group,'/',indicator,'/output/new_model/edu_0_cell_draws_eb.RData'))) {
    Sys.sleep(600) 
  }
  
# 5. Cross-validation
# TO-DO: write cross-val environment, write loop to wait for job submitted by run_MBG.R to finish

# 6. Rake to GBD locations
# TO-DO
  
# 7. Diagnostics (compile relevant objects and save in Shiny folder)
source('mbg_central/plot_covariates.R')
source('mbg_central/plot_preds_and_data.R')

# Time-stamp everything
source('mbg_central/time_stamp.R')

## END
