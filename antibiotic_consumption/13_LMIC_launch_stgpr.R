#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Register and launch an ST-GPR model from R ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list =ls())

central_root <- '/ihme/code/geospatial/annieb6/st_gpr/stgpr'
setwd(central_root)

source('r_functions/registration/register.R')
source('r_functions/registration/sendoff.R')

# Arguments
path_to_config <- '/ihme/code/geospatial/annieb6/lbd_amr/antibiotic_modelling_code/antibiotic_consumption/abx_sales_config.csv'
project <- 'proj_geospatial'

models_to_run = c(15:19)
for(i in models_to_run){
  model_index_id<- i
  
  # Registration
  run_id <- register_stgpr_model(
    path_to_config = path_to_config,
    model_index_id = model_index_id)
  
  # Sendoff
  stgpr_sendoff(run_id,
                project,
                log_path = '/ihme/code/geospatial/annieb6/lbd_amr/antibiotics/errors')
  
  #add runID to the config
  config  <- read.csv(path_to_config, stringsAsFactors = F)
  config$run_id[config$model_index_id == model_index_id] <- run_id
  write.csv(config, path_to_config, row.names = F, na = "")
}

#~~~~~#
# END #
#~~~~~#