# Time-stamp everything

# Set date to track this model run
run_date <- gsub("-","_",Sys.time())
run_date <- gsub(":","_",run_date)
run_date <- gsub(" ","_",run_date)

# Go through all files/folders that got generated in this run
# Make list of files/folders
to_rename <- c(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/new_model_tempimage.RData'),
               paste0('/snfs1/temp/geospatial/inla_intermediate/inla_', run_date),
               paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date)
               )

for(item in to_rename) {
  file.rename(item, gsub('new_model',run_date,item))
}

## END
