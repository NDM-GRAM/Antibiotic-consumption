#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Script to loop through selected polygons within  #
# a shapefile and resample them to points. Saves   #
# the points in a new file                         #     
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list =ls())
library(raster)
library(rgeos)
library(seegMBG, lib = 'C:/Users/annieb/Desktop/R_packages')

#setup
shp               <- as.character(commandArgs()[4])
ind               <- as.character(commandArgs()[5])

# read in the locations to resample
locs_list <-  readRDS(paste0('/share/homes/annieb6/AMR/', ind, '/resample_polys/locs_list.rds'))

#read in the population raster (using 2010 raster)
pop_raster <- raster('/home/j/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 4)
pop_raster <- disaggregate(pop_raster, 5)

#get the shapefile
my_shp <- readRDS(paste0('/share/geospatial/rds_shapefiles/', shp,'.rds'))
  
#get the location codes
my_locs <- locs_list$location_code[locs_list$shapefile == shp]
  
  for(p in my_locs){
    #get the unique poly
    poly <- my_shp[my_shp$GAUL_CODE == p,]
      
    #resample points
    message('Resampling poly ', p)
    mypoints <- getPoints(shape    = poly,
                          raster   = pop_raster,
                          n        = 0.001,
                          perpixel = TRUE,
                          prob     = TRUE)
    
    #for Male (Maldives) resampling not working (as likely too small) so use centroid
    if(shp == 'admin2013_1' & p == "1918"){
      mypoints <- gCentroid(poly)
      mypoints <- data.frame(mypoints)
      mypoints$weight <- 1
    }
    mypoints <- data.frame(mypoints)
    colnames(mypoints) <- c('longitude', 'latitude', 'weight')
    
    mypoints$location_code <- p
    
    if(p==my_locs[1]){
      shp_points <- mypoints
    } else{
      shp_points <- rbind(shp_points, mypoints)
    }
  }

shp_points$shapefile <- shp
saveRDS(shp_points, paste0('/share/homes/annieb6/AMR/', ind, '/resample_polys/resamples_', shp, '.rds'))
