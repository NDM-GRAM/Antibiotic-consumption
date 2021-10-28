#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Access and save select rasters for making maps of results in mapping software #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list =ls())

# Setup
library(raster)
library(data.table)

# Model run details
run_date                 <- '2021_01_29_03_32_09'
indicator                <- 'mdr_typhi'
indicator_group          <- 'lbd_amr'

#create an output directory
outputdir <- paste0('/ihme/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date ,'/model_validation/rasters')
dir.create(outputdir, showWarnings = F)

#read in the aggregated results
adm0 <- read.csv(paste0('/ihme/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date ,'/pred_derivatives/admin_summaries/', indicator, '_admin_0_unraked_summary.csv'), stringsAsFactors = F)
adm1 <- read.csv(paste0('/ihme/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date ,'/pred_derivatives/admin_summaries/', indicator, '_admin_1_unraked_summary.csv'), stringsAsFactors = F)
adm2 <- read.csv(paste0('/ihme/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date ,'/pred_derivatives/admin_summaries/', indicator, '_admin_2_unraked_summary.csv'), stringsAsFactors = F)

#read in the shapefiles used to agregate results
adm0_shp <- shapefile('/snfs1/WORK/11_geospatial/admin_shapefiles/2020_05_21/lbd_standard_admin_0.shp')
adm1_shp <- shapefile('/snfs1/WORK/11_geospatial/admin_shapefiles/2020_05_21/lbd_standard_admin_1.shp')
adm2_shp <- shapefile('/snfs1/WORK/11_geospatial/admin_shapefiles/2020_05_21/lbd_standard_admin_2.shp')

#reshape data files
adm0 <- data.table(adm0)
mean_adm0 <- dcast(adm0, ADM0_CODE~year, value.var = 'mean')

adm1 <- data.table(adm1)
mean_adm1 <- dcast(adm1, ADM1_CODE~year, value.var = 'mean')

adm2 <- data.table(adm2)
mean_adm2 <- dcast(adm2, ADM2_CODE~year, value.var = 'mean')
upper_adm2 <- dcast(adm2, ADM2_CODE~year, value.var = 'upper')
lower_adm2 <- dcast(adm2, ADM2_CODE~year, value.var = 'lower')

#merge shapefiles and data files
mean_adm0 <-  merge(adm0_shp, mean_adm0, by = 'ADM0_CODE', all.x = F, all.y = F)
mean_adm1 <-  merge(adm1_shp, mean_adm1, by = 'ADM1_CODE', all.x = F, all.y = F)
mean_adm2 <-  merge(adm2_shp, mean_adm2, by = 'ADM2_CODE', all.x = F, all.y = F)
lower_adm2 <-  merge(adm2_shp, lower_adm2, by = 'ADM2_CODE', all.x = F, all.y = F)
upper_adm2 <-  merge(adm2_shp, upper_adm2, by = 'ADM2_CODE', all.x = F, all.y = F)

#rasterise the desired years
#1. 2018 for all resolutions
adm0_2018_mean <- rasterize(mean_adm0, raster(extent(mean_adm0), res = 0.1), field = '2018' )
adm1_2018_mean <- rasterize(mean_adm1, raster(extent(mean_adm1), res = 0.1), field = '2018' )
adm2_2018_mean <- rasterize(mean_adm2, raster(extent(mean_adm2), res = 0.05), field = '2018' )

writeRaster(adm0_2018_mean, paste0(outputdir, '/cough_abx_adm0_2018_mean.tif'), format = 'GTiff', overwrite = T)
writeRaster(adm1_2018_mean, paste0(outputdir, '/cough_abx_adm1_2018_mean.tif'), format = 'GTiff', overwrite = T)
writeRaster(adm2_2018_mean, paste0(outputdir, '/cough_abx_adm2_2018_mean.tif'), format = 'GTiff', overwrite = T)

#2. upper and lower 2018 adm2
adm2_2018_upper <- rasterize(upper_adm2, raster(extent(mean_adm2), res = 0.05), field = '2018' )
adm2_2018_lower <- rasterize(lower_adm2, raster(extent(mean_adm2), res = 0.05), field = '2018' )

writeRaster(adm2_2018_upper, paste0(outputdir, '/cough_abx_adm2_2018_upper.tif'), format = 'GTiff', overwrite = T)
writeRaster(adm2_2018_lower, paste0(outputdir, '/cough_abx_adm2_2018_lower.tif'), format = 'GTiff', overwrite = T)

#3. 5 yearly admin 2
adm2_1990_mean <- rasterize(mean_adm2, raster(extent(mean_adm2), res = 0.05), field = '1990' )
adm2_1995_mean <- rasterize(mean_adm2, raster(extent(mean_adm2), res = 0.05), field = '1995' )
adm2_2000_mean <- rasterize(mean_adm2, raster(extent(mean_adm2), res = 0.05), field = '2000' )
adm2_2005_mean <- rasterize(mean_adm2, raster(extent(mean_adm2), res = 0.05), field = '2005' )
adm2_2010_mean <- rasterize(mean_adm2, raster(extent(mean_adm2), res = 0.05), field = '2010' )
adm2_2015_mean <- rasterize(mean_adm2, raster(extent(mean_adm2), res = 0.05), field = '2015' )

writeRaster(adm2_1990_mean, paste0(outputdir, '/', indicator, '_adm2_1990_mean.tif'), format = 'GTiff', overwrite = T)
writeRaster(adm2_1995_mean, paste0(outputdir, '/', indicator, '_adm2_1995_mean.tif'), format = 'GTiff', overwrite = T)
writeRaster(adm2_2000_mean, paste0(outputdir, '/', indicator, '_adm2_2000_mean.tif'), format = 'GTiff', overwrite = T)
writeRaster(adm2_2005_mean, paste0(outputdir, '/', indicator, '_adm2_2005_mean.tif'), format = 'GTiff', overwrite = T)
writeRaster(adm2_2010_mean, paste0(outputdir, '/', indicator, '_adm2_2010_mean.tif'), format = 'GTiff', overwrite = T)
writeRaster(adm2_2015_mean, paste0(outputdir, '/cough_abx_adm2_2015_mean.tif'), format = 'GTiff', overwrite = T)

# For pixels level read in the raster brick and split
pixels_mean <- brick(paste0('/ihme/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date ,'/cough_antibiotics_mean_raster.tif'))
pixels_mean_2018 <- pixels_mean[[19]]
writeRaster(pixels_mean_2018, paste0(outputdir, '/cough_abx_pxl_2018_mean.tif'), format = 'GTiff', overwrite = T)

#~~~~~#
# END #
#~~~~~#