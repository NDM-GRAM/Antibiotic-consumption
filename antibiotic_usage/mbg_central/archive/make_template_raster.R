# build admin unit rasters

# load packages
library(raster, lib.loc = package_lib)
library(rgdal, lib.loc = package_lib)

message('Creating rasters of admin units')
# ~~~~~~~~~~~~~
# load shapefiles
template_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/templates')

# admin units
prettymap <- shapefile(paste0(root,'/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad0.shp'))
writeOGR(prettymap, template_dir, paste0(target_country,'_adm0'), driver="ESRI Shapefile", overwrite_layer=TRUE)

# save nice shapefiles for meshes/plotting
prettymap.ad2 <- shapefile(paste0(root,'/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad2.shp'))
writeOGR(prettymap.ad2, template_dir, paste0(target_country,'_adm2'), driver="ESRI Shapefile", overwrite_layer=TRUE)

prettymap.ad1 <- shapefile(paste0(root,'/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad1.shp'))
writeOGR(prettymap.ad1, template_dir, paste0(target_country,'_adm1'), driver="ESRI Shapefile", overwrite_layer=TRUE)

# example raster layer as a template
ras <- raster(paste0(root,'/temp/geospatial/U5M_africa/data/clean/covs_transformed.grd')) * 0

template <- mask <- mask(crop(ras,prettymap),prettymap)

# save rasters
writeRaster(template,
            file = paste0(template_dir,'/template5kdata'),
            overwrite = TRUE)

## END

