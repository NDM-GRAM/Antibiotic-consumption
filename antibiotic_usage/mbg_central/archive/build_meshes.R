# load packages
library(INLA, lib.loc = package_lib)
library(raster, lib.loc = package_lib)
library(data.table, lib.loc = package_lib)

message('Creating spatial and temporal meshes')

# read in the data
d = fread(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/input_data.csv'))
d <- subset(d, !is.na(latitude))
d <- subset(d, latitude!=0)

simple <- shapefile(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/templates/Africa_adm0.shp'))

max.edge <- c(0.15, 5)

mesh_s <- inla.mesh.2d(
  boundary = inla.sp2segment(simple),
  loc = cbind(d$longitude,d$latitude),
  max.edge = max.edge,
  offset = c(.1, 1),
  cutoff = max.edge[1]
)

#plot(mesh_s,asp=1);points(d$longitude,d$latitude,col=d$year)

# make period mesh
pers=c(1:4)

mesh_t <- inla.mesh.1d(
  loc = pers,
  degree = 1,
  boundary = rep("free", 2)
)

# write to disk
save(mesh_s,
     mesh_t,
     file = paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/templates/meshes.RData'))

## END
