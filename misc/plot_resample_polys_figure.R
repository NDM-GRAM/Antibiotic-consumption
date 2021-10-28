library(ggplot2)
library(rgdal)
library(raster)
library(rgeos)
library(seegMBG)
rm(list = ls())

#read in the raster and shapefile
pop_raster <- raster('/home/j/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 4)
pop_raster <- disaggregate(pop_raster, 5)

my_shp <- readOGR('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2020/master/shapefiles/GBD2020_mapping_final.shp')
poly <- my_shp[my_shp$loc_id == 484,]  #Gauteng in South Africa as an example

#settings for getpoints
shape    = poly
raster   = pop_raster
n        = 0.001
perpixel = TRUE
prob     = TRUE

#run the get points function line by line
raster <- raster::crop(raster, shape)
raster <- safeMask(raster, shape)
vals <- getValues(raster)


n_valid <- length(seegSDM:::notMissingIdx(raster))
n <- ceiling(n * n_valid)
x <- seegSDM::bgSample(raster, 10000, prob = prob, spatial = FALSE, 
                       replace = TRUE)
x <- as.data.frame(x)
n_unique <- nrow(unique(x))
n <- pmin(n, n_unique)
kmn <- kmeans(x, n)
u <- kmn$centers
weights <- table(kmn$cluster)/length(kmn$cluster)
rownames(u) <- NULL
colnames(u) <- colnames(x)
u <- cbind(u, weights = weights)

#get outputs of the resampling to plot
scattered_points <- data.frame(x)
colnames(scattered_points) <-  c('long', 'lat')
weighted_points <- data.frame(u)
colnames(weighted_points) <-  c('long', 'lat', 'weight')

myPalette <- colorRampPalette(brewer.pal(9, "BuPu"))
mycolours = myPalette(100)

test_spdf <- as(raster, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

png('/ihme/homes/annieb6/AMR/antibiotic_use/resample_polys_fig/western_cape/fig_a.png',
    height =10, width =10, units = 'cm', res = 300)
ggplot() +  
  geom_tile(data=test_df, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=poly, aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=0.75) +
  scale_fill_gradientn(colours = mycolours) +
  coord_equal()+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")+
  labs(x = NULL, y = NULL, fill = NULL)

dev.off()


png('/ihme/homes/annieb6/AMR/antibiotic_use/resample_polys_fig/western_cape/fig_b.png',
    height =10, width =10, units = 'cm', res = 300)
ggplot()+
  geom_polygon(data = poly, aes(x = long, y = lat, group = group), colour = "black", fill = 'lightgrey')+
  geom_point(data = scattered_points, aes(x = long, y = lat), alpha = 0.5, size = 0.5)+
  theme_bw()+
  coord_equal()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  labs(x = NULL, y = NULL, alpha = NULL)
dev.off()

png('/ihme/homes/annieb6/AMR/antibiotic_use/resample_polys_fig/western_cape/fig_c.png',
    height =10, width =10, units = 'cm', res = 300)
ggplot()+
  geom_polygon(data = poly, aes(x = long, y = lat, group = group), colour = "black", fill = 'lightgrey')+
  geom_point(data = weighted_points, aes(x = long, y = lat, size = weight), colour = 'blue')+
  coord_equal()+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")+
  labs(x = NULL, y = NULL, size = NULL)
dev.off()
