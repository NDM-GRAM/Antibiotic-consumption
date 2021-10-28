# Plot the percentage change in total antibiotic use from 2000 to 2018
library(sf)
library(RColorBrewer)
library(ggplot2)
source('H:/Functions/seqlast.R')

setwd('Z:/AMR/Covariates/antibiotic_use/results')

mydata <- read.csv('ddd_per_1000_per_day.csv', stringsAsFactors = F)

#calculate the difference between 2000 and 2018 and the percentage change
mydata$difference <- mydata$X2018-mydata$X2000
mydata$percentage_change <- (mydata$difference/mydata$X2000)*100


#merge to a shapefile and plot
shp <- st_read('D:/Z_drive/Shapefiles/GBD2019/GBD2019_analysis_final.shp')
shp <- shp[shp$level == 3,] 
shp <- st_simplify(shp, dTolerance = 0.1, preserveTopology = T)

my_shp <- merge(shp, mydata, by.x = 'loc_name', by.y = 'Country') 


# plot them out

myPalette <- colorRampPalette(brewer.pal(9, "RdYlBu"))
mycolours = myPalette(100)
mycolours <- rev(mycolours)

#Absolute difference
png('figures/maps/temporal_difference.png',
    height = 20, width = 40, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp, aes(fill = difference),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = c("#0571b0","#f7f7f7","#d7191c"), 
                       values = rescale(c(-10,0,30)),
                       guide = "colorbar", limits=c(-10,30)) +
  labs(fill = 'Difference in DDD/1000/day')
dev.off()

#percentage change
png('figures/maps/percentage_change.png',
    height = 20, width = 40, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp, aes(fill = percentage_change),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = c("#0571b0","#f7f7f7","#d7191c"), 
                       values = rescale(c(-50,0,350)),
                      guide = "colorbar", limits=c(-50,350)) +
  labs(fill = 'Percentage change (%)')
dev.off()

