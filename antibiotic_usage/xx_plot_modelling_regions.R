#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Plot out the MBG modelling regions used #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(list = ls())
library(sf)
library(raster)
library(ggplot2)

shp <- st_read('Z:/AMR/Shapefiles/admin2013_0.shp')

# merge on the modelling regions
reg <- read.csv("Z:/AMR/Misc/MBG_regions/MBG_modelling_regions.csv")

my_shp <- merge(shp, reg, by.x = 'COUNTRY_ID', by.y = 'iso3', all.x = T, all.y = T)
my_shp$region <-  as.character(my_shp$region)

my_shp$region[my_shp$region == 'dia_mid_east'] <- 'Middle East' 
my_shp$region[my_shp$region == 'dia_cssa'] <- 'Central sub-Saharan Africa' 
my_shp$region[my_shp$region == 'balkans_ext'] <- 'Balkans & Caucasus' 
my_shp$region[my_shp$region == 'caucasus'] <- 'Balkans & Caucasus' 
my_shp$region[my_shp$region == 'dia_malay+dia_oceania'] <- 'Malay & Oceania' 
my_shp$region[my_shp$region == 'dia_essa'] <- 'Eastern & Southern sub-Saharan Africa' 
my_shp$region[my_shp$region == 'dia_wssa'] <- 'Western sub-Saharan Africa' 
my_shp$region[my_shp$region == 'dia_south_asia'] <- 'South Asia' 
my_shp$region[my_shp$region == 'dia_mcaca'] <- 'Central America & Caribbean' 
my_shp$region[my_shp$region == 'dia_s_america'] <- 'South America' 
my_shp$region[my_shp$region == 'dia_sssa'] <- 'Eastern & Southern sub-Saharan Africa' 
my_shp$region[my_shp$region == 'dia_afr_horn'] <- 'Horn of Africa' 
my_shp$region[my_shp$region == 'dia_name'] <- 'North Africa' 
my_shp$region[my_shp$region == 'dia_central_asia+kaz+mng'] <- 'Central Asia' 
my_shp$region[my_shp$region == 'dia_se_asia'] <- 'Southeast Asia' 
my_shp$region[is.na(my_shp$region)] <-  'High income (not modelled)'

my_shp$region <-  as.factor(my_shp$region)
levels(my_shp$region)

my_shp$region <- factor(my_shp$region, levels = c("Balkans & Caucasus",
                                                  "Central America & Caribbean", 
                                                  "Central Asia",
                                                  "Central sub-Saharan Africa",  
                                                  "Eastern & Southern sub-Saharan Africa",
                                                   "Horn of Africa",              
                                                   "Malay & Oceania",                 
                                                   "Middle East",                          
                                                   "North Africa",                     
                                                   "South America",                    
                                                   "South Asia",                      
                                                   "Southeast Asia",        
                                                   "Western sub-Saharan Africa",
                                                   "High income (not modelled)"))


png('Z:/AMR/Covariates/antibiotic_use/abx_use/figures/modelling_regions.png',
    height = 20, width = 30, unit = 'cm', res = 300)
ggplot()+
  geom_sf(data = my_shp, aes(fill = region),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  scale_fill_manual(values = c("#c22f33",
                               "#33c229",
                               "#bf2c93",
                               "#9c6b2f",
                               "#26a35c",
                               "#36b4c7",
                               "#36619c",
                               "#4a2ac9",
                               "#a13b56",
                               '#992dbd',
                               '#bfa026',
                               '#c95f2a',
                               '#8a40a1',
                               "#D3D3D3"))+
  labs(fill ='Region')+
  theme(legend.title = element_text(size = 10),
         legend.text = element_text(size = 8))

dev.off()


