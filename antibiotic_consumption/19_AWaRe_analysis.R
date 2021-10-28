#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot out the proportion of antibiotics by AWARE category ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list =ls())
library(data.table)
library(raster)
library(ggplot2)
library(viridis)
library(sp)
library(ggpubr)
source('C:/Users/Annie/Documents/random_functions/simpleCap.R')

setwd('C:/Users/Annie/Documents/GRAM/antibiotic_use/antibiotic_consumption/')

#1. Read in and prep datasets ####
IQVIA <- fread('IQVIA/datasets/cleaned_ddds_2000_2018.csv')
IQVIA <-  IQVIA[IQVIA$ATC3 != 'J04A',]

AWaRe <- fread('IQVIA/lookup_tables/AWaRE_v2.csv', stringsAsFactors = F)
IQVIA <-  merge(IQVIA, AWaRe, by = c('ATC5'), all.x = T, all.y = F)
IQVIA$AWARE[is.na(IQVIA$AWARE)] <-  'Other'
IQVIA$AWARE[IQVIA$AWARE==""] <-  'Other'
IQVIA$total_ddd <- rowSums(IQVIA[,.(hospital_ddd, retail_ddd,combined_ddd)], na.rm = T)
IQVIA$total_ddd_per_1000_pop <- rowSums(IQVIA[,.(hospital_ddd_per_1000_pop, retail_ddd_per_1000_pop, combined_ddd_per_1000_pop)], na.rm = T)

IQVIA <- IQVIA[,.(total_ddd, ddd_per_1000=total_ddd_per_1000_pop),
               by = c("super_region", "region", "country", "iso3", "loc_id", "year", "AWARE")]

rm(AWaRe)

ddd_per_1000 <- IQVIA[,.(ddd = sum(total_ddd),  ddd_per_1000 = sum(ddd_per_1000)),
                       by = c("super_region", "region", "country", "iso3", "loc_id", "year", "AWARE")]

ddd_per_1000$ddd_per_1000[ddd_per_1000$ddd_per_1000<0] <- 0
ddd_per_1000$ddd[ddd_per_1000$ddd<0] <- 0

sum_ddd_per_1000 <- IQVIA[,.(total_ddd = sum(total_ddd), total_ddd_per_1000 = sum(ddd_per_1000)),
                       by = c("super_region", "region", "country", "iso3", "loc_id", "year")]

mydata <- merge(ddd_per_1000, sum_ddd_per_1000, by = c("super_region", "region", "country", "iso3", "loc_id", "year"))

mydata$prop_aware <- round(mydata$ddd/mydata$total_ddd,3) 

#add data for Algeria reserve abx for the map
DZA <- mydata[mydata$iso3 == 'DZA' & mydata$AWARE == 'Watch',]
DZA$AWARE <-  'Reserve'
DZA$prop_aware <- 0
DZA$total_ddd_per_1000 <-  0
DZA$ddd_per_1000 <-  0
mydata <-  rbind(mydata, DZA)
rm(DZA)

# Plot out bar charts for 2000 and 2018
mydata$country <- sapply(mydata$country, simpleCap)
mydata$country <-  gsub('And', '&', mydata$country)

mydata$AWARE <- factor(mydata$AWARE, levels = c("Other", "Reserve", "Watch", "Access"))
write.csv(mydata, 'results/AwARE.csv', row.names = F)

mydata_2000 <- mydata[mydata$year == 2000,]
mydata_2000 <- mydata_2000[order(-mydata_2000$AWARE, -mydata_2000$prop_aware),]
countries <- unique(mydata_2000$country)
mydata_2000$country <-  factor(mydata_2000$country, levels = countries)

mydata_2018 <- mydata[mydata$year == 2018,]
mydata_2018 <- mydata_2018[order(-mydata_2018$AWARE, -mydata_2018$prop_aware),]
countries <- unique(mydata_2018$country)
mydata_2018$country <-  factor(mydata_2018$country, levels = countries)

plot_2000<-
  ggplot(mydata_2000[!is.na(mydata_2000$prop_aware) & mydata_2000$country!='Algeria'], aes(x = country, y = prop_aware, fill =AWARE))+
    geom_bar(position="fill", stat="identity")+
    labs(x = NULL, y = 'Proportion of antibiotics consumed', fill = NULL)+
    geom_hline(yintercept = 0.6, linetype = "dashed", color='black', size=1)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_fill_manual(values = c('#984ea3', '#4daf4a', '#377eb8', '#e41a1c'))+
    scale_y_continuous("Percentage of antibiotics consumed", breaks = c(0, .25, .50, .75, 1), labels = c('0', '25', '50', '75', '100'), expand = c(0, 0))+
    theme(legend.position = 'bottom',
        axis.title=element_text(size=8))


plot_2018<-
  ggplot(mydata_2018, aes(x = country, y = prop_aware, fill =AWARE))+
    geom_bar(position="fill", stat="identity")+
    labs(x = NULL, y = 'Proportion of antibiotics consumed', fill = NULL)+
    geom_hline(yintercept = 0.6, linetype = "dashed", color='black', size=1)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_fill_manual(values = c('#984ea3', '#4daf4a', '#377eb8', '#e41a1c'))+
    scale_y_continuous("Percentage of antibiotics consumed", breaks = c(0, .25, .50, .75, 1), labels = c('0', '25', '50', '75', '100'), expand = c(0, 0))+
    theme(legend.position = 'bottom',
          axis.title=element_text(size=8))
  
png('results/figures/plots/AWaRE_2000_2018v2.png',
    height = 20, width = 40, units = 'cm', res = 300)
  ggarrange(plot_2000, plot_2018, ncol = 1, labels = c('a', 'b'), common.legend = TRUE, legend="bottom")
dev.off()

#2. Plot map of proportion of AWaRE antibitoics ####
shp <- st_read('c:/Users/Annie/Documents/GRAM/shapefiles/IQVIA_analysis_simple.shp')
shp <- shp[shp$level == 3 | shp$loc_id == 44533 | shp$loc_id == 354,]
shp <- shp[shp$loc_id != 6,]

#merge on the data
my_shp <- merge(shp, mydata, by = 'loc_id') 

background_shp <- shp[!(shp$loc_id %in% my_shp$loc_id[my_shp$year == 2018]) | shp$ihme_lc_id == 'KEN' | shp$ihme_lc_id == 'VEN',]

my_shp$AWARE <- factor(my_shp$AWARE, levels = c("Access", "Watch", 'Reserve', 'Other'))  

#a. For 2018
png('results/figures/maps/AWaRe_2018v2.png',
     height = 20, width = 20, units = 'cm', res = 200)
ggplot()+
  geom_sf(data = background_shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2018,], aes(fill = prop_aware),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt')+
  facet_wrap(~AWARE, ncol = 2)+
  labs(fill = 'Proportion of total antibiotics')
dev.off()


#b. For 2000
png('results/figures/maps/AWaRe_2000.png',
    height = 20, width = 20, units = 'cm', res = 200)
ggplot()+
  geom_sf(data = background_shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2000,], aes(fill = prop_aware),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt')+
  facet_wrap(~AWARE, ncol = 2)+
  labs(fill = 'Proportion of total antibiotics')
dev.off()

#c. For 2005
png('results/figures/maps/AWaRe_2005.png',
    height = 20, width = 20, units = 'cm', res = 200)
ggplot()+
  geom_sf(data = background_shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2005,], aes(fill = prop_aware),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt')+
  facet_wrap(~AWARE, ncol = 2)+
  labs(fill = 'Proportion of total antibiotics')
dev.off()

#d. For 2010
png('results/figures/maps/AWaRe_2010.png',
    height = 20, width = 20, units = 'cm', res = 200)
ggplot()+
  geom_sf(data = background_shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2010,], aes(fill = prop_aware),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt')+
  facet_wrap(~AWARE, ncol = 2)+
  labs(fill = 'Proportion of total antibiotics')
dev.off()

#e. For 2015
png('results/figures/maps/AWaRe_2015.png',
    height = 20, width = 20, units = 'cm', res = 200)
ggplot()+
  geom_sf(data = background_shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2015,], aes(fill = prop_aware),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt')+
  facet_wrap(~AWARE, ncol = 2)+
  labs(fill = 'Proportion of total antibiotics')
dev.off()


#a. GLobal
global_ddds <- IQVIA[,.(ddd = sum(total_ddd)),
                       by = c("year", "AWARE")]

global_total <- IQVIA[,.(total_ddd = sum(total_ddd)),
                           by = c("year")]

global_data <- merge(global_ddds, global_total)

global_data$prop_aware <- round(global_data$ddd/global_data$total_ddd,3) 

global_data$AWARE <-  factor(global_data$AWARE, levels = c('Other', 'Reserve', 'Watch', 'Access'))


png('results/GPR5/figures/plots/AWARE_all_IQVIA_by_year.png',
    height = 10, width = 15, units = 'cm', res = 300)
ggplot(global_data, aes(x = year, y = prop_aware, fill =AWARE))+
  geom_bar(position="fill", stat="identity")+
  labs(fill = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values = c('#984ea3', '#4daf4a', '#377eb8', '#e41a1c'))+
  scale_y_continuous("Percentage of antibiotics consumed", breaks = c(0, .25, .50, .75, 1), labels = c('0', '25', '50', '75', '100'), expand = c(0, 0))+
  scale_x_continuous("Year", breaks = c(2000:2018), expand = c(0, 0))+
  theme(legend.position = 'bottom',
        axis.title=element_text(size=8))
dev.off()

#~~~~~#
# END #
#~~~~~#