#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Combined HIC and LMIC model results #
# Plot maps, figures and tables of    #
# antibitoic consumption results      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list =ls())
library(data.table)
library(ggplot2)
library(raster)
library(sf)
library(rgeos)
library(RColorBrewer)
library(readxl)
source('H:/Functions/seqlast.R')

setwd('Z:/AMR/Covariates/antibiotic_use/results')
dir.create('figures/maps', recursive = T, showWarnings = F)
dir.create('figures/plots', showWarnings = F)
dir.create('tables' , showWarnings = F)

# 1. Read in and merge the model results ####
#a. Read in the model results
HIC <- data.table(read.csv('imputed_HIC_J01.csv', stringsAsFactors = F))
HIC <- HIC[HIC$loc_id!=354,]
HIC <- HIC[HIC$loc_id!=44533,]
lmic <- data.table(read.csv("model_estimates.csv", stringsAsFactors = F))

#b. Read in the shapefile and limit top required locations
shp <- st_read('D:/Z_drive/Shapefiles/GBD2019/GBD2019_analysis_final.shp')
shp <- shp[shp$level == 3,] 
shp <- st_simplify(shp, dTolerance = 0.1, preserveTopology = T)

lmic <- lmic[!(lmic$location_id %in% HIC$loc_id),]
lmic <- lmic[lmic$location_id %in% shp$loc_id,]
lmic <- unique(lmic)
HIC <- HIC[HIC$loc_id %in% shp$loc_id,]

#c. Combine HIC and LMIC model results
mydata <- rbind(HIC[,.(loc_id, year, ddd_per_1000, ddd_per_1000_lower = ddd_per_1000, ddd_per_1000_upper = ddd_per_1000)], 
                lmic[,.(loc_id = location_id, year = year_id, ddd_per_1000 = gpr_mean, ddd_per_1000_lower = gpr_lower, ddd_per_1000_upper = gpr_upper)])

mydata <- mydata[mydata$year >=2000,]

#d. Add on the population
pop <- read.csv('Z:/AMR/Misc/GBD_populations/GBD_total_populations.csv', stringsAsFactors = F)
mydata <- merge(mydata, pop, by.x = c('loc_id', 'year'), by.y = c('location_id', 'year_id'), all.x = T, all.y = F)
rm(pop)

#e. Merge on all location info to the data
locs <- read.csv('Z:/AMR/Misc/IHME_location_hierarchy/cleaned_ihme_hierarchy.csv', stringsAsFactors = F)
locs <- locs[1:3]
mydata <- merge(locs, mydata, by.x =c('location_id'), by.y = c('loc_id'), all.x = F, all.y = F)
colnames(mydata)[2] <-  'Country'
colnames(mydata)[3] <-  'reg'
mydata <- merge(locs, mydata, by.x =c('location_id'), by.y = c('reg'), all.x = F, all.y = T)
colnames(mydata)[2] <-  'Region'

colnames(mydata)[3] <-  'spr'
mydata <- merge(locs, mydata, by.x =c('location_id'), by.y = c('spr'), all.x = F, all.y = T)
colnames(mydata)[2] <-  'Super region'

#f. Calculate metrics
mydata$ddd_per_1000_per_day <- mydata$ddd_per_1000/365
mydata$ddd_per_1000_per_day_lower <- mydata$ddd_per_1000_lower/365
mydata$ddd_per_1000_per_day_upper <- mydata$ddd_per_1000_upper/365
mydata$ddd <- mydata$ddd_per_1000*(mydata$population/1000)
mydata$ddd_lower <- mydata$ddd_per_1000_lower*(mydata$population/1000)
mydata$ddd_upper <- mydata$ddd_per_1000_upper*(mydata$population/1000)

mydata <- mydata[c('Super region', 'Region', 'Country', 'year', 'population',
                   'ddd_per_1000_per_day','ddd_per_1000_per_day_lower','ddd_per_1000_per_day_upper', 
                   'ddd', 'ddd_lower', 'ddd_upper')]

#g. Save out the results
mydata <- data.table(mydata)
write.csv(mydata, 'all_results.csv', row.names = F)
ddd_per_day <- dcast(mydata, `Super region` + Region + Country ~ year, value.var='ddd_per_1000_per_day')
write.csv(ddd_per_day, 'ddd_per_1000_per_day.csv', row.names = F)

#2. Create  maps ####
#get a background shapefile to mask western sahara and french guyana
background <- st_read('Z:/AMR/Shapefiles/admin2013_0.shp')
background <- background[background$name == 'Western Sahara' | background$name == 'French Guiana',]
background <- st_simplify(background, dTolerance = 0.1, preserveTopology = T)

#merge data and geometry info
my_shp <- merge(shp, mydata, by.y = 'Country', by.x = 'loc_name') 

#Select colour scheme
myPalette <- colorRampPalette(brewer.pal(9, "YlGnBu"))
mycolours = myPalette(100)

#a. Map of total antibiotic consumption per 1000 population per day for 2018
pdf('figures/maps/J01_DDD__per_day_2018.pdf',
    height = 8, width = 15)
ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2018,], aes(fill = `ddd_per_1000_per_day`),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = mycolours, limit = c(0, 50), labels = seqlast(0, 50, 10), breaks = seq(0, 50, 10))+
  # scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 50))+
  labs(fill = 'DDD/1000/day')
dev.off()

#b. Map of total antibiotic consumption per 1000 population per day for all years
pdf('figures/maps/J01_DDD__per_day_all_years.pdf',
     height = 8, width = 15)
ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = my_shp, aes(fill = `ddd_per_1000_per_day`),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = mycolours, limit = c(0, 50), labels = seqlast(0, 50, 10), breaks = seq(0, 50, 10))+
  labs(fill = 'DDD/1000/day')+
  facet_wrap(~year, ncol = 5)
dev.off()

#c. Map of total antibiotic consumption per 1000 population per day for every 5 years
png('figures/maps/J01_DDD_per_day_5_yearly.png',
    height = 20, width = 40, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = my_shp[my_shp$year == 2000 |my_shp$year == 2005 |my_shp$year == 2010 |my_shp$year == 2015,], aes(fill = `ddd_per_1000_per_day`),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = mycolours, limit = c(0, 50), labels = seqlast(0, 50, 10), breaks = seq(0, 50, 10))+
  labs(fill = 'DDD/1000/day')+
  facet_wrap(~year, ncol = 2)
dev.off()

#d Map of 2018 ddd/1000/year with the uncertainty intervals
uncertainty <- melt(mydata, id.vars = c('Country', 'year'), 
                    measure.vars = c('ddd_per_1000_per_day', 'ddd_per_1000_per_day_lower', 'ddd_per_1000_per_day_upper'),
                    value.name = 'DDD/1000/day')
uncertainty$variable <- as.character(uncertainty$variable)
uncertainty$variable[uncertainty$variable == 'ddd_per_1000_per_day'] <-  'Mean'
uncertainty$variable[uncertainty$variable == 'ddd_per_1000_per_day_lower'] <-  'Lower'
uncertainty$variable[uncertainty$variable == 'ddd_per_1000_per_day_upper'] <-  'Upper'
uncertainty <- uncertainty[!(toupper(uncertainty$Country) %in% toupper(HIC$country)),]
uncertainty <-  merge(shp, uncertainty, by.y = 'Country', by.x = 'loc_name') 

pdf('figures/maps/J01_DDD_per_day_2018_uncertainty.pdf',
    height = 8, width = 5)
ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = shp, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = uncertainty[uncertainty$year == 2018,], aes(fill = `DDD/1000/day`),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = mycolours, limit = c(0, 55), labels = seqlast(0, 50, 10), breaks = seq(0, 50, 10))+
  labs(fill = 'DDD/1000/day')+
  facet_wrap(~variable, ncol = 1)
dev.off()

#3. Create plots of antibitoic use over time by income group and super region ####
#a. calculatethe total DDD/1000/day for LMIC and HICs
mydata <- data.table(mydata)
HIC <- read.csv("Z:/AMR/Misc/world_bank_regions/HICs.csv", stringsAsFactors = F)
income <- mydata
income$income <- 'Low and middle income countries'
income$income[income$Country %in% HIC$loc_name] <-  'High income countries'

income <- income[,.(ddd = sum(ddd),
                    ddd_lower = sum(ddd_lower),
                    ddd_upper = sum(ddd_upper),
                    ddd_per_1000_per_day = (sum(ddd)/(sum(population)/1000))/365,
                    ddd_per_1000_per_day_lower = (sum(ddd_lower)/(sum(population)/1000))/365,
                    ddd_per_1000_per_day_upper = (sum(ddd_upper)/(sum(population)/1000))/365),
                 by = c('income', 'year')]

write.csv(income, "tables/ddd_by_income_group.csv", row.names = F)

#b. Calculate DDD/1000/day by super region
spr_reg <- mydata[,.(ddd = sum(ddd),
                     ddd_lower = sum(ddd_lower),
                     ddd_upper = sum(ddd_upper),
                     ddd_per_1000_per_day = (sum(ddd)/(sum(population)/1000))/365,
                     ddd_per_1000_per_day_lower = (sum(ddd_lower)/(sum(population)/1000))/365,
                     ddd_per_1000_per_day_upper = (sum(ddd_upper)/(sum(population)/1000))/365),
                  by = c('Super region', 'year')]
write.csv(spr_reg, "tables/spr_reg_ddds.csv", row.names = F)

#c. Calculate DDD/1000/day by super region
global_ests <- mydata[,.(ddd = sum(ddd),
                     ddd_lower = sum(ddd_lower),
                     ddd_upper = sum(ddd_upper),
                     ddd_per_1000_per_day = (sum(ddd)/(sum(population)/1000))/365,
                     ddd_per_1000_per_day_lower = (sum(ddd_lower)/(sum(population)/1000))/365,
                     ddd_per_1000_per_day_upper = (sum(ddd_upper)/(sum(population)/1000))/365),
                  by = c('year')]

write.csv(global_ests, "tables/global_ddds.csv", row.names = F)

#d. Plot graph of the DDD/1000 pop/year for each super-region and world bank income group for each year 2000-2018
pdf('figures/plots/super_regional_trends+income+UI.pdf',
    height = 7.9, width = 11.8)
ggplot()+
  geom_line(data = spr_reg, aes(x = year, y = ddd_per_1000_per_day, group = `Super region`, colour = `Super region`), size = 1.5)+
  geom_ribbon(data = spr_reg, aes(x = year, ymin = ddd_per_1000_per_day_lower, ymax = ddd_per_1000_per_day_upper, fill = `Super region`), alpha = 0.1)+
  geom_line(data = income[income == 'High income countries',], aes(x = year, y = ddd_per_1000_per_day, group = income, linetype = income), colour = 'black', size = 1)+
  geom_line(data = income[income != 'High income countries',], aes(x = year, y = ddd_per_1000_per_day, group = income, linetype = income), colour = 'black', size = 1)+
  geom_ribbon(data = income[income != 'High income countries',], aes(x = year, ymin = ddd_per_1000_per_day_lower, ymax = ddd_per_1000_per_day_upper), alpha = 0.1, fill = 'black')+
  scale_colour_manual(values = c("#fe9929", #central europe
                                 "#8c2d04", #HI
                                 # '#000000', #WB HIC
                                 "#e78ac3", #latin America
                                 # '#000000', #WB LMIC
                                 "#984ea3", #NAME
                                 "#4daf4a", #South Asia
                                 "#377eb8", # SE Asia
                                 "#e41a1c"))+ #SSA+
  scale_fill_manual(values = c("#fe9929", #central europe
                                 "#8c2d04", #HI
                                 # '#000000', #WB HIC
                                 "#e78ac3", #latin America
                                 # '#000000', #WB LMIC
                                 "#984ea3", #NAME
                                 "#4daf4a", #South Asia
                                 "#377eb8", # SE Asia
                                 "#e41a1c"), guide = 'none')+
  scale_linetype_manual(values = c('longdash', 'dotdash'))+
  theme_bw()+
  labs(x = 'Year', y = 'Defined Daily Doses per 1000 population per day', colour = 'GBD Super Region', linetype = 'World Bank Income Group')+
  ylim(0,30)
dev.off()

#4. Create table 1 of DDD/1000/pop for each region and super region for 2018####
mydata <-  mydata[mydata$year == 2018,]

totalddd <- sum(mydata$ddd)
lowerddds <- sum(mydata$ddd_lower)
upperddds <- sum(mydata$ddd_upper)

table1_regions <- mydata[,.(`Total antibiotics consumed (Million DDDs)` = paste0(prettyNum(round(sum(ddd)/1000000,0),big.mark=","), " [",
                                                                                 prettyNum(round(sum(ddd_lower)/1000000,0),big.mark=","), "-",
                                                                                 prettyNum(round(sum(ddd_upper)/1000000,0),big.mark=","),"]"),
                            `Percentage of global antibiotic consumption` = round(sum(ddd)/totalddd*100,1),
                            `DDD/1000/day` = paste0(prettyNum(round((sum(ddd)/(sum(population)/1000))/365,1),big.mark=","), " [",
                                                    prettyNum(round((sum(ddd_lower)/(sum(population)/1000))/365,1),big.mark=","), "-",
                                                    prettyNum(round((sum(ddd_upper)/(sum(population)/1000))/365,1),big.mark=","), "]")),
                         by = c('Region')] 


table1_spr_regions <- mydata[,.(`Total antibiotics consumed (Million DDDs)` = paste0(prettyNum(round(sum(ddd)/1000000,0),big.mark=","), " [",
                                                                                     prettyNum(  round(sum(ddd_lower)/1000000,0),big.mark=","), "-",
                                                                                     prettyNum(round(sum(ddd_upper)/1000000,0),big.mark=","),"]"),
                                `Percentage of global antibiotic consumption` = round(sum(ddd)/totalddd*100,1),
                                `DDD/1000/day` = paste0(prettyNum(round((sum(ddd)/(sum(population)/1000))/365,1),big.mark=","), " [",
                                                        prettyNum(round((sum(ddd_lower)/(sum(population)/1000))/365,1),big.mark=","), "-",
                                                        prettyNum(round((sum(ddd_upper)/(sum(population)/1000))/365,1),big.mark=","), "]")),                        by = c('Super region')]

table1_global <-  mydata[,.(`Total antibiotics consumed (Million DDDs)` = paste0(prettyNum(round(sum(ddd)/1000000,0),big.mark=","), " [",
                                                                                 prettyNum(round(sum(ddd_lower)/1000000,0),big.mark=","), "-",
                                                                                 prettyNum(round(sum(ddd_upper)/1000000,0),big.mark=","),"]"),
                            `Percentage of global antibiotic consumption` = round(sum(ddd)/totalddd*100,1),
                            `DDD/1000/day` = paste0(prettyNum(round((sum(ddd)/(sum(population)/1000))/365,1),big.mark=","), " [",
                                                    prettyNum(round((sum(ddd_lower)/(sum(population)/1000))/365,1),big.mark=","), "-",
                                                    prettyNum(round((sum(ddd_upper)/(sum(population)/1000))/365,1),big.mark=","), "]"))]

table1_global$`Super region` <-  'Global' 
table1_global$Region <- ""
table1_spr_regions$Region <- ""
table1_regions$`Super region` <- ""

table1 <- rbind(table1_regions, table1_spr_regions, table1_global)

write.csv(table1, 'tables/table1+UIs.csv', row.names = F)
#~~~~~#
# END #
#~~~~~#

