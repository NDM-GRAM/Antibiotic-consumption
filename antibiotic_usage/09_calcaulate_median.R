rm(list = ls())
library(data.table)
library(foreign)

setwd('Z:/AMR/Covariates/antibiotic_use')

#Read in the results
mydata <- data.table(read.csv('abx_use/current/admin_summaries/cough_antibiotics_admin_0_unraked_summary.csv', stringsAsFactors = F))

#Merge on additional location id 
locs <- read.dbf("Z:/AMR/Shapefiles/GBD2019/GBD2019_analysis_final_loc_set_22.dbf")
locs <- locs[locs$level == 3,]
locs <- locs[c('loc_name', 'loc_id')]
locs$loc_name <- as.character(locs$loc_name)

#clean up some names to merge
mydata$ADM0_NAME[mydata$ADM0_NAME == 'Micronesia'] <- 'Federated States of Micronesia'
mydata$ADM0_NAME[mydata$ADM0_NAME == 'Republic of Congo'] <- 'Congo'
mydata$ADM0_NAME[grepl('Tom', mydata$ADM0_NAME)] <- "Sao Tome and Principe"
mydata$ADM0_NAME[mydata$ADM0_NAME == 'North Macedonia'] <- 'Macedonia'
mydata$ADM0_NAME[mydata$ADM0_NAME == "Côte d'Ivoire"] <- "Cote d'Ivoire"
mydata$ADM0_NAME[mydata$ADM0_NAME == 'Gambia'] <- 'The Gambia'
mydata$ADM0_NAME[mydata$ADM0_NAME == 'Gambia'] <- 'The Gambia'

mydata <- merge(mydata, locs, by.x = 'ADM0_NAME', by.y = 'loc_name', all.x = T, all.y = F)

#merge on the region names
locs <- read.csv('Z:/AMR/Misc/IHME_location_hierarchy/cleaned_ihme_hierarchy.csv', stringsAsFactors = F)
locs <- locs[1:3]
mydata <- merge(locs, mydata, by.x =c('location_id'), by.y = c('loc_id'), all.x = F, all.y = T)
colnames(mydata)[2] <-  'Country'
colnames(mydata)[3] <-  'reg'
mydata <- merge(locs, mydata, by.x =c('location_id'), by.y = c('reg'), all.x = F, all.y = T)
colnames(mydata)[2] <-  'Region'

colnames(mydata)[3] <-  'spr'
mydata <- merge(locs, mydata, by.x =c('location_id'), by.y = c('spr'), all.x = F, all.y = T)
colnames(mydata)[2] <-  'Super region'

mydata$Region[mydata$ADM0_NAME == "Kosovo"] <- "Central Europe"
mydata$`Super region`[mydata$ADM0_NAME == "Kosovo"] <- "Central Europe, Eastern Europe, and Central Asia"

mydata$Region[mydata$ADM0_NAME == "Western Sahara"] <- "Western Sub-Saharan Africa"
mydata$`Super region`[mydata$ADM0_NAME == "Western Sahara"] <- "Sub-Saharan Africa"

mydata$Region[mydata$ADM0_NAME == "French Guiana"] <- "Tropical Latin America"
mydata$`Super region`[mydata$ADM0_NAME == "French Guiana"] <- "Latin America and Caribbean"


#summarise the median by super region-year
mydata <- data.table(mydata)
super_region_med <- mydata[,.(spr_reg_median = round(median(mean),2),
                              lower_IQR = round(quantile(mean, 0.25),2),
                              upper_IQR = round(quantile(mean, 0.75),2)),
                           by = c('Super region', 'year')]

region_med <- mydata[,.(reg_median = round(median(mean),2)),
                           by = c('Region', 'year')]
