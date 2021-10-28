rm(list = ls())
library(data.table)
library(foreign)

setwd('D:/Z_drive/Covariates/antibiotic_use')

ATC <- data.table(read.csv('results/ATC3_total_DDDs.csv', stringsAsFactors = F))
J01 <- data.table(read.csv('results/all_results.csv', stringsAsFactors = F))
names(J01) <-  tolower(names(J01))
J01$country <-  toupper(J01$country)

ATC <- dcast(ATC, country+year ~ ATC3, value.var = 'ddd_per_1000')
mydata <- merge(J01[,.(country, year, ddd_per_1000)], ATC, by = c('country', 'year'))

locs <- read.dbf('D:/Z_drive/Shapefiles/GBD2019/GBD2019_analysis_final_loc_set_22.dbf')
locs$country <-  toupper(locs$loc_name)
locs <- data.table(locs)
locs <- locs[,.(loc_id, parent_id, level, ihme_lc_id, country)]
#change the country name for a couple of ones which are subnational and country names
locs$country[locs$loc_id == 25344] <- 'NIGER STATE'
locs$country[locs$loc_id == 533] <- 'GEORGIA STATE'

mydata <- merge(mydata, locs, by = 'country', all.x = T, all.y = T)

#check for missing countries (level 3)
table(mydata$country[is.na(mydata$ddd_per_1000) & mydata$level == 3])

subnats <- mydata[mydata$level >3 | mydata$level == 0,]
table(substr(subnats$ihme_lc_id, 1, 3))

subnats <-  subnats[,.(country = rep(country, 19), 
                       year = rep(2000:2018, each = length(subnats$country)), 
                       loc_id = rep(loc_id, 19),
                       parent_id = rep(parent_id, 19),
                       level = 5,
                       ihme_lc_id = substr(ihme_lc_id, 1, 3))]

subnats <- merge(subnats, mydata[,c(2:11, 15)], by = c('ihme_lc_id', 'year'), all.x = T, all.y = F)
mydata <- mydata[!(mydata$loc_id %in% subnats$loc_id),]
mydata <-  rbind(mydata, subnats)

mydata <- mydata[,1:12]

#add the 1990-1999 as the 2000 value
imputed <- mydata[mydata$year == 2000,]
imputed <- imputed[,.(country = rep(country, 10), 
                      year = rep(1990:1999, each = length(country)),
                      ddd_per_1000 = rep(ddd_per_1000, 10),
                      J01A = rep(J01A, 10), 
                      J01B = rep(J01B, 10),
                      J01C = rep(J01C, 10),
                      J01D = rep(J01D, 10),
                      J01E = rep(J01E, 10),        
                      J01F = rep(J01F, 10),
                      J01G = rep(J01G, 10),
                      J01M = rep(J01M, 10),
                      loc_id = rep(loc_id, 10))]

covs <- rbind(mydata, imputed)
covs <- covs[,.(loc_id, year, ddd_per_1000, J01A, J01B, J01C, J01D, J01E, J01F, J01G, J01M)]
write.csv(covs, 'results/final_abx_cov_values.csv', row.names = F)

old_covs <- read.csv('C:/Users/annieb/Desktop/cleaned_covs.csv', stringsAsFactors = F)
old_covs$ddd_per_1000 <-  NULL
old_covs$J01A <- NULL
old_covs$J01B <- NULL
old_covs$J01C <- NULL
old_covs$J01D <- NULL
old_covs$J01E <- NULL
old_covs$J01F <- NULL
old_covs$J01G <- NULL
old_covs$J01M <- NULL

new_covs <- merge(old_covs, covs, by.x = c('location_id', 'year_id'), by.y = c('loc_id', 'year'))
write.csv(new_covs, 'C:/Users/annieb/Desktop/cleaned_covs.csv', row.names = F)
