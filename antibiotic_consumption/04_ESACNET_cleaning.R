#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Clean and impute missing data for ECDC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(xlsx)
library(data.table)
library(foreign)
library(mice)
library(VIM)
library(sjmisc)

setwd('Z:/AMR/Covariates/antibiotic_use/ESAC-NET/')

#1. Data prep ####
total <- read.xlsx('full_J01_esac_net.xlsx', sheetName = 'combined')
community <- read.xlsx('full_J01_esac_net.xlsx', sheetName = 'community')
hosp <- read.xlsx('full_J01_esac_net.xlsx', sheetName = 'hospital')

hosp <- data.table(hosp)
community <- data.table(community)
total <- data.table(total)

#reshape
cols <- colnames(hosp)[grep('X', colnames(hosp))]

hosp <- melt(hosp, id.vars = 'country', measure.vars = cols, variable.name = 'year', value.name = 'hospital_ddd_per_1000_pop')
hosp$year <- as.character(hosp$year)
hosp$year <- gsub('X', '', hosp$year)
hosp$hospital_ddd_per_1000_pop <- hosp$hospital_ddd_per_1000_pop*365

community <- melt(community, id.vars = 'country', measure.vars = cols, variable.name = 'year', value.name = 'retail_ddd_per_1000_pop')
community$year <- as.character(community$year)
community$year <- gsub('X', '', community$year)
community$retail_ddd_per_1000_pop <- community$retail_ddd_per_1000_pop*365

total <- melt(total, id.vars = 'country', measure.vars = cols, variable.name = 'year', value.name = 'total_ddd_per_1000_pop')
total$year <- as.character(total$year)
total$year <- gsub('X', '', total$year)
total$total_ddd_per_1000_pop <- total$total_ddd_per_1000_pop*365

mydata <- merge(hosp, community, all.x = T, all.y = T)
mydata <- merge(mydata, total, all.x = T, all.y = T)
mydata$year <- as.numeric(mydata$year)
rm(hosp, community, total, cols)

#remove any countries with no data
mydata <- mydata[!(is.na(mydata$hospital_ddd_per_1000_pop) & is.na(mydata$retail_ddd_per_1000_pop) & is.na(mydata$total_ddd_per_1000_pop)),]
mydata$country <- as.character(mydata$country)

#merge on the population
pop <- read.csv('Z:/AMR/Misc/GBD_populations/GBD_total_populations.csv', stringsAsFactors = F)
pop <- data.table(pop)
locs <- read.dbf('Z:/AMR/Shapefiles/GBD2019/GBD2019_analysis_final.dbf')
locs <- locs[locs$level == 3,]
locs$loc_name <- as.character(locs$loc_name)
locs$ihme_lc_id <- as.character(locs$ihme_lc_id)
locs <- locs[c('spr_reg_id',
               'region_id',
               'loc_id',
               'ihme_lc_id',
               'GAUL_CODE',
               'loc_name')]
pop <- merge(pop, locs, by.x = 'location_id', by.y = 'loc_id')

mydata <- merge(mydata, pop, by.x = c('country', 'year'), by.y = c('loc_name', 'year_id'), all.x = T, all.y = F)
rm(locs, pop)

colnames(mydata)[colnames(mydata) == 'ihme_lc_id'] <- 'iso3'
colnames(mydata)[colnames(mydata) == 'location_id'] <- 'loc_id'

#2. Impute missing values ####
# remove excess columns
env.covs <- mydata
env.covs$iso3 <-  NULL
env.covs$loc_id <-  NULL
env.covs$GAUL_CODE <-  NULL

png('Z:/AMR/Covariates/antibiotic_use/ESAC-NET/missing_data.png',
    height = 20, width = 20, units = 'cm', res = 200)
aggr(env.covs, col=c('navyblue','yellow'),
     numbers=TRUE, sortVars=TRUE,
     labels=gsub('_per_1000_pop', '', names(env.covs)), cex.axis=.7,
     gap=3, ylab=c("Missing data","Pattern"))
dev.off()

env.covs$total_ddd_per_1000_pop <-  NULL

##IMPUTE THE MISSING VALUES
#for final estimates can increase maxit = 1000 and m = 10 (but this will take quite a while) --> run for a while on the cluster
imputed.data <- mice(data = env.covs, m=10, maxit = 100, method = 'cart', seed = 1234)

png('Z:/AMR/Covariates/antibiotic_use/ESAC-NET/imputation.png',
    height = 20, width = 20, units = 'cm', res = 200)
  plot(imputed.data)
dev.off()
 
png('Z:/AMR/Covariates/antibiotic_use/ESAC-NET/imputation2.png',
    height = 20, width = 20, unit = 'cm', res = 200)
  stripplot(imputed.data, hospital_ddd_per_1000_pop~.imp, pch=20, cex=2)
dev.off()

#MERGE WITH THE ORIGINAL DATA FRAME
imputed.data <- merge_imputations(mydata, imputed.data,
                                  ori = mydata, summary = c("hist"))
png('Z:/AMR/Covariates/antibiotic_use/ESAC-NET/imputation3.png',
    height = 20, width = 20, unit = 'cm', res = 200)
imputed.data
dev.off()

#EXTRACT THE IMPUTE VALUES FOR VARIABLES OF INTEREST
imputed.data <- imputed.data$data

#squeeze the imputations to the max and min of what is in the original dataset 
imputed.data$retail_ddd_per_1000_pop_imp <- squeeze(imputed.data$retail_ddd_per_1000_pop_imp, c(min(imputed.data$retail_ddd_per_1000_pop, na.rm = T), max(imputed.data$retail_ddd_per_1000_pop, na.rm = T)))
imputed.data$hospital_ddd_per_1000_pop_imp <- squeeze(imputed.data$hospital_ddd_per_1000_pop_imp, c(min(imputed.data$hospital_ddd_per_1000_pop, na.rm = T), max(imputed.data$hospital_ddd_per_1000_pop, na.rm = T)))

imputed.data <- data.table(imputed.data)

#3. Clean up imputed dataset ####
imputed.data$retail_ddd_per_1000_pop[is.na(imputed.data$retail_ddd_per_1000_pop)] <- imputed.data$retail_ddd_per_1000_pop_imp[is.na(imputed.data$retail_ddd_per_1000_pop)]
imputed.data$hospital_ddd_per_1000_pop[is.na(imputed.data$hospital_ddd_per_1000_pop)] <- imputed.data$hospital_ddd_per_1000_pop_imp[is.na(imputed.data$hospital_ddd_per_1000_pop)]
imputed.data$retail_ddd_per_1000_pop_imp <-  NULL
imputed.data$hospital_ddd_per_1000_pop_imp <-  NULL
imputed.data$total_ddd_per_1000_pop[is.na(imputed.data$total_ddd_per_1000_pop)] <- rowSums(imputed.data[,.(hospital_ddd_per_1000_pop, retail_ddd_per_1000_pop)], na.rm = T)[is.na(imputed.data$total_ddd_per_1000_pop)]

#calculate total DDDs
imputed.data$retail_ddd <- imputed.data$retail_ddd_per_1000_pop*(imputed.data$population/1000)
imputed.data$hospital_ddd <- imputed.data$hospital_ddd_per_1000_pop*(imputed.data$population/1000)
imputed.data$total_ddd <- imputed.data$total_ddd_per_1000_pop*(imputed.data$population/1000)

#subset to desired variables
imputed.data <- imputed.data[,.(super_region = spr_reg_id, region = region_id, country, iso3, loc_id, GAUL_CODE,
                                year,
                                ddd = total_ddd, 
                                ddd_per_1000 = total_ddd_per_1000_pop)
                             ]
write.csv(imputed.data, 'cleaned_datasets/J01_DDD.csv', row.names = F)
#~~~~~#
# END #
#~~~~~#