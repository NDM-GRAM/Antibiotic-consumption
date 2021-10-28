#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Clean the IQVIA data for 2014-2018      #
# Convert the standard units to kilograms #
# and calculate DDDs                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(data.table)

#Read in raw data
setwd('Z:/AMR/Covariates/antibiotic_use/IQVIA')
IQVIA <- read.csv('datasets/raw_data/IQVIA_raw_kg.csv', stringsAsFactors = F)
IQVIA <- data.table(IQVIA)
colnames(IQVIA) <- c('antimicrobial', 'country', 'channel', 'year', 'kg')

#~~~~~~~~~~~~~~~~~~~~~#
#i. Basic cleaning ####
#~~~~~~~~~~~~~~~~~~~~~#
#remove negative values
summary(IQVIA$kg)
IQVIA$kg[IQVIA$kg<0] <- NA 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#1. Summarise which countries have data from each channel ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
channel <- unique(IQVIA[,.(country, channel)])
channel$ind <- 1
channel <- dcast(channel, country ~ channel, value.var = 'ind')
channel$source <- NA
channel$source[channel$Hospital == 1 & channel$Retail == 1] <-  'Hospital & Retail'
channel$source[channel$Hospital == 1 & channel$Combined == 1] <-  'Hospital & Combined'
channel$source[channel$Retail == 1 & is.na(channel$Hospital) & is.na(channel$Combined)] <-  'Retail only'
channel$source[channel$Hospital == 1 & is.na(channel$Retail) & is.na(channel$Combined)] <-  'Hospital only'
channel$source[channel$Combined == 1 & is.na(channel$Retail) & is.na(channel$Hospital)] <-  'Combined only'
channel$source[channel$All == 1 & is.na(channel$Retail) & is.na(channel$Hospital)& is.na(channel$Combined)] <-  'All only'

channel <- channel[,.(country, source)]
table(channel$source)

write.csv(channel, 'datasets/IQVIA_channels.csv', row.names = F)
rm(channel)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#2. reshape to have the kgs by channel ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
mydata <- dcast(IQVIA, country + year + antimicrobial ~ channel, value.var = 'kg')
rm(IQVIA)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#3. Match to ATC categories ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
ATC <- read.csv('lookup_tables/ATC5_lookup.csv', stringsAsFactors = F, na.strings = "")

#only includes substances meeting inclusion criteria so this will drop everything
#merge onto the data
mydata <- merge(mydata, ATC, by = 'antimicrobial', all.x=F, all.y = T)  
rm(ATC)

#correct Ecuador Clarithromycin data
#IQVIAs conversion between SU and KG is wrong here, see 'investigating_IQVIA for details. Have recalculated this.
mydata$Retail[mydata$country=='Ecuador' & mydata$antimicrobial == 'CLARITHROMYCIN' & mydata$year == 2014] <- 1837.82
mydata$Retail[mydata$country=='Ecuador' & mydata$antimicrobial == 'CLARITHROMYCIN' & mydata$year == 2015] <- 1650.79
mydata$Retail[mydata$country=='Ecuador' & mydata$antimicrobial == 'CLARITHROMYCIN' & mydata$year == 2016] <- 1627.08
mydata$Retail[mydata$country=='Ecuador' & mydata$antimicrobial == 'CLARITHROMYCIN' & mydata$year == 2017] <- 1567.97
mydata$Retail[mydata$country=='Ecuador' & mydata$antimicrobial == 'CLARITHROMYCIN' & mydata$year == 2018] <- 1651.4

#~~~~~~~~~~~~~~~~~~~~~~~~~~#
#4. Match to DDD values ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~#
#read in and merge on DDD
DDD <- read.csv('lookup_tables/DDD_lookup.csv', stringsAsFactors = F, na.strings = "")

DDD$DDD_O.g. <-  NULL
DDD$DDD_P.g. <-  NULL

for(i in 1:4){
  mydata <- merge(mydata, DDD, by.x = paste0('ATC5_', i), by.y = 'ATC5', allow.cartesian=TRUE, all.x = T)
  colnames(mydata)[colnames(mydata) == 'DDD'] <- paste0('DDD_', i)
}
rm(DDD, i)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#5. Split up relevent combinations and assign to component antibiotics ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
props <- read.csv('Z:/AMR/Covariates/antibiotic_use/IQVIA/lookup_tables/combined_substance_lookup.csv', stringsAsFactors = F)
colnames(props) <- gsub('X.', '', colnames(props) )

mydata <- merge(mydata, props, by = 'ATC5', all.x = T, all.y = T, allow.cartesian = T)

#if only one substance then set this too 100%
mydata$compound_1[is.na(mydata$compound_1)] <- 100

mydata$hospital_ddd_1 <- (mydata$Hospital/(mydata$DDD_1/1000))*(mydata$compound_1/100)
mydata$hospital_ddd_2 <- (mydata$Hospital/(mydata$DDD_2/1000))*(mydata$compound_2/100)
mydata$hospital_ddd_3 <- (mydata$Hospital/(mydata$DDD_3/1000))*(mydata$compound_3/100)
mydata$hospital_ddd_4 <- (mydata$Hospital/(mydata$DDD_4/1000))*(mydata$compound_4/100)

mydata$retail_ddd_1 <- (mydata$Retail/(mydata$DDD_1/1000))*(mydata$compound_1/100)
mydata$retail_ddd_2 <- (mydata$Retail/(mydata$DDD_2/1000))*(mydata$compound_2/100)
mydata$retail_ddd_3 <- (mydata$Retail/(mydata$DDD_3/1000))*(mydata$compound_3/100)
mydata$retail_ddd_4 <- (mydata$Retail/(mydata$DDD_4/1000))*(mydata$compound_4/100)

mydata$combined_ddd_1 <- (mydata$Combined/(mydata$DDD_1/1000))*(mydata$compound_1/100)
mydata$combined_ddd_2 <- (mydata$Combined/(mydata$DDD_2/1000))*(mydata$compound_2/100)
mydata$combined_ddd_3 <- (mydata$Combined/(mydata$DDD_3/1000))*(mydata$compound_3/100)
mydata$combined_ddd_4 <- (mydata$Combined/(mydata$DDD_4/1000))*(mydata$compound_4/100)

#reshape to long
clean_data <- melt(mydata, id.vars = c("country", "year"),
                   measure.vars = list(c("ATC5_1", "ATC5_2", "ATC5_3"),
                                       c("hospital_ddd_1", "hospital_ddd_2", "hospital_ddd_3"),
                                       c("retail_ddd_1", "retail_ddd_2", "retail_ddd_3"),
                                       c("combined_ddd_1", "combined_ddd_2", "combined_ddd_3")))
  
clean_data$variable <- NULL                                     
colnames(clean_data) <- c("country", "year", 'ATC5', 'hospital_ddd', 'retail_ddd', 'combined_ddd')

#remove non J01 and J04 antibiotics
clean_data <- clean_data[(grepl('J01', clean_data$ATC5)) |(grepl('J04', clean_data$ATC5)),]

#get the main groups
clean_data$ATC3 <- substr(clean_data$ATC5, 1, 4)
clean_data$ATC4 <- substr(clean_data$ATC5, 1, 5)

# aggregate same antibiotics together
clean_data <- clean_data[, .(hospital_ddd = sum(hospital_ddd),
                             retail_ddd = sum(retail_ddd), 
                             combined_ddd = sum(combined_ddd)),
                         by = c('country', 'year','ATC3', 'ATC4', 'ATC5')]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#6. Match to the location info ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(foreign)

mydata <- clean_data

locs <- read.dbf("Z:/AMR/Shapefiles/GBD2019/GBD2019_analysis_final_loc_set_22.dbf")

locs <- locs[c('spr_reg_id',
               'region_id',
               'loc_id',
               'ihme_lc_id',
               'GAUL_CODE',
               'loc_name')]

locs$loc_name <- as.character(locs$loc_name)
locs$ihme_lc_id <- as.character(locs$ihme_lc_id)
locs$loc_name[locs$loc_name == 'Hong Kong Special Administrative Region of China'] <- 'Hong Kong'

mydata$country[mydata$country == 'Bosnia-Herzegovina'] <- 'Bosnia and Herzegovina' 
mydata$country[mydata$country == 'Slovak Republic'] <- 'Slovakia' 
mydata$country[mydata$country == 'UK'] <- 'United Kingdom' 
mydata$country[mydata$country == 'USA'] <- 'United States' 
mydata$country[mydata$country == 'China'] <- 'China (without Hong Kong and Macao)' 

mydata <- data.frame(mydata)
mydata <- merge(mydata, locs, by.x = 'country', by.y = 'loc_name', all.x = T, all.y = F) 

unique(mydata$country[is.na(mydata$loc_id)])
mydata$country[mydata$country == 'China (without Hong Kong and Macao)'] <- 'China'
mydata$ihme_lc_id [mydata$country == 'China'] <- 'CHN'

mydata$spr_reg_id[mydata$country == 'Central America'] <-  130
mydata$region_id[mydata$country == 'Central America'] <-  124
mydata$GAUL_CODE[mydata$country == 'Central America'] <-  9999
mydata$loc_id[mydata$country == 'Central America'] <-  9999

mydata$spr_reg_id[mydata$country == 'French West Africa'] <-  166
mydata$region_id[mydata$country == 'French West Africa'] <-  199
mydata$GAUL_CODE[mydata$country == 'French West Africa'] <-  9998
mydata$loc_id[mydata$country == 'French West Africa'] <-  9998

mydata <- data.table(mydata)
mydata <- mydata[,.(spr_reg_id, region_id, country, ihme_lc_id, loc_id, GAUL_CODE, year, ATC3, ATC4, ATC5, hospital_ddd, retail_ddd, combined_ddd)]
colnames(mydata) <-  c('super_region', 'region', 'country', 'iso3', 'loc_id', 'GAUL_CODE', 'year', "ATC3", "ATC4", "ATC5", "hospital_ddd", "retail_ddd", "combined_ddd")
rm(locs, clean_data, props)

mydata$iso3[mydata$country == 'French West Africa'] <- 'FWA'
mydata$iso3[mydata$country == 'Central America'] <- 'CAM'

write.csv(mydata, 'datasets/cleaned_ddds_2014_2018.csv', row.names = F)
#~~~~~#
# END #
#~~~~~#