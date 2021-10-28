#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Clean up the datasets for the models #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(plyr)
library(data.table)
library(foreign)
library(ggplot2)
rm(list = ls())

setwd('Z:/AMR/Covariates/antibiotic_use/')

IQVIA <-data.table(read.csv('IQVIA/imputation/imputed_ddd_per_1000_2000_2018.csv', stringsAsFactors = F))
WHO <- read.csv("WHO/ATC_DDD_per_1000_clean.csv", stringsAsFactors = F)
KEN <- read.csv('other_sources/KEN/full_KEN_data.csv', stringsAsFactors = F)
WSM <- read.csv('other_sources/WSM/WSM_abx_ddd.csv', stringsAsFactors = F)

#Use denmark and netherlands from ESAC net not IQVIA as have fill time series
IQVIA <- IQVIA[IQVIA$country != 'DENMARK' & IQVIA$country != 'NETHERLANDS',]

#Read in J01 totals
ESAC <- read.csv('ESAC-NET/cleaned_datasets/J01_DDD.csv', stringsAsFactors = F)

#restrict ESAC-NET to only countries not in the dataset
ESAC <- ESAC[!(ESAC$iso3 %in% IQVIA$iso3),] 

#Remove duplicate data (keep brazil as is very different)
WHO <- WHO[!(WHO$iso3 %in% ESAC$iso3),] 
WHO <- WHO[!(WHO$iso3 %in% IQVIA$iso3) | WHO$iso3 == 'BRA',] 

#Capitalise all countries for consistancy
WHO$country <-  toupper(WHO$country)
KEN$country <-  toupper(KEN$country)
WSM$country <-  toupper(WSM$country)
ESAC$country <-  toupper(ESAC$country)

#set the source
IQVIA$source <- 'IQVIA'
WHO$source <- 'WHO'
KEN$source <- 'KEN'
WSM$source <- 'WSM'
ESAC$source <- 'ESAC'

# split the french west Africa and Central America data between the encompasing countries ####
#get the population numbers
pop <- read.csv('Z:/AMR/Misc/GBD_populations/GBD_total_populations.csv', stringsAsFactors = F)
pop <- data.table(pop)
names(pop) <- c('loc_id', 'year', 'pop')
pop <- pop[year>=2000 & year <=2018,]

#calculate popuation of french west africa and central america
FWA <- c(200, 201, 205, 202, 170, 173, 208, 211, 216, 218)
CA <- c(126, 127, 128, 129, 131 ,132)

FWA_pop <- pop[loc_id %in% FWA,]
CA_pop <- pop[loc_id %in% CA,]

rm(CA, FWA)

#get the location info
locs <- read.dbf("Z:/AMR/Shapefiles/GBD2019/GBD2019_analysis_final_loc_set_22.dbf")

locs <- locs[c('spr_reg_id',
               'region_id',
               'loc_id',
               'ihme_lc_id',
               'GAUL_CODE',
               'loc_name')]

locs$loc_name <- as.character(locs$loc_name)
locs$ihme_lc_id <- as.character(locs$ihme_lc_id)

FWA_pop <- merge(FWA_pop, locs) 
CA_pop <- merge(CA_pop, locs) 

#get the proportion in each country each year and multiply the DDD by this
FWA_pop <- data.table(FWA_pop)
FWA_total_pop <- FWA_pop[,.(total_pop = sum(pop)),
                         by = c('year')] 

FWA_pop <- merge(FWA_pop, FWA_total_pop, by = 'year')
rm(FWA_total_pop)
FWA_pop$prop <- FWA_pop$pop/FWA_pop$total_pop
FWA <- IQVIA[iso3 == 'FWA']
FWA <- FWA[,.(year, ATC3, ATC4, ATC5, total_ddd, total_ddd_per_1000_pop)]
FWA_pop <- merge(FWA_pop, FWA, by = 'year', allow.cartesian = T)
rm(FWA)
FWA_pop$total_ddd <- FWA_pop$total_ddd*FWA_pop$prop 

FWA_pop <- FWA_pop[,.(super_region = spr_reg_id,
                      region = region_id,
                      country = toupper(loc_name),
                      iso3 = ihme_lc_id,
                      loc_id,
                      GAUL_CODE,
                      year, 
                      ATC3, ATC4, ATC5, pop,
                      total_ddd, total_ddd_per_1000_pop, 
                      source = 'IQVIA')]

CA_pop <- data.table(CA_pop)
CA_total_pop <- CA_pop[,.(total_pop = sum(pop)),
                       by = c('year')] 

CA_pop <- merge(CA_pop, CA_total_pop, by = 'year')
rm(CA_total_pop)
CA_pop$prop <- CA_pop$pop/CA_pop$total_pop
CA <- IQVIA[iso3 == 'CAM']
CA <- CA[,.(year, ATC3, ATC4, ATC5, total_ddd, total_ddd_per_1000_pop)]
CA_pop <- merge(CA_pop, CA, by = 'year', allow.cartesian = T)
rm(CA)
CA_pop$total_ddd <- CA_pop$total_ddd*CA_pop$prop 

CA_pop <- CA_pop[,.(super_region = spr_reg_id,
                      region = region_id,
                      country = toupper(loc_name),
                      iso3 = ihme_lc_id,
                      loc_id,
                      GAUL_CODE,
                      year, 
                      ATC3, ATC4, ATC5, pop,
                      total_ddd, total_ddd_per_1000_pop, 
                      source = 'IQVIA')]

#remove FWA and CA in the dataset with these values
IQVIA <- IQVIA[iso3 != 'FWA']
IQVIA <- IQVIA[iso3 != 'CAM']

#combine all data
mydata <- rbind.fill(IQVIA, KEN, WHO, WSM, CA_pop, FWA_pop)
mydata <-  data.table(mydata)
mydata$ATC3 <-  trim(mydata$ATC3)

#collapse to desired variables
J01 <- mydata[mydata$ATC3 != 'J04A',]

J01 <- J01[,.(ddd = sum(total_ddd),
                 ddd_per_1000 = sum(total_ddd_per_1000_pop)),
              by = c("super_region", "region", "country", "iso3", "loc_id", "GAUL_CODE", "year", "pop", 'source')]

ATC3 <- mydata[,.(ddd = sum(total_ddd),
                  ddd_per_1000 = sum(total_ddd_per_1000_pop)),
               by = c("super_region", "region", "country", "iso3", "loc_id", "GAUL_CODE", "year", "pop", "ATC3", 'source')]

TB <- ATC3[ATC3 == 'J04A']
ATC3 <- ATC3[ATC3!='J04A']

#reshape ATC3
ATC3 <- dcast(ATC3, super_region + region + country + iso3 + loc_id + GAUL_CODE + year + pop + source ~ ATC3, value.var = 'ddd_per_1000')

#merge ESAC-net onto the collapsed data
J01 <- rbind(J01, ESAC)

#save files
write.csv(J01, 'combined_sources/J01_DDD_2000_2018.csv', row.names = F)
write.csv(TB, 'combined_sources/TB_DDD_2000_2018.csv', row.names = F)
write.csv(ATC3, 'combined_sources/ATC3_DDD_2000_2018.csv', row.names = F)

J01$source <-  as.factor(J01$source)

#plot out the raw data
pdf('combined_sources/input_ddd_per_1000.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(J01$super_region))){
  subset <- J01[J01$super_region == unique(J01$super_region)[i],]
  print(ggplot(subset)+
          geom_point(aes(x = year, y = ddd_per_1000, colour = source))+
          facet_wrap(~country)+
          ylim(0,22000)
  )}
dev.off()

pdf('combined_sources/TB_ddd_per_1000.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(J01$super_region))){
  subset <- TB[TB$super_region == unique(TB$super_region)[i],]
  print(ggplot(subset)+
          geom_point(aes(x = year, y = ddd_per_1000, colour = source))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

#~~~~~#
# END #
#~~~~~#
