#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Impute the missing hosptital/retail values for country-years missing that channel #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list =ls())
library(mice)
library(VIM)
library(sjmisc)
library(data.table)
library(ggplot2)

setwd('Z:/AMR/Covariates/antibiotic_use/IQVIA')

#1. Prep data for imputation ####
# Read in the cleaned DDD dataset
mydata <- read.csv('datasets/cleaned_ddds_2000_2018.csv', stringsAsFactors = F)
mydata <-  data.table(mydata)

#remove China 2011 as retails is 5* lower than for other years
mydata$retail_ddd_per_1000_pop[mydata$country=='CHINA' & mydata$year == 2011] <- NA
# mydata$hospital_ddd_per_1000_pop[mydata$country=='CHINA' & mydata$year == 2011] <- NA
mydata$loc_id[mydata$country == 'CHINA'] <- 44533
mydata$GAUL_CODE[mydata$country == 'CHINA'] <- -1

#remove the DDD numbers (will back calculate these later)
mydata$retail_ddd <-  NULL
mydata$hospital_ddd <-  NULL
mydata$combined_ddd <-  NULL

#ensure that 0's are correct and the NAs are there where there was no data collected for that channel
data.availability <- mydata[,.(hospital = sum(hospital_ddd_per_1000_pop, na.rm = T),
                   retail = sum(retail_ddd_per_1000_pop, na.rm = T),
                   combined = sum(combined_ddd_per_1000_pop, na.rm = T)),
                by = c('country', 'year')]

data.availability$hospital[data.availability$hospital == 0] <- NA
data.availability$retail[data.availability$retail == 0] <- NA
data.availability$combined[data.availability$combined == 0] <- NA
data.availability$hospital[data.availability$hospital > 0] <- 0
data.availability$retail[data.availability$retail > 0] <- 0
data.availability$combined[data.availability$combined > 0] <- 0

mydata <- merge(mydata, data.availability, by = c('country', 'year'))

#drop country years which have no data
mydata <- mydata[!(is.na(mydata$hospital) & is.na(mydata$retail) & is.na(mydata$combined)),]

#ensure that 0's and NAs are correct
mydata$hospital_ddd_per_1000_pop[is.na(mydata$hospital_ddd_per_1000_pop) | mydata$hospital_ddd_per_1000_pop == 0] <- mydata$hospital[is.na(mydata$hospital_ddd_per_1000_pop) | mydata$hospital_ddd_per_1000_pop == 0]
mydata$retail_ddd_per_1000_pop[is.na(mydata$retail_ddd_per_1000_pop) | mydata$retail_ddd_per_1000_pop == 0] <- mydata$retail[is.na(mydata$retail_ddd_per_1000_pop) | mydata$retail_ddd_per_1000_pop == 0]
mydata$combined_ddd_per_1000_pop[is.na(mydata$combined_ddd_per_1000_pop) | mydata$combined_ddd_per_1000_pop == 0] <- mydata$combined[is.na(mydata$combined_ddd_per_1000_pop) | mydata$combined_ddd_per_1000_pop == 0]

mydata$hospital <-  NULL
mydata$retail <-  NULL
mydata$combined <-  NULL
rm(data.availability)

# remove excess columns - these are all country specific and dont want to put too duplicate these measures in the model
extra <- mydata[,.(country, iso3, loc_id, GAUL_CODE, year, pop)]
extra <- unique(extra)
mydata$iso3 <-  NULL
mydata$loc_id <-  NULL
mydata$GAUL_CODE <-  NULL
mydata$pop <- NULL

# 2. Run the imputation using the CART method (other algorithms were trialled) ####
# Run the imputations for each ATC3 class seperately                              #
env.covs <- data.frame(mydata)

# Remove records with combined DDDs as want to use 
# this value and not impute retail and hospital
env.covs <- env.covs[is.na(env.covs$combined_ddd_per_1000_pop),]
env.covs$combined_ddd_per_1000_pop <-  NULL

temp_data <- env.covs
ATC3 <- unique(temp_data$ATC3)
  
for(a in ATC3){
    env.covs <- temp_data[temp_data$ATC3 == a,]

    #CHECK WHATS MISSING IN THE VARIABLES
    png(paste0('imputation/', a, '_missing_data.png'),
        height = 20, width = 20, unit = 'cm', res = 200)

    aggr(env.covs, col=c('navyblue','yellow'),
         numbers=TRUE, sortVars=TRUE,
         labels=gsub('_per_1000_pop', '', names(env.covs)), cex.axis=.7,
         gap=3, ylab=c("Missing data","Pattern"))

    dev.off()
    ##IMPUTE THE MISSING VALUES
    imputed.data <- mice(data = env.covs, m=10, maxit = 100, method = 'cart', seed = 1234)
    
    #plot the imputations
    png(paste0('imputation/', a, '_imputations.png'),
        height = 20, width = 20, unit = 'cm', res = 200)

   print(plot(imputed.data))
    dev.off()

    png(paste0('imputation/', a, '_hospital_imputations2.png'),
        height = 20, width = 20, unit = 'cm', res = 200)
    print(stripplot(imputed.data, hospital_ddd_per_1000_pop~.imp, pch=20, cex=2))
    dev.off()
    
    png(paste0('imputation/', a, '_retail_imputations2.png'),
        height = 20, width = 20, unit = 'cm', res = 200)
    print(stripplot(imputed.data, retail_ddd_per_1000_pop~.imp, pch=20, cex=2))
    dev.off()
    
    #MERGE WITH THE ORIGINAL DATA FRAME
    imputed.data <- merge_imputations(env.covs, imputed.data,
                                      ori = env.covs, summary = c("hist"))
    
    png(paste0('imputation/', a, '_imputations3.png'),
        height = 20, width = 20, unit = 'cm', res = 200)
    print(imputed.data)
    dev.off()
    
    #EXTRACT THE IMPUTE VALUES FOR VARIABLES OF INTEREST
    imputed.data <- imputed.data$data
    
    #if retail hasnt been imputed then add the column
    if(sum(imputed.data$retail_ddd_per_1000_pop_imp, na.rm = T) == 0){
      imputed.data$retail_ddd_per_1000_pop_imp <- imputed.data$retail_ddd_per_1000_pop
    }
    #if hospital hasnt been imputed then add the column
    if(sum(imputed.data$hospital_ddd_per_1000_pop_imp, na.rm = T) == 0){
      imputed.data$hospital_ddd_per_1000_pop_imp <- imputed.data$hospital_ddd_per_1000_pop
    }
    
    #squeeze the imputations to the max and min of what is in the original dataset, ensuring plausible values
    imputed.data$retail_ddd_per_1000_pop_imp <- squeeze(imputed.data$retail_ddd_per_1000_pop_imp, c(min(imputed.data$retail_ddd_per_1000_pop, na.rm = T), max(imputed.data$retail_ddd_per_1000_pop, na.rm = T)))
    imputed.data$hospital_ddd_per_1000_pop_imp <- squeeze(imputed.data$hospital_ddd_per_1000_pop_imp, c(min(imputed.data$hospital_ddd_per_1000_pop, na.rm = T), max(imputed.data$hospital_ddd_per_1000_pop, na.rm = T)))
    
    imputed.data <- data.table(imputed.data)

    #clean to selected variables
    imputed.data <- imputed.data[,.(super_region, region, country,
                                    year,
                                    ATC3, ATC4, ATC5,
                                    hospital_ddd_per_1000_pop = hospital_ddd_per_1000_pop_imp, 
                                    retail_ddd_per_1000_pop = retail_ddd_per_1000_pop_imp, 
                                    combined_ddd_per_1000_pop = NA)]
    
    #merge datasets together
    if(a == ATC3[1]){
      combined_data1 <- imputed.data
    } else {
      combined_data1 <- rbind(combined_data1, imputed.data)
    }
  }  
  
    combined_data <- combined_data1

#3. Clean up imputed dataset ####
#add the data with combined DDDs to this data
combined_ddd <- mydata[!is.na(mydata$combined_ddd_per_1000_pop),]
imputed.data <- rbind(combined_data, combined_ddd)

#sum to get total DDDs
imputed.data$total_ddd_per_1000_pop <- rowSums(imputed.data[,.(hospital_ddd_per_1000_pop, retail_ddd_per_1000_pop, combined_ddd_per_1000_pop)], na.rm = T)

#add on the extra location columns
extra <- extra[extra$loc_id != 6,] #have just the china without Hong Kong
imputed.data <- merge(imputed.data, extra, by = c('country', 'year'), allow.cartesian = T)

#back calculate the total DDDs
imputed.data$total_ddd <- imputed.data$total_ddd_per_1000_pop*(imputed.data$pop/1000)
imputed.data$hospital_ddd <- imputed.data$hospital_ddd_per_1000_pop*(imputed.data$pop/1000)
imputed.data$retail_ddd <- imputed.data$retail_ddd_per_1000_pop*(imputed.data$pop/1000)
imputed.data$combined_ddd <- imputed.data$combined_ddd_per_1000_pop*(imputed.data$pop/1000)

#order columns
imputed.data <- imputed.data[,.(super_region, region, country, iso3, loc_id, GAUL_CODE,
                                year,
                                ATC3, ATC4, ATC5,
                                pop,
                                hospital_ddd, retail_ddd, combined_ddd, total_ddd,
                                hospital_ddd_per_1000_pop, retail_ddd_per_1000_pop, combined_ddd_per_1000_pop, total_ddd_per_1000_pop)]

write.csv(imputed.data, paste0('imputation/imputed_ddd_per_1000_2000_2018.csv'), row.names = F)

# 4. Plot out the data ####
#to compare to the total from non imputed
#aggregate to global and plot
global_data <- imputed.data[,.(total_ddd_per_1000 = sum(total_ddd_per_1000_pop, na.rm = T)),
                      by = 'year']

country_data <- imputed.data[,.(total_ddd_per_1000 = sum(total_ddd_per_1000_pop, na.rm = T),
                                hospital_ddd = sum(hospital_ddd_per_1000_pop),
                                retail_ddd = sum(retail_ddd_per_1000_pop)),
                       by = c('country','super_region', 'year')]


png('imputation/global_total_ddds_per_1000.png')
ggplot(global_data)+
  geom_point(aes(x = year, y = log(total_ddd_per_1000)))+
  scale_x_continuous("Year", 
                     breaks = seq(2000, 2018, 1),
                     labels = 2000:2018)
dev.off()


pdf('imputation/country_total_ddds.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset)+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country)+
          ylim(0,15000)
  )}
dev.off()

#~~~~~#
# END #
#~~~~~#