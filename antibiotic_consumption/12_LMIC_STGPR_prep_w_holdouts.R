#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Prep antibitoic consumption data for STGPR with holdouts # 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(data.table)
library(foreign)
rm(list = ls())

date = '2020_10_05'

#get the location files
locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')

#read in the master LMIC dataset
lmic_master <- read.csv(paste0('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/holdouts/', date, '/LMIC_master_data.csv'), stringsAsFactors = F)

# Prep the input data
# Ensure all correct variables for input data
input_data <- read.csv('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/STGPR_input_2020_10_05_outliered.csv', stringsAsFactors = F)

input_data <- data.table(input_data)

# Combine China and Hong Kong to get loc id 6.
# CHN_HGK <- input_data[input_data$location_id == 354 | input_data$location_id == 44533, ]
# input_data <- input_data[input_data$location_id != 354,]
# input_data <- input_data[input_data$location_id != 44533,]
# 
# CHN_HGK <- CHN_HGK[,.(location_id = 6,
#                       super_region = 4,
#                       region = 5 ,
#                       country = 'China',
#                       iso3 = 'CHN' ,
#                       GAUL_CODE = 33364,
#                       ddd = sum(ddd),
#                       pop = sum(pop)),
#                    by = c('year_id', 'source')]
# 
# CHN_HGK$ddd_per_1000 <-  CHN_HGK$ddd/(CHN_HGK$pop/1000)
# input_data <- rbind(input_data, CHN_HGK)
# rm(CHN_HGK)

#assign the fold IDs
lmic_folds <- unique(lmic_master[c('country', 'master_fold_id')])
country <- input_data$country[!(input_data$country %in% lmic_master$country)]
country <- unique(country)
country <- country[sample(length(country))]
master_fold_id <- cut(seq(1,length(country)),breaks=5,labels=FALSE)
folds <- as.data.table(cbind(country, master_fold_id))
folds <-  rbind(lmic_folds, folds)
input_data <- as.data.table(input_data)
input_data <- merge(input_data, folds, by = c('country'))
input_data$master_fold_id <- as.numeric(input_data$master_fold_id)

# input_data <- input_data[,.(nid = source,
#                             location_id = location_id,
#                             year_id = year_id,
#                             age_group_id = rep(22, length(input_data$location_id)),
#                             sex_id = rep(3, length(input_data$location_id)),
#                             measure = rep('continuous', length(input_data$location_id)),
#                             val = ddd_per_1000,
#                             variance = ddd_per_1000/(1000^2),
#                             sample_size = pop,
#                             is_outlier = rep(0, length(input_data$location_id)),
#                             master_fold_id)]

for(h in 1:5){
  # Get LMIC predications
  lmic <- read.csv(paste0('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/holdouts/', date, '/holdout_', h, '/custom_stage1_df.csv'), stringsAsFactors = F)
  lmic <- unique(lmic)
  lmic <- data.table(lmic)
  lmic[, cv_custom_stage_1 := exp(cv_custom_stage_1)]
  
  # Get HIC predications
  hic <- read.csv('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/input_data/imputed_HIC_J01.csv', stringsAsFactors = F)
  hic <- data.table(hic)
  hic <- hic[hic$year>=2000,]
  hic <- hic[,.(location_id = loc_id, year_id = year, age_group_id = 22, sex_id = 3, cv_custom_stage_1 = ddd_per_1000)]
  
  # Join together
  mydata <- rbind(lmic, hic)
  
  # reshape wide
  mydata <- dcast(mydata, location_id ~ year_id, value.var = 'cv_custom_stage_1')
  
  # Merge onto locations
  mydata <- merge(locs, mydata, by.x = 'loc_id', by.y = 'location_id', all.x = T, all.y = T)
  mydata <- data.table(mydata)
  mydata$loc_name <- as.character(mydata$loc_name)
  
  # Look at the missing values
  table(mydata$loc_name[mydata$level == 3 & is.na(mydata$`2014`)])
  
  # impute the values for subnationals
  #for subnational locations use the same value as the national
  subnats <- mydata[mydata$level == 4,]
  subnats <- subnats[,1:10]
  subnats <- merge(subnats, mydata[,.(loc_id,`2000`, `2001`,`2002`, `2003`,`2004`,`2005`, `2006`, `2007`, `2008`,
                                      `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`)],
                   by.x = 'parent_id', by.y = 'loc_id', all.x = T, all.y = F)
  
  #merge the subnationals onto the national data
  mydata <- mydata[mydata$level != 4,]
  mydata <- rbind(mydata, subnats)
  
  #look at level 5 locations, imput the national data
  subnats <- mydata[mydata$level == 5 & is.na(mydata$`2014`),]
  unique(substr(subnats$ihme_lc_id, 1, 3))
  subnats[,11:29][grep('GBR', subnats$ihme_lc_id)] <- mydata[,11:29][mydata$ihme_lc_id == 'GBR']
  subnats[,11:29][grep('IND', subnats$ihme_lc_id)] <- mydata[,11:29][mydata$ihme_lc_id == 'IND']
  subnats[,11:29][grep('RUS', subnats$ihme_lc_id)] <- mydata[,11:29][mydata$ihme_lc_id == 'RUS']
  mydata <- mydata[!(mydata$loc_id %in% subnats$loc_id),]
  mydata <- rbind(mydata, subnats)
  
  #look at level 6 locations, imput the national data
  subnats <- mydata[mydata$level == 6 & is.na(mydata$`2014`),]
  unique(substr(subnats$ihme_lc_id, 1, 3))
  subnats[,11:29] <- mydata[,11:29][mydata$ihme_lc_id == 'GBR']
  mydata <- mydata[!(mydata$loc_id %in% subnats$loc_id),]
  mydata <- rbind(mydata, subnats)
  rm(subnats)
  
  #for some reason theres extra Kenya records, replace these with kenya
  mydata$`2000`[mydata$region_id == 0] <- mydata$`2000`[mydata$loc_id == 180]
  mydata$`2001`[mydata$region_id == 0] <- mydata$`2001`[mydata$loc_id == 180]
  mydata$`2002`[mydata$region_id == 0] <- mydata$`2002`[mydata$loc_id == 180]
  mydata$`2003`[mydata$region_id == 0] <- mydata$`2003`[mydata$loc_id == 180]
  mydata$`2004`[mydata$region_id == 0] <- mydata$`2004`[mydata$loc_id == 180]
  mydata$`2005`[mydata$region_id == 0] <- mydata$`2005`[mydata$loc_id == 180]
  mydata$`2006`[mydata$region_id == 0] <- mydata$`2006`[mydata$loc_id == 180]
  mydata$`2007`[mydata$region_id == 0] <- mydata$`2007`[mydata$loc_id == 180]
  mydata$`2008`[mydata$region_id == 0] <- mydata$`2008`[mydata$loc_id == 180]
  mydata$`2009`[mydata$region_id == 0] <- mydata$`2009`[mydata$loc_id == 180]
  mydata$`2010`[mydata$region_id == 0] <- mydata$`2010`[mydata$loc_id == 180]
  mydata$`2011`[mydata$region_id == 0] <- mydata$`2011`[mydata$loc_id == 180]
  mydata$`2012`[mydata$region_id == 0] <- mydata$`2012`[mydata$loc_id == 180]
  mydata$`2013`[mydata$region_id == 0] <- mydata$`2013`[mydata$loc_id == 180]
  mydata$`2014`[mydata$region_id == 0] <- mydata$`2014`[mydata$loc_id == 180]
  mydata$`2015`[mydata$region_id == 0] <- mydata$`2015`[mydata$loc_id == 180]
  mydata$`2016`[mydata$region_id == 0] <- mydata$`2016`[mydata$loc_id == 180]
  mydata$`2017`[mydata$region_id == 0] <- mydata$`2017`[mydata$loc_id == 180]
  mydata$`2018`[mydata$region_id == 0] <- mydata$`2018`[mydata$loc_id == 180]
  
  table(mydata$loc_name[is.na(mydata$`2014`)])
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Format and save data ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~#
  mydata <- melt(mydata, id.vars = c('loc_id'),
                 measure.vars = c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018'),
                 variable.name = 'year_id',
                 value.name = 'cv_custom_stage_1')
  
  mydata <- mydata[,.(location_id = loc_id,
                      year_id = year_id,
                      age_group_id = rep(22, length(mydata$loc_id)),
                      sex_id = rep(3, length(mydata$loc_id)),
                      cv_custom_stage_1)]
  
  write.csv(mydata, paste0('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/holdouts/', date, '/holdout_', h, '/custom_stage1_df_clean.csv'), row.names = F)
  
  # Subset and save the input data
  my_input_data <- input_data[input_data$master_fold_id != h,]
  
  write.csv(my_input_data, paste0('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/holdouts/', date, '/holdout_', h, '/input_data.csv'), row.names = F)
  
  message(paste0('Completed holdout ', h))
}  
