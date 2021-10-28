#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Read in each survey and calculate the proprotions #
# seeking treatment at                              #
# 1. formal healthcare                              #
# 2. public hospitals                               #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Data cleaning, combining and matching geographies #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())

#Load required packages
library(readstata13)
library(foreign)
library(data.table)
library(gdata)
library('RCMIP5', lib = '/share/homes/annieb6/')

setwd(paste0("/snfs1/WORK/11_geospatial/07_data extraction/AMR/treatment_seeking/")) 
date <- format(Sys.Date(), "%Y_%m_%d")

# list files in the directory
file.list <- list.files(, ".dta")

for(i in 1:length(file.list)){
  message(paste0("Processing file ", i, " out of ", length(file.list)))
  mydata <- read.dta13(file.list[i],nonint.factors = T)
  
  #1. Restrict to variables required
  mydata <- mydata[c("nid",
                     "year_start",
                     "year_end",
                     "ihme_loc_id",
                     "geospatial_id",
                     "pweight",
                     "strata",
                     'survey_name',
                     "sex_id",
                     "age_month",
                     "age_year",
                     "int_year",
                     "urban",
                     "had_cough",
                     "diff_breathing",
                     "chest_symptoms",
                     'illness_definition',
                     "had_fever",
                     "had_cough_recall_period_weeks",
                     "cough_facility_visited",
                     'cough_antibiotics')]
  
  #change community hospital to be classified with clinics
  mydata$cough_facility_visited <- gsub('community hospital', 'clinic', mydata$cough_facility_visited)
  
  #2. Restrict to children under 5
  mydata$age_year[mydata$nid == 375030 & is.na(mydata$age_year)] <- 2.5   # this survey is for under 5's but most have missing age var so impute
  mydata$age_year[mydata$nid == 125230 & is.na(mydata$age_year)] <- 2.5   # this survey is for under 5's but most have missing age var so impute
  mydata$age_year[mydata$nid == 18815 & is.na(mydata$age_year)] <- 2.5   # this survey is for under 5's but most have missing age var so impute
  
  mydata <- mydata[which(mydata$age_year<5),]
  
  #clean up facility visited
  mydata$cough_facility_visited[grep('hospital', mydata$cough_facility_visited)] <- 'hospital'
  mydata$cough_facility_visited[grep('clinic', mydata$cough_facility_visited)] <- 'clinic'
  mydata$cough_facility_visited[grep('dispensary', mydata$cough_facility_visited)] <- 'dispensary/shop'
  mydata$cough_facility_visited[grep('shop', mydata$cough_facility_visited)] <- 'dispensary/shop'
  mydata$cough_facility_visited[grep('other', mydata$cough_facility_visited)] <- 'other'
  mydata$cough_facility_visited[grep('none', mydata$cough_facility_visited)] <- 'none/not specified'
  mydata$cough_facility_visited[is.na(mydata$cough_facility_visited)] <- 'none/not specified'

  #create a yes/no by survey whether all LRI variables are available (LRI1 being without fever, LRI2 being with fever)
  mydata$LRI1 <- 0
  mydata$LRI2 <- 0
  
  mydata$LRI1[which(mydata$had_cough==1 & mydata$chest_symptoms == 1 & mydata$diff_breathing == 1)] <- 1
  if(sum(mydata$LRI1) == 0){mydata$LRI1 <- NA}
    
  mydata$LRI2[which(mydata$had_cough==1 & mydata$chest_symptoms == 1 & mydata$diff_breathing == 1 & mydata$had_fever == 1)] <- 1 
  if(sum(mydata$LRI2) == 0){mydata$LRI2 <- NA}
  
  
  #combined datasets
  # If there is antibiotic info and the facility visited then add to the dataset
  if(length(mydata$cough_facility_visited[mydata$cough_facility_visited!='none/not specified'])>1 & 
     length(mydata$cough_antibiotics[!is.na(mydata$cough_antibiotics)])>1){
    if(i == 1){
      combined.data <- mydata
    } else {
      combined.data <- rbind(combined.data, mydata)
    }
  }
}

saveRDS(combined.data, '/ihme/homes/annieb6/AMR/antibiotic_use/treatment_seeking/line_level_data_temp.RDS')
mydata <- readRDS('/ihme/homes/annieb6/AMR/antibiotic_use/treatment_seeking/line_level_data_temp.RDS')
mydata <- data.table(mydata)

#summarise how many kids and studies have the LRI symptoms

check_LRI_definitition <- mydata[,.(n_children = (length(nid)),
                                    cough = sum(had_cough, na.rm = T),
                                    LRI1 = sum(LRI1, na.rm = T),
                                    LRI2 = sum(LRI2, na.rm = T),
                                    cough_antibiotics = sum(cough_antibiotics, na.rm = T),
                                    healthcare_reported = length(cough_facility_visited[!is.na(cough_facility_visited)])),
                                 by = c('ihme_loc_id', 'year_end', 'nid', 'illness_definition')]


check_LRI_definitition$LRI1[check_LRI_definitition$LRI1 == 0] <- NA
check_LRI_definitition$LRI2[check_LRI_definitition$LRI2 == 0] <- NA
check_LRI_definitition$healthcare_reported[check_LRI_definitition$healthcare_reported == 0] <- NA
check_LRI_definitition$cough_antibiotics[check_LRI_definitition$cough_antibiotics == 0] <- NA
check_LRI_definitition$cough[check_LRI_definitition$cough == 0] <- NA

check_LRI_definitition <- check_LRI_definitition[!is.na(check_LRI_definitition$cough),]
check_LRI_definitition <- check_LRI_definitition[!is.na(check_LRI_definitition$cough_antibiotics),]
check_LRI_definitition <- check_LRI_definitition[!is.na(check_LRI_definitition$healthcare_reported),]

length(check_LRI_definitition$nid[!is.na(check_LRI_definitition$LRI1)])
length(check_LRI_definitition$nid[!is.na(check_LRI_definitition$LRI2)])
table(check_LRI_definitition$illness_definition)

#3. Drop kids without LRI
# Cough chest infections and difficulty breathing as lots of surveys dont have fever (especially eastern/central europe)
LRI <- mydata[which(mydata$LRI1 == 1),]

#for each survey calculate the total abx usage and by healthcare type
total <- LRI[,.(total_LRI = .N,
                total_abx_n = sum(cough_antibiotics, na.rm = T)),
                by = c('ihme_loc_id', 'year_end', 'nid', 'illness_definition')]

healthcare <- LRI[,.(abx_n = sum(cough_antibiotics, na.rm = T)),
                by = c('ihme_loc_id', 'year_end', 'nid', 'illness_definition', 'cough_facility_visited')]

healthcare <- dcast(healthcare, ihme_loc_id+year_end+nid+illness_definition~cough_facility_visited, value.var = 'abx_n')

total <- merge(total, healthcare, by = c("ihme_loc_id", "year_end", "nid", "illness_definition"))
total <- total[total$total_abx_n!=0,]

# total$clinic_percent <- round(total$clinic/total$total_abx_n*100,0)
# total$`dispensary/shop_percent` <- round(total$`dispensary/shop`/total$total_abx_n*100,0)
# total$hospital_percent <- round(total$hospital/total$total_abx_n*100,0)
# total$`none/not specified_percent` <- round(total$`none/not specified`/total$total_abx_n*100,0)
# total$other_percent <- round(total$other/total$total_abx_n*100,0)
# total$`traditional medicine_percent` <- round(total$`traditional medicine`/total$total_abx_n*100,0)


write.csv(total, '/ihme/homes/annieb6/AMR/antibiotic_use/treatment_seeking/abx_use_by_ts_2.csv', row.names = F)

total$hospital[is.na(total$hospital)] <- 0
#exclusions
total <- total[total$total_abx_n>50,]
total <- total[!(grep("_", total$ihme_loc_id)),] #exclude subnationals
total <- total[total$nid!=7401,] #exclude subnationals

#correct an ethiopia survey which has erronous extraction
total$clinic[total$nid == 218568] <- 59
total$`dispensary/shop`[total$nid == 218568] <- 4
total$`traditional medicine`[total$nid == 218568] <-1

#remove argentina (not LMIC)
total <- total[total$ihme_loc_id != 'ARG',]

#calculate the formal healthcare proportion
total$formal_n <-  total$hospital+total$clinic 
total$formal_percent <- round(total$formal_n/total$total_abx_n*100,0)

#merge on the super regions and regions information
locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2020/master/shapefiles/GBD2020_analysis_final_loc_set_22.dbf')
locs <- locs[,6:8]

total <- merge(total, locs, by.x = 'ihme_loc_id', by.y = 'ihme_lc_id')

total$spr_reg_id[total$spr_reg_id == 103] <- 'Latin America & Caribbean'
total$spr_reg_id[total$spr_reg_id == 137] <- 'North Africa & Middle East'
total$spr_reg_id[total$spr_reg_id == 158] <- 'South Asia'
total$spr_reg_id[total$spr_reg_id == 166] <- 'Sub-Saharan Africa'
total$spr_reg_id[total$spr_reg_id == 31] <- 'Central Europe, Eastern Europe & Central Asia'
total$spr_reg_id[total$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'
total$spr_reg_id[total$spr_reg_id == 64] <- 'Central Europe, Eastern Europe & Central Asia'   #64 is acutally high income but for some reaon GBD have georgia as High income not eastern europe/central asia


#summarise by super regions
spr_reg_summary <- total[,.(median_formal_percent = round(median(formal_percent),0),
                    lower_quartile = round(quantile(formal_percent, 0.25),0),
                    upper_quartile = round(quantile(formal_percent, 0.75),0),
                    number_sureys = length(nid)),
                 by = c('spr_reg_id')]


#try by region
total$region_id[total$region_id == 32] <- "Central Asia"
total$region_id[total$region_id == 42] <- "Central Europe"
total$region_id[total$region_id == 56] <- "Eastern Europe"
total$region_id[total$region_id ==120] <- "Andean Latin America"
total$region_id[total$region_id ==104] <- "Caribbean"
total$region_id[total$region_id ==124] <- "Central Latin America"
total$region_id[total$region_id ==134] <- "Tropical Latin America"
total$region_id[total$region_id ==138] <- "North Africa & Middle East"
total$region_id[total$region_id ==159] <- "South Asia"
total$region_id[total$region_id ==9] <- "Southeast Asia"
total$region_id[total$region_id ==167] <- "Central Sub-Saharan Africa"
total$region_id[total$region_id ==174] <- "Eastern Sub-Saharan Africa"
total$region_id[total$region_id ==192] <- "Southern Sub-Saharan Africa"
total$region_id[total$region_id ==199] <- "Western Sub-Saharan Africa"

region_summary <- total[,.(median_formal_percent = round(median(formal_percent),0),
                    lower_quartile = round(quantile(formal_percent, 0.25),0),
                    upper_quartile = round(quantile(formal_percent, 0.75),0),
                    number_sureys = length(nid)),
                 by = c('region_id')]

write.csv(spr_reg_summary, '/ihme/homes/annieb6/AMR/antibiotic_use/treatment_seeking/LRI1_super_region_summary.csv', row.names = FALSE)
write.csv(region_summary, '/ihme/homes/annieb6/AMR/antibiotic_use/treatment_seeking/LRI1_region_summary.csv', row.names = FALSE)

