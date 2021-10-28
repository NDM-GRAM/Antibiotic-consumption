#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# The included variables were extracted using IHMEs UBCOV code              # 
# The variables in each survey were identified, renamed and extracted       #
# Each survey was saved as a .dta file, all with the same variable names    #
# All input data are one row per child in the survey                        #
#                                                                           #
# Each survey is read in and the data are cleaned to produce binary         #
# indicators of whether each child had symptoms of (a) diarrhoea,           #
# (b) cough with/wihtout difficulty breathing and chest symptoms, and       #
# (c) fever, and whether the child recieved antibitoics for that indication #
# The data are then matched to a database of geolocations and collapsed to  #
# one row per location with the prevalence of symptoms and antibiotic use   #
# calculated. Administraitve district polygons are resampled to point       #
# locations. The prevalence of antibitoic use for cough symptoms is then    #
# adjusted to be the equivalence of that for LRI and finalised              #
#                                                                           #
# Code written by Annie Browne: annie.browne@ndm.ox.ac.uk                   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())

#Load required packages
library(readstata13)
library(foreign)
library(data.table)
library(ggplot2)
library(ggforce, lib = '/share/homes/annieb6/temp_packages/')

#1. Initial data cleaning, combine surveys, one row per child. ####
date <- format(Sys.Date(), "%Y_%m_%d")
merge.geo.codebook = TRUE # Do you want to get all of the geography codebooks from J (TRUE) or use an already merged RDS (FALSE)

setwd("/snfs1/WORK/11_geospatial/07_data extraction/AMR/treatment_seeking/")
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
                       "had_diarrhea",
                       "had_diarrhea_recall_period_weeks",
                       "diarrhea_antibiotics",
                       "had_cough",
                       "diff_breathing",
                       "chest_symptoms",
                       'illness_definition',
                       "had_cough_recall_period_weeks",
                       "cough_antibiotics",
                       "had_fever",
                       "had_fever_recall_period_weeks",
                       "fever_antibiotics"
                       )]

    #a. Restrict to children under 5
    mydata$age_year[mydata$nid == 375030 & is.na(mydata$age_year)] <- 2.5   # this survey is for under 5's but most have missing age var so impute
    mydata$age_year[mydata$nid == 125230 & is.na(mydata$age_year)] <- 2.5   # this survey is for under 5's but most have missing age var so impute
    mydata$age_year[mydata$nid == 18815 & is.na(mydata$age_year)] <- 2.5   # this survey is for under 5's but most have missing age var so impute
    
    mydata <- mydata[which(mydata$age_year<5),]

    #b. Drop kids with no illness
    mydata <- mydata[which(mydata$had_diarrhea==1|
                             mydata$had_cough==1|
                             mydata$had_fever==1),]

    #c. Create had cough indicator based on which symptoms are required for question to be asked
    if(!is.na(unique(mydata$illness_definition)) & unique(mydata$illness_definition) == 'cough & difficulty breathing') {
      mydata$had_cough[mydata$had_cough == 1 & mydata$diff_breathing == 1] <- 1
      mydata$had_cough[mydata$diff_breathing == 0] <- 0
      mydata$had_cough[is.na(mydata$diff_breathing)] <- NA
    }

    if(!is.na(unique(mydata$illness_definition)) & unique(mydata$illness_definition) == 'cough & fever') {
      mydata$had_cough[mydata$had_cough == 1 & mydata$had_fever == 1] <- 1
      mydata$had_cough[mydata$had_fever == 0] <- 0
      mydata$had_cough[is.na(mydata$had_fever)] <- NA
    }

    if(!is.na(unique(mydata$illness_definition)) & unique(mydata$illness_definition) == 'cough & difficulty breathing or fever') {
      mydata$had_cough_temp <-  0
      mydata$had_cough_temp[mydata$had_cough == 1 & mydata$diff_breathing == 1] <- 1
      mydata$had_cough_temp[mydata$had_cough == 1 & mydata$mydata$had_fever == 1] <- 1
      mydata$had_cough_temp[is.na(mydata$had_cough)] <- NA
      mydata$had_cough_temp[is.na(mydata$diff_breathing) & is.na(mydata$had_fever)] <- NA
      mydata$had_cough_temp[is.na(mydata$diff_breathing) & mydata$had_fever == 0] <- NA
      mydata$had_cough_temp[is.na(mydata$had_fever) & mydata$diff_breathing == 0] <- NA
      mydata$had_cough <- mydata$had_cough_temp
      mydata$had_cough_temp <- NULL
    }

    if(!is.na(unique(mydata$illness_definition)) & unique(mydata$illness_definition) == 'cough, difficulty breathing & chest symptoms') {
      mydata$had_cough[mydata$diff_breathing == 0 | mydata$chest_symptoms == 0] <- NA
      mydata$had_cough[is.na(mydata$diff_breathing) | is.na(mydata$chest_symptoms)] <- NA
    }

    if(!is.na(unique(mydata$illness_definition)) & unique(mydata$illness_definition) == 'cough, chest symptoms & fever') {
      mydata$had_cough[mydata$had_fever == 0 | mydata$chest_symptoms == 0] <- NA
      mydata$had_cough[is.na(mydata$had_fever) | is.na(mydata$chest_symptoms)] <- NA
    }

    #d. Ensure there are no values were there shouldnt be
    mydata$diarrhea_antibiotics[mydata$had_diarrhea == 0| is.na(mydata$had_diarrhea)] <- NA
    mydata$cough_antibiotics[mydata$had_cough == 0 | is.na(mydata$had_cough)] <- NA
    mydata$fever_antibiotics[mydata$had_fever == 0 | is.na(mydata$had_fever) ] <- NA

    #e. If survey asks about treatment but no antibiotics then change these to NA
    if(sum(mydata$diarrhea_antibiotics, na.rm = T)==0){
      mydata$diarrhea_antibiotics <- NA
    }

    if(sum(mydata$cough_antibiotics, na.rm = T)==0){
      mydata$cough_antibiotics <- NA
    }

    if(sum(mydata$fever_antibiotics, na.rm = T)==0){
      mydata$fever_antibiotics <- NA
    }

    #f. order data and select data
    mydata <- mydata[c("nid",
                       "year_start",
                       "year_end",
                       "ihme_loc_id",
                       "geospatial_id",
                       "pweight",
                       "strata",
                       'survey_name',
                       "sex_id",
                       "age_year",
                       "int_year",
                       "urban",
                       "had_diarrhea",
                       "had_diarrhea_recall_period_weeks",
                       "diarrhea_antibiotics",
                       "had_cough",
                       "diff_breathing",
                       "chest_symptoms",
                       "illness_definition",
                       "had_cough_recall_period_weeks",
                       "cough_antibiotics",
                       'had_fever',
                       'had_fever_recall_period_weeks',
                       "fever_antibiotics")]

    #g. If there is any antibiotic info then add to the dataset
    if(sum(mydata$diarrhea_antibiotics, na.rm=T) >1 | sum(mydata$cough_antibiotics, na.rm=T) >1 | sum(mydata$fever_antibiotics, na.rm=T) >1){

      #combined datasets
      if(exists('combined.data')){
        combined.data <- rbind(combined.data, mydata)
      } else {
        combined.data <- mydata
      }
    } else {
    rm(mydata)
  }
}

combined.data$nid[combined.data$nid == 146860] <- 210231

#h. save combined dataset
saveRDS(combined.data, paste0('/share/homes/annieb6/AMR/antibiotic_use/datasets/combined_data_', date, '.RDS'))

#2. Join this to geography information ####

#a. Read in codebook matching each cluster to the smallest possible location 
combined.codebooks <- readRDS("/share/homes/annieb6/AMR/misc/geography_codebook.RDS")
colnames(combined.data)[4] <- "iso3"
combined.data$nid <- as.numeric(combined.data$nid)

#b. Merge locations onto the data
matched.data <- merge(combined.data, combined.codebooks, all.x = T,  by = c('nid', 'iso3', 'geospatial_id'))

#c. Ad hoc corrections
# This polygon doesnt exist in the shapefile and cannot decipher what it is
matched.data$location_code[matched.data$shapefile == 'stats_mng_adm3_mics_2013' & matched.data$location_code == 1646] <- NA
matched.data$shapefile[matched.data$shapefile == 'stats_mng_adm3_mics_2013' & matched.data$location_code == 1646] <- NA

#these surveys have no location information
matched.data <- matched.data[which(matched.data$nid !=264590),]
matched.data <- matched.data[which(matched.data$nid !=13516),]

rm(combined.codebooks, combined.data)

#d Check that all locations have been matched
unmatched.locations <- matched.data[is.na(matched.data$location_name),]

unmatched.locations <- unique(unmatched.locations[c("nid", "geospatial_id", "iso3", "survey_name")])
unmatched.locations <- unmatched.locations[unmatched.locations$nid!= 337877,]

if(length(unmatched.locations$nid) >0){
  write.csv(unmatched.locations, paste0('/share/homes/annieb6/AMR/antibiotic_use/data_checks/unmatched_locations_', date, '.csv'), row.names = F)
  message("CHECK UNMATCHED LOCATIONS")
}

#d. Rename some columns 
colnames(matched.data)[colnames(matched.data)=="lat"] <- "latitude"
colnames(matched.data)[colnames(matched.data)=="long"] <- "longitude"
colnames(matched.data)[colnames(matched.data)=="iso3"] <- "country"
colnames(matched.data)[colnames(matched.data)=="year_end"] <- "year"
colnames(matched.data)[colnames(matched.data)=="survey_name"] <- "source"
colnames(matched.data)[colnames(matched.data)=="pweight"] <- "weight"
colnames(matched.data)[colnames(matched.data)=="geospatial_id"] <- "cluster_number"


#3. Data exclusions - after running data checks the following surveys/data points are excluded ####
matched.data <- matched.data[which(matched.data$survey_name!= "CVD_GEMS"),]   # Hospital based study with very low sample size
matched.data$diarrhea_antibiotics[matched.data$nid == 19076] <- NA # This surveys only asked about 1 type of antibiotic therefore not an accurate usage percentage

matched.data$cough_antibiotics[matched.data$nid == 18854] <- NA # too small sample size
matched.data$cough_antibiotics[matched.data$nid == 218563] <- NA # only asks about injections
matched.data$cough_antibiotics[matched.data$nid == 19557] <- NA # large amoutn of missing data
matched.data <- matched.data[matched.data$nid != 189048,]  # v small sample size and strange values
matched.data$diarrhea_antibiotics[matched.data$nid == 148649] <- NA # complete outlier and unrealistically low value
matched.data$diarrhea_antibiotics[matched.data$nid == 218619] <- NA # SLE 2017, complete outlier and unrealistically low value, has option of 'antidiarheals' which i think many would have misclassified antibiotics as.

matched.data <- matched.data[matched.data$nid!= 270627,]
matched.data <- matched.data[matched.data$nid!= 210182,]
matched.data <- matched.data[matched.data$nid!= 11645,]
matched.data <- matched.data[matched.data$nid!= 76699,] #low sample size skewing data
matched.data <- matched.data[matched.data$nid!= 20235,] #pre 2000 & outlier
matched.data <- matched.data[matched.data$nid!= 20767,]
matched.data <- matched.data[matched.data$nid!= 27590,]
matched.data <- matched.data[matched.data$nid!= 20371,]
matched.data <- matched.data[matched.data$nid!= 19064,]
matched.data <- matched.data[matched.data$nid!= 19035,]
matched.data <- matched.data[matched.data$nid!= 951,]
matched.data <- matched.data[matched.data$nid!= 19188,]
matched.data <- matched.data[matched.data$nid!= 20518,]
matched.data <- matched.data[matched.data$nid!= 20584,]
matched.data <- matched.data[matched.data$nid!= 20711,]
matched.data <- matched.data[matched.data$nid!= 21068,]
matched.data <- matched.data[matched.data$nid!= 19350,]
matched.data <- matched.data[matched.data$nid!= 20339,] #MDA - survey only asks about a couple of abx

matched.data$diarrhea_antibiotics[matched.data$nid == 148649] <- NA
matched.data$had_diarrhea[matched.data$nid == 148649] <- NA
matched.data$cough_antibiotics[matched.data$nid == 19410] <- NA
matched.data$had_cough[matched.data$nid == 19410] <- NA
matched.data$cough_antibiotics[matched.data$nid == 19482] <- NA
matched.data$had_cough[matched.data$nid == 19482] <- NA
matched.data$cough_antibiotics[matched.data$nid == 20202] <- NA
matched.data$had_cough[matched.data$nid == 20202] <- NA
matched.data$cough_antibiotics[matched.data$nid == 20626] <- NA
matched.data$had_cough[matched.data$nid == 20626] <- NA
matched.data$cough_antibiotics[matched.data$nid == 21079] <- NA
matched.data$had_cough[matched.data$nid == 21079] <- NA

#4. Data Checks ####
matched.data <- data.table(matched.data)

#a. Create a csv of values from the survey and check these manually to ensure correct/logical
checks <- matched.data[,.(year_start = min(int_year),
                             year_end = max(int_year),
                             pweight_missing = round(length(pweight[is.na(pweight)])/length(pweight)*100,0),
                             strata_missing = round(length(strata[is.na(strata)])/length(strata)*100,0),
                             had_diarrhea = weighted.mean(had_diarrhea, pweight, na.rm = T),
                             had_diarrhea_recall_period_weeks = min(had_diarrhea_recall_period_weeks),
                             diarrhea_antibiotics = weighted.mean(diarrhea_antibiotics, pweight, na.rm = T),
                             had_cough = weighted.mean(had_cough, pweight, na.rm = T),
                             had_cough_recall_period_weeks = min(had_cough_recall_period_weeks),
                             cough_antibiotics = weighted.mean(cough_antibiotics, pweight, na.rm = T),
                             had_fever = weighted.mean(had_fever, pweight, na.rm = T),
                             had_fever_recall_period_weeks = min(had_fever_recall_period_weeks),
                             fever_antibiotics = weighted.mean(fever_antibiotics, pweight, na.rm = T),
                             N = ((sum(pweight))**2)/sum(pweight**2),
                             percent_points = round(length(lat[!is.na(lat)])/length(geospatial_id)*100,2),
                             percent_polygons = round(length(location_code[!is.na(location_code)])/length(geospatial_id)*100,2),
                             missing_location_percent =round(length(nid[(is.na(lat)) & (is.na(location_code))])/length(geospatial_id)*100,2)),
                         by = .(nid, iso3, survey_name)]

write.csv(checks, paste0('/share/homes/annieb6/AMR/antibiotic_use/data_checks/data_checks_', date, '.csv'), row.names = F)

unique(checks$survey_name)
checks$survey_name[!(checks$survey_name== 'UNICEF_MICS' | checks$survey_name=='MACRO_DHS' | checks$survey_name=='MACRO_AIS')] <-  'Other'
checks$survey_name[checks$survey_name == 'MACRO_DHS' | checks$survey_name == 'MACRO_AIS'] <-  'DHS'
checks$survey_name[checks$survey_name == 'UNICEF_MICS'] <-  'MICS'

#b. plot out antibiotic use in LRI and check for outliers
checks$iso3 <- substr(checks$iso3, 1, 3)
pdf(paste0("/share/homes/annieb6/AMR/antibiotic_use/data_checks/input_data_", date, ".pdf"),
    height = 17,
    width = 12)

for(i in 1:(ceiling((length(unique(checks$iso3)))/12))){
  print(ggplot(checks, aes(x = year_end, y = cough_antibiotics))+
          geom_point(aes(colour = survey_name, size = N))+
          theme(legend.title = element_blank())+
          facet_wrap_paginate(~iso3, nrow = 4, ncol = 3, page = i)
  )}

dev.off()

#c. save data
saveRDS(matched.data, paste0("/share/homes/annieb6/AMR/antibiotic_use/datasets/antibiotic_use_clean_", date,".rds"))

#4. Collapse data to one row per cluster for each indicator ####
rm(list = ls())
date <- format(Sys.Date(), "%Y_%m_%d")

#a.Prepping data for collapsing
# Read in geomatched data
mydata <- readRDS(paste0("/share/homes/annieb6/AMR/antibiotic_use/datasets/antibiotic_use_clean_", date,".rds"))
illness_def <- unique(data.frame(mydata)[c('nid', 'illness_definition')])
                             
mydata <- data.table(mydata)
mydata$country <- substr(mydata$country, 1, 3) 
mydata$shapefile[mydata$location_code == "#N/A"] <- NA
mydata$location_code[mydata$location_code == "#N/A"] <- NA
mydata$shapefile[is.na(mydata$location_code) | mydata$location_code == "" | mydata$location_code == "NA"] <- NA

#b. Keep only desired columns
mydata <- mydata[, .(nid, source, year, weight, cluster_number, country,
                     point, latitude, longitude, location_code, shapefile,
                     diarrhea_antibiotics, cough_antibiotics, fever_antibiotics)]

#c. drop data without antibiotic usage data
diarrhea_data <- mydata[which(!is.na(mydata$diarrhea_antibiotics)),]
diarrhea_data$cough_antibiotics <- NULL
diarrhea_data$fever_antibiotics <- NULL
cough_data <- mydata[which(!is.na(mydata$cough_antibiotics)),]
cough_data$diarrhea_antibiotics <- NULL
cough_data$fever_antibiotics <- NULL
fever_data <- mydata[which(!is.na(mydata$fever_antibiotics)),]
fever_data$diarrhea_antibiotics <- NULL
fever_data$cough_antibiotics <- NULL

#for each indicator collapse and resample polygons
for(ind in c('diarrhea', 'cough', 'fever')){
  message(ind)
  mydata <- get(paste0(ind, '_data'))
  colnames(mydata)[12] <-  'antibiotics'
  
#d. Subset into point and polygon datasets and collapse
mydata[, has_points := !is.na(latitude) & !is.na(longitude)]
point_data <- mydata[ has_points == 1, ]
poly_data  <- mydata[ has_points != 1, ]

#e. Collapse point data
point_collapse <- point_data[, .(antibiotics = sum(antibiotics, na.rm = T), 
                                 N = .N,
                                 sum_of_sample_weights = sum(weight)) ,
                             by = .(nid, source, year, country,
                                    latitude, longitude)]

point_collapse$weight <- 1

#f. Collapsing polygon data using the person-weights

#Drop any rows without location or weight informaion
rows_before_drop <- nrow(poly_data)
poly_data <- poly_data[ !is.na(shapefile) & !is.na(location_code) & !is.na(weight), ]
rows_dropped <- rows_before_drop - nrow(poly_data)

message(paste0("Additionally, ",rows_dropped," rows were dropped due to ",
               "missing geographical information or survey weights (for areal ",
               "data only).")
)

# collapse
poly_collapse <- poly_data[,.(antibiotics = weighted.mean(antibiotics, weight, na.rm = T),
                              N = ((sum(weight))**2)/sum(weight**2),
                              sum_of_sample_weights = sum(weight)),
                           by = .(nid, source, year, country, shapefile, 
                                  location_code)]

poly_collapse$antibiotics <- poly_collapse$antibiotics*poly_collapse$N

#g. Bind points and polygons back into a single data.table and final bits of data cleaning
collapsed_all <- rbindlist(list(point_collapse, poly_collapse),
                           use.names = TRUE,
                           fill = TRUE)

#h. Calculate the rate of antibiotic use
collapsed_all$rate <- collapsed_all$antibiotics/collapsed_all$N

#i. Order columns required
collapsed_all <- collapsed_all[,.(nid,
                                  source,
                                  year,
                                  country,
                                  antibiotics,
                                  N,
                                  rate,
                                  weight,
                                  latitude,
                                  longitude,
                                  shapefile,
                                  location_code,
                                  sum_of_sample_weights)]

#j. final bit of cleaning so will run in resample_polys
collapsed_all$weight <- 1
collapsed_all$weight[is.na(collapsed_all$latitude)] <- NA
collapsed_all$shapefile <- as.character(collapsed_all$shapefile)

#k. Save output data
saveRDS(collapsed_all,
        file = paste0("/share/homes/annieb6/AMR/antibiotic_use/datasets/", ind, "_antibiotics_collapsed_", date, ".rds"))


#l. merge the files together to resample polys
colnames(collapsed_all)[5] <- paste0(ind, "_", "antibiotics")
colnames(collapsed_all)[6] <- paste0(ind, "_", "N")
colnames(collapsed_all)[7] <- paste0(ind, "_", "rate")
colnames(collapsed_all)[13] <- paste0(ind, "_", "sum_of_sample_weights")

assign(paste0(ind, '_data'), collapsed_all)
}

#m. final cleaning of collapsed data
rm(collapsed_all, mydata, point_collapse, poly_collapse, point_data, poly_data)

collapsed_all <- merge(diarrhea_data, cough_data, by = c( "nid", 
                                                  "source", 
                                                  "year", 
                                                  "country",
                                                  "weight",
                                                  "latitude",
                                                  "longitude",
                                                  "shapefile",
                                                  'location_code'), all.x = T, all.y = T)

collapsed_all <- merge(collapsed_all, fever_data, by = c( "nid", 
                                                   "source", 
                                                   "year", 
                                                   "country",
                                                   "weight",
                                                   "latitude",
                                                   "longitude",
                                                   "shapefile",
                                                   'location_code'), all.x = T, all.y = T)

# Replace 'cluster_number' with 'cluster_id'  which is used to represent unique rows in the collapsed data
collapsed_all[, cluster_id := 1:nrow(collapsed_all)]

rm(diarrhea_data, cough_data, fever_data)

collapsed_all$country[collapsed_all$country == 'RKS'] <- 'XKO'

#n. add extra data from reports
extra <- read.csv('/ihme/homes/annieb6/AMR/antibiotic_use/MBG_extra_data.csv', stringsAsFactors = F)
collapsed_all <- rbind(collapsed_all, extra)

#o. save the data to resample polys
saveRDS(collapsed_all, paste0("/share/homes/annieb6/AMR/antibiotic_use/datasets/antibiotic_use_collapsed_", date,".rds"))

#5. Resample polygons ####
rm(list = ls())

#a. create an output directory and read the data back in
dir.create('/share/homes/annieb6/AMR/antibiotic_use/resample_polys/', showWarnings = F)
mydata <- readRDS(paste0("/share/homes/annieb6/AMR/antibiotic_use/datasets/antibiotic_use_collapsed_", date,".rds"))
mydata <- data.frame(mydata)

#b. Identify data requiring polygon resampling and list of shapefiles and polygons to resample
poly_data <- mydata[!is.na(mydata$shapefile) & !is.na(mydata$shapefile),]
locs_list <- unique(poly_data[c('shapefile', 'location_code')])
locs_list <- locs_list[order(locs_list$shapefile),]
saveRDS(locs_list, '/share/homes/annieb6/AMR/antibiotic_use/resample_polys/locs_list.rds')

# list of shapefiles
shp_list <- unique(poly_data$shapefile)

#c. Create a qsub to run a script to resample the polygons in each shapefile 
for(s in shp_list){
  script             <- '/share/code/geospatial/annieb6/lbd_amr/antibiotics/1_data_prep/resample_polys_loop.R'
  shell              <- '/share/code/geospatial/annieb6/lbd_core//mbg_central/share_scripts/shell_sing.sh'
  job_name           <- paste(s, 'resample', sep = "_")
  cores              <- 2
  runtime            <- '-l h_rt=00:00:10:00'
  memory             <- '-l m_mem_free=5G'
  proj               <- 'proj_geospatial'
  queue              <- 'all.q'
  output             <- '/share/homes/annieb6/logs/output/'
  errors             <- '/share/homes/annieb6/logs/errors/'
  
  qsub <- paste('qsub',
                '-e', errors,
                '-o', output,
                '-P', proj,
                '-N', job_name,
                '-q', queue,
                '-cwd -l archive=TRUE',
                memory,
                '-l fthread=2',
                runtime,
                '-v sing_image=default',
                '-p 0',
                shell,
                script,
                s,
                'antibiotic_use',
                'fin', sep =" ")
system(qsub)
}

#d. check that all have been resampled
resamples_polys <- list.files('/ihme/homes/annieb6/AMR/antibiotic_use/resample_polys/', '.rds')
resamples_polys <- gsub('resamples_', '', resamples_polys)
resamples_polys <- gsub('.rds', '', resamples_polys)
failed <- shp_list[!(shp_list%in% resamples_polys)]

if(length(failed)>0){
  for(s in failed){
    qsub <- gsub('m_mem_free=5G', 'm_mem_free=10G', qsub)
    qsub <- gsub('h_rt=00:00:10:00', 'h_rt=00:00:30:00', qsub)
    qsub2 <- gsub(shp_list[length(shp_list)], s, qsub)
    system(qsub2)
  }
}

#e. Merge the resampled polygon data (latitudes, longitudes and weights) onto the collapsed data
resamples_polys <- list.files('/ihme/homes/annieb6/AMR/antibiotic_use/resample_polys/', '.rds')
resamples_polys <- resamples_polys[c(-1)]

for(r in resamples_polys){
  my_poly <- readRDS(paste0('/ihme/homes/annieb6/AMR/antibiotic_use/resample_polys/', r))

  if(r==resamples_polys[1]){
    combined_polys <- my_poly
  } else{
    combined_polys <- rbind(combined_polys, my_poly)
  }
}

poly_data$latitude <- NULL
poly_data$longitude <- NULL
poly_data$weight <- NULL

poly_data <- merge(poly_data, combined_polys, by = c('shapefile', 'location_code'), all.x = T, all.y = T)
poly_data$location_code <- NULL
poly_data$shapefile <- NULL

#f. Remove polys from the original dataset
point_data <- mydata[!(is.na(mydata$latitude)) & mydata$latitude != "" & mydata$latitude != "NA",]
point_data$location_code <- NULL
point_data$shapefile <- NULL

#g. Merge points and polygon data
newdata <- rbind(point_data, poly_data)
newdata <- data.table(newdata)
newdata[, cluster_id := 1:nrow(newdata)]

rm(combined_polys, my_poly, mydata, point_data, poly_data)

#h. Set all data pre 2000 as 2000 (only includes the round of DHS covering the year 2000)
newdata$year[newdata$year<2000] <- 2000

#6. Save the cleaned data for each indicator
#a. Diarrhea
diarrhea_data <- newdata[, .(nid, source, year, country, cluster_id, weight,
                     latitude, longitude, diarrhea_antibiotics, diarrhea_N, diarrhea_rate, diarrhea_sum_of_sample_weights)]

diarrhea_data <- diarrhea_data[which(!is.na(diarrhea_data$diarrhea_antibiotics)),]
colnames(diarrhea_data)[10] <- 'N'
colnames(diarrhea_data)[11] <- 'rate'
colnames(diarrhea_data)[12] <- 'sum_of_sample_weights'

#save files
saveRDS(diarrhea_data,
        file = paste0("/share/homes/annieb6/AMR/antibiotic_use/datasets/diarrhea_antibiotics_resampled_polys_", date, ".rds"))

write.csv(diarrhea_data,
          file = paste0("/share/geospatial/mbg/input_data/diarrhea_antibiotics_", date, ".csv"), row.names = F)

#b. cough
cough_data <- newdata[, .(nid, source, year, country, cluster_id, weight,
                            latitude, longitude, cough_antibiotics, cough_N, cough_rate, cough_sum_of_sample_weights)]

cough_data <- cough_data[which(!is.na(cough_data$cough_antibiotics)),]
colnames(cough_data)[10] <- 'N'
colnames(cough_data)[11] <- 'rate'
colnames(cough_data)[12] <- 'sum_of_sample_weights'

#7. Crosswalk the cough data to be the equivalent of LRI, based on the results of LRI_crosswalk.R ####
illness_def$illness_definition <- trim(illness_def$illness_definition)

cough_data <- merge(cough_data, illness_def, by = 'nid', all.x = T, all.y = F)
cough_data$illness_definition[cough_data$nid == 21313] <- 'LRI'
cough_data$illness_definition[cough_data$nid == 27215] <- 'LRI'
cough_data$illness_definition[cough_data$nid == 11434] <- 'LRI'
cough_data$illness_definition[is.na(cough_data$illness_definition)] <-  'cough'

cough_data$rate[cough_data$illness_definition == 'cough'] <- 0.053295084+(1.106530815*cough_data$rate[cough_data$illness_definition == 'cough'])
cough_data$rate[cough_data$illness_definition == 'cough & difficulty breathing'] <- 0.02117734+(1.086483551*cough_data$rate[cough_data$illness_definition == 'cough & difficulty breathing'])
cough_data$rate[cough_data$illness_definition == 'cough & fever'] <- 0.038256862+(1.050385028*cough_data$rate[cough_data$illness_definition == 'cough & fever'])
cough_data$rate[cough_data$illness_definition == 'cough & rapid breathing or fever'] <- 0.036027946+(1.077390759*cough_data$rate[cough_data$illness_definition == 'cough & rapid breathing or fever'])
cough_data$rate[cough_data$illness_definition == 'cough, difficulty breathing & chest symptoms'] <- -0.001796053+(1.017771737*cough_data$rate[cough_data$illness_definition == 'cough, difficulty breathing & chest symptoms'])

cough_data$rate[cough_data$rate <0] <- 0
cough_data$rate[cough_data$rate >1] <- 1
cough_data$cough_antibiotics <- cough_data$rate*cough_data$N

#save files
saveRDS(cough_data,
        file = paste0("/share/homes/annieb6/AMR/antibiotic_use/datasets/cough_antibiotics_", date, ".rds"))

write.csv(cough_data, 
          file = paste0("/share/geospatial/mbg/input_data/cough_antibiotics_", date, ".csv"), row.names = F)

#c. fever
fever_data <- newdata[, .(nid, source, year, country, cluster_id, weight,
                            latitude, longitude, fever_antibiotics, fever_N, fever_rate, fever_sum_of_sample_weights)]

fever_data <- fever_data[which(!is.na(fever_data$fever_antibiotics)),]
colnames(fever_data)[10] <- 'N'
colnames(fever_data)[11] <- 'rate'
colnames(fever_data)[12] <- 'sum_of_sample_weights'

#save files
saveRDS(fever_data,
        file = paste0("/share/homes/annieb6/AMR/antibiotic_use/datasets/fever_antibiotics_resampled_polys_", date, ".rds"))

write.csv(fever_data,
          file = paste0("/share/geospatial/mbg/input_data/fever_antibiotics_", date, ".csv"), row.names = F)

##### END #####