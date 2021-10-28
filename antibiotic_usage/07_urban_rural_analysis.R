rm(list = ls())
library(tidyverse)

core_repo         <- '/share/code/geospatial/annieb6/lbd_core/'
indicator_group   <- 'antibiotics'
indicator         <- 'cough_antibiotics'
run_date          <- '2020_02_15_03_49_34'
raked             <- FALSE
pop_measure       <- 'a0004t'
age               <- 0
holdout           <- 0
shapefile_version <- '2019_12_12'
share_dir         <- paste0("/share/geospatial/mbg/",indicator_group,"/",indicator,"/output/",run_date,"/")

# Load limited and specific MBG functions
mbg_functions <- c('mbg_functions.R', 'prep_functions.R',
                   'covariate_functions.R', 'misc_functions.R',
                   'post_estimation_functions.R', 'gbd_functions.R',
                   'shiny_functions.R', 'holdout_functions.R',
                   'categorical_variable_functions.R', 'validation_functions.R',
                   'seegMBG_transform_functions.R', 'shapefile_functions.R')
source(paste0(core_repo, '/mbg_central/setup.R'))
source_functions(paste(core_repo, 'mbg_central', mbg_functions, sep = '/'))

my_regions <- c('dia_name', 'dia_cssa', 'dia_wssa', 'dia_malay+dia_oceania', 'dia_mid_east', 'dia_essa+dia_sssa-syc', 'dia_mcaca', 'dia_central_asia+kaz+mng', 'dia_afr_horn', 'balkans_ext+caucasus', 'dia_se_asia', 'dia_south_asia', 'dia_s_america-tto')
# my_regions <- c('dia_name', 'dia_cssa', 'dia_wssa', 'dia_malay+dia_oceania', 'dia_mid_east', 'dia_essa+dia_sssa-syc', 'dia_mcaca', 'dia_central_asia+kaz+mng', 'dia_afr_horn', 'balkans_ext+caucasus', 'dia_se_asia', 'dia_south_asia')

for (r in my_regions){
  qsub <- paste0('qsub -e /ihme/homes/annieb6/logs/errors -o /ihme/homes/annieb6/logs/output -P proj_geo_nodes -N urban_abx_', r, ' -q geospatial.q -cwd -l archive=TRUE -l m_mem_free=400G -l fthread=6 -l h_rt=00:24:00:00  -v sing_image=default -p 0  /share/code/geospatial/annieb6/lbd_core//mbg_central/share_scripts/shell_sing.sh /share/code/geospatial/annieb6/lbd_amr/antibiotics/3_post_processing/urban_rural_working.R ', r, ' fin')
  system(qsub)
}


for(ur in c('urban', 'rural')){

  combine_aggregation(rd = run_date,
                      indic = indicator,
                      ig = indicator_group,
                      ages     = 0,
                      regions  = c(my_regions, 'dia_name'),
                      holdouts = 0,
                      raked    = F,
                      delete_region_files = F,
                      dir_to_search = paste0('/ihme/geospatial/mbg/antibiotics/cough_antibiotics/output/2020_02_15_03_49_34/urban_rural/', ur, '/'))

  output_dir = paste0('/ihme/geospatial/mbg/antibiotics/cough_antibiotics/output/2020_02_15_03_49_34/urban_rural/', ur, '/admin_summaries/')
  dir.create(output_dir)

  #summarise admins
  load(paste0('/ihme/geospatial/mbg/antibiotics/cough_antibiotics/output/2020_02_15_03_49_34/urban_rural/', ur, '/cough_antibiotics_unraked_admin_draws_eb_bin0_0.RData'))

  sp_hierarchy_list <- mutate_if(sp_hierarchy_list, is.factor,
                                   as.character)
    sp_hierarchy_list <- mutate_at(sp_hierarchy_list, grep("_CODE",
                                                           names(sp_hierarchy_list), value = T), as.numeric)
    for (ad in c(0,1,2)) {
      ad_summary_table <- make_admin_pred_summary(admin_pred = get(paste0("admin_",
                                                                          ad)), sp_hierarchy_list, summary_stats = c('mean', 'upper', 'lower'))
      fwrite(ad_summary_table, file = paste0(output_dir,
                                             indicator, "_admin_", ad, "_summary.csv"))
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Analysing the results #### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())

rural_adm0 <- read.csv('/ihme/geospatial/mbg/antibiotics/cough_antibiotics/output/2020_02_15_03_49_34/urban_rural/rural/admin_summaries/cough_antibiotics_admin_0_summary.csv', stringsAsFactors = F)
urban_adm0 <- read.csv('/ihme/geospatial/mbg/antibiotics/cough_antibiotics/output/2020_02_15_03_49_34/urban_rural/urban/admin_summaries/cough_antibiotics_admin_0_summary.csv', stringsAsFactors = F)

names(rural_adm0) <- c("ADM0_CODE", "ADM0_NAME", "region", "year", "rural_mean", "rural_upper", "rural_lower")
names(urban_adm0) <- c("ADM0_CODE", "ADM0_NAME", "region", "year", "urban_mean", "urban_upper", "urban_lower")
adm0 <- merge(rural_adm0, urban_adm0)

# Scatter plot of one against the other (colour by region/super region)
#merge on the super regions info
locs <- read.dbf("/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf")
locs <- locs[locs$level == 3,]
locs <- locs[c('loc_name', 'ihme_lc_id', 'spr_reg_id')]
locs$loc_name <- as.character(locs$loc_name)
locs$ihme_lc_id <- as.character(locs$ihme_lc_id)
locs$spr_reg_id <- as.character(locs$spr_reg_id)

#clean up some names to merge
adm0$ADM0_NAME[adm0$ADM0_NAME == 'Micronesia'] <- 'Federated States of Micronesia'
adm0$ADM0_NAME[adm0$ADM0_NAME == 'Republic of Congo'] <- 'Congo'
adm0$ADM0_NAME[grepl('Tom', adm0$ADM0_NAME)] <- "Sao Tome and Principe"
adm0$ADM0_NAME[adm0$ADM0_NAME == 'North Macedonia'] <- 'Macedonia'
adm0$ADM0_NAME[adm0$ADM0_NAME == "Côte d'Ivoire"] <- "Cote d'Ivoire"
adm0$ADM0_NAME[adm0$ADM0_NAME == 'Gambia'] <- 'The Gambia'

adm0 <- merge(adm0, locs, by.x = 'ADM0_NAME', by.y = 'loc_name', all.x = T, all.y = F)

#Define the super-regions (for colour on the plot)
adm0$region[adm0$spr_reg_id == 103] <- 'Latin America & Caribbean'
adm0$region[adm0$spr_reg_id == 137] <- 'North Africa & Middle East'
adm0$region[adm0$spr_reg_id == 158] <- 'South Asia'
adm0$region[adm0$spr_reg_id == 166] <- 'Sub-Saharan Africa'
adm0$region[adm0$spr_reg_id == 31] <- 'Central Europe, Eastern Europe & Central Asia'
adm0$region[adm0$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'
adm0$region[adm0$spr_reg_id == 64] <- 'Central Europe, Eastern Europe & Central Asia'   #64 is acutally high income but for some reaon GBD have georgia as High income not eastern europe/central asia

adm0$ihme_lc_id[adm0$ADM0_NAME == 'French Guiana'] <- 'GUF'
adm0$region[adm0$ADM0_NAME == 'French Guiana'] <- 'Latin America & Caribbean'
adm0$ihme_lc_id[adm0$ADM0_NAME == 'Kosovo'] <- 'RKS'
adm0$region[adm0$ADM0_NAME == 'Kosovo'] <- 'Central Europe, Eastern Europe & Central Asia'
adm0$ihme_lc_id[adm0$ADM0_NAME == 'Western Sahara'] <- 'ESH'
adm0$region[adm0$ADM0_NAME == 'Western Sahara'] <- 'Sub-Saharan Africa'

png('/ihme/geospatial/mbg/antibiotics/cough_antibiotics/output/2020_02_15_03_49_34/urban_rural/scatter_2018.png',
    height = 20, width = 35, units = 'cm', res = 300)
ggplot(adm0[adm0$year == 2018,])+
  geom_abline(slope = 1, intercept = 0, colour = 'black')+
  geom_point(aes(x = urban_mean*100, y = rural_mean*100, colour = region))+
  scale_colour_manual(values = c("#fe9929",
                                 "#e78ac3",
                                 "#984ea3",
                                 "#4daf4a",
                                 "#377eb8",
                                 "#e41a1c"))+
  xlim(0,100)+
  ylim(0,100)+
  theme_bw()+
  labs(x = 'Urban antibiotic usage (%)',
       y = 'Rural antibiotic usage (%)',
       colour = 'GBD Super region')
dev.off()

png('/ihme/geospatial/mbg/antibiotics/cough_antibiotics/output/2020_02_15_03_49_34/urban_rural/scatter_2000.png',
    height = 20, width = 35, units = 'cm', res = 300)
ggplot(adm0[adm0$year == 2000,])+
  geom_abline(slope = 1, intercept = 0, colour = 'black')+
  geom_point(aes(x = urban_mean*100, y = rural_mean*100, colour = region))+
  scale_colour_manual(values = c("#fe9929",
                                 "#e78ac3",
                                 "#984ea3",
                                 "#4daf4a",
                                 "#377eb8",
                                 "#e41a1c"))+
  xlim(0,100)+
  ylim(0,100)+
  theme_bw()+
  labs(x = 'Urban antibiotic usage (%)',
       y = 'Rural antibiotic usage (%)',
       colour = 'GBD Super region')
dev.off()

#plot the percentage difference
adm0$percent_diff <- ((adm0$rural_mean-adm0$urban_mean)/adm0$urban_mean)*100
adm0$region <- factor(adm0$region, levels = c("Sub-Saharan Africa",
                                              "Southeast Asia, East Asia & Oceania",                                   
                                              "South Asia",
                                              "Latin America & Caribbean",
                                              "North Africa & Middle East",
                                              "Central Europe, Eastern Europe & Central Asia"))

adm0 <-  adm0[order(-adm0$year, adm0$region, adm0$percent_diff),]
countries <- unique(adm0$ihme_lc_id[adm0$year == 2018])
adm0$ihme_lc_id <- factor(adm0$ihme_lc_id, levels = countries)

png('/ihme/geospatial/mbg/antibiotics/cough_antibiotics/output/2020_02_15_03_49_34/urban_rural/deviation_2018_v2.png',
    width=40, height=15, units = 'cm', res = 300)
ggplot(adm0[adm0$year == 2018 & !is.na(adm0$percent_diff),])+
  geom_bar(stat = "identity", aes(x = ihme_lc_id, y = percent_diff, fill = region))+
  scale_fill_manual(values = c("#e41a1c",
                               "#377eb8",
                               "#4daf4a",
                               "#e78ac3",
                               "#984ea3",
                                "#fe9929"))+
  labs(x = 'Country',
       y = 'Relative percentage difference of rural to urban anitbiotic usage',
       fill = 'GBD Super region')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position = 'bottom')
dev.off()

write.csv(adm0, '/ihme/geospatial/mbg/antibiotics/cough_antibiotics/output/2020_02_15_03_49_34/urban_rural/urban_rural_abx_usage.csv', row.names = F)
#guyana and western sahara do not have values and some of the countries are 100% urban/rural  

