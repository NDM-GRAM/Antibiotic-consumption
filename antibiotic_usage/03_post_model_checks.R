#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Post model checks and plots to assess model fit and result #### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# i. Setup the packages, run information and functions required #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
indicator                <- 'cough_antibiotics'
indicator_group          <- 'antibiotics'
core_repo                <- '/share/code/geospatial/annieb6/global_antibitoic_consumption/anitbiotic_use_model/'
run_date                 <- '2020_02_15_03_49_34'

# Load MBG packages and functions
package_list      <- readLines(paste0(core_repo, "/mbg_central/share_scripts/common_inputs/package_list.csv"))
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

# Load additional functions and libraries
libs <- c('RColorBrewer', 'sp', 'sf', 'ggpubr', 'ggrepel')
lapply(libs, library, character.only = TRUE, quietly = TRUE)
rm(libs)

source('/share/code/geospatial/annieb6/global_antibitoic_consumption/anitbiotic_use_model/03a_model_validation_functions.R')

#Re-load config file
config <- set_up_config(repo = core_repo,
                        indicator = indicator,
                        indicator_group = indicator_group,
                        run_date = run_date,
                        post_est_only = TRUE)

if (class(year_list) == "character") year_list <- eval(parse(text=year_list))
if (class(Regions) == "character" & length(Regions) == 1) Regions <- eval(parse(text=Regions))
outputdir <- file.path('/share/geospatial/mbg',indicator_group,indicator,'output',run_date,'/model_validation/')
dir.create(outputdir, showWarnings = F)
input_data_date <- datatag

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. Plot out the time series maps and eye-ball results ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
admin_maps(adm_levels = c(0, 1, 2),
           run_date,
           indicator,
           indicator_group,
           years = seq(2000, 2015, 5),
           outdir = outputdir, 
           high_bad = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 2. Compare estimates to raw data & compare to previous model runs  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

input <- fread(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator,'/output/',run_date, '/input_data.csv'))
input$point <- 0
input$sum_of_sample_weights <- 1
input_admins <- input_aggregate_admin(indicator,
                                      indicator_group,
                                      run_date = NULL,
                                      regions = Regions,
                                      input_data = input,
                                      indicator_family = "binomial",
                                      sample_column = "sum_of_sample_weights",
                                      shapefile_version = modeling_shapefile_version)


admin0 <- as.data.frame(input_admins[1])
admin1 <- as.data.frame(input_admins[2])
admin2 <- as.data.frame(input_admins[3])

colnames(admin0) <- gsub('ad0.', '', colnames(admin0))
colnames(admin1) <- gsub('ad1.', '', colnames(admin1))
colnames(admin2) <- gsub('ad2.', '', colnames(admin2))

dir.create(paste0(outputdir, '/aggregated_input_data/'), showWarnings = F)
saveRDS(admin0, paste0(outputdir, 'aggregated_input_data/input_admin0.RDS'))
saveRDS(admin1, paste0(outputdir, 'aggregated_input_data/input_admin1.RDS'))
saveRDS(admin2, paste0(outputdir, 'aggregated_input_data/input_admin2.RDS'))

# Time-series plots
# Set up directories and files
#Set run_dates (for multiple model runs), indicator (or indicators), indicator_group, out_dir per your preferences
run_dates <- c(run_date)
run_label <- c('new_run')
indicators <- c(indicator)
in_dir <- paste0('/share/geospatial/mbg/', indicator_group,'/', indicator,'/output/',run_dates,'/pred_derivatives/admin_summaries/')

in_file_ad0 <- paste0(in_dir, indicators, "_admin_0_unraked_summary.csv")
in_file_ad1 <- paste0(in_dir, indicators, "_admin_1_unraked_summary.csv")
in_file_ad2 <- paste0(in_dir, indicators, "_admin_2_unraked_summary.csv")

# Prepare inputs
# Read in all mode admin aggregations, adding run label to each with add_run_label function
ad0_df <- lapply(in_file_ad0, fread) %>% add_run_label(run_label)
ad1_df <- lapply(in_file_ad1, fread) %>% add_run_label(run_label)
ad2_df <- lapply(in_file_ad2, fread) %>% add_run_label(run_label)

# Read in the raw data aggregates
admin0 <- readRDS(paste0(outputdir, '/aggregated_input_data/input_admin0.RDS'))
admin1 <- readRDS(paste0(outputdir, '/aggregated_input_data/input_admin1.RDS'))
admin2 <- readRDS(paste0(outputdir, '/aggregated_input_data/input_admin2.RDS'))

admin0 <- as.data.table(admin0)
admin1 <- as.data.table(admin1)
admin2 <- as.data.table(admin2)

subnational_ts_plots (ad0_df = ad0_df,
                      ad1_df= ad1_df,
                      ad2_df= ad2_df,
                      ind_title = indicator,
                      out_dir = paste0(outputdir, '/time_series_plots/'),
                      out_filename_format = "subnational_ts_plots_%s.pdf",
                      val_range = c(0,1),
                      highisbad = T,
                      ad0_map_regions = Regions,
                      ad0_map_region_titles = Regions,
                      plot_levels = c("ad0"),
                      multiple_runs = T,
                      plot_data = T,
                      ad0_data = admin0,
                      ad1_data = admin1,
                      ad2_data = admin2,
                      verbose = T,
                      shapefile_version = modeling_shapefile_version)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3. Plot the parts of the model & covariates ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#b. Covariate importance plots
get_cov_weights(indicator,
                indicator_group,
                run_date,
                regions = Regions,
                outdir =  paste0(outputdir, '/covariate_importance/'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 4. Analyse hyperparameters ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(gridExtra)
dir.create(paste0(outputdir, '/hyperparameters/'))

# Create table of hyperparameters
use_stacking_covs = TRUE
use_gp = TRUE
hyperparameters <- clean_model_results_table()

pdf(paste0(outputdir, '/hyperparameters/hyperparameter_tables.pdf'))
lapply(hyperparameters, function(x){
table <- tableGrob(x)
grid.newpage()
h <- grobHeight(table)
w <- grobWidth(table)
title <- textGrob(names(x)[5], y=unit(0.5,"npc") + h,
                  vjust=0, gp=gpar(fontsize=20))
gt <- gTree(children=gList(table, title))
grid.draw(gt)})
dev.off()

lapply(hyperparameters, function(x){
  names(x)[6]
})

# Plots of hyperparameters
plot_hyperparameters(indicator = indicator,
                     indicator_group = indicator_group,
                     run_date = run_date,
                     age = 0,
                     holdout = 0,
                     save_file = paste0(outputdir, '/hyperparameters/inla_hyperparameters.pdf'),
                     regions = Regions)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#5. Check colinearity in GAM ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#a. calculate the pairwise correlation between all covariates
covariate_corr_matrix (indicator = indicator,
                        indicator_group = indicator_group,
                        run_date = run_date,
                        regions = Regions,
                        out_dir = paste0(outputdir, '/covariate_correlation/'))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#6. Get out of sample fit stats for aggregated adm0, 1 & 2 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
run_in_oos <- get_is_oos_draws(ind_gp = indicator_group,
                               ind = indicator,
                               rd = run_date,
                               ind_fm = 'binomial',
                               age = 0,
                               nperiod = 19,
                               yrs = 2000:2018,
                               get.oos = as.logical(makeholdouts),
                               write.to.file = TRUE,
                               year_col = 'year',
                               shapefile_version = modeling_shapefile_version)

## set out_dir
out_dir <- paste0(outputdir, "/oos_metrics/")
dir.create(out_dir, recursive = T, showWarnings = F)

## for admin0
draws.df <- fread(sprintf("/share/geospatial/mbg/%s/%s/output/%s/output_draws_data.csv",
                          indicator_group, indicator, run_date))

unique(draws.df$region)
#clean up region names
draws.df$region[draws.df$region == "balkans_ext+caucasus"] <-  'Balkans & Caucasus'
draws.df$region[draws.df$region == "dia_mcaca"] <-  'Central America & Caribbean'
draws.df$region[draws.df$region == "dia_central_asia+kaz+mng"] <-  'Central Asia'
draws.df$region[draws.df$region == "dia_cssa"] <-  'Central sub-Saharan Africa'
draws.df$region[draws.df$region == "dia_essa+dia_sssa-syc"] <-  'Eastern & Southern sub-Saharan Africa'
draws.df$region[draws.df$region == "dia_afr_horn"] <-  'Horn of Africa'
draws.df$region[draws.df$region == "dia_malay+dia_oceania"] <-  'Malay & Oceania'
draws.df$region[draws.df$region == "dia_mid_east"] <-  'Middle East'
draws.df$region[draws.df$region == "dia_name"] <-  'North Africa'
draws.df$region[draws.df$region == "dia_s_america-tto"] <-  'South America'
draws.df$region[draws.df$region == "dia_south_asia"] <-  'South Asia'
draws.df$region[draws.df$region == "dia_se_asia"] <-  'Southeast Asia'
draws.df$region[draws.df$region == "dia_wssa"] <-  'Western sub-Saharan Africa'

country.pvtable <- get_pv_table(d = draws.df,
                                indicator = indicator,
                                indicator_group = indicator_group,
                                rd = run_date,
                                aggregate_on='country',
                                draws = as.numeric(samples),
                                coverage_probs = c(95),
                                result_agg_over =  c('oos', 'region'),
                                weighted = TRUE,
                                family = 'binomial',
                                plot = TRUE,
                                plot_by = 'region',
                                plot_by_title = 'region',
                                plot_ci = FALSE,
                                plot_ci_level = 95,
                                ci_color = "grey",
                                point_alpha = 1,
                                point_color = "black",
                                plot_title = indicator,
                                plot_ncol = 4,
                                save_csv = T,
                                out.dir = out_dir)

country.pvtable <- get_pv_table(d = draws.df,
                                indicator = indicator,
                                indicator_group = indicator_group,
                                rd = run_date,
                                aggregate_on='country',
                                draws = as.numeric(samples),
                                coverage_probs = c(95),
                                result_agg_over =  c('oos'),
                                weighted = TRUE,
                                family = 'binomial',
                                plot = TRUE,
                                # plot_by = 'region',
                                # plot_by_title = 'region',
                                plot_ci = FALSE,
                                plot_ci_level = 95,
                                ci_color = "grey",
                                point_alpha = 1,
                                point_color = "black",
                                plot_title = indicator,
                                plot_ncol = 4,
                                save_csv = T,
                                out.dir = paste0(out_dir, '/country/'))

ad1.pvtable <- get_pv_table(d = draws.df,
                            indicator = indicator,
                            indicator_group = indicator_group,
                            rd = run_date,
                            aggregate_on='ad1',
                            draws = as.numeric(samples),
                            coverage_probs = c(95),
                            result_agg_over =  c('oos', 'region'),
                            weighted = TRUE,
                            family = 'binomial',
                            plot = TRUE,
                            plot_by = 'region',
                            plot_by_title = 'region',
                            plot_ci = FALSE,
                            plot_ci_level = 95,
                            ci_color = "grey",
                            point_alpha = 1,
                            point_color = "black",
                            plot_title = indicator,
                            plot_ncol = 4,
                            save_csv = T,
                            out.dir = paste0(out_dir, '/admin1/'))

ad1.pvtable <- get_pv_table(d = draws.df,
                            indicator = indicator,
                            indicator_group = indicator_group,
                            rd = run_date,
                            aggregate_on='ad1',
                            draws = as.numeric(samples),
                            coverage_probs = c(95),
                            result_agg_over =  c('oos'),
                            weighted = TRUE,
                            family = 'binomial',
                            plot = TRUE,
                            # plot_by = 'region',
                            # plot_by_title = 'region',
                            plot_ci = FALSE,
                            plot_ci_level = 95,
                            ci_color = "grey",
                            point_alpha = 1,
                            point_color = "black",
                            plot_title = indicator,
                            plot_ncol = 4,
                            save_csv = T,
                            out.dir = paste0(out_dir, '/admin1/'))

ad2.pvtable <- get_pv_table(d = draws.df,
                            indicator = indicator,
                            indicator_group = indicator_group,
                            rd = run_date,
                            aggregate_on='ad2',
                            draws = as.numeric(samples),
                            coverage_probs = c(95),
                            result_agg_over =  c('year','oos', 'region'),
                            weighted = TRUE,
                            family = 'binomial',
                            plot = TRUE,
                            plot_by = 'region',
                            plot_by_title = 'region',
                            plot_ci = FALSE,
                            plot_ci_level = 95,
                            ci_color = "grey",
                            point_alpha = 1,
                            point_color = "black",
                            plot_title = indicator,
                            plot_ncol = 4,
                            save_csv = T,
                            out.dir = out_dir)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#7. Plot the child stackers ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
plot_child_stackers(indicator = indicator,
                    indicator_group = indicator_group,
                    run_date = run_date,
                    regions = Regions,
                    start_year = 1990,
                    end_year = 2018,
                    out_dir = paste0(outputdir, '/stackers'),
                    pop_measure = 'a0004t')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#8. Plot absolute errors ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dir.create(paste0(outputdir, '/abs_error/'))
gaul_list <- get_adm0_codes(regions,
                            shapefile_version = modeling_shapefile_version)

abs_err_plot_list <- plot_abs_errors(gaul_list = gaul_list,
                                     df = run_in_oos, ## takes output from get_is_oos_draws()
                                     sample = ifelse((as.logical(makeholdouts)==TRUE), 'BOTH', "IS"),
                                     subset_shape = subset_shape,
                                     ind = indicator,
                                     ind_gp = indicator_group,
                                     rd = run_date,
                                     save.dir = paste0(outputdir, '/abs_error/'))

#~~~~~~~~~~~~~#
# End of file #
#~~~~~~~~~~~~~#