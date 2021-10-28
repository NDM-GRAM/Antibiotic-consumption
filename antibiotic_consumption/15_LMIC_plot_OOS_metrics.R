#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Get the STGPR model runs for holdouts and plot the OOS stats #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Get the input data with the fold ID's in it
rm(list = ls())
library(ggplot2)
library(caret)
library(ini, lib = '/share/homes/annieb6/temp_packages/')
source('/share/code/st_gpr/central/stgpr/r_functions/utilities/utility.r')

run_date = '2020_10_05'
mydata <- read.csv(paste0('/share/homes/annieb6/AMR/antibiotic_use/sales_data/holdouts/', run_date, '/LMIC_master_data.csv'), stringsAsFactors = F)
#restrict to LMICs

run_ids <- c('164453',
             '164456',
             '164459',
             '164462',
             '164465'
             )

#load in the GPR results and look at the OOS fits
for(i in 1:length(run_ids)){
  gpr <- model_load(run_ids[i], 'gpr')
  gpr <- gpr[,1:5]
  gpr$age_group_id <- NULL
  gpr$sex_id <- NULL
  mydata <- merge(mydata, gpr, by = c('location_id', 'year_id'))
  colnames(mydata)[colnames(mydata) == 'gpr_mean'] <- paste0('se_stgpr_ho', i)
  
}

mydata$se_stgpr_oos <- NA
mydata$se_stgpr_oos[mydata$master_fold_id == 1] <- mydata$se_stgpr_ho1[mydata$master_fold_id == 1]
mydata$se_stgpr_oos[mydata$master_fold_id == 2] <- mydata$se_stgpr_ho2[mydata$master_fold_id == 2]
mydata$se_stgpr_oos[mydata$master_fold_id == 3] <- mydata$se_stgpr_ho3[mydata$master_fold_id == 3]
mydata$se_stgpr_oos[mydata$master_fold_id == 4] <- mydata$se_stgpr_ho4[mydata$master_fold_id == 4]
mydata$se_stgpr_oos[mydata$master_fold_id == 5] <- mydata$se_stgpr_ho5[mydata$master_fold_id == 5]

mydata$val <- exp(mydata$n)
se_stgpr_r2 <- cor(mydata$val, mydata$se_stgpr_oos)^2
se_stgpr_rmse <- RMSE(mydata$val, mydata$se_stgpr_oos)

min <- min(c(mydata$val, mydata$se_stgpr_oos))
max <- max(c(mydata$val, mydata$se_stgpr_oos))

#plot out the predicted vs estimated
png(paste0('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/holdouts/', run_date, '/oos_correlation.png'),
    height = 12, width = 12, res = 350, unit = 'in')
print(
  ggplot(mydata)+
    geom_point(aes(x = val, y=se_stgpr_oos))+
    xlim(1000, 23000)+
    ylim(1000, 23000)+
    geom_abline(slope = 1, intercept = 0, colour = 'red')+
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    labs(
      x = "Data Estimate",
      y = "Mean Prediction",
      size = "Weight",
      title = ("OOS Validation Plot for Stacked Ensemble-STGPR"),
      subtitle = paste0("RMSE = ", round(se_stgpr_rmse,0), ";    R2 = ", round(se_stgpr_r2,2)))
) 
dev.off()

#~~~~~#
# END #
#~~~~~#