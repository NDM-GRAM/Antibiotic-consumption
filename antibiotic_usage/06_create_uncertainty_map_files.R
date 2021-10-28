#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create files to plot the relative uncertainty of each admin 2 unit #
# The output csvs can the be plotted in GIS software                 #  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())

setwd('Z:/AMR/Covariates/antibiotic_use/abx_use')
# uncertainty map files
results <- read.csv('current/admin_summaries/cough_antibiotics_admin_2_unraked_summary.csv', stringsAsFactors = F)
results <-  results[results$year == 2018,]
results <- results[!is.na(results$mean),]

#rank the districts 1:4 based on antibiotics use (low to high)
results$abx_use <- NA
results$abx_use[results$mean <= quantile(results$mean, 0.25)] <- 1
results$abx_use[results$mean > quantile(results$mean, 0.25) & results$mean <= quantile(results$mean, 0.5)] <- 2
results$abx_use[results$mean > quantile(results$mean, 0.5) & results$mean <= quantile(results$mean, 0.75)] <- 3
results$abx_use[results$mean > quantile(results$mean, 0.75)] <- 4

#calculate uncertainty
results$relative_uncertainty <- (results$upper - results$lower)/results$mean

#rank the districts 1:4 based on relative uncertainty (low to high)
results$uncertainty <- NA
results$uncertainty[results$relative_uncertainty <= quantile(results$relative_uncertainty, 0.25)] <- 1
results$uncertainty[results$relative_uncertainty > quantile(results$relative_uncertainty, 0.25) & results$relative_uncertainty <= quantile(results$relative_uncertainty, 0.5)] <- 2
results$uncertainty[results$relative_uncertainty > quantile(results$relative_uncertainty, 0.5) & results$relative_uncertainty <= quantile(results$relative_uncertainty, 0.75)] <- 3
results$uncertainty[results$relative_uncertainty > quantile(results$relative_uncertainty, 0.75)] <- 4

#split out the layers for the uncertainty map
abx1 <- results[results$abx_use == 1,]
abx2 <- results[results$abx_use == 2,]
abx3 <- results[results$abx_use == 3,]
abx4 <- results[results$abx_use == 4,]

write.csv(abx1, 'current/uncertainty_map/adm2_abx1.csv', row.names = F)
write.csv(abx2, 'current/uncertainty_map/adm2_abx2.csv', row.names = F)
write.csv(abx3, 'current/uncertainty_map/adm2_abx3.csv', row.names = F)
write.csv(abx4, 'current/uncertainty_map/adm2_abx4.csv', row.names = F)
