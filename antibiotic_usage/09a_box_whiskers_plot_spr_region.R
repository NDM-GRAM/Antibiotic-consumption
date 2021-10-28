rm(list = ls())
library(data.table)
library(ggplot2)
library(foreign)
library(stringr)

mydata <- fread("Z:/AMR/Covariates/antibiotic_use/abx_use/current/admin_summaries/cough_antibiotics_admin_0_unraked_summary.csv")

#Merge on additional location information for plotting
locs <- read.dbf("Z:/AMR/Shapefiles/GBD2019/GBD2019_analysis_final_loc_set_22.dbf")
locs <- locs[locs$level == 3,]
locs <- locs[c('loc_name', 'ihme_lc_id', 'spr_reg_id')]
locs$loc_name <- as.character(locs$loc_name)
locs$ihme_lc_id <- as.character(locs$ihme_lc_id)
locs$spr_reg_id <- as.character(locs$spr_reg_id)

#clean up some names to merge
mydata$ADM0_NAME[mydata$ADM0_NAME == 'Micronesia'] <- 'Federated States of Micronesia'
mydata$ADM0_NAME[mydata$ADM0_NAME == 'Republic of Congo'] <- 'Congo'
mydata$ADM0_NAME[grepl('Tom', mydata$ADM0_NAME)] <- "Sao Tome and Principe"
mydata$ADM0_NAME[mydata$ADM0_NAME == 'North Macedonia'] <- 'Macedonia'
mydata$ADM0_NAME[mydata$ADM0_NAME == "Côte d'Ivoire"] <- "Cote d'Ivoire"
mydata$ADM0_NAME[mydata$ADM0_NAME == 'Gambia'] <- 'The Gambia'

mydata <- merge(mydata, locs, by.x = 'ADM0_NAME', by.y = 'loc_name', all.x = T, all.y = F)

#Define the super-regions (for colour on the plot)
mydata$region[mydata$spr_reg_id == 103] <- 'Latin America & Caribbean'
mydata$region[mydata$spr_reg_id == 137] <- 'North Africa & Middle East'
mydata$region[mydata$spr_reg_id == 158] <- 'South Asia'
mydata$region[mydata$spr_reg_id == 166] <- 'Sub-Saharan Africa'
mydata$region[mydata$spr_reg_id == 31] <- 'Central Europe, Eastern Europe & Central Asia'
mydata$region[mydata$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'
mydata$region[mydata$spr_reg_id == 64] <- 'Central Europe, Eastern Europe & Central Asia'   #64 is acutally high income but for some reaon GBD have georgia as High income not eastern europe/central asia

mydata$ihme_lc_id[mydata$ADM0_NAME == 'French Guiana'] <- 'GUF'
mydata$region[mydata$ADM0_NAME == 'French Guiana'] <- 'Latin America & Caribbean'
mydata$ihme_lc_id[mydata$ADM0_NAME == 'Kosovo'] <- 'RKS'
mydata$region[mydata$ADM0_NAME == 'Kosovo'] <- 'Central Europe, Eastern Europe & Central Asia'
mydata$ihme_lc_id[mydata$ADM0_NAME == 'Western Sahara'] <- 'ESH'
mydata$region[mydata$ADM0_NAME == 'Western Sahara'] <- 'Sub-Saharan Africa'

# Plot out box plot of antibitoic usage for 2000 and 2018
mydata$region <- str_wrap(mydata$region, width = 10)

#box and whiskers plot of medians for 2000 and 2018
png('Z:/AMR/Covariates/antibiotic_use/abx_use/figures/plots/median_usage_spr_reg.png',
    height = 10, width = 15, unit = 'cm', res = 300)
ggplot(mydata[mydata$year == 2000|mydata$year == 2018,])+
  geom_boxplot(aes(x = region, y = mean*100, alpha = as.factor(year), fill = region))+
  ylim(0,100)+
  scale_fill_manual(values = c("#fe9929",
                                 "#e78ac3",
                                 "#984ea3",
                                 "#4daf4a",
                                 "#377eb8",
                                 "#e41a1c"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = 'Super region', y = 'Antibiotic usage (%)')+
  theme(legend.position = "none")
dev.off()
