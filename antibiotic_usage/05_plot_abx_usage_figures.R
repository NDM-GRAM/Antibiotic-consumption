#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot antibiotic usage results for admin 2 and national estimates. #
# This code creates too figures, plotting the range of antibitoic   #
# usage estimated for each country (for 2000 and 2018 comparison    #
# and just 2018), and the relative deviation in the antibiotic      #
# usage estimates for each country                                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(data.table)
library(foreign)
library(ggplot2)
library(gridExtra)
library(grid)

setwd('Z:/AMR/Covariates/antibiotic_use')

#Read in the results
admin0 <- fread('abx_use/current/admin_summaries/cough_antibiotics_admin_0_unraked_summary.csv', stringsAsFactors = F)
admin2 <- fread('abx_use/current/admin_summaries/cough_antibiotics_admin_2_unraked_summary.csv', stringsAsFactors = F)

admin0 <- admin0[,1:5]
names(admin0)[5] <- 'country_mean'
admin2$region <- NULL
admin2 <- admin2[!is.na(admin2$mean),]

#merge the admin 0 and admin 2
mydata <- merge(admin2, admin0, all.x = F, all.y = F)

#Calculate the deviation from the mean
mydata$absolute_deviation <- mydata$mean - mydata$country_mean
mydata$relative_deviation <- mydata$absolute_deviation/mydata$country_mean

#Get the values for the most deviation above and below the mean for each country, and the min and max admin 2 estimates
deviations <- mydata[,.(min_adm2 = min(mean),
                        max_adm2 = max(mean),
                        min_deviation = min(absolute_deviation),
                        max_deviation = max(absolute_deviation),
                        mean_deviation = mean(abs(relative_deviation))),
                     by = c('ADM0_NAME', 'year', 'country_mean')]

#get a total value of realitve deviation
deviations$total_deviation <- deviations$max_deviation - deviations$min_deviation

#Merge on additional location information for plotting
locs <- read.dbf("Z:/AMR/Shapefiles/GBD2019/GBD2019_analysis_final_loc_set_22.dbf")
locs <- locs[locs$level == 3,]
locs <- locs[c('loc_name', 'ihme_lc_id', 'spr_reg_id')]
locs$loc_name <- as.character(locs$loc_name)
locs$ihme_lc_id <- as.character(locs$ihme_lc_id)
locs$spr_reg_id <- as.character(locs$spr_reg_id)

#clean up some names to merge
deviations$ADM0_NAME[deviations$ADM0_NAME == 'Micronesia'] <- 'Federated States of Micronesia'
deviations$ADM0_NAME[deviations$ADM0_NAME == 'Republic of Congo'] <- 'Congo'
deviations$ADM0_NAME[grepl('Tom', deviations$ADM0_NAME)] <- "Sao Tome and Principe"
deviations$ADM0_NAME[deviations$ADM0_NAME == 'North Macedonia'] <- 'Macedonia'
deviations$ADM0_NAME[deviations$ADM0_NAME == "Côte d'Ivoire"] <- "Cote d'Ivoire"
deviations$ADM0_NAME[deviations$ADM0_NAME == 'Gambia'] <- 'The Gambia'
deviations$ADM0_NAME[deviations$ADM0_NAME == 'Gambia'] <- 'The Gambia'

deviations <- merge(deviations, locs, by.x = 'ADM0_NAME', by.y = 'loc_name', all.x = T, all.y = F)

#Define the super-regions (for colour on the plot)
deviations$region[deviations$spr_reg_id == 103] <- 'Latin America & Caribbean'
deviations$region[deviations$spr_reg_id == 137] <- 'North Africa & Middle East'
deviations$region[deviations$spr_reg_id == 158] <- 'South Asia'
deviations$region[deviations$spr_reg_id == 166] <- 'Sub-Saharan Africa'
deviations$region[deviations$spr_reg_id == 31] <- 'Central Europe, Eastern Europe & Central Asia'
deviations$region[deviations$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'
deviations$region[deviations$spr_reg_id == 64] <- 'Central Europe, Eastern Europe & Central Asia'   #64 is acutally high income but for some reaon GBD have georgia as High income not eastern europe/central asia

deviations$ihme_lc_id[deviations$ADM0_NAME == 'French Guiana'] <- 'GUF'
deviations$region[deviations$ADM0_NAME == 'French Guiana'] <- 'Latin America & Caribbean'
deviations$ihme_lc_id[deviations$ADM0_NAME == 'Kosovo'] <- 'RKS'
deviations$region[deviations$ADM0_NAME == 'Kosovo'] <- 'Central Europe, Eastern Europe & Central Asia'
deviations$ihme_lc_id[deviations$ADM0_NAME == 'Western Sahara'] <- 'ESH'
deviations$region[deviations$ADM0_NAME == 'Western Sahara'] <- 'Sub-Saharan Africa'


#Order data based on the national mean (so same as main figure), change to factor for plotting
deviations <- deviations[order(-deviations$year, deviations$country_mean),]
countries <- unique(deviations$ihme_lc_id)
deviations$ihme_lc_id <- factor(deviations$ihme_lc_id, levels = countries)

# Create plots
#Plot the relative deviations for 2018
deviations_2018 <- 
ggplot(deviations[deviations$year == 2018,])+
  geom_crossbar(aes(x = ihme_lc_id, y = 0, ymin = min_deviation, ymax = max_deviation, colour = region), size=1.5, width=0.05)+
  geom_point(aes(x = ihme_lc_id, y = 0), color='#000000', shape=18, size=1.8)+
  scale_colour_manual(values = c("#fe9929",
                                 "#e78ac3",
                                 "#984ea3",
                                 "#4daf4a",
                                 "#377eb8",
                                 "#e41a1c"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab('Country')+
  labs(colour = "GBD Super-region", title = 'Relative deviation from country mean')+
  scale_y_continuous("Relative deviation", breaks = c(-.3, -.2, -0.1, 0, 0.1, 0.2, 0.3), labels = c('-0.3', '-0.2', '-0.1', '0', '0.1', '0.2', '0.3'), expand = c(0, 0), limits = c(-.4, .4))+
  # ylim(0,1)+
  guides(alpha=F)+
  theme(legend.position = 'bottom')


#Plot the range of relative deviations for 2000 and 2018
deviations_2000_2018 <- 
ggplot()+
  # geom_hline(yintercept = c(0.25, 0.5, 0.75), colour = 'grey', size = 0.5)+
  geom_crossbar(data = deviations[deviations$year == 2000,], aes(x = ihme_lc_id, y = 0, ymin = min_deviation, ymax = max_deviation),colour = '#969696', size=1.5, width=0.01)+
  geom_crossbar(data = deviations[deviations$year == 2018,], aes(x = ihme_lc_id, y = 0, ymin = min_deviation, ymax = max_deviation, colour = region), size=1.5, width=0.01, position=position_nudge(x=.5))+
  geom_point(data = deviations, aes(x = ihme_lc_id, y = 0), color='#000000', shape=18, size=1)+
  geom_point(data = deviations, aes(x = ihme_lc_id, y = 0), color='#000000', shape=18, size=1, position = position_nudge(x=.5))+
  scale_colour_manual(values = c("#fe9929",
                                 "#e78ac3",
                                 "#984ea3",
                                 "#4daf4a",
                                 "#377eb8",
                                 "#e41a1c"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab('Country')+
  labs(colour = "GBD Super-region", title = 'Relative deviation from country mean')+
  scale_y_continuous("Relative deviation", breaks = c(-.4, -.2, 0, 0.2, 0.4), labels = c('-0.4', '-0.2', '0', '0.2', '0.4'), expand = c(0, 0), limits = c(-.4, .4))+
  # ylim(0,1)+
  guides(alpha=F)+
  theme(legend.position = 'bottom')

#Plot the mean relative deviations for 2000 and 2018
mean_deviations_2000_2018 <- 
  ggplot()+
  geom_crossbar(data = deviations[deviations$year == 2000,], aes(x = ihme_lc_id, y = 0, ymin = 0, ymax = mean_deviation),colour = '#969696', size=1.5, width=0.01)+
  geom_crossbar(data = deviations[deviations$year == 2018,], aes(x = ihme_lc_id, y = 0, ymin = 0, ymax = mean_deviation, colour = region), size=1.5, width=0.01, position=position_nudge(x=.5))+
  scale_colour_manual(values = c("#fe9929",
                                 "#e78ac3",
                                 "#984ea3",
                                 "#4daf4a",
                                 "#377eb8",
                                 "#e41a1c"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab('Country')+
  labs(colour = "GBD Super-region", title = 'Mean relative deviation')+
  scale_y_continuous("Mean relative deviation (%)", breaks = c(0, 0.1, 0.2, 0.3), labels = c('0', '10', '20', '30'), expand = c(0, 0), limits = c(0, 0.3))+
  # ylim(0,1)+
  guides(alpha=F)+
  theme(legend.position = 'bottom')

#Plot the admin2 ranges for 2018
admin2_2018 <- 
ggplot(deviations[deviations$year == 2018,])+
  geom_crossbar(aes(x = ihme_lc_id, y = country_mean*100, ymin = min_adm2*100, ymax = max_adm2*100, colour = region), size=1.5, width=0.05)+
  geom_point(aes(x = ihme_lc_id, y = country_mean*100), color='#000000', shape=18, size=1.8)+
  scale_colour_manual(values = c("#fe9929",
                                 "#e78ac3",
                                 "#984ea3",
                                 "#4daf4a",
                                 "#377eb8",
                                 "#e41a1c"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab(NULL)+
  labs(colour = "GBD Super-region", title = 'Antibiotic usage')+
  scale_y_continuous("Percentage of antibiotic usage in children with LRI", breaks = c(0, 25, 50, 75, 100), labels = c('0', '25', '50', '75', '100'), expand = c(0, 0), limits = c(0,100))+
  guides(alpha=F)+
  theme(legend.position = 'none')

#Plot the admin2 ranges for 2000 and 2018
admin2_2000_2018 <- 
ggplot()+
  geom_crossbar(data = deviations[deviations$year == 2000,], aes(x = ihme_lc_id, y = country_mean*100, ymin = min_adm2*100, ymax = max_adm2*100),colour = '#969696', size=1.5, width=0.01)+
  geom_crossbar(data = deviations[deviations$year == 2018,], aes(x = ihme_lc_id, y = country_mean*100, ymin = min_adm2*100, ymax = max_adm2*100, colour = region), size=1.5, width=0.01, position=position_nudge(x=.5))+
  geom_point(data = deviations[deviations$year == 2000,], aes(x = ihme_lc_id, y = country_mean*100), color='#000000', shape=17, size=1.5)+
  geom_point(data = deviations[deviations$year == 2018,], aes(x = ihme_lc_id, y = country_mean*100), color='#000000', shape=18, size=1.5, position = position_nudge(x=.5))+
  scale_colour_manual(values = c("#fe9929",
                                 "#e78ac3",
                                 "#984ea3",
                                 "#4daf4a",
                                 "#377eb8",
                                 "#e41a1c"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab(NULL)+
  labs(colour = "GBD Super-region", title = 'Antibiotic usage')+
  scale_y_continuous("Percentage of antibiotic usage in children with LRI", breaks = c(0, 25, 50, 75, 100), labels = c('0', '25', '50', '75', '100'), expand = c(0, 0), limits = c(0,100))+
  # ylim(0,1)+
  guides(alpha=F)+
  theme(legend.position = 'none')

#Save plots
png('abx_use/figures/plots/admin2_2018.png', 
    height = 10, width = 35, units = 'cm', res = 300)
admin2_2018
dev.off()

png('abx_use/figures/plots/adm2_2000_2018.png', 
    height = 10, width = 35, units = 'cm', res = 300)
admin2_2000_2018
dev.off()  

png('abx_use/figures/plots/realitve_deviation_2000_2018.png', 
    height = 10, width = 35, units = 'cm', res = 300)
deviations_2000_2018
dev.off()

png('abx_use/figures/plots/realitve_deviation_2018.png', 
    height = 10, width = 35, units = 'cm', res = 300)
deviations_2018
dev.off()


# Save multi-figure
pdf('abx_use/figures/plots/admin_2_relative_deviation_figure_2000_2018.pdf',
    height=14, width=22
)
grid.arrange(admin2_2000_2018, deviations_2000_2018, nrow = 2, heights=c(8,3.5))
vp <- grid::viewport(
  x = unit(.03, 'npc'),
  y = unit(.92, 'npc'),
  width = unit(.2, 'npc'),
  height= unit(.16, 'npc'),
  just = c('left','top')
)
grid::pushViewport(vp)
dev.off()

png('abx_use/figures/plots/admin_2_relative_deviation_figure_2018.png',
    height=14, width=22, units = 'in', res = 300
)
grid.arrange(admin2_2018, deviations_2018, nrow = 2, heights=c(8,3.5))
vp <- grid::viewport(
  x = unit(.03, 'npc'),
  y = unit(.92, 'npc'),
  width = unit(.2, 'npc'),
  height= unit(.16, 'npc'),
  just = c('left','top')
)
grid::pushViewport(vp)
dev.off()
 
#mean relative deviations figure
png('abx_use/figures/plots/mean_relative_deviation_figure_2000_2018.png',
    height=14, width=22, units = 'in', res = 300
)
grid.arrange(admin2_2000_2018, mean_deviations_2000_2018, nrow = 2, heights=c(8,3.5))
vp <- grid::viewport(
  x = unit(.03, 'npc'),
  y = unit(.92, 'npc'),
  width = unit(.2, 'npc'),
  height= unit(.16, 'npc'),
  just = c('left','top')
)
grid::pushViewport(vp)
dev.off()

# Save out the files
write.csv(deviations, 'abx_use/realitve_deviation.csv', row.names = F)
