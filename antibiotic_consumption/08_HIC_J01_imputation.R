#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Impute data for the High Income Countries #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list =ls())
library(mice)
library(VIM)
library(sjmisc)
library(data.table)
library(ggplot2)
library(Metrics)
library(foreign)
library(RColorBrewer)

setwd('Z:/AMR/Covariates/antibiotic_use/')

#1. Data prep ###
# Read in sales data
mydata <- read.csv('combined_sources/J01_DDD_2000_2018.csv', stringsAsFactors = F)
mydata <- data.table(mydata)

#remove brunei as this is incomplete data and is far too low to be believable
mydata <- mydata[mydata$country != 'BRUNEI',]

# Read in the HIC countries
HIC <- read.csv('Z:/AMR/Misc/world_bank_regions/HICs.csv', stringsAsFactors = F)
HIC <- data.table(HIC)
HIC <- HIC[HIC$level == 3,]

# Add on China, Russia and Lebanon as modelling these with the HICs
locs <- read.dbf('Z:/AMR/Shapefiles/GBD2019/GBD2019_analysis_final.dbf')
extra <- locs[locs$loc_id == 44533 | locs$loc_id == 354 | locs$loc_id == 62 | locs$loc_id == 146 ,]
HIC <- rbind(HIC, extra)
rm(locs, extra)

# Limit the sales data to HICs
mydata <- mydata[mydata$loc_id %in% HIC$loc_id,]

# Merge on the missing locations & years
HIC <- HIC[!(HIC$loc_id %in% mydata$loc_id),]

#add on pop
pop <- read.csv('Z:/AMR/Misc/GBD_populations/GBD_total_populations.csv', stringsAsFactors = F)
pop <- data.table(pop)
names(pop) <- c('loc_id', 'year', 'pop')
pop <- pop[year>=2000 & year <=2018,]

for(i in 2000:2018){
  HIC_extra <- HIC[,.(loc_id, 
                      year = i,
                      super_region = spr_reg_id,
                      region = region_id,
                      country = loc_name,
                      iso3 = ihme_lc_id,
                      GAUL_CODE,
                      ddd = NA,
                      ddd_per_1000 = NA,
                      source = 'imputed')]
  
  HIC_extra <-  merge(HIC_extra, pop, by = c('loc_id', 'year'), all.x = T, all.y = F)
  mydata <- rbind(mydata, HIC_extra)
}

cyp <- mydata[mydata$loc_id == 77 & mydata$year == 2006,]
cyp <- cyp[,.(loc_id = loc_id, 
              year = 2000:2005,
              super_region = super_region,
              region = region,
              country = country,
              iso3 = iso3,
              GAUL_CODE,
              ddd = NA,
              ddd_per_1000 = NA,
              source = 'imputed')]

# brn <- merge(brn, pop, by = c('loc_id', 'year'), all.x = T, all.y = F)
cyp <- merge(cyp, pop, by = c('loc_id', 'year'), all.x = T, all.y = F)

mydata <- rbind(mydata, cyp)

#2. Imputation ####
extra <- mydata[,.(country, iso3, loc_id, GAUL_CODE, year, pop)]
extra <-  unique(extra)
env.covs <- data.frame(mydata)
env.covs$ddd <- NULL
env.covs$iso3 <- NULL
env.covs$loc_id <- NULL
env.covs$GAUL_CODE <- NULL
env.covs$pop <- NULL

#CHECK WHATS MISSING IN THE VARIABLES
png('combined_sources/HIC_imputation/missing_data.png',
    height = 20, width = 20, unit = 'cm', res = 200)
  aggr(env.covs, col=c('navyblue','yellow'),
       numbers=TRUE, sortVars=TRUE,
       labels=names(env.covs), cex.axis=.7,
       gap=3, ylab=c("Missing data","Pattern"))
dev.off()

##IMPUTE THE MISSING VALUES
imputed.data <- mice(data = env.covs, m=5, maxit = 50, method = 'cart', seed = 1234)

# Plot imputations
png('combined_sources/HIC_imputation/imputed_data.png',
    height = 20, width = 20, unit = 'cm', res = 200)
  plot(imputed.data)
dev.off()

png('combined_sources/HIC_imputation/imputed_data2.png',
    height = 20, width = 20, unit = 'cm', res = 200)
  stripplot(imputed.data, ddd_per_1000~.imp, pch=20, cex=2)
dev.off()

#MERGE WITH THE ORIGINAL DATA FRAME
imputed.data <- merge_imputations(env.covs, imputed.data,
                                  ori = env.covs, summary = c("hist"))
png('combined_sources/HIC_imputation/imputed_data3.png',
    height = 20, width = 20, unit = 'cm', res = 200)
  imputed.data
dev.off()

#EXTRACT THE IMPUTE VALUES FOR VARIABLES OF INTEREST
imputed.data <- imputed.data$data

#squeeze the imputations to the max and min of what is in the original dataset 
imputed.data$ddd_per_1000_imp <- squeeze(imputed.data$ddd_per_1000_imp, c(min(imputed.data$ddd_per_1000, na.rm = T), max(imputed.data$ddd_per_1000, na.rm = T)))

imputed.data <- data.table(imputed.data)

#3. Clean up imputed data ####
imputed.data <- imputed.data[,.(super_region, region, country,
                                year,
                                ddd_per_1000 = ddd_per_1000_imp,
                                source)]

imputed.data$ddd <- imputed.data$ddd_per_1000*(imputed.data$pop/1000)

# #add on the extra location columns
imputed.data <- merge(imputed.data, extra, by = c('country', 'year'), allow.cartesian = T)

#back calculate the DDDs
imputed.data <- imputed.data[,.(super_region, region, country, iso3, loc_id, GAUL_CODE,
                                year, ddd = ddd_per_1000*(pop/1000), ddd_per_1000, pop, source)]
                             
imputed.data$source <- as.factor(imputed.data$source)
source_col <- unique(imputed.data$source)
cols <- brewer.pal(n = 5, name = "Spectral")
names(cols) <- source_col

#4. Plot out estimates
pdf('combined_sources/HIC_imputation/imputed_high_income_J01.pdf',
    height = 8.3, width = 11.7)

for(i in 1:length(unique(imputed.data$region))){
  subset <- imputed.data[imputed.data$region == unique(imputed.data$region)[i]]
  print(
    ggplot(subset)+
      geom_point(aes(x = year, y = ddd_per_1000, colour = source))+
      facet_wrap(~iso3, nrow = ceiling(sqrt(length(unique(subset$country)))))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_colour_manual('source', values = c('#e41a1c', '#377eb8', '#4daf4a'))+
      ylim(min(imputed.data$ddd_per_1000),max(imputed.data$ddd_per_1000))
  )
} 
dev.off()

# Add China in as the sum of HK and CHN
imputed.data <- imputed.data[imputed.data$loc_id != 6,]
CHN_HGK <- imputed.data[imputed.data$loc_id == 354 | imputed.data$loc_id == 44533, ]
CHN_HGK  <- CHN_HGK [,.(loc_id = 6,
                        super_region = 4,
                        region = 5,
                        country = 'China',
                        iso3 = 'CHN',
                        GAUL_CODE = 147295,
                        ddd = sum(ddd),
                        source = 'IQVIA',
                        pop = sum(pop)),
                     by = 'year'] 

CHN_HGK$ddd_per_1000 <- CHN_HGK$ddd/(CHN_HGK$pop/1000) 
imputed.data <- rbind(imputed.data, CHN_HGK)

# Save the data
write.csv(imputed.data, 'results/imputed_HIC_J01.csv', row.names = F)

#~~~~~#
# END #
#~~~~~#