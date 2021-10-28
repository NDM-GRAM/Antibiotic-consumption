#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Analyse antibiotic consumption for ATC level 3 class #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(ggplot2)
library(data.table)
library(raster)
library(RColorBrewer)
library(viridis)
library(sf)

setwd('C:/Users/Annie/Documents/GRAM/antibiotic_use/antibiotic_consumption/')

#1. Read in and combine all of the atc3 files ####
IQVIA <- fread('IQVIA/datasets/cleaned_ddds_2000_2018.csv')
KEN <- fread('other_sources/KEN/full_KEN_data.csv')
WSM <- fread('other_sources/WSM/WSM_abx_ddd.csv')
ESAC <- fread('ESAC-NET/ATC_DDD_2000_2018.csv')
colnames(ESAC)[1] <-  'super_region'
ESAC <- ESAC[ESAC$total_ddd>0,]

#Capitalise all countries for consistancy
KEN$country <-  toupper(KEN$country)
WSM$country <-  toupper(WSM$country)

#calculate totals for IQVIA
IQVIA$total_ddd <- rowSums(IQVIA[,.(hospital_ddd, retail_ddd,combined_ddd)], na.rm = T)
IQVIA$total_ddd_per_1000_pop <- rowSums(IQVIA[,.(hospital_ddd_per_1000_pop, retail_ddd_per_1000_pop, combined_ddd_per_1000_pop)], na.rm = T)

#get netherlands from IQVIA to use later
NLD <- IQVIA[IQVIA$iso3 == 'NLD',]

# Exlcusions to remove duplicated data 
IQVIA <- IQVIA[IQVIA$country != 'DENMARK']
ESAC <- ESAC[!(ESAC$iso3 %in% IQVIA$iso3),] 

#set the source
IQVIA$source <- 'IQVIA'
KEN$source <- 'KEN'
WSM$source <- 'WSM'
ESAC$source <- 'ESAC'

KEN$ATC3 <-  trim(KEN$ATC3)

# collapse the uncollapsed datasets
IQVIA <- IQVIA[,.(total_ddd = sum(total_ddd),
                  total_ddd_per_1000_pop = sum(total_ddd_per_1000_pop)),
               by = c("super_region", "region", "country", "iso3", "loc_id", "GAUL_CODE", "year", "ATC3", "pop", "source")] 

KEN <- KEN[,.(total_ddd = sum(total_ddd),
              total_ddd_per_1000_pop = sum(total_ddd_per_1000_pop)),
           by = c("super_region", "region", "country", "iso3", "loc_id", "GAUL_CODE", "year", "ATC3", "pop", "source")] 

WSM <- WSM[,.(total_ddd = sum(total_ddd),
              total_ddd_per_1000_pop = sum(total_ddd_per_1000_pop)),
           by = c("super_region", "region", "country", "iso3", "loc_id", "GAUL_CODE", "year", "ATC3", "pop", "source")] 

# Combined datasets
# mydata <- rbind(ESAC, WHO, IQVIA, KEN, WSM)
mydata <- rbind(ESAC, IQVIA, KEN, WSM)
rm(ESAC, IQVIA, KEN, WSM)

#exclude pre 2000
mydata <- mydata[mydata$year>= 2000,]
#exclude J01R and X classes and TB
mydata <- mydata[mydata$ATC3 != 'J04A',]
mydata <- mydata[mydata$ATC3 != 'J01X',]
mydata <- mydata[mydata$ATC3 != 'J01R',]

#limit to required variables
mydata <- mydata[,.(country, year, ATC3, ATC3_ddd = total_ddd, ATC3_ddd_per_1000 = total_ddd_per_1000_pop)]

#reshape wide
mydata <- dcast(mydata, country+year~ATC3, value.var = 'ATC3_ddd')

#2. Get the input totals and calculate the proportion in each class ####
inputs <- fread('combined_sources/J01_DDD_2000_2018_05_10_20.csv')

#remove netherlands and replace with IQVIA as the sum of abx classes is way lower than the total for some years in esac net
inputs <- inputs[inputs$country != 'NETHERLANDS',]
inputs <- inputs[,.(super_region, region, country, year, total_ddd = ddd, total_ddd_per_1000 = ddd_per_1000, population = pop, source)]

#change the codes to names for the super regions and regions
regs <- read.csv('c:/Users/Annie/Documents/GRAM/misc/ihme_location_hierarchy.csv', stringsAsFactors = F)
regs <-  regs[,1:2]
inputs <- merge(inputs, regs, by.x = 'region', by.y = 'location_id')
inputs$region <- NULL
colnames(inputs)[colnames(inputs) == 'location_name'] <- 'region'
inputs <- merge(inputs, regs, by.x = 'super_region', by.y = 'location_id')
inputs$super_region <- NULL
colnames(inputs)[colnames(inputs) == 'location_name'] <- 'super_region'
rm(regs)

NLD <- NLD[,.(super_region = 'High-income',
              region = 'Western Europe',
              total_ddd = sum(total_ddd),
              total_ddd_per_1000 = sum(total_ddd_per_1000_pop)),
           by = c('country', 'year', 'pop')]
colnames(NLD)[colnames(NLD) == 'pop'] <- 'population'

# French west africa and central america are combined in the ATC3 file but have country level estimates for total J01 DDDs
# Calculate ratios of antibiotics in these regions and apply the proportions to the country totals estimates
FWA <-  inputs[inputs$source == 'IQVIA' & (inputs$country == 'BENIN'|
                 inputs$country == 'BURKINA FASO' |
                 inputs$country == 'CAMEROON' |
                 inputs$country == "COTE D'IVOIRE" |
                 inputs$country == 'GUINEA' |
                 inputs$country == 'SENEGAL' |
                 inputs$country == 'TOGO' |
                 inputs$country == 'CONGO' |
                 inputs$country == 'GABON' |
                 inputs$country == 'MALI'),]

FWA <- FWA[,.(region = NA,
              population = sum(population),
              total_ddd = sum(total_ddd)),
           by = c('super_region', 'year')]

FWA$total_ddd_per_1000 <- FWA$total_ddd/(FWA$population/1000)
FWA$country <- 'FRENCH WEST AFRICA'
FWA$region <- NA

CA <-  inputs[inputs$source == 'IQVIA' & (inputs$country == 'COSTA RICA'|
             inputs$country == 'EL SALVADOR' |
             inputs$country == 'GUATEMALA' |
             inputs$country == "HONDURAS" |
             inputs$country == 'NICARAGUA' |
             inputs$country == 'PANAMA'),]

CA <- CA[,.(population = sum(population),
            total_ddd = sum(total_ddd)),
         by = c('super_region','region', 'year')]

CA$total_ddd_per_1000 <- CA$total_ddd/(CA$population/1000)
CA$country <-  'CENTRAL AMERICA'
inputs <- inputs[!(inputs$source == 'IQVIA' & (inputs$country == 'BENIN'|
                                                 inputs$country == 'BURKINA FASO' |
                                                 inputs$country == 'CAMEROON' |
                                                 inputs$country == "COTE D'IVOIRE" |
                                                 inputs$country == 'GUINEA' |
                                                 inputs$country == 'SENEGAL' |
                                                 inputs$country == 'TOGO' |
                                                 inputs$country == 'CONGO' |
                                                 inputs$country == 'GABON' |
                                                 inputs$country == 'MALI' |
                                                 inputs$country == 'COSTA RICA'|
                                                 inputs$country == 'EL SALVADOR' |
                                                 inputs$country == 'GUATEMALA' |
                                                 inputs$country == "HONDURAS" |
                                                 inputs$country == 'NICARAGUA' |
                                                 inputs$country == 'PANAMA')),]

inputs$source <-  NULL
inputs <- rbind(inputs, FWA, CA, NLD)
rm(CA, FWA, NLD)

mydata <- merge(inputs, mydata, by= c('country', 'year'), all.x = F, all.y = T)

#3. Calcaulte the proportions of each antibitoic class for the regional proportions ####
regions <- mydata[,.(reg_J01A = sum(J01A, na.rm = T)/sum(total_ddd),
                     reg_J01B = sum(J01B, na.rm = T)/sum(total_ddd),
                     reg_J01C = sum(J01C, na.rm = T)/sum(total_ddd),
                     reg_J01D = sum(J01D, na.rm = T)/sum(total_ddd),
                     reg_J01E = sum(J01E, na.rm = T)/sum(total_ddd),
                     reg_J01F = sum(J01F, na.rm = T)/sum(total_ddd),
                     reg_J01G = sum(J01G, na.rm = T)/sum(total_ddd),
                     reg_J01M = sum(J01M, na.rm = T)/sum(total_ddd)),
                  by = c('region', 'year')]
                     
  
#make the french west africa both central and western sSA (remove the other western sSA as is just Burkina Faso WHO data)
cssa <- regions[is.na(regions$region),]
cssa$region <- 'Central Sub-Saharan Africa'
wssa <- regions[is.na(regions$region),]
wssa$region <- 'Western Sub-Saharan Africa'
regions <- regions[regions$region!='Western Sub-Saharan Africa',]
regions <- rbind(regions, cssa, wssa)
rm(cssa, wssa)

super_regions <- mydata[,.(spr_reg_J01A = sum(J01A, na.rm = T)/sum(total_ddd),
                           spr_reg_J01B = sum(J01B, na.rm = T)/sum(total_ddd),
                           spr_reg_J01C = sum(J01C, na.rm = T)/sum(total_ddd),
                           spr_reg_J01D = sum(J01D, na.rm = T)/sum(total_ddd),
                           spr_reg_J01E = sum(J01E, na.rm = T)/sum(total_ddd),
                           spr_reg_J01F = sum(J01F, na.rm = T)/sum(total_ddd),
                           spr_reg_J01G = sum(J01G, na.rm = T)/sum(total_ddd),
                           spr_reg_J01M = sum(J01M, na.rm = T)/sum(total_ddd)),
                    by = c('super_region', 'year')]


#calculate proportion of each antibiotic class
mydata <- mydata[,.(country, year, super_region, region,
                    J01A = J01A/total_ddd,
                    J01B = J01B/total_ddd,
                    J01C = J01C/total_ddd,
                    J01D = J01D/total_ddd,
                    J01E = J01E/total_ddd,
                    J01F = J01F/total_ddd,
                    J01G = J01G/total_ddd,
                    J01M = J01M/total_ddd)]



#4. Apply the proportions to the modelled estimates of DDD/1000/day ####
J01 <- fread('results/all_results.csv')
J01 <- J01[,.(super_region = `Super region`, region = Region, country = toupper(Country), year, total_ddd = ddd, total_ddd_per_1000 = ddd_per_1000_per_day, population)]

#remove FWA and central america from the proportions (as these will then get sorted by regions)
mydata <- mydata[!(mydata$country == 'FRENCH WEST AFRICA' | mydata$country == 'CENTRAL AMERICA' | mydata$country == 'HONG KONG'),]

all_data <- merge(J01, mydata, by= c('country', 'year'), all.x = T, all.y = T)
all_data$super_region.x[is.na(all_data$super_region.x)] <-  all_data$super_region.y[is.na(all_data$super_region.x)]
all_data$super_region.y <- NULL
colnames(all_data)[colnames(all_data)== 'super_region.x'] <-  'super_region'
all_data$region.x[is.na(all_data$region.x)] <-  all_data$region.y[is.na(all_data$region.x)]
all_data$region.y <- NULL
colnames(all_data)[colnames(all_data)== 'region.x'] <-  'region'

#change to factors as for some reason merge isnt working
#for some reason there are 2 western Europes and cannot tell the difference
regions$region[regions$region == unique(regions$region)[18]] <- 'Western Europe'
all_data$super_region <-  as.factor(all_data$super_region)
all_data$region <-  as.factor(all_data$region)
super_regions$super_region <-  as.factor(super_regions$super_region)
regions$region <-  as.factor(regions$region)
levels(all_data$super_region) <-  levels(super_regions$super_region)
levels(regions$region) <- levels(all_data$region)

#match the region and super region proportions and replace where required
all_data <- merge(all_data, regions, by = c('region', 'year'), all.x = T, allow.cartesian=TRUE)
all_data <- merge(all_data, super_regions, by = c('super_region', 'year'), all.x = T, allow.cartesian=TRUE)

all_data$J01A[is.na(all_data$J01A)] <- all_data$reg_J01A[is.na(all_data$J01A)]
all_data$J01A[is.na(all_data$J01A)] <- all_data$spr_reg_J01A[is.na(all_data$J01A)]

all_data$J01B[is.na(all_data$J01B)] <- all_data$reg_J01B[is.na(all_data$J01B)]
all_data$J01B[is.na(all_data$J01B)] <- all_data$spr_reg_J01B[is.na(all_data$J01B)]

all_data$J01C[is.na(all_data$J01C)] <- all_data$reg_J01C[is.na(all_data$J01C)]
all_data$J01C[is.na(all_data$J01C)] <- all_data$spr_reg_J01C[is.na(all_data$J01C)]

all_data$J01D[is.na(all_data$J01D)] <- all_data$reg_J01D[is.na(all_data$J01D)]
all_data$J01D[is.na(all_data$J01D)] <- all_data$spr_reg_J01D[is.na(all_data$J01D)]


all_data$J01E[is.na(all_data$J01E)] <- all_data$reg_J01E[is.na(all_data$J01E)]
all_data$J01E[is.na(all_data$J01E)] <- all_data$spr_reg_J01E[is.na(all_data$J01E)]

all_data$J01F[is.na(all_data$J01F)] <- all_data$reg_J01F[is.na(all_data$J01F)]
all_data$J01F[is.na(all_data$J01F)] <- all_data$spr_reg_J01F[is.na(all_data$J01F)]

all_data$J01G[is.na(all_data$J01G)] <- all_data$reg_J01G[is.na(all_data$J01G)]
all_data$J01G[is.na(all_data$J01G)] <- all_data$spr_reg_J01G[is.na(all_data$J01G)]

all_data$J01M[is.na(all_data$J01M)] <- all_data$reg_J01M[is.na(all_data$J01M)]
all_data$J01M[is.na(all_data$J01M)] <- all_data$spr_reg_J01M[is.na(all_data$J01M)]

all_data$spr_reg_J01A <-  NULL
all_data$spr_reg_J01B <-  NULL
all_data$spr_reg_J01C <-  NULL
all_data$spr_reg_J01D <-  NULL
all_data$spr_reg_J01E <-  NULL
all_data$spr_reg_J01F <-  NULL
all_data$spr_reg_J01G <-  NULL
all_data$spr_reg_J01M <-  NULL
all_data$reg_J01A <-  NULL
all_data$reg_J01B <-  NULL
all_data$reg_J01C <-  NULL
all_data$reg_J01D <-  NULL
all_data$reg_J01E <-  NULL
all_data$reg_J01F <-  NULL
all_data$reg_J01G <-  NULL
all_data$reg_J01M <-  NULL

all_data$J01A <- all_data$J01A*all_data$total_ddd
all_data$J01B <- all_data$J01B*all_data$total_ddd
all_data$J01C <- all_data$J01C*all_data$total_ddd
all_data$J01D <- all_data$J01D*all_data$total_ddd
all_data$J01E <- all_data$J01E*all_data$total_ddd
all_data$J01F <- all_data$J01F*all_data$total_ddd
all_data$J01G <- all_data$J01G*all_data$total_ddd
all_data$J01M <- all_data$J01M*all_data$total_ddd

# Reshape long
all_data <- melt(all_data, id.vars = c('super_region', 'region', 'country', 'year', 'population'),
                 measure.vars = c("J01A", "J01B", "J01C", "J01D", "J01E", "J01F", "J01G", "J01M"),
                 variable.name = "ATC3", value.name = "ddd")


all_data$ddd_per_1000 <- all_data$ddd/(all_data$population/1000)
all_data$ddd_per_1000_per_day <- all_data$ddd_per_1000/365

write.csv(all_data, 'results/ATC3_total_DDDs.csv', row.names = F)

#5. Plot maps of DDD/1000/day by ATC 3 class ####
mydata <- all_data
mydata$ATC3 <-  as.character(mydata$ATC3)
mydata$ATC3[mydata$ATC3 == 'J01A'] <- 'J01A: Tetracyclines'
mydata$ATC3[mydata$ATC3 == 'J01B'] <- 'J01B: Amphenicols'
mydata$ATC3[mydata$ATC3 == 'J01C'] <- 'J01C: Penicillins'
mydata$ATC3[mydata$ATC3 == 'J01D'] <- 'J01D: Other beta-lactams'
mydata$ATC3[mydata$ATC3 == 'J01E'] <- 'J01E: Sulfonamides and trimethoprim'
mydata$ATC3[mydata$ATC3 == 'J01F'] <- 'J01F: Macrolides, lincosamides & streptogramins'
mydata$ATC3[mydata$ATC3 == 'J01G'] <- 'J01G: Aminoglycosides'
mydata$ATC3[mydata$ATC3 == 'J01M'] <- 'J01M: Quinolones'

#merge onto shapefile
shp <- st_read('C:/Users/Annie/Documents/GRAM/shapefiles/GBD2020_mapping_final.shp')
shp <- shp[shp$level == 3,]
shp$country <- toupper(shp$loc_name)

#merge data onto the shapefile
my_shp <- merge(shp, mydata, by = 'country') 

#a.For 2018
png('results/figures/maps/DDD_1000_ATC3_2018v2.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = my_shp[my_shp$year == 2018,], aes(fill = ddd_per_1000_per_day),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt', limits = c(0, 26))+
  facet_wrap(~ATC3, ncol = 2)+
  labs(fill='DDD/1000/day')
dev.off()

#b. For 2000
png('results/figures/maps/DDD_1000_ATC3_2000.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = my_shp[my_shp$year == 2000,], aes(fill = ddd_per_1000_per_day),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt', limits = c(0, 26))+
  facet_wrap(~ATC3, ncol = 2)+
  labs(fill='DDD/1000/day')
dev.off()

#c. For 2005
png('results/figures/maps/DDD_1000_ATC3_2005.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = my_shp[my_shp$year == 2005,], aes(fill = ddd_per_1000_per_day),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt', limits = c(0, 26))+
  facet_wrap(~ATC3, ncol = 2)+
  labs(fill='DDD/1000/day')
dev.off()

#d. For 2010
png('results/figures/maps/DDD_1000_ATC3_2010.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = my_shp[my_shp$year == 2010,], aes(fill = ddd_per_1000_per_day),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt', limits = c(0, 26))+
  facet_wrap(~ATC3, ncol = 2)+
  labs(fill='DDD/1000/day')
dev.off()

#e. For 2015
png('results/figures/maps/DDD_1000_ATC3_2015.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = my_shp[my_shp$year == 2015,], aes(fill = ddd_per_1000_per_day),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt', limits = c(0, 26))+
  # scale_fill_continuous()
  facet_wrap(~ATC3, ncol = 2)+
  labs(fill='DDD/1000/day')
dev.off()

#6. Calcaulte the global consumption of each class and plot ####
global <- mydata[,.(total_ddd = sum(ddd),
                    total_ddd_per_1000_pop = sum(ddd)/(sum(population)/1000),
                    total_ddd_per_1000_pop_per_day = (sum(ddd)/(sum(population)/1000))/365),
                by = c('year', 'ATC3')]

png('results/figures/plots/ATC3_total_ddd.png',
    height = 10, width = 20, units = 'cm', res = 300)
ggplot(global)+
  geom_line(aes(x = year, y = total_ddd, group = ATC3, colour = ATC3))+
  theme_bw()+
  labs(y = 'Total DDDs', x = 'Year', colour = NULL)
dev.off()

png('results/figures/plots/ATC3_ddd_per_1000_day.png',
    height = 10, width = 20, units = 'cm', res = 300)
ggplot(global)+
  geom_line(aes(x = year, y = total_ddd_per_1000_pop_per_day, group = ATC3, colour = ATC3))+
  theme_bw()+
  labs(y = 'DDDs per 1000 population per day', x = 'Year', colour = NULL)
dev.off()

#~~~~~#
# END #
#~~~~~#