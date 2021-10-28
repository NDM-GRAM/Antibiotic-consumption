#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Calculate the proportion of antibitoics by select ATC4 class #
# and apply to the modelled estimates                          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(ggplot2)
library(data.table)

setwd('C:/Users/Annie/Documents/GRAM/antibiotic_use/antibiotic_consumption/')

#1. Read in the atc4 file (IQVIA only) ####
mydata <- fread('IQVIA/datasets/cleaned_ddds_2000_2018.csv')

#exclude pre 2000 and tb drugs
mydata <- mydata[year>= 2000,]
mydata <- mydata[ATC3 != 'J04A',]

#exclude J01R and X classes
mydata <- mydata[ATC3 != 'J01X',]
mydata <- mydata[ATC3 != 'J01R',]

# collapse the uncollapsed datasets
# sum across hospital and retail
mydata$total_ddd <- rowSums(mydata[,.(hospital_ddd, retail_ddd,combined_ddd)], na.rm = T)
mydata$total_ddd_per_1000_pop <- rowSums(mydata[,.(hospital_ddd_per_1000_pop, retail_ddd_per_1000_pop, combined_ddd_per_1000_pop)], na.rm = T)

#sum within the country
ATC4_data <- mydata[,.(ATC4_ddd = sum(total_ddd),
                    ATC4_ddd_per_1000 = sum(total_ddd_per_1000_pop)),
                 by = c( "country", "year", "ATC4")] 

#restrict to ATC4 classes of interest
ATC4_data <- ATC4_data[ATC4 == 'J01DH'| #carbapenems
                   ATC4 == 'J01MA'| # fluoroquinolones
                   ATC4 == 'J01CA'| #Broad spec penicillins
                   ATC4 == 'J01DD',] # 3rd gen cephalosporins

#reshape wide
ATC4_data <- dcast(ATC4_data, country+year~ATC4, value.var = 'ATC4_ddd')

#2. Get the input totals and calculate the proportion in each class ####
inputs <- mydata[,.(total_ddd = sum(total_ddd),
                    total_ddd_per_1000 = sum(total_ddd_per_1000_pop)),
                 by = c('super_region', 'region', 'country', 'year', 'pop')]

#correct central america super region
inputs$super_region[inputs$super_region == 130] <- 103

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

mydata <- merge(inputs, ATC4_data, by= c('country', 'year'))
#remove 0's
mydata <- mydata[mydata$total_ddd != 0,]

mydata$J01CA_per_1000 <- mydata$J01CA/(mydata$pop/1000)
mydata$J01DD_per_1000 <- mydata$J01DD/(mydata$pop/1000)
mydata$J01DH_per_1000 <- mydata$J01DH/(mydata$pop/1000)
mydata$J01MA_per_1000 <- mydata$J01MA/(mydata$pop/1000)

#3. Calcaulte the proportions of each antibitoic class for the regional proportions ####
regions <- mydata[,.(reg_J01CA = sum(J01CA, na.rm = T)/sum(total_ddd),
                     reg_J01DD = sum(J01DD, na.rm = T)/sum(total_ddd),
                     reg_J01DH = sum(J01DH, na.rm = T)/sum(total_ddd),
                     reg_J01MA = sum(J01MA, na.rm = T)/sum(total_ddd)),
                  by = c('region', 'year')]

super_regions <- mydata[,.(spr_reg_J01CA = sum(J01CA, na.rm = T)/sum(total_ddd),
                           spr_reg_J01DD = sum(J01DD, na.rm = T)/sum(total_ddd),
                           spr_reg_J01DH = sum(J01DH, na.rm = T)/sum(total_ddd),
                           spr_reg_J01MA = sum(J01MA, na.rm = T)/sum(total_ddd)),
                        by = c('super_region', 'year')]

#calculate proportion of each antibiotic class
mydata <- mydata[,.(country, year, super_region, region,
                    J01CA = J01CA/total_ddd,
                    J01DD = J01DD/total_ddd,
                    J01DH = J01DH/total_ddd,
                    J01MA = J01MA/total_ddd)]

#seperate out the countries of french west Africa and central america and combine hong kong and china
BENIN <- mydata[mydata$country =='FRENCH WEST AFRICA',]
BENIN$country <- 'BENIN'
BURKINA_FASO <- mydata[mydata$country =='FRENCH WEST AFRICA',]
BURKINA_FASO$country <- 'BURKINA FASO'
CAMEROON <- mydata[mydata$country =='FRENCH WEST AFRICA',]
CAMEROON$country <- 'CAMEROON'
CIV <- mydata[mydata$country =='FRENCH WEST AFRICA',]
CIV$country <- "COTE D'IVOIRE"
GUINEA <- mydata[mydata$country =='FRENCH WEST AFRICA',]
GUINEA$country <- 'GUINEA'
SENEGAL <- mydata[mydata$country =='FRENCH WEST AFRICA',]
SENEGAL$country <- 'SENEGAL'
MALI <- mydata[mydata$country =='FRENCH WEST AFRICA',]
MALI$country <- 'MALI'
GABON <- mydata[mydata$country =='FRENCH WEST AFRICA',]
GABON$country <- 'GABON'
CONGO <- mydata[mydata$country =='FRENCH WEST AFRICA',]
CONGO$country <- 'CONGO'
TOGO <- mydata[mydata$country =='FRENCH WEST AFRICA',]
TOGO$country <- 'TOGO'

CRI <- mydata[mydata$country =='CENTRAL AMERICA',]
CRI$country <- 'COSTA RICA'
HONDURAS <- mydata[mydata$country =='CENTRAL AMERICA',]
HONDURAS$country <- 'HONDURAS'
PANAMA <- mydata[mydata$country =='CENTRAL AMERICA',]
PANAMA$country <- 'PANAMA'
NICARAGUA <- mydata[mydata$country =='CENTRAL AMERICA',]
NICARAGUA$country <- 'NICARAGUA'
GUATEMALA <- mydata[mydata$country =='CENTRAL AMERICA',]
GUATEMALA$country <- 'GUATEMALA'
SLV <- mydata[mydata$country =='CENTRAL AMERICA',]
SLV$country <- 'EL SALVADOR'

mydata <- mydata[mydata$country !='FRENCH WEST AFRICA',]
mydata <- mydata[mydata$country !='CENTRAL AMERICA',]

mydata <- rbind(mydata,SLV,GUATEMALA,NICARAGUA,PANAMA,HONDURAS,CRI,
                TOGO,CONGO,MALI,SENEGAL,GUINEA,GABON,CIV,BENIN,CAMEROON,BURKINA_FASO)

rm(SLV,GUATEMALA,NICARAGUA,PANAMA,HONDURAS,CRI,
   TOGO,CONGO,MALI,SENEGAL,GUINEA,GABON,CIV,BENIN,CAMEROON,BURKINA_FASO)



#4. Apply the proportions to the modelled estimates of DDD/1000/day ####
J01 <- fread('results/all_results.csv')
J01 <- J01[,.(super_region = `Super region`, region = Region, country = toupper(Country), year, total_ddd = ddd, total_ddd_per_1000 = ddd_per_1000_per_day, population)]

all_data <- merge(J01, mydata, by= c('country', 'year'), all.x = T, all.y = T)
all_data$super_region.x[is.na(all_data$super_region.x)] <-  all_data$super_region.y[is.na(all_data$super_region.x)]
all_data$super_region.y <- NULL
colnames(all_data)[colnames(all_data)== 'super_region.x'] <-  'super_region'
all_data$region.x[is.na(all_data$region.x)] <-  all_data$region.y[is.na(all_data$region.x)]
all_data$region.y <- NULL
colnames(all_data)[colnames(all_data)== 'region.x'] <-  'region'

#change to factors as for some reason merge isnt working
all_data$super_region <-  as.factor(all_data$super_region)
all_data$region <-  as.factor(all_data$region)
super_regions$super_region <-  as.factor(super_regions$super_region)
regions$region <-  as.factor(regions$region)
levels(all_data$super_region) <-  levels(super_regions$super_region)
levels(regions$region) <- levels(all_data$region)

#match the region and super region proportions and replace where required
all_data <- merge(all_data, regions, by = c('region', 'year'), all.x = T)
all_data <- merge(all_data, super_regions, by = c('super_region', 'year'), all.x = T)

all_data$J01CA[is.na(all_data$J01CA)] <- all_data$reg_J01CA[is.na(all_data$J01CA)]
all_data$J01CA[is.na(all_data$J01CA)] <- all_data$spr_reg_J01CA[is.na(all_data$J01CA)]

all_data$J01DD[is.na(all_data$J01DD)] <- all_data$reg_J01DD[is.na(all_data$J01DD)]
all_data$J01DD[is.na(all_data$J01DD)] <- all_data$spr_reg_J01DD[is.na(all_data$J01DD)]

all_data$J01DH[is.na(all_data$J01DH)] <- all_data$reg_J01DH[is.na(all_data$J01DH)]
all_data$J01DH[is.na(all_data$J01DH)] <- all_data$spr_reg_J01DH[is.na(all_data$J01DH)]

all_data$J01MA[is.na(all_data$J01MA)] <- all_data$reg_J01MA[is.na(all_data$J01MA)]
all_data$J01MA[is.na(all_data$J01MA)] <- all_data$spr_reg_J01MA[is.na(all_data$J01MA)]


all_data$spr_reg_J01CA <-  NULL
all_data$spr_reg_J01DD <-  NULL
all_data$spr_reg_J01DH <-  NULL
all_data$spr_reg_J01MA <-  NULL
all_data$reg_J01CA <-  NULL
all_data$reg_J01DD <-  NULL
all_data$reg_J01DH <-  NULL
all_data$reg_J01MA <-  NULL

all_data$J01CA <- all_data$J01CA*all_data$total_ddd_per_1000
all_data$J01DD <- all_data$J01DD*all_data$total_ddd_per_1000
all_data$J01DH <- all_data$J01DH*all_data$total_ddd_per_1000
all_data$J01MA <- all_data$J01MA*all_data$total_ddd_per_1000

# Reshape long
all_data <- melt(all_data, id.vars = c('super_region', 'region', 'country', 'year', 'population'),
                 measure.vars = c("J01CA", "J01DD", "J01DH", "J01MA"),
                 variable.name = "ATC4", value.name = "ddd_per_1000_per_day")


# all_data$ddd_per_1000 <- all_data$ddd/(all_data$population/1000)
all_data$ddd <- all_data$ddd_per_1000*365*(all_data$population/1000)

write.csv(all_data, 'results/ATC4_estimates.csv', row.names = F)
rm(inputs, J01, regions, super_regions)

#5 .Plot out the consumption of these select antibiotic classes for select regions ####
mydata <- all_data
mydata$ATC4 <-  as.character(mydata$ATC4)
mydata$super_region <-  as.character(mydata$super_region)

mydata$ATC4[mydata$ATC4 == 'J01CA'] <- 'Broad spectrum penicillins'
mydata$ATC4[mydata$ATC4 == 'J01DD'] <- 'Third-generation cephalosporins'
mydata$ATC4[mydata$ATC4 == 'J01DH'] <- 'Carbapenems'
mydata$ATC4[mydata$ATC4 == 'J01MA'] <- 'Fluoroquinolones'

global <- mydata[,.(total_ddd = sum(ddd),
                    total_ddd_per_1000_pop = sum(ddd)/(sum(population)/1000),
                    total_ddd_per_1000_pop_per_day = (sum(ddd)/(sum(population)/1000))/365),
                 by = c('year', 'ATC4')]

super_regions <- mydata[,.(total_ddd = sum(ddd),
                           total_ddd_per_1000_pop = sum(ddd)/(sum(population)/1000),
                           total_ddd_per_1000_pop_per_day = (sum(ddd)/(sum(population)/1000))/365),
                        by = c('super_region', 'year', 'ATC4')]

write.csv(super_regions, 'results/tables/ATC4_spr_reg.csv', row.names = F)

# By select super regions
super_regions <-  data.frame(super_regions)
pdf('results/figures/plots/ATC4/IQVIA_raw_select_spr_reg.pdf',
    height = 5, width = 10)
ggplot(super_regions[super_regions$super_region =="Sub-Saharan Africa"|
                       super_regions$super_region == "North Africa and Middle East" |
                       super_regions$super_region == "South Asia"  |
                       super_regions$super_region=="High-income",])+
  geom_line(aes(x = year, y = total_ddd_per_1000_pop_per_day, group = super_region, colour = super_region), size = 1)+
  facet_wrap(~ATC4, scales = "free_y")+
  scale_colour_manual(values = c("#8c2d04", #HI
                                 "#984ea3", #NAME
                                 "#4daf4a", #South Asia
                                 "#e41a1c"))+ #SSA
  expand_limits(y = 0)+
  theme_bw()+
  labs(y = 'Defined Daily doses per 1000 population per day', x = 'Year', colour = 'GBD Super regions')
dev.off()

#~~~~~#
# END #
#~~~~~#