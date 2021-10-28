#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Cleaning IQVIA 2000-2013 data      #
# Convert SU to kgs then to DDDs     #
# Combined with the 2014-2018 data   #
# Plot out the cleaned data to check #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(data.table)
library(tools)
library(ggplot2)
library(tidyr)

setwd('Z:/AMR/Covariates/antibiotic_use/IQVIA')

#i. Data prep ####
# read in the datasets
mydata <- read.csv('datasets/raw_data/raw_IQVIA_2000_2013-corrected2.csv', stringsAsFactors = F)
colnames(mydata)[colnames(mydata) == 'X2008_NEW'] <- 'X2008'

#add the 2008 old data for the following channels
#(there are 2 entries for 2008, from the old pannel and the new pannel, these countries are only in the old, use the new for everything else)
mydata$X2008[mydata$Country.Panel == 'LATVIA HOSPITAL'] <-  mydata$X2008_OLD[mydata$Country.Panel == 'LATVIA HOSPITAL']
mydata$X2008[mydata$Country.Panel == 'UKRAINE RETAIL'] <-  mydata$X2008_OLD[mydata$Country.Panel == 'UKRAINE RETAIL']
mydata$X2008[mydata$Country.Panel == 'SHANGHAI HOSPITAL'] <-  mydata$X2008_OLD[mydata$Country.Panel == 'SHANGHAI HOSPITAL']
mydata$X2008_OLD <- NULL

#add the Ukraine hospital data from the previous version (Ukraine is not in the updated version from IQVIA)
ukraine <- read.csv('datasets/raw_data/raw_IQVIA_2000_2013.csv', stringsAsFactors = F)
ukraine <- ukraine[ukraine$Country.Panel == 'UKRAINE HOSPITAL',]
colnames(ukraine)[colnames(ukraine) == 'ï..Country'] <- 'Country'
ukraine$Local.Product <-  NA
ukraine$Local.Pack <-  NA

mydata <- rbind(mydata, ukraine)
rm(ukraine)

# Step1: Exclusions####
#a) Restrict to J01 antibiotics
mydata <- mydata[grepl('J1', mydata$ATC4),]
mydata <- mydata[which(mydata$Molecule.List != 'Not Available'),]

#b) drop any rows with all 0's
mydata <- mydata[which(!(mydata$X2000 == 0 &
                           mydata$X2001 == 0 &
                           mydata$X2002 == 0 &
                           mydata$X2003 == 0 &
                           mydata$X2004 == 0 &
                           mydata$X2005 == 0 &
                           mydata$X2006 == 0 &
                           mydata$X2007 == 0 &
                           mydata$X2008 == 0 &
                           mydata$X2009 == 0 &
                           mydata$X2010 == 0 &
                           mydata$X2011 == 0 &
                           mydata$X2012 == 0 &
                           mydata$X2013 == 0)),]

#c) drop any extra topical or vetinary antibiotics
mydata <- mydata[!(grepl("^K", mydata$NFC123)|
                     grepl("^M", mydata$NFC123)|
                     grepl("^V", mydata$NFC123)),]

mydata <- mydata[which(mydata$NFC123!= 'GYV (IMPLANTS)'),]
mydata$Molecule.List <- gsub('#', ' + ', mydata$Molecule.List)

mydata <- mydata[which(mydata$International.Product != 'DICRYSTICIN S'),] #This appears to just be vetinarary
mydata <- mydata[which(mydata$International.Product != 'OXYTET'),] #This appears to just be vetinarary

#Correct the country panels (based on conversation with IQVIA)
mydata$Country.Panel[mydata$Country.Panel =='S. AFRICA TOT MKT'] <- 'S. AFRICA RETAIL'
mydata$Country[mydata$Country == 'DENMARK COMBINED'] <- 'DENMARK'

#south africa hospital >-2010 is just dodgey (conversation with IQVIA says this isnt used by them)
mydata$X2010[mydata$Country.Panel == 'S. AFRICA HOSPITAL'] <- 0
mydata$X2011[mydata$Country.Panel == 'S. AFRICA HOSPITAL'] <- 0
mydata$X2012[mydata$Country.Panel == 'S. AFRICA HOSPITAL'] <- 0
mydata$X2013[mydata$Country.Panel == 'S. AFRICA HOSPITAL'] <- 0

#remove duplicate channels
mydata <- mydata[mydata$Country != 'NETHERLANDS (IMS)',]
mydata <- mydata[mydata$Country != 'DENMARK (IMS)',]

# Step 2: Specific corrections to pack strength etc info ####
#make shanghai part of China
mydata$Country[mydata$Country == 'SHANGHAI'] <-  'CHINA'

#fix the penamecillin values in Hungary (Based on the country specific pack information)
mydata$International.Strength[mydata$Molecule.List == 'PENAMECILLIN' & mydata$International.Pack == 'TAB 500K 100'] <- '342MG'
mydata$International.Strength[mydata$Molecule.List == 'PENAMECILLIN' & mydata$International.Pack == 'TAB 1M 10'] <- '683MG'
mydata$International.Pack[mydata$Molecule.List == 'PENAMECILLIN' & mydata$International.Pack == 'TAB 500K 100'] <- 'TAB 342MG 100'
mydata$International.Pack[mydata$Molecule.List == 'PENAMECILLIN' & mydata$International.Pack == 'TAB 1M 10'] <- 'TAB 683MG 10'

#sisomycing isnt 10KG/1ML suprisingly, assuming its 10MG/1ML
mydata$International.Strength[mydata$Molecule.List == 'SISOMICIN' & mydata$International.Strength == '10KG/1ML'] <- '10MG/1ML'

#SULFAMETHOXAZOLE + TRIMETHOPRIM - assuming the is MG not IU as all other entries for this are
mydata$International.Strength[mydata$Molecule.List == 'SULFAMETHOXAZOLE + TRIMETHOPRIM' & mydata$International.Pack == 'ORL SUSP 200MG/5ML+40M/5ML 1 30ML'] <- '200MG/5ML+40MG/5ML'
mydata$International.Strength[mydata$Molecule.List == 'SULFAMETHOXAZOLE + TRIMETHOPRIM' & mydata$International.Pack == 'ORL SUSP 400MG/5ML+80M 1 50ML'] <- '400MG/5ML+80MG/ML'

# These entries have more strengths than molcules, remove the extra strength here
mydata$International.Strength[mydata$Molecule.List == 'CEFIXIME + CLAVULANIC ACID' & mydata$International.Pack == 'FC TAB 200MG+200MG+120M 6'] <-  '200MG+200MG'
mydata$International.Strength[mydata$Molecule.List == 'CIPROFLOXACIN' & mydata$International.Pack == 'INF VIAL 2M/1ML 1 100ML'] <-  '2MG/ML'
mydata$International.Strength[mydata$Molecule.List == 'CIPROFLOXACIN' & mydata$International.Pack == 'INF BTL 2M/1ML 1 100ML'] <-  '2MG/ML'
mydata$International.Strength[mydata$Molecule.List == 'CLARITHROMYCIN' & mydata$International.Pack == 'DRY VIAL 1G+500K 1 10ML'] <-  '1G'
mydata$International.Strength[mydata$Molecule.List == 'COLISTIN' & mydata$International.Pack == 'DRY VIAL 1M+1M 1'] <-  '1M'
mydata$International.Strength[mydata$Molecule.List == 'LORACARBEF' & mydata$International.Pack == 'ORL DRY SUSP 200IU/5ML 1 40ML'] <- '200MG/5ML'
mydata$International.Strength[mydata$Molecule.List == 'NETILMICIN' & mydata$International.Pack == 'AMP 50K/1ML 1 2ML'] <- '50MG/1ML'
mydata$International.Strength[mydata$Molecule.List == 'NETILMICIN' & mydata$International.Pack == 'IM AMP 100M/1ML 1 2ML'] <- '100MG/1ML'

#Cannot find any mention of a lidocain plus oxytetracycline capsule so assuming is all oxy as no mention of the lidocaine concentraion
mydata$Molecule.List[mydata$Molecule.List == 'LIDOCAINE + OXYTETRACYCLINE' & mydata$International.Pack == 'CAP 250MG 1000'] <-  'OXYTETRACYCLINE'

#remove the lactobacillus weight - this looks wrong, likely million cells, not MG so ignore this
mydata$International.Strength[mydata$Molecule.List == 'CEFDINIR + LACTOBACILLUS ACIDOPHILUS' & mydata$International.Pack == 'FC TAB 150MG+45MG 10'] <- '150MG'
mydata$International.Strength[mydata$Molecule.List == 'CEFDINIR + LACTOBACILLUS ACIDOPHILUS' & mydata$International.Pack == 'FC TAB 300MG+60MG 10'] <- '300MG'

#This states mg/hr assuming mg/ml
mydata$International.Strength[mydata$International.Strength == '2MG/1HR'] <- '2MG/1ML'

#Add the values where they are missing in the denominator, based on the pack info and other entries
mydata$International.Strength[mydata$International.Strength == '2MG/ML'] <- '2MG/1ML'
mydata$International.Strength[mydata$International.Pack == 'ORL SUSP 400MG/5ML+80M 1 50ML'] <- '400MG/5ML+80MG/5ML'

#this says its 500,000,000 IU of penicillin G tablet wich would be a 300g tablet! think this must be out by a factor  of 1000
mydata$International.Strength[mydata$International.Pack == 'TAB 500M 12'] <-  '500K'

#this says its 30,000,000 IU of spiramycin tablet wich would be a 9.4g tablet! think this must be out by a factor  of 10
mydata$International.Strength[mydata$International.Pack == 'FC TAB 30M 50'] <-  '3M'

#rescale the mess that is penicillin G (after conversations with IQVIA)
#a) if <10IU assume this is million, e.g. 2.4 IU == 2,400,000IU
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '2.4IU'] <- '2.4M' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '4IU'] <- '4M' 

#b) if >100 & <1000 assume it is 100,000 eg 200IU = 200,000 IU
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '200IU'] <- '200K' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '200IU/1ML'] <- '200K/1ML' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '300IU'] <- '300K' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '300IU/1ML'] <- '300K/1ML' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '500IU'] <- '500K' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '500IU/1ML'] <- '500K/1ML' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '600IU'] <- '600K' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '800IU'] <- '800K' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '300IU+100IU'] <- '300K+100K' 

#c) think the 1200M and 20M are 1.2M and 2M
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '1200M'] <- '1.2M' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '20M'] <- '2M'

#d) If in the 10's of millions divide my 10 eg. 50M -> 5M
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '5M/1ML'] <- '5M/1ML' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '30M'] <- '3M'
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '24M'] <- '2.4M' 
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '20M'] <- '2M'
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '10M'] <- '1M' 

#e) If in the <10Ks make millions e.g. 1K -> 1M
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '1K'] <- '1M'
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '5K'] <- '5M'
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '6K'] <- '6M'

#f) If in the >=10Ks <100Ks multiply by 10 e.g. 10K -> 100K
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '10K'] <- '100K'
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '12K'] <- '120K'
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '42K/1ML'] <- '420K/1ML'
mydata$International.Strength[mydata$Molecule.List == 'PENICILLIN G' & mydata$International.Strength == '50K'] <- '500K'

#Others related to total pack weight
mydata$International.Strength[mydata$International.Strength=='1%+200MG'] <-  '200MG'

#Polymixin B by XGEN only comes in 500,000 unit vials which is 60mg, use this value
mydata$International.Strength[mydata$International.Product=='POLYMYXIN B   XGEN' & mydata$International.Pack == 'UNK PWD 100% 1'] <-  '60MG'

#Items with multiple antibitoics and only one strength (looked up individually)
#for CEFADROXIL + TRIMETHOPRIM there preperations have 125:25mg = 150mg, assume this
mydata$International.Strength[mydata$Molecule.List == 'CEFADROXIL + TRIMETHOPRIM' & mydata$International.Strength == '150MG'] <- '125MG + 25MG'
#No info of CEFALEXIN + TRIMETHOPRIM being 125mg, but cefalexin alone often is so assign to this
mydata$Molecule.List[mydata$Molecule.List == 'CEFALEXIN + TRIMETHOPRIM' & mydata$International.Strength == '125MG'] <- 'CEFALEXIN'
#ERYTHROMYCIN + OXYTETRACYCLINE = other preps are 125+125, assume is this
mydata$International.Strength[mydata$Molecule.List == 'ERYTHROMYCIN + OXYTETRACYCLINE' & mydata$International.Strength == '250MG'] <- '125MG + 125MG'
#FOSFOMYCIN + TRIMETHOPRIM has multiple single concentrations, split these up based in existing evidence
mydata$International.Strength[mydata$Molecule.List == 'FOSFOMYCIN + TRIMETHOPRIM' & mydata$International.Strength == '75MG'] <- '62.5MG + 12.5MG'
mydata$International.Strength[mydata$Molecule.List == 'FOSFOMYCIN + TRIMETHOPRIM' & mydata$International.Strength == '125MG'] <- '125MG + 25MG'
mydata$International.Strength[mydata$Molecule.List == 'FOSFOMYCIN + TRIMETHOPRIM' & mydata$International.Strength == '150MG'] <- '125MG + 25MG'
mydata$International.Strength[mydata$Molecule.List == 'FOSFOMYCIN + TRIMETHOPRIM' & mydata$International.Strength == '200MG'] <- '166.7MG + 33.3MG'
mydata$International.Strength[mydata$Molecule.List == 'FOSFOMYCIN + TRIMETHOPRIM' & mydata$International.Strength == '100MG'] <- '83.3MG + 16.7MG'
mydata$International.Strength[mydata$Molecule.List == 'FOSFOMYCIN + TRIMETHOPRIM' & mydata$International.Strength == '140MG'] <- '116.7MG + 23.3MG'

#OLEANDOMYCIN + TETRACYCLINE
mydata$International.Strength[mydata$Molecule.List == 'OLEANDOMYCIN + TETRACYCLINE' & mydata$International.Strength == '125MG'] <- '41.8MG+83.5MG'

#AMOXICILLIN#CLAVULANIC ACID (DGJ)
mydata$International.Strength[mydata$Local.Pack == 'SR.D 636.5MG PAED 10.1G'] <- '2.97G/5ML+212MG/5ML'
mydata$International.Pack[mydata$Local.Pack == 'SR.D 636.5MG PAED 10.1G'] <- 'ORL DRY SUSP 2.97G/5ML+212MG/5ML 1 10.1G'

# These entries had the wrong antibiotic assigned to them as the same product name is diferent antibtioics in different countries.
# correct these based on info provided by IQVIA
mydata$Molecule.List[mydata$International.Product == 'ROXIL' & (mydata$Country == 'SAUDI ARABIA' | mydata$Country == 'KUWAIT')] <- "CEFADROXIL"
mydata$Molecule.List[mydata$International.Product == 'AXOCILLIN' & (mydata$Country == 'MALAYSIA' | mydata$Country == 'SINGAPORE')] <- "CLOXACILLIN"
mydata$Molecule.List[mydata$International.Product == 'AMPLIBIOTIC' & mydata$Country == 'C. AMERICA'] <- "ERYTHROMYCIN"
mydata$Molecule.List[mydata$International.Product == 'CLINIMYCIN' & (mydata$Country == 'ALGERIA' | mydata$Country == 'JORDAN')] <- "CLINDAMYCIN"
mydata$Molecule.List[mydata$International.Product == 'CLOXCIN' & mydata$Country == 'PAKISTAN'] <- "AMPICILLIN#CLOXACILLIN"
mydata$Molecule.List[mydata$International.Product == 'ERACILLIN K' & mydata$Country == 'BANGLADESH'] <- "PENICILLIN V"
mydata$Molecule.List[mydata$International.Product == 'ERYCYCLIN' & mydata$Country == 'UKRAINE'] <- "ERYTHROMYCIN#OXYTETRACYCLINE"
mydata$Molecule.List[mydata$International.Product == 'FLUCLOXACILL  BRIG' & mydata$Country == 'CHINA'] <- "AMOXICILLIN#FLUCLOXACILLIN"
mydata$Molecule.List[mydata$International.Product == 'MAXIBIOTIC' & mydata$Country == 'PAKISTAN'] <- "DOXYCYCLINE"
mydata$Molecule.List[mydata$International.Product == 'NOVOCLOX' & mydata$Country == 'PAKISTAN'] <- "AMPICILLIN#CLOXACILLIN"
mydata$Molecule.List[mydata$International.Product == 'ROXIMYCIN' & mydata$Country == 'KOREA'] <- "ROTHIXROMYCIN"
mydata$Molecule.List[mydata$International.Product == 'TRIFAMYCETIN' & mydata$Country == 'INDONESIA'] <- "CHLORAMPHENICOL"
mydata$Molecule.List[mydata$International.Product == 'DURABIOTIC' & mydata$Country == 'ISRAEL'] <- 'PENICILLIN G'

# Step3: Match to ATC5 code ####
ATC5_lookup <- read.csv('lookup_tables/ATC5_lookup_NEW.csv', stringsAsFactors = F)
ATC5_lookup$Molecule.List  <-  NULL
colnames(ATC5_lookup)[1] <- 'Molecule.List'
ATC5_lookup$reason_for_exclusion  <-  NULL
ATC5_lookup[ATC5_lookup == ""] <-  NA

mydata <-  merge(mydata, ATC5_lookup, by = 'Molecule.List', all.x = T)

# remove extra excludes
mydata <- mydata[mydata$EXCLUDE. == 0,]
mydata$EXCLUDE. <- NULL

#check any which dont match
unique(mydata$Molecule.List[is.na(mydata$ATC5)]) #these are all ones which I do not have ATC codes therefore exclude

#drop umatched (as are excluded from this analysis due to non J01, vetinerary or topical antibiotics)
mydata <- mydata[!is.na(mydata$ATC5),]
rm(ATC5_lookup)

# Step 4: Get the strength and concentration for each molecule from the international strength column ####
mydata$International.Strength_original  <- mydata$International.Strength

#split anything with a +
mydata <- separate(mydata, International.Strength, into = c('IS1', 'IS2', 'IS3'), sep = "\\+", remove = FALSE)
mydata$IS1[mydata$IS1 == 'VARI STR'|mydata$IS1 == 'NA INTSTR' | mydata$IS1 == 'COMBI STR'] <- NA

#a. Seperate those with /XXX to get the denominators out
mydata <- separate(mydata, IS1, into = c('strength1', 'denominator1'), sep = "\\/", remove = FALSE)
mydata <- separate(mydata, IS2, into = c('strength2', 'denominator2'), sep = "\\/", remove = FALSE)
mydata <- separate(mydata, IS3, into = c('strength3', 'denominator3'), sep = "\\/", remove = FALSE)

mydata$IS1 <-  NULL
mydata$IS2 <-  NULL
mydata$IS3 <-  NULL

#b. define the units for each value
mydata$units_1 <- NA
mydata$units_2 <- NA
mydata$units_3 <- NA

#If values are in MG with nothing else
mydata$units_1[grepl('MG', mydata$strength1)] <- 'MG'
mydata$units_2[grepl('MG', mydata$strength2)] <- 'MG'
mydata$units_3[grepl('MG', mydata$strength3)] <- 'MG'

mydata$units_1[grepl('G', mydata$strength1) & !grepl('MG', mydata$strength1)] <- 'G'
mydata$units_2[grepl('G', mydata$strength2) & !grepl('MG', mydata$strength2)] <- 'G'
mydata$units_3[grepl('G', mydata$strength3) & !grepl('MG', mydata$strength3)] <- 'G'

mydata$units_1[grepl('Y', mydata$strength1)] <- 'Y'
mydata$units_2[grepl('Y', mydata$strength2)] <- 'Y'
mydata$units_3[grepl('Y', mydata$strength3)] <- 'Y'

mydata$units_1[grepl('K', mydata$strength1)] <- 'K'
mydata$units_2[grepl('K', mydata$strength2)] <- 'K'
mydata$units_3[grepl('K', mydata$strength3)] <- 'K'

mydata$units_1[grepl('IU', mydata$strength1)] <- 'IU'
mydata$units_2[grepl('IU', mydata$strength2)] <- 'IU'
mydata$units_3[grepl('IU', mydata$strength3)] <- 'IU'

mydata$units_1[grepl('%', mydata$strength1)] <- '%'
mydata$units_2[grepl('%', mydata$strength2)] <- '%'
mydata$units_3[grepl('%', mydata$strength3)] <- '%'

mydata$units_1[grepl('M', mydata$strength1)& !grepl('MG', mydata$strength1)& !grepl('ML', mydata$strength1)] <- 'M'
mydata$units_2[grepl('M', mydata$strength2) & !grepl('MG', mydata$strength2)& !grepl('ML', mydata$strength2)] <- 'M'
mydata$units_3[grepl('M', mydata$strength3) & !grepl('MG', mydata$strength3)& !grepl('ML', mydata$strength3)] <- 'M'

mydata$units_1[grepl('ML', mydata$strength1)] <- 'ML'
mydata$units_2[grepl('ML', mydata$strength2)] <- 'ML'
mydata$units_3[grepl('ML', mydata$strength3)] <- 'ML'

mydata$units_1[grepl('DOSE', mydata$strength1)] <- 'DOSE'
mydata$units_2[grepl('DOSE', mydata$strength2)] <- 'DOSE'
mydata$units_3[grepl('DOSE', mydata$strength3)] <- 'DOSE'

mydata$strength1 <- gsub('MG', '', mydata$strength1)
mydata$strength2 <- gsub('MG', '', mydata$strength2)
mydata$strength3 <- gsub('MG', '', mydata$strength3)

mydata$strength1 <- gsub('G', '', mydata$strength1)
mydata$strength2 <- gsub('G', '', mydata$strength2)
mydata$strength3 <- gsub('G', '', mydata$strength3)

mydata$strength1 <- gsub('Y', '', mydata$strength1)
mydata$strength2 <- gsub('Y', '', mydata$strength2)
mydata$strength3 <- gsub('Y', '', mydata$strength3)

mydata$strength1 <- gsub('K', '', mydata$strength1)
mydata$strength2 <- gsub('K', '', mydata$strength2)
mydata$strength3 <- gsub('K', '', mydata$strength3)

mydata$strength1 <- gsub('IU', '', mydata$strength1)
mydata$strength2 <- gsub('IU', '', mydata$strength2)
mydata$strength3 <- gsub('IU', '', mydata$strength3)

mydata$strength1 <- gsub('%', '', mydata$strength1)
mydata$strength2 <- gsub('%', '', mydata$strength2)
mydata$strength3 <- gsub('%', '', mydata$strength3)

mydata$strength1 <- gsub('ML', '', mydata$strength1)
mydata$strength2 <- gsub('ML', '', mydata$strength2)
mydata$strength3 <- gsub('ML', '', mydata$strength3)

mydata$strength1 <- gsub('M', '', mydata$strength1)
mydata$strength2 <- gsub('M', '', mydata$strength2)
mydata$strength3 <- gsub('M', '', mydata$strength3)

mydata$strength1 <- gsub('DOSE', '', mydata$strength1)
mydata$strength2 <- gsub('DOSE', '', mydata$strength2)
mydata$strength3 <- gsub('DOSE', '', mydata$strength3)

#assume that any without symbols are proportions
mydata$units_1[is.na(mydata$units_1) & !is.na(mydata$strength1)] <-  'PROP'
mydata$units_2[is.na(mydata$units_2) & !is.na(mydata$strength2)] <-  'PROP'
mydata$units_3[is.na(mydata$units_3) & !is.na(mydata$strength3)] <-  'PROP'

mydata$strength1 <-  as.numeric(mydata$strength1)
mydata$strength2 <-  as.numeric(mydata$strength2)
mydata$strength3 <-  as.numeric(mydata$strength3)

#all denominators are in MLs or are the number of doses, make doses 1 and get rid of the units
mydata$denominator1[which(mydata$denominator1 == 'DOSE')] <- 1
mydata$denominator2[which(mydata$denominator2 == 'DOSE')] <- 1
mydata$denominator3[which(mydata$denominator3 == 'DOSE')] <- 1
mydata$denominator1 <-  gsub('ML', '', mydata$denominator1)
mydata$denominator2 <-  gsub('ML', '', mydata$denominator2)
mydata$denominator3 <-  gsub('ML', '', mydata$denominator3)
mydata$denominator1 <-  as.numeric(mydata$denominator1)
mydata$denominator2 <-  as.numeric(mydata$denominator2)
mydata$denominator3 <-  as.numeric(mydata$denominator3)

# Step 5: Convert the weights into standard measure (mg or IU) where possible and % to proportoins ####
#i. If anything is listed as >50g assume this is a typo and change to MG
mydata$units_2[which(mydata$strength1>=50 & mydata$units_1 == 'G' & mydata$units_2 == 'G')] <- 'MG'
mydata$units_1[which(mydata$strength1>=50 & mydata$units_1 == 'G')] <- 'MG'
mydata$units_2[which(mydata$strength2>=50 & mydata$units_2 == 'G')] <- 'MG'

#grams to mg = *1000
mydata$strength1[which(mydata$units_1 == 'G')] <- mydata$strength1[which(mydata$units_1 == 'G')]*1000
mydata$strength2[which(mydata$units_2 == 'G')] <- mydata$strength2[which(mydata$units_2 == 'G')]*1000
mydata$strength3[which(mydata$units_3 == 'G')] <- mydata$strength3[which(mydata$units_3 == 'G')]*1000

#micrograms to mg = *0.001
mydata$strength1[which(mydata$units_1 == 'Y')] <- mydata$strength1[which(mydata$units_1 == 'Y')]*0.001
mydata$strength2[which(mydata$units_2 == 'Y')] <- mydata$strength2[which(mydata$units_2 == 'Y')]*0.001
mydata$strength3[which(mydata$units_3 == 'Y')] <- mydata$strength3[which(mydata$units_3 == 'Y')]*0.001

mydata$units_1[which(mydata$units_1 == 'G' | mydata$units_1 == 'Y')] <- 'MG'
mydata$units_2[which(mydata$units_2 == 'G' | mydata$units_2 == 'Y')] <- 'MG'
mydata$units_3[which(mydata$units_3 == 'G' | mydata$units_3 == 'Y')] <- 'MG'

# K TO IU
mydata$strength1[which(mydata$units_1 == 'K')] <- mydata$strength1[which(mydata$units_1 == 'K')]*1000
mydata$strength2[which(mydata$units_2 == 'K')] <- mydata$strength2[which(mydata$units_2 == 'K')]*1000
mydata$strength3[which(mydata$units_3 == 'K')] <- mydata$strength3[which(mydata$units_3 == 'K')]*1000

#M to IU
mydata$strength1[which(mydata$units_1 == 'M')] <- mydata$strength1[which(mydata$units_1 == 'M')]*1000000
mydata$strength2[which(mydata$units_2 == 'M')] <- mydata$strength2[which(mydata$units_2 == 'M')]*1000000
mydata$strength3[which(mydata$units_3 == 'M')] <- mydata$strength3[which(mydata$units_3 == 'M')]*1000000

mydata$units_1[which(mydata$units_1 == 'M' | mydata$units_1 == 'K')] <- 'IU'
mydata$units_2[which(mydata$units_2 == 'M' | mydata$units_2 == 'K')] <- 'IU'
mydata$units_3[which(mydata$units_3 == 'M' | mydata$units_3 == 'K')] <- 'IU'

#% to proportion
mydata$strength1[which(mydata$units_1 == '%')] <- mydata$strength1[which(mydata$units_1 == '%')]*0.01
mydata$strength2[which(mydata$units_2 == '%')] <- mydata$strength2[which(mydata$units_2 == '%')]*0.01
mydata$strength3[which(mydata$units_3 == '%')] <- mydata$strength3[which(mydata$units_3 == '%')]*0.01
mydata$units_1[which(mydata$units_1 == '%')] <- 'PROP'
mydata$units_2[which(mydata$units_2 == '%')] <- 'PROP'
mydata$units_3[which(mydata$units_3 == '%')] <- 'PROP'

# Step 6: Convert the IUs to MG####
#if lactobacillus (or other probiotics) stated then assume this is of negligable weight
mydata$strength2[mydata$Molecule.List == 'AMOXICILLIN + LACTOBACILLUS ACIDOPHILUS'] <-  NA
mydata$units_2[mydata$Molecule.List == 'AMOXICILLIN + LACTOBACILLUS ACIDOPHILUS'] <-  NA

mydata$strength3[mydata$Molecule.List == 'AMOXICILLIN + CLAVULANIC ACID + LACTOBACILLUS ACIDOPHILUS'] <-  NA
mydata$units_3[mydata$Molecule.List == 'AMOXICILLIN + CLAVULANIC ACID + LACTOBACILLUS ACIDOPHILUS'] <-  NA

mydata$strength3[mydata$Molecule.List == 'AMOXICILLIN + DICLOXACILLIN + LACTOBACILLUS ACIDOPHILUS'] <-  NA
mydata$units_3[mydata$Molecule.List == 'AMOXICILLIN + DICLOXACILLIN + LACTOBACILLUS ACIDOPHILUS'] <-  NA

mydata$strength2[mydata$Molecule.List == 'CEFADROXIL + LACTOBACILLUS ACIDOPHILUS'] <-  NA
mydata$units_2[mydata$Molecule.List == 'CEFADROXIL + LACTOBACILLUS ACIDOPHILUS'] <-  NA

mydata$strength3[mydata$Molecule.List == 'CEFIXIME + CLOXACILLIN + LACTOBACILLUS ACIDOPHILUS'] <-  NA
mydata$units_3[mydata$Molecule.List == 'CEFIXIME + CLOXACILLIN + LACTOBACILLUS ACIDOPHILUS'] <-  NA

mydata$strength3[mydata$Molecule.List == 'CEFIXIME + DICLOXACILLIN + LACTOBACILLUS ACIDOPHILUS'] <-  NA
mydata$units_3[mydata$Molecule.List == 'CEFIXIME + DICLOXACILLIN + LACTOBACILLUS ACIDOPHILUS'] <-  NA

mydata$strength2[mydata$Molecule.List == 'CEFIXIME + LACTOBACILLUS ACIDOPHILUS'] <-  NA
mydata$units_2[mydata$Molecule.List == 'CEFIXIME + LACTOBACILLUS ACIDOPHILUS'] <-  NA

mydata$strength3[mydata$Molecule.List == 'CEFPODOXIME PROXETIL + DICLOXACILLIN + LACTOBACILLUS ACIDOPHILUS'] <-  NA
mydata$units_3[mydata$Molecule.List == 'CEFPODOXIME PROXETIL + DICLOXACILLIN + LACTOBACILLUS ACIDOPHILUS'] <-  NA

mydata$strength2[mydata$Molecule.List == 'CEFPODOXIME PROXETIL + LACTOBACILLUS ACIDOPHILUS'] <-  NA
mydata$units_2[mydata$Molecule.List == 'CEFPODOXIME PROXETIL + LACTOBACILLUS ACIDOPHILUS'] <-  NA

mydata$strength2[mydata$Molecule.List == 'DOXYCYCLINE + LACTOBACILLUS ACIDOPHILUS'] <-  NA
mydata$units_2[mydata$Molecule.List == 'DOXYCYCLINE + LACTOBACILLUS ACIDOPHILUS'] <-  NA

#(can find no mention of DUCIDAL containing this probiotic, especially not 50mg of it!)
mydata$strength3[mydata$Molecule.List == 'AMOXICILLIN + DICLOXACILLIN + SACCHAROMYCES BOULARDII'] <-  NA
mydata$units_3[mydata$Molecule.List == 'AMOXICILLIN + DICLOXACILLIN + SACCHAROMYCES BOULARDII'] <-  NA

# If proteases/enzymes assume negligable weight
mydata$strength2[mydata$Molecule.List == 'AMPICILLIN + CHYMOTRYPSIN + TRYPSIN'] <-  NA
mydata$units_2[mydata$Molecule.List == 'AMPICILLIN + CHYMOTRYPSIN + TRYPSIN'] <-  NA
mydata$strength3[mydata$Molecule.List == 'AMPICILLIN + CHYMOTRYPSIN + TRYPSIN'] <-  NA
mydata$units_3[mydata$Molecule.List == 'AMPICILLIN + CHYMOTRYPSIN + TRYPSIN'] <-  NA

mydata$strength2[mydata$Molecule.List == 'AMPICILLIN + PROTEASE'] <-  NA
mydata$units_2[mydata$Molecule.List == 'AMPICILLIN + PROTEASE'] <-  NA

mydata$strength3[mydata$Molecule.List == 'AMOXICILLIN + CLOXACILLIN + SERRAPEPTASE'] <-  NA
mydata$units_3[mydata$Molecule.List == 'AMOXICILLIN + CLOXACILLIN + SERRAPEPTASE'] <-  NA

mydata$strength1[mydata$Molecule.List == 'CHYMOTRYPSIN + DOXYCYCLINE + TRYPSIN'] <-  NA
mydata$units_1[mydata$Molecule.List == 'CHYMOTRYPSIN + DOXYCYCLINE + TRYPSIN'] <-  NA
mydata$strength3[mydata$Molecule.List == 'CHYMOTRYPSIN + DOXYCYCLINE + TRYPSIN'] <-  NA
mydata$units_3[mydata$Molecule.List == 'CHYMOTRYPSIN + DOXYCYCLINE + TRYPSIN'] <-  NA

mydata$strength1[mydata$Molecule.List == 'CHYMOTRYPSIN + TETRACYCLINE + TRYPSIN'] <-  NA
mydata$units_1[mydata$Molecule.List == 'CHYMOTRYPSIN + TETRACYCLINE + TRYPSIN'] <-  NA
mydata$strength3[mydata$Molecule.List == 'CHYMOTRYPSIN + TETRACYCLINE + TRYPSIN'] <-  NA
mydata$units_3[mydata$Molecule.List == 'CHYMOTRYPSIN + TETRACYCLINE + TRYPSIN'] <-  NA

mydata$strength1[mydata$Molecule.List == 'PROTEASE + TETRACYCLINE'] <-  NA
mydata$units_1[mydata$Molecule.List == 'PROTEASE + TETRACYCLINE'] <-  NA

# Now use values obtained to convert IUs to MG where applicable (values looked up are MG/1000IU)
# values were obtained from https://mypharmatools.com/othertools/iu and https://www.etoolsage.com/converter/IU_Converter.asp
#AMOXICILLIN + NYSTATIN = 0.175mg/1,000IU
mydata$strength2[mydata$Molecule.List=='AMOXICILLIN + NYSTATIN' & mydata$units_2 == 'IU'] <- (mydata$strength2[mydata$Molecule.List=='AMOXICILLIN + NYSTATIN' & mydata$units_2 == 'IU']/1000)*0.175
mydata$units_2[mydata$Molecule.List=='AMOXICILLIN + NYSTATIN' & mydata$units_2 == 'IU'] <- 'MG'

#DEMECLOCYCLINE + STREPTODORNASE + STREPTOKINASE	2.7	2.09
mydata$strength2[mydata$Molecule.List=='DEMECLOCYCLINE + STREPTODORNASE + STREPTOKINASE' & mydata$units_2 == 'IU'] <- (mydata$strength2[mydata$Molecule.List=='DEMECLOCYCLINE + STREPTODORNASE + STREPTOKINASE' & mydata$units_2 == 'IU']/1000)*2.7
mydata$strength3[mydata$Molecule.List=='DEMECLOCYCLINE + STREPTODORNASE + STREPTOKINASE' & mydata$units_3 == 'IU'] <- (mydata$strength3[mydata$Molecule.List=='DEMECLOCYCLINE + STREPTODORNASE + STREPTOKINASE' & mydata$units_3 == 'IU']/1000)*2.09
mydata$units_2[mydata$Molecule.List=='DEMECLOCYCLINE + STREPTODORNASE + STREPTOKINASE' & mydata$units_2 == 'IU'] <- 'MG'
mydata$units_3[mydata$Molecule.List=='DEMECLOCYCLINE + STREPTODORNASE + STREPTOKINASE' & mydata$units_3 == 'IU'] <- 'MG'

#GLUCOSE + PENICILLIN G 0.6
mydata$strength2[mydata$Molecule.List=='GLUCOSE + PENICILLIN G' & mydata$units_2 == 'IU'] <- (mydata$strength2[mydata$Molecule.List=='GLUCOSE + PENICILLIN G' & mydata$units_2 == 'IU']/1000)*0.6
mydata$units_2[mydata$Molecule.List=='GLUCOSE + PENICILLIN G' & mydata$units_2 == 'IU'] <-  'MG'

#METRONIDAZOLE + SPIRAMYCIN 0.313
mydata$strength2[which(mydata$Molecule.List=='METRONIDAZOLE + SPIRAMYCIN' & mydata$units_2 == 'IU')] <- mydata$strength2[which(mydata$Molecule.List=='METRONIDAZOLE + SPIRAMYCIN' & mydata$units_2 == 'IU')]/1000*0.313
mydata$units_2[mydata$Molecule.List=='METRONIDAZOLE + SPIRAMYCIN' & mydata$units_2 == 'IU'] <-  'MG'

# PENICILLIN G + TOLYCAINE	0.6
mydata$strength1[which(mydata$Molecule.List=='PENICILLIN G + TOLYCAINE' & mydata$units_1 == 'IU')] <- 
  (mydata$strength1[which(mydata$Molecule.List=='PENICILLIN G + TOLYCAINE' & mydata$units_1 == 'IU')]/1000)*0.6
mydata$units_1[mydata$Molecule.List=='PENICILLIN G + TOLYCAINE' & mydata$units_1 == 'IU'] <-  'MG'

# NYSTATIN + TETRACYCLINE	0.175
mydata$strength1[which(mydata$Molecule.List=='NYSTATIN + TETRACYCLINE' & mydata$units_1 == 'IU')] <- 
  (mydata$strength1[which(mydata$Molecule.List=='NYSTATIN + TETRACYCLINE' & mydata$units_1 == 'IU')]/1000)*0.175
mydata$units_1[mydata$Molecule.List=='NYSTATIN + TETRACYCLINE' & mydata$units_1 == 'IU'] <-  'MG'

#KANAMYCIN + PENICILLIN G this actually penicillin G + procaine 0.991
mydata$strength2[which(mydata$Molecule.List=='KANAMYCIN + PENICILLIN G' & mydata$units_2 == 'IU')] <- 
  ((mydata$strength2[which(mydata$Molecule.List=='KANAMYCIN + PENICILLIN G' & mydata$units_2 == 'IU')]+mydata$strength3[which(mydata$Molecule.List=='KANAMYCIN + PENICILLIN G' & mydata$units_3 == 'IU')])/1000)*0.991
mydata$units_2[mydata$Molecule.List=='KANAMYCIN + PENICILLIN G' & mydata$units_2 == 'IU'] <-  'MG'
mydata$units_3[mydata$Molecule.List=='KANAMYCIN + PENICILLIN G' & mydata$units_3 == 'IU'] <-  NA
mydata$strength3[mydata$Molecule.List=='KANAMYCIN + PENICILLIN G'] <-  NA

#PENICILLIN G + STREPTOMYCIN	- this is actually penicillinG + procaine + streptomycin therefore 0.991	1.274 sum the first 2 to make pen+procaine
mydata$strength1[which(mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN' & mydata$units_1 == 'IU')] <-
  ((mydata$strength1[which(mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN'& mydata$units_1 == 'IU')]+mydata$strength2[which(mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN'& mydata$units_1 == 'IU')])/1000)*0.991
mydata$units_1[mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN'] <-  'MG'

mydata$strength2[which(mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN')] <- mydata$strength3[which(mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN')]
mydata$strength3[mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN'] <- NA

mydata$units_2[which(mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN')] <- mydata$units_3[which(mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN')]
mydata$units_3[mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN'] <- NA

mydata$strength2[which(mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN' & mydata$units_2 == 'IU')] <- (mydata$strength2[which(mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN'& mydata$units_2 == 'IU')]/1000)*1.274
mydata$units_2[mydata$Molecule.List=='PENICILLIN G + STREPTOMYCIN' & mydata$units_2 == 'IU'] <-  'MG'

# AMPICILLIN	0.625
mydata$strength1[which(mydata$Molecule.List == 'AMPICILLIN' & mydata$units_1 == 'IU')] <- (mydata$strength1[which(mydata$Molecule.List == 'AMPICILLIN' & mydata$units_1 == 'IU')]/1000)*0.625
mydata$units_1[mydata$Molecule.List == 'AMPICILLIN' & mydata$units_1 == 'IU'] <-  'MG'

# COLISTIN	0.033
mydata$strength1[which(mydata$Molecule.List == 'COLISTIN' & mydata$units_1 == 'IU')] <- 
  (mydata$strength1[which(mydata$Molecule.List == 'COLISTIN' & mydata$units_1 == 'IU')]/1000)*0.033
mydata$units_1[mydata$Molecule.List == 'COLISTIN' & mydata$units_1 == 'IU'] <-  'MG'

# ERYTHROMYCIN	1.087
mydata$strength1[which(mydata$Molecule.List == 'ERYTHROMYCIN' & mydata$units_1 == 'IU')] <- 
  (mydata$strength1[which(mydata$Molecule.List == 'ERYTHROMYCIN' & mydata$units_1 == 'IU')]/1000)*1.087
mydata$units_1[mydata$Molecule.List == 'ERYTHROMYCIN' & mydata$units_1 == 'IU'] <-  'MG'

# GENTAMICIN	1.613
mydata$strength1[which(mydata$Molecule.List == 'GENTAMICIN' & mydata$units_1 == 'IU')] <- 
  (mydata$strength1[which(mydata$Molecule.List == 'GENTAMICIN' & mydata$units_1 == 'IU')]/1000)*1.613
mydata$units_1[mydata$Molecule.List == 'GENTAMICIN' & mydata$units_1 == 'IU'] <-  'MG'

# PENICILLIN G	0.6
mydata$strength1[which(mydata$Molecule.List == 'PENICILLIN G' & mydata$units_1 == 'IU')] <- 
  (mydata$strength1[which(mydata$Molecule.List == 'PENICILLIN G' & mydata$units_1 == 'IU')]/1000)*0.6
mydata$units_1[mydata$Molecule.List == 'PENICILLIN G' & mydata$units_1 == 'IU'] <-  'MG'

mydata$strength2[which(mydata$Molecule.List == 'PENICILLIN G' & mydata$units_2 == 'IU')] <- 
  (mydata$strength2[which(mydata$Molecule.List == 'PENICILLIN G' & mydata$units_2 == 'IU')]/1000)*0.6
mydata$units_2[mydata$Molecule.List == 'PENICILLIN G' & mydata$units_2 == 'IU'] <-  'MG'

mydata$strength3[which(mydata$Molecule.List == 'PENICILLIN G' & mydata$units_3 == 'IU')] <- 
  (mydata$strength3[which(mydata$Molecule.List == 'PENICILLIN G' & mydata$units_3 == 'IU')]/1000)*0.6
mydata$units_3[mydata$Molecule.List == 'PENICILLIN G' & mydata$units_3 == 'IU'] <-  'MG'

# PENICILLIN V	0.59
mydata$strength1[which(mydata$Molecule.List == 'PENICILLIN V' & mydata$units_1 == 'IU')] <- 
  (mydata$strength1[which(mydata$Molecule.List == 'PENICILLIN V' & mydata$units_1 == 'IU')]/1000)*0.59
mydata$units_1[mydata$Molecule.List == 'PENICILLIN V' & mydata$units_1 == 'IU'] <-  'MG'

# POLYMYXIN B	0.119
mydata$strength1[which(mydata$Molecule.List == 'POLYMYXIN B' & mydata$units_1 == 'IU')] <- 
  (mydata$strength1[which(mydata$Molecule.List == 'POLYMYXIN B' & mydata$units_1 == 'IU')]/1000)*0.119
mydata$units_1[mydata$Molecule.List == 'POLYMYXIN B' & mydata$units_1 == 'IU'] <-  'MG'

# SPIRAMYCIN	0.313
mydata$strength1[which(mydata$Molecule.List == 'SPIRAMYCIN' & mydata$units_1 == 'IU')] <- 
  (mydata$strength1[which(mydata$Molecule.List == 'SPIRAMYCIN' & mydata$units_1 == 'IU')]/1000)*0.313
mydata$units_1[mydata$Molecule.List == 'SPIRAMYCIN' & mydata$units_1 == 'IU'] <-  'MG'

# PENICILLIN G + PROCAINE	0.991
mydata$strength1[which(mydata$Molecule.List == 'PENICILLIN G + PROCAINE' & mydata$units_1 == 'IU')] <- 
 ((mydata$strength1[which(mydata$Molecule.List == 'PENICILLIN G + PROCAINE'& mydata$units_1 == 'IU')]+mydata$strength2[which(mydata$Molecule.List == 'PENICILLIN G + PROCAINE'& mydata$units_1 == 'IU')])/1000)*0.991
mydata$units_1[mydata$Molecule.List == 'PENICILLIN G + PROCAINE' & mydata$units_1 == 'IU'] <-  'MG'

mydata$strength2[mydata$Molecule.List == 'PENICILLIN G + PROCAINE'] <-  NA
mydata$units_2[mydata$Molecule.List == 'PENICILLIN G + PROCAINE'] <-  NA

#CEFIXIME + CLAVULANIC ACID - remove the extra value, assume a probiotic
mydata$strength3[mydata$Molecule.List == 'CEFIXIME + CLAVULANIC ACID' & !is.na(mydata$strength3)] <- NA
mydata$units_3[mydata$Molecule.List == 'CEFIXIME + CLAVULANIC ACID' & !is.na(mydata$units_3)] <- NA

# CEFAMANDOLE NAFATE + LIDOCAINE some dont have values for lidocain, assume same os others
mydata$strength2[mydata$Molecule.List=='CEFAMANDOLE NAFATE + LIDOCAINE'] <- 15
mydata$units_2[mydata$Molecule.List=='CEFAMANDOLE NAFATE + LIDOCAINE'] <- 'MG'

## Another correction
#The weights in AMPICILLIN + LIDOCAINE + SULBACTAM are messed up. Sometimes the lidocaine is second, sometimes third.
# Some dont have lidocaine weights, if this is the case then ignore the lidocaine and assume the weights are ampicillin and sulbactam
# if all weights sum the ampicillin and sulbactam. Assume lidocaine is the smallest weight
mydata$ATC5_2[mydata$Molecule.List == 'AMPICILLIN + LIDOCAINE + SULBACTAM' & is.na(mydata$strength3)] <- NA
mydata$Molecule.List[mydata$Molecule.List == 'AMPICILLIN + LIDOCAINE + SULBACTAM' & is.na(mydata$strength3)] <- 'AMPICILLIN + SULBACTAM'

mydata$strength1[mydata$Molecule.List == 'AMPICILLIN + LIDOCAINE + SULBACTAM' & !is.na(mydata$strength3)] <- mydata$strength1[mydata$Molecule.List == 'AMPICILLIN + LIDOCAINE + SULBACTAM' & !is.na(mydata$strength3)]+mydata$strength3[mydata$Molecule.List == 'AMPICILLIN + LIDOCAINE + SULBACTAM' & !is.na(mydata$strength3)] 
mydata$strength3[mydata$Molecule.List == 'AMPICILLIN + LIDOCAINE + SULBACTAM' & !is.na(mydata$strength3)] <- NA

#drop lomefloxacin rows with IU as cannot figure about the antibitoic amount and is only a small amount
mydata <-  mydata[which(mydata$units_1 != 'IU'),]

# Step 7: Get the total pack weight/volume from international pack ####
library(stringr)
mydata$total.weight <- word(mydata$International.Pack,-1)
mydata$total.weight_units <-  NA

mydata$total.weight_units[str_sub(mydata$total.weight, -1) == 'G' & str_sub(mydata$total.weight, -2) != 'MG'] <- 'G'
mydata$total.weight_units[str_sub(mydata$total.weight, -1) == 'L' & str_sub(mydata$total.weight, -2) != 'ML'] <- 'L'
mydata$total.weight_units[str_sub(mydata$total.weight, -2) == 'MG'] <- 'MG'
mydata$total.weight_units[str_sub(mydata$total.weight, -2) == 'ML'] <- 'ML'
mydata$total.weight_units[str_sub(mydata$total.weight, -2) == 'KG'] <- 'KG'

mydata$total.weight[is.na(mydata$total.weight_units)] <-  NA
mydata$total.weight <- gsub('MG', '', mydata$total.weight)
mydata$total.weight <- gsub('ML', '', mydata$total.weight)
mydata$total.weight <- gsub('KG', '', mydata$total.weight)
mydata$total.weight <- gsub('G', '', mydata$total.weight)
mydata$total.weight <- gsub('L', '', mydata$total.weight)
mydata$total.weight <-  as.numeric(mydata$total.weight)

#convert total weights into MG or ML
mydata$total.weight[which(mydata$total.weight_units == 'G')] <- 
  mydata$total.weight[which(mydata$total.weight_units == 'G')]*1000

mydata$total.weight[which(mydata$total.weight_units == 'KG')] <- 
  mydata$total.weight[which(mydata$total.weight_units == 'KG')]*1000000

mydata$total.weight[which(mydata$total.weight_units == 'L')] <- 
  mydata$total.weight[which(mydata$total.weight_units == 'L')]*1000

mydata$total.weight_units[mydata$total.weight_units == 'KG' | mydata$total.weight_units == 'G'] <- 'MG'
mydata$total.weight_units[mydata$total.weight_units == 'L'] <-  'ML'

mydata$total.weight <-  as.numeric(mydata$total.weight)

# Step 8: Calculate the strength if it's in a proportion ####
#drop the entries which are a proportion but have no pack weight as this is not calculable
# check <- mydata[which(mydata$units_1 == 'PROP' & is.na(mydata$total.weight)),]
mydata <- mydata[!(mydata$units_1 == 'PROP' & is.na(mydata$total.weight)),]
mydata$strength1[which(mydata$units_1 == 'PROP' & mydata$total.weight_units == 'ML')] <- mydata$strength1[which(mydata$units_1 == 'PROP' & mydata$total.weight_units == 'ML')]*mydata$total.weight[which(mydata$units_1 == 'PROP' & mydata$total.weight_units == 'ML')] * 1000 
mydata$strength1[which(mydata$units_1 == 'PROP' & mydata$total.weight_units == 'MG')] <- mydata$strength1[which(mydata$units_1 == 'PROP' & mydata$total.weight_units == 'MG')]*mydata$total.weight[which(mydata$units_1 == 'PROP' & mydata$total.weight_units == 'MG')] 
mydata$strength2[which(mydata$units_2 == 'PROP' & mydata$total.weight_units == 'ML')] <- mydata$strength2[which(mydata$units_2 == 'PROP' & mydata$total.weight_units == 'ML')]*mydata$total.weight[which(mydata$units_2 == 'PROP' & mydata$total.weight_units == 'ML')] * 1000 
mydata$strength2[which(mydata$units_2 == 'PROP' & mydata$total.weight_units == 'MG')] <- mydata$strength2[which(mydata$units_2 == 'PROP' & mydata$total.weight_units == 'MG')]*mydata$total.weight[which(mydata$units_2 == 'PROP' & mydata$total.weight_units == 'MG')] 

#all strengths are now in MG
mydata$units_1 <- NULL
mydata$units_2 <- NULL
mydata$units_3 <- NULL

# Step 9: Sum the weights for the items with one ATC code but muliple weights ####
# extra_vals <- mydata[which(is.na(mydata$ATC5_2) & !is.na(mydata$strength2)),]
mydata$strength1[which(is.na(mydata$ATC5_2) & !is.na(mydata$strength2) & !is.na(mydata$strength3))] <- 
  mydata$strength1[which(is.na(mydata$ATC5_2) & !is.na(mydata$strength2) & !is.na(mydata$strength3))]+
  mydata$strength2[which(is.na(mydata$ATC5_2) & !is.na(mydata$strength2) & !is.na(mydata$strength3))]+
  mydata$strength3[which(is.na(mydata$ATC5_2) & !is.na(mydata$strength2) & !is.na(mydata$strength3))]
  
mydata$strength1[which(is.na(mydata$ATC5_2) & !is.na(mydata$strength2) & is.na(mydata$strength3))] <- 
  mydata$strength1[which(is.na(mydata$ATC5_2) & !is.na(mydata$strength2) & is.na(mydata$strength3))]+
  mydata$strength2[which(is.na(mydata$ATC5_2) & !is.na(mydata$strength2) & is.na(mydata$strength3))]

mydata$strength2[which(is.na(mydata$ATC5_2))] <- NA
mydata$strength3[which(is.na(mydata$ATC5_3) & is.na(mydata$ATC5_2))] <- NA

#identify those with 2 ATC codes but three values - needs to be done on a cases by case basis
# extra_vals <- mydata[which(is.na(mydata$ATC5_3) & !is.na(mydata$strength3)),]
# table(extra_vals$Molecule.List)

#AMOXICILLIN + CLAVULANIC ACID + NIMESULIDE
mydata$strength1[which(mydata$Molecule.List == 'AMOXICILLIN + CLAVULANIC ACID + NIMESULIDE')] <- 
  mydata$strength1[which(mydata$Molecule.List == 'AMOXICILLIN + CLAVULANIC ACID + NIMESULIDE')]+
  mydata$strength2[which(mydata$Molecule.List == 'AMOXICILLIN + CLAVULANIC ACID + NIMESULIDE')]

mydata$strength2[which(mydata$Molecule.List == 'AMOXICILLIN + CLAVULANIC ACID + NIMESULIDE')] <- 
  mydata$strength3[which(mydata$Molecule.List == 'AMOXICILLIN + CLAVULANIC ACID + NIMESULIDE')]
  
mydata$strength3[which(mydata$Molecule.List == 'AMOXICILLIN + CLAVULANIC ACID + NIMESULIDE')] <- NA
  
#AMPICILLIN + SULFAMETHOXAZOLE + TRIMETHOPRIM
mydata$strength2[which(mydata$Molecule.List == 'AMPICILLIN + SULFAMETHOXAZOLE + TRIMETHOPRIM')] <- 
  mydata$strength2[which(mydata$Molecule.List == 'AMPICILLIN + SULFAMETHOXAZOLE + TRIMETHOPRIM')]+
  mydata$strength3[which(mydata$Molecule.List == 'AMPICILLIN + SULFAMETHOXAZOLE + TRIMETHOPRIM')]

mydata$strength3[which(mydata$Molecule.List == 'AMPICILLIN + SULFAMETHOXAZOLE + TRIMETHOPRIM')] <- NA

#BROMHEXINE + SULFAMETHOXAZOLE + TRIMETHOPRIM
mydata$strength2[mydata$Molecule.List == 'BROMHEXINE + SULFAMETHOXAZOLE + TRIMETHOPRIM'] <- 
  mydata$strength2[mydata$Molecule.List == 'BROMHEXINE + SULFAMETHOXAZOLE + TRIMETHOPRIM']+
  mydata$strength3[mydata$Molecule.List == 'BROMHEXINE + SULFAMETHOXAZOLE + TRIMETHOPRIM']

mydata$strength3[mydata$Molecule.List == 'BROMHEXINE + SULFAMETHOXAZOLE + TRIMETHOPRIM'] <-  NA
mydata$denominator3[mydata$Molecule.List == 'BROMHEXINE + SULFAMETHOXAZOLE + TRIMETHOPRIM'] <-  NA

#	GUAIFENESIN + SULFAMETHOXAZOLE + TRIMETHOPRIM
mydata$strength2[mydata$Molecule.List == 'GUAIFENESIN + SULFAMETHOXAZOLE + TRIMETHOPRIM'] <- 
  mydata$strength2[mydata$Molecule.List == 'GUAIFENESIN + SULFAMETHOXAZOLE + TRIMETHOPRIM']+
  mydata$strength3[mydata$Molecule.List == 'GUAIFENESIN + SULFAMETHOXAZOLE + TRIMETHOPRIM']

mydata$strength3[mydata$Molecule.List == 'GUAIFENESIN + SULFAMETHOXAZOLE + TRIMETHOPRIM'] <- NA
mydata$denominator3[mydata$Molecule.List == 'GUAIFENESIN + SULFAMETHOXAZOLE + TRIMETHOPRIM'] <- NA

#NAPROXEN + TETRACYCLINE
mydata$strength2[mydata$Molecule.List == 'NAPROXEN + TETRACYCLINE' & !is.na(mydata$strength3)] <- 
  mydata$strength2[mydata$Molecule.List == 'NAPROXEN + TETRACYCLINE'& !is.na(mydata$strength3)]+
  mydata$strength3[mydata$Molecule.List == 'NAPROXEN + TETRACYCLINE'& !is.na(mydata$strength3)]

mydata$strength3[mydata$Molecule.List == 'NAPROXEN + TETRACYCLINE'& !is.na(mydata$strength3)] <-  NA
mydata$denominator3[mydata$Molecule.List == 'NAPROXEN + TETRACYCLINE'& !is.na(mydata$denominator3)] <-  NA

#PHENAZOPYRIDINE + SULFAMETHOXAZOLE + TRIMETHOPRIM
mydata$strength2[mydata$Molecule.List == 'PHENAZOPYRIDINE + SULFAMETHOXAZOLE + TRIMETHOPRIM'] <- 
  mydata$strength2[mydata$Molecule.List == 'PHENAZOPYRIDINE + SULFAMETHOXAZOLE + TRIMETHOPRIM']+
  mydata$strength3[mydata$Molecule.List == 'PHENAZOPYRIDINE + SULFAMETHOXAZOLE + TRIMETHOPRIM']

mydata$strength3[mydata$Molecule.List == 'PHENAZOPYRIDINE + SULFAMETHOXAZOLE + TRIMETHOPRIM'] <- NA

# Step 10: Split up the SU values for cases where there are multiple ATC codes ####
#reshape to long
mydata <- data.table(mydata)
mydata <- melt(mydata, id.vars = c("Molecule.List", "Country",                        
                                    "Country.Panel", "ATC4",                    
                                    "International.Product", "International.Pack", 'Local.Pack',
                                    "NFC123", "International.Strength",         
                                    "strength1", "denominator1",
                                    "strength2", "denominator2",                  
                                    "strength3", "denominator3", "ATC5", "ATC5_1",
                                    "ATC5_2", "ATC5_3", "total.weight", "total.weight_units"),
                measure.vars = c("X2000", "X2001",
                                 "X2002", "X2003",                          
                                 "X2004", "X2005",                          
                                 "X2006", "X2007",                          
                                 "X2008", "X2009",                          
                                 "X2010", "X2011",                          
                                 "X2012", "X2013"),
                variable.name = 'year', value.name = 'SU')


mydata$year <- gsub('X', '', mydata$year)
mydata$year <- as.numeric(mydata$year)

#get the proportion of each molecule which is each antibiotic
mydata$prop1 <- mydata$strength1/rowSums(mydata[,.(strength1, strength2, strength3)], na.rm = T)
mydata$prop2 <- mydata$strength2/rowSums(mydata[,.(strength1, strength2, strength3)], na.rm = T)
mydata$prop3 <- mydata$strength3/rowSums(mydata[,.(strength1, strength2, strength3)], na.rm = T)

mydata$SU1 <- mydata$SU * mydata$prop1
mydata$SU2 <- mydata$SU * mydata$prop2
mydata$SU3 <- mydata$SU * mydata$prop3

mydata$prop1 <-  NULL
mydata$prop2 <-  NULL
mydata$prop3 <-  NULL

mydata$ATC5_1[which(is.na(mydata$ATC5_1))] <- mydata$ATC5[which(is.na(mydata$ATC5_1))]

#reshape again, for the combination antibiotics
mydata <- melt(mydata, id.vars = c("Molecule.List", "Country",                        
                                   "Country.Panel", "ATC4",                    
                                   "International.Product", "International.Pack", 'Local.Pack',
                                   "NFC123", "International.Strength", "total.weight", "total.weight_units", 'year'),
               measure.vars = list(c("ATC5_1", "ATC5_2", "ATC5_3"),
                                   c("strength1", "strength2", "strength3"),
                                   c('denominator1', 'denominator2', 'denominator3'),
                                   c("SU1", "SU2", "SU3")),
               value.name = c('ATC5', 'strength', 'denominator', 'SU'))


mydata$variable <-  NULL
mydata <- mydata[!is.na(mydata$ATC5),]
mydata <- mydata[mydata$ATC5 != 'NON ABX',]

# Step 11: Calculate KGs ####
#get the NFC - definitions of what the standard units are
mydata$NFC <- substr(mydata$NFC123, 1, 3)
mydata$kg <- NA

#corrections this based on the strength
mydata$strength[mydata$International.Pack == 'ORL DRY SUSP 59.4%+4.25% 1 10.1G'] <- 636.5
mydata$strength[mydata$nfc == 'GPB' & mydata$International.Pack == 'DRY RT VIAL COMBI STR 1' & mydata$Local.Pack == 'VIAL 2.40M 6ML'] <- 1439.71
mydata$total.weight[mydata$nfc == 'GPB' & mydata$International.Pack == 'DRY RT VIAL COMBI STR 1' & mydata$Local.Pack == 'VIAL 2.40M 6ML'] <- 6
mydata$total.weight_units[mydata$nfc == 'GPB' & mydata$International.Pack == 'DRY RT VIAL COMBI STR 1' & mydata$Local.Pack == 'VIAL 2.40M 6ML'] <- 'ML'

#read in the NFC definition sheet
NFC <- read.csv('D:/Z_drive/Covariates/antibiotic_use/IQVIA/lookup_tables/NFC_lookup.csv', stringsAsFactors = F)

strength_X_SU <- NFC$NFC[NFC$MG =='strength*SU']
strength_X_SU <-  strength_X_SU[!is.na(strength_X_SU)]
No_5G_doses <- NFC$NFC[which(NFC$MG =='(5000/total.weight)*SU*strength')] 
No_1G_doses <- NFC$NFC[which(NFC$MG =='(1000/total.weight)*SU*strength')] 
No_5ml_doses <- NFC$NFC[which(NFC$MG =='(denominator/5)*strength*SU')]
No_1ml_doses <- NFC$NFC[which(NFC$MG =='(denominator/1)*strength*SU')]
No_units <- NFC$NFC[which(NFC$MG == '(total.weight/denominator)*strength*SU')]
demoninator_dependant <- NFC$NFC[which(NFC$MG =='If denominator present: (total.weight/denominator)*strength*SU; If no denominator present: strength*SU')]
weight_25G_dependant <- NFC$NFC[which(NFC$MG == '<25G: strength*SU; >25G: (5000/total.weight)*strength*SU')]
weight_30ML_dependant <- NFC$NFC[which(NFC$MG == '<30ML: (denominator/1)*strength*SU; >=30ML: (denominator/5)*strength*SU')]

# Corrections and current assumptions (based on IQVIA responses)
#For DGK or DGM assume that if the denominator is missing that this is /5ml 
mydata$denominator[is.na(mydata$denominator) & (mydata$NFC == 'DGK' | mydata$NFC == 'DGM')] <- 5

#fix some total weights which are missing with those from country specific
to_fix <- mydata[is.na(mydata$total.weight),]
mydata <- mydata[which(!is.na(mydata$total.weight)),]
to_fix$total.weight <- NULL
to_fix$total.weight_units <- NULL
fixes <- read.csv('lookup_tables/total_weight_fixes.csv', stringsAsFactors = F)
fixes$notes <-  NULL
to_fix <-  merge(to_fix, fixes, by = c("International.Pack", "Local.Pack", "NFC"), all.x = T)
mydata <- rbind(mydata, to_fix)
rm(to_fix, fixes)

#calculate the easier ones
mydata$kg[mydata$NFC %in% strength_X_SU] <- mydata$strength[mydata$NFC %in% strength_X_SU]*mydata$SU[mydata$NFC %in% strength_X_SU]
mydata$kg[mydata$NFC %in% No_5ml_doses] <- (mydata$denominator[mydata$NFC %in% No_5ml_doses]/5) * mydata$strength[mydata$NFC %in% No_5ml_doses] * mydata$SU[mydata$NFC %in% No_5ml_doses]
mydata$kg[mydata$NFC %in% No_1ml_doses] <- (mydata$denominator[mydata$NFC %in% No_1ml_doses]/1) * mydata$strength[mydata$NFC %in% No_1ml_doses] * mydata$SU[mydata$NFC %in% No_1ml_doses]
mydata$kg[mydata$NFC %in% No_units] <- (mydata$total.weight[mydata$NFC %in% No_units]/mydata$denominator[mydata$NFC %in% No_units]) * mydata$strength[mydata$NFC %in% No_units] * mydata$SU[mydata$NFC %in% No_units]

mydata$kg[mydata$NFC %in% demoninator_dependant & !is.na(mydata$denominator)] <- (mydata$total.weight[mydata$NFC %in% demoninator_dependant & !is.na(mydata$denominator)]/mydata$denominator[mydata$NFC %in% demoninator_dependant & !is.na(mydata$denominator)])* mydata$strength[mydata$NFC %in% demoninator_dependant & !is.na(mydata$denominator)] * mydata$SU[mydata$NFC %in% demoninator_dependant & !is.na(mydata$denominator)]
mydata$kg[mydata$NFC %in% demoninator_dependant & is.na(mydata$denominator)] <- mydata$strength[mydata$NFC %in% demoninator_dependant & is.na(mydata$denominator)] * mydata$SU[mydata$NFC %in% demoninator_dependant & is.na(mydata$denominator)]

#recalculate weight in g if in ML then calculate kgs if DEH or DEK
mydata$total.weight[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'ML' & !is.na(mydata$denominator)] <- 
  (mydata$total.weight[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'ML' & !is.na(mydata$denominator)]/mydata$denominator[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'ML' & !is.na(mydata$denominator)])*mydata$strength[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'ML' & !is.na(mydata$denominator)]
mydata$total.weight[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'ML' & is.na(mydata$denominator)] <- 
  mydata$strength[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'ML' & is.na(mydata$denominator)]
mydata$total.weight_units[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'ML'] <- 'MG'  

mydata$kg[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'MG' & mydata$total.weight<25000] <- mydata$strength[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'MG' & mydata$total.weight<25000] * mydata$SU[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'MG' & mydata$total.weight<25000]
mydata$kg[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'MG' & mydata$total.weight>=25000] <- (5000/mydata$total.weight[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'MG' & mydata$total.weight>=25000]) * mydata$strength[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'MG' & mydata$total.weight>=25000] * mydata$SU[mydata$NFC %in% weight_25G_dependant & mydata$total.weight_units == 'MG' & mydata$total.weight>=25000] 

#For DGJ
#if in mg calculate the total weight in ml (then if no denominator assume that the weight is the total weight)
mydata$total.weight[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'MG' & !is.na(mydata$denominator))] <- 
  mydata$total.weight[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'MG' & !is.na(mydata$denominator))]/(mydata$strength[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'MG' & !is.na(mydata$denominator))]/mydata$denominator[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'MG' & !is.na(mydata$denominator))])
mydata$total.weight_units[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'MG' & !is.na(mydata$denominator))] <- 'ML'

mydata$total.weight[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'MG' & is.na(mydata$denominator))] <- 
  mydata$total.weight[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'MG' & is.na(mydata$denominator))]/mydata$strength[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'MG' & is.na(mydata$denominator))]
mydata$total.weight_units[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'MG' & is.na(mydata$denominator))] <- 'ML'

# If total volume is in ML and denominator unavailable the mg is the total for the whole substance
# so calculate kg as (strength/total.weight)*SU if <30ML (i.e per 1ml) and (strength/(total.weight/5))*SU if >=30ml (i.e per 5ml)
mydata$kg[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight<30 & is.na(mydata$denominator))] <-  
  (mydata$strength[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight<30 & is.na(mydata$denominator))]/mydata$total.weight[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight<30 & is.na(mydata$denominator))]) * mydata$SU[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight<30 & is.na(mydata$denominator))]

mydata$kg[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight>=30 & is.na(mydata$denominator))] <-
  ( mydata$strength[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight>=30 & is.na(mydata$denominator))]/(mydata$total.weight[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight>=30 & is.na(mydata$denominator))]/5)) * mydata$SU[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight>=30 & is.na(mydata$denominator))]

#If total volume in ML and denominator available then the mg is per denominator and adjust so this is by 1ml and 5ml denominators for <30 and >30mls
# i.e strength/correct denominator * SU
mydata$kg[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight<30 & !is.na(mydata$denominator))] <- 
  (mydata$strength[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight<30 & !is.na(mydata$denominator))]/(1/mydata$denominator[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight<30 & !is.na(mydata$denominator))]))*mydata$SU[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight<30 & !is.na(mydata$denominator))]

mydata$kg[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight>=30 & !is.na(mydata$denominator))] <- 
  (mydata$strength[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight>=30 & !is.na(mydata$denominator))]/(5/mydata$denominator[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight>=30 & !is.na(mydata$denominator))]))*mydata$SU[which(mydata$NFC %in% weight_30ML_dependant & mydata$total.weight_units == 'ML' & mydata$total.weight>=30 & !is.na(mydata$denominator))]

#For AEA and AEB assume that 1 dose is 1g and calculate from there
#if total weight is in mg calculate the antibiotc as a proportion of the total dose for the number of 1g doses
mydata$kg[which(mydata$NFC %in% No_1G_doses & mydata$total.weight_units == 'MG')] <- (1000/mydata$total.weight[which(mydata$NFC %in% No_1G_doses & mydata$total.weight_units == 'MG')])* mydata$strength[which(mydata$NFC %in% No_1G_doses & mydata$total.weight_units == 'MG')] * mydata$SU[which(mydata$NFC %in% No_1G_doses & mydata$total.weight_units == 'MG')]
#if total weight is in ml or is non existent than the strength of antibitoic is the total weight so use this
mydata$kg[which(mydata$NFC %in% No_1G_doses & mydata$total.weight_units != 'MG')] <- (mydata$strength[which(mydata$NFC %in% No_1G_doses & mydata$total.weight_units != 'MG')]/1000) * mydata$SU[which(mydata$NFC %in% No_1G_doses & mydata$total.weight_units != 'MG')]

# BEB, BEY and DEY
#if total weight is in mg calculate the antibiotc as a proportion of the total dose for the number of 5G doses
mydata$kg[which(mydata$NFC %in% No_5G_doses & mydata$total.weight_units == 'MG')] <- (5000/mydata$total.weight[which(mydata$NFC %in% No_5G_doses & mydata$total.weight_units == 'MG')])* mydata$strength[which(mydata$NFC %in% No_5G_doses & mydata$total.weight_units == 'MG')] * mydata$SU[which(mydata$NFC %in% No_5G_doses & mydata$total.weight_units == 'MG')]
#if total weight is in ml or is non existent than the strength of antibitoic is the total weight so use this
mydata$kg[which(mydata$NFC %in% No_5G_doses & mydata$total.weight_units != 'MG')] <- (mydata$strength[which(mydata$NFC %in% No_5G_doses & mydata$total.weight_units != 'MG')]/5000) * mydata$SU[which(mydata$NFC %in% No_5G_doses & mydata$total.weight_units != 'MG')]

#convert to kgs (currently in mg)
mydata$kg <-  mydata$kg/1000000
summary(mydata$kg)

rm(NFC, demoninator_dependant, No_1G_doses, No_1ml_doses, No_5G_doses, No_5ml_doses, No_units, strength_X_SU, weight_25G_dependant, weight_30ML_dependant)

# Step 12: Aggregate KGs by ATC5 code and channel ####
mydata <- mydata[!is.na(mydata$kg),]

#determine the channel (hospital, retail or combined)
mydata$channel <- NA
mydata$channel[grepl('NON RETAIL', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('RETAIL', mydata$Country.Panel)] <- 'Retail' 
mydata$channel[grepl('HOSPITAL', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('RET', mydata$Country.Panel) & is.na(mydata$channel)] <- 'Retail' 
mydata$channel[grepl('HOSP', mydata$Country.Panel) & is.na(mydata$channel)] <- 'Hospital' 
mydata$channel[grepl('COMBINED', mydata$Country.Panel)] <- 'Combined' 
mydata$channel[grepl('TOTAL', mydata$Country.Panel)] <- 'Combined' 
mydata$channel[grepl('CLINIC', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('US DRUGSTORES', mydata$Country.Panel)] <- 'Retail' 
mydata$channel[grepl('US FOODSTORES', mydata$Country.Panel)] <- 'Retail' 
mydata$channel[grepl('US FED FACILITIES', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('ITALY DPC', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('RUSSIA RLO', mydata$Country.Panel)] <- 'Retail' 
mydata$channel[grepl('RUSSIAN FED. DLO', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('SAUDI ARABIA LPO', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('URUGUAY MUTUALES', mydata$Country.Panel)] <- 'Retail' 
mydata$channel[grepl('US HMO', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('US LONG TERM CARE', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('INDONESIA DRUG', mydata$Country.Panel)] <- 'Retail' 
mydata$channel[grepl('US DRUGSTORES', mydata$Country.Panel)] <- 'Retail' 
mydata$channel[grepl('US HOME HLTH CARE', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('US MAIL SERVICE', mydata$Country.Panel)] <- 'Retail' 
mydata$channel[grepl('US MISCELLANEOUS', mydata$Country.Panel)] <- 'Hospital' 
mydata$channel[grepl('NETHERLNDS XPONENT', mydata$Country.Panel)] <- 'Retail' 
mydata$channel[grepl('INDONESIA TOT MKT', mydata$Country.Panel)] <- 'Combined' 

#aggregate by channel
mydata <- mydata[,.(kg = sum(kg)),
                          by = c( "Country", "channel", "year", "ATC5")]

# reshape to by channel
mydata <- dcast(mydata, Country + year + ATC5 ~ channel, value.var = 'kg')
mydata$ATC5 <-  trim(mydata$ATC5)

# step 13: Convert to DDDs ####
DDD <- read.csv('lookup_tables/DDD_lookup.csv', stringsAsFactors = F, na.strings = "")

DDD$DDD_O.g. <-  NULL
DDD$DDD_P.g. <-  NULL

mydata <- merge(mydata, DDD, by = 'ATC5', allow.cartesian=TRUE, all.x = T)

#Calculate DDDs and restrict to required data
mydata <- mydata[,.(country = Country,
                    year,
                    ATC3 = substr(mydata$ATC5, 1, 4),
                    ATC4 = substr(mydata$ATC5, 1, 5),
                    ATC5,
                    hospital_ddd = mydata$Hospital / (mydata$DDD/1000),
                    retail_ddd = mydata$Retail / (mydata$DDD/1000),
                    combined_ddd = mydata$Combined / (mydata$DDD/1000))]


# Step 14: Match to location and population information and add in the cleaned 2014-2018 data ####
library(foreign)
locs <- read.dbf("Z:/AMR/Shapefiles/GBD2019/GBD2019_analysis_final_loc_set_22.dbf")

locs <- locs[c('spr_reg_id',
               'region_id',
               'loc_id',
               'ihme_lc_id',
               'GAUL_CODE',
               'loc_name')]

locs$loc_name <- as.character(locs$loc_name)
locs$ihme_lc_id <- as.character(locs$ihme_lc_id)

locs$loc_name[locs$loc_name == 'Hong Kong Special Administrative Region of China'] <- 'Hong Kong'
locs$loc_name <-  toupper(locs$loc_name)

mydata$country[mydata$country == 'BOSNIA'] <- 'BOSNIA AND HERZEGOVINA' 
mydata$country[mydata$country == 'UK'] <- 'UNITED KINGDOM' 
mydata$country[mydata$country == 'USA'] <- 'UNITED STATES' 
mydata$country[mydata$country == 'China'] <- 'CHINA (WITHOUT HONG KONG AND MACAO)' 
mydata$country[mydata$country == 'C. AMERICA'] <- 'CENTRAL AMERICA' 
mydata$country[mydata$country == 'DENMARK (IMS)'] <- 'DENMARK' 
mydata$country[mydata$country == 'FR. WEST AFRICA'] <- 'FRENCH WEST AFRICA' 
mydata$country[mydata$country == 'DOMINICAN REPUBLIK'] <- 'DOMINICAN REPUBLIC' 
mydata$country[mydata$country == 'KOREA'] <- 'SOUTH KOREA' 
mydata$country[mydata$country == 'S. AFRICA'] <- 'SOUTH AFRICA' 
mydata$country[mydata$country == 'UAE'] <- 'UNITED ARAB EMIRATES' 
mydata$country[mydata$country == 'CZECH'] <- 'CZECH REPUBLIC' 

mydata <- data.frame(mydata)
mydata <- merge(mydata, locs, by.x = 'country', by.y = 'loc_name', all.x = T, all.y = F) 

unique(mydata$country[is.na(mydata$loc_id)])
mydata$ihme_lc_id [mydata$country == 'CHINA'] <- 'CHN'

mydata$spr_reg_id[mydata$country == 'CENTRAL AMERICA'] <-  130
mydata$region_id[mydata$country == 'CENTRAL AMERICA'] <-  124
mydata$GAUL_CODE[mydata$country == 'CENTRAL AMERICA'] <-  9999
mydata$loc_id[mydata$country == 'CENTRAL AMERICA'] <-  9999

mydata$spr_reg_id[mydata$country == 'FRENCH WEST AFRICA'] <-  166
mydata$region_id[mydata$country == 'FRENCH WEST AFRICA'] <-  199
mydata$GAUL_CODE[mydata$country == 'FRENCH WEST AFRICA'] <-  9998
mydata$loc_id[mydata$country == 'FRENCH WEST AFRICA'] <-  9998

mydata <- data.table(mydata)
mydata <- mydata[,.(spr_reg_id, region_id, country, ihme_lc_id, loc_id, GAUL_CODE, year, ATC3, ATC4, ATC5, hospital_ddd, retail_ddd, combined_ddd)]
colnames(mydata) <-  c('super_region', 'region', 'country', 'iso3', 'loc_id', 'GAUL_CODE', 'year', "ATC3", "ATC4", "ATC5", "hospital_ddd", "retail_ddd", "combined_ddd")

mydata$iso3[mydata$country == 'FRENCH WEST AFRICA'] <- 'FWA'
mydata$iso3[mydata$country == 'CENTRAL AMERICA'] <- 'CAM'

write.csv(mydata, 'datasets/cleaned_ddds_2000_2013.csv', row.names = F)
rm(DDD, locs)

#merge on the cleaned 2014-2018 data
oldata <- read.csv('datasets/cleaned_ddds_2014_2018.csv', stringsAsFactors = F)
oldata$country <-  toupper(oldata$country)
mydata <- rbind(mydata, oldata)
rm(oldata)

#calculate DDD/1000 population
pop <- read.csv('Z:/AMR/Misc/GBD_populations/GBD_total_populations.csv', stringsAsFactors = F)
pop <- data.table(pop)
names(pop) <- c('loc_id', 'year', 'pop')
pop <- pop[year>=2000 & year <=2018,]

#calculate popuation of french west africa and central america
FWA <- c(200, 201, 205, 202, 170, 173, 208, 211, 216, 218)
CA <- c(126, 127, 128, 129, 131 ,132)

FWA_pop <- pop[loc_id %in% FWA,]
CA_pop <- pop[loc_id %in% CA,]

FWA_pop <- FWA_pop[,.(pop = sum(pop)),
                   by = c('year')]

CA_pop <- CA_pop[,.(pop = sum(pop)),
                 by = c('year')]

rm(FWA, CA)

FWA <- mydata[mydata$country == 'FRENCH WEST AFRICA',]
CA <- mydata[mydata$country == 'CENTRAL AMERICA',]
mydata <- mydata[mydata$country != 'FRENCH WEST AFRICA' & mydata$country != 'CENTRAL AMERICA',]

mydata <- merge(mydata, pop, by = c('loc_id', 'year'), all.x = T, all.y = F)
FWA <- merge(FWA, FWA_pop, by = 'year', all.x = T, all.y = F)
CA <- merge(CA, CA_pop, by = 'year', all.x = T, all.y = F)

mydata <- rbind(mydata, FWA, CA)

#calculate the DDD/1000 population
mydata$hospital_ddd_per_1000_pop <- mydata$hospital_ddd/(mydata$pop/1000)
mydata$retail_ddd_per_1000_pop <- mydata$retail_ddd/(mydata$pop/1000)
mydata$combined_ddd_per_1000_pop <- mydata$combined_ddd/(mydata$pop/1000)

rm(CA, CA_pop, FWA, FWA_pop, pop)
write.csv(mydata, 'datasets/cleaned_ddds_2000_2018.csv', row.names = F)

# Step 15: Plot out the raw data ####
mydata <- data.table(mydata)
mydata$total_ddd <- rowSums(mydata[,.(hospital_ddd, retail_ddd, combined_ddd)], na.rm = T )
mydata$total_ddd_per_1000 <- rowSums(mydata[,.(hospital_ddd_per_1000_pop, retail_ddd_per_1000_pop, combined_ddd_per_1000_pop)], na.rm = T )

#aggregate to global and plot
global_data <- mydata[,.(total_ddd = sum(total_ddd, na.rm = T),
                         total_ddd_per_1000 = sum(total_ddd_per_1000, na.rm = T)),
                         by = 'year']

country_data <- mydata[,.(total_ddd = sum(total_ddd, na.rm = T),
                          total_ddd_per_1000 = sum(total_ddd_per_1000, na.rm = T)),
                      by = c('country','super_region', 'year')]

country_data$total_ddd_per_1000[country_data$total_ddd == 0] <- NA

png('raw_plots/global_total_ddds_per_1000.png')
  ggplot(global_data)+
    geom_point(aes(x = year, y = log(total_ddd_per_1000)))+
    scale_x_continuous("Year", 
                       breaks = seq(2000, 2018, 1),
                       labels = 2000:2018)
  dev.off()

png('raw_plots/global_total_ddds.png')
  ggplot(global_data)+
    geom_point(aes(x = year, y = total_ddd))+
    scale_x_continuous("Year", 
                       breaks = seq(2000, 2018, 1),
                       labels = 2000:2018)
  dev.off()
  
  
pdf('raw_plots/country_total_ddds.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
      subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
      print(ggplot(subset)+
      geom_point(aes(x = year, y = total_ddd_per_1000))+
      facet_wrap(~country)+
      ylim(0,15000)
)}
dev.off()

pdf('raw_plots/country_total_ddds_free_scale.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset)+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

## by atc3 code
global_data <- mydata[,.(total_ddd = sum(total_ddd, na.rm = T),
                         total_ddd_per_1000 = sum(total_ddd_per_1000, na.rm = T)),
                      by = c('year', 'ATC3')]

country_data <- mydata[,.(total_ddd = sum(total_ddd, na.rm = T),
                          total_ddd_per_1000 = sum(total_ddd_per_1000, na.rm = T)),
                       by = c('country','super_region', 'year', 'ATC3')]

country_data$total_ddd_per_1000[country_data$total_ddd == 0] <- NA

png('raw_plots/global_total_ddds_per_1000_by_ATC3.png',
    height = 30, width = 30, units = 'cm', res = 150)
ggplot(global_data)+
  geom_point(aes(x = year, y = (total_ddd_per_1000)))+
  scale_x_continuous("Year", 
                     breaks = seq(2000, 2018, 2),
                     labels = seq(2000, 2018, 2))+
  facet_wrap(~ATC3, scales = 'free_y')+
  theme(axis.text.x = element_text(angle = 90))
dev.off()

png('raw_plots/global_total_ddds_by_ATC3.png',
    height = 30, width = 30, units = 'cm', res = 150)
ggplot(global_data)+
  geom_point(aes(x = year, y = total_ddd))+
  scale_x_continuous("Year", 
                     breaks = seq(2000, 2018, 2),
                     labels = seq(2000, 2018, 2))+
  facet_wrap(~ATC3, scales = 'free_y')+
  theme(axis.text.x = element_text(angle = 90))
dev.off()


pdf('raw_plots/country_total_ddds_J01A.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset[subset$ATC3 == 'J01A'])+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
        )}
dev.off()

pdf('raw_plots/country_total_ddds_J01B.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset[subset$ATC3 == 'J01B'])+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

pdf('raw_plots/country_total_ddds_J01C.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset[subset$ATC3 == 'J01C'])+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

pdf('raw_plots/country_total_ddds_J01D.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset[subset$ATC3 == 'J01D'])+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

pdf('raw_plots/country_total_ddds_J01E.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset[subset$ATC3 == 'J01E'])+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

pdf('raw_plots/country_total_ddds_J01F.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset[subset$ATC3 == 'J01F'])+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

pdf('raw_plots/country_total_ddds_J01G.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset[subset$ATC3 == 'J01G'])+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

pdf('raw_plots/country_total_ddds_J01M.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset[subset$ATC3 == 'J01M'])+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

pdf('raw_plots/country_total_ddds_J01R.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset[subset$ATC3 == 'J01R'])+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

pdf('raw_plots/country_total_ddds_J01X.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(country_data$super_region))){
  subset <- country_data[country_data$super_region == unique(country_data$super_region)[i],]
  print(ggplot(subset[subset$ATC3 == 'J01X'])+
          geom_point(aes(x = year, y = total_ddd_per_1000))+
          facet_wrap(~country, scales = 'free_y')
  )}
dev.off()

#~~~~~#
# END #
#~~~~~#
