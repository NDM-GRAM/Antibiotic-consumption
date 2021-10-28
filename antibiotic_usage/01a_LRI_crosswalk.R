#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Conduct a crosswalk between the prevalence of antibitoic #
# use for various combinations of respiratory symptoms and #
# for LRI (defined) as cough with difficulty breathing,    #
# chest symptoms and fever. The coefficients of the        #
# crosswalk are used to adjust the data in 01_data_cleaning#
#                                                          #
# Code written by Annie Browne: annie.browne@ndm.ox.ac.uk  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(foreign)
library(data.table)
library(ggpubr)

#1. Read in the combined dataset and restrict to required variables ####
date = '2019_09_25'
mydata <- readRDS(paste0('/share/homes/annieb6/AMR/antibiotic_use/datasets/combined_data_', date, '.RDS'))

#for some reason these surveys dont have an illness definition, insert it here
mydata$illness_definition[mydata$nid == 125230] <- 'cough'
mydata$illness_definition[mydata$nid == 286782] <- 'cough'
mydata$illness_definition[mydata$nid == 44870] <- 'cough'
mydata$illness_definition[mydata$nid == 9999] <- 'cough'

names(mydata)
mydata <- mydata[c("nid",
                   "year_end",
                   "ihme_loc_id",
                   "pweight",
                   "had_cough",
                   "diff_breathing",
                   "chest_symptoms",
                   "illness_definition",
                   "cough_antibiotics",
                   'had_fever')]

mydata <- data.table(mydata)
mydata <- mydata[mydata$had_cough == 1,]
mydata$illness_definition <- trimws(mydata$illness_definition, 'both')
table(mydata$illness_definition)

#2. Collapse data into the datasets for different symptoms of LRI (by nid) ####
#a. For all children with cough
cough <- mydata[mydata$illness_definition == 'cough',]

cough <- cough[,. (cough_abx = weighted.mean(cough_antibiotics, pweight, na.rm = T),
                   cough_N = ((sum(pweight))**2)/sum(pweight**2)),
              by = .(nid, ihme_loc_id, year_end)]

#b. For cough and difficulty breathing
cough_diff_breathing <- mydata[mydata$illness_definition == 'cough' | mydata$illness_definition == 'cough & difficulty breathing',]
cough_diff_breathing <- cough_diff_breathing[cough_diff_breathing$had_cough == 1 & cough_diff_breathing$diff_breathing == 1,]

cough_diff_breathing <- cough_diff_breathing[,. (cough_diff_breathing_abx = weighted.mean(cough_antibiotics, pweight, na.rm = T),
                                                 cough_diff_breathing_N = ((sum(pweight))**2)/sum(pweight**2)),
                                          by = .(nid, ihme_loc_id, year_end)]

#c. For cough and fever
cough_fever <- mydata[mydata$illness_definition == 'cough' | mydata$illness_definition == 'cough & fever',]
cough_fever <- cough_fever[cough_fever$had_cough == 1 & cough_fever$had_fever == 1,]

cough_fever <- cough_fever[,. (cough_fever_abx = weighted.mean(cough_antibiotics, pweight, na.rm = T),
                               cough_fever_N = ((sum(pweight, na.rm = T))**2)/sum(pweight**2, na.rm = T)),
                                             by = .(nid, ihme_loc_id, year_end)]

#d. For cough with difficulty breathing or fever
cough_breathing_fever <- mydata[mydata$illness_definition == 'cough' | mydata$illness_definition == 'cough & rapid breathing or fever',]
cough_breathing_fever <- cough_breathing_fever[cough_breathing_fever$had_cough == 1 & (cough_breathing_fever$had_fever == 1 | cough_breathing_fever$diff_breathing == 1),]

cough_breathing_fever <- cough_breathing_fever[,. (cough_breathing_fever_abx = weighted.mean(cough_antibiotics, pweight, na.rm = T),
                                                   cough_breathing_fever_N = ((sum(pweight))**2)/sum(pweight**2)),
                                                by = .(nid, ihme_loc_id, year_end)]

#e. For cough with difficulty breathing and chest symptoms
cough_breathin_chest <- mydata[mydata$illness_definition == 'cough' | mydata$illness_definition == 'cough, difficulty breathing & chest symptoms' | mydata$illness_definition == 'cough & difficulty breathing',]
cough_breathin_chest <- cough_breathin_chest[cough_breathin_chest$had_cough == 1 & cough_breathin_chest$diff_breathing  == 1 & cough_breathin_chest$chest_symptoms == 1,]

cough_breathin_chest <- cough_breathin_chest[,. (cough_breathin_chest_abx = weighted.mean(cough_antibiotics, pweight, na.rm = T),
                                                 cough_breathin_chest_N = ((sum(pweight))**2)/sum(pweight**2)),
                                                 by = .(nid, ihme_loc_id, year_end)]


#f. For LRI definition (cough with difficulty breathing, chest symptoms and fever)
lri <- mydata[mydata$had_cough == 1 & mydata$diff_breathing & mydata$chest_symptoms == 1 & mydata$had_fever == 1,]

lri <- lri[,. (lri_abx = weighted.mean(cough_antibiotics, pweight, na.rm = T),
               lri_N = ((sum(pweight))**2)/sum(pweight**2)),
           by = .(nid, ihme_loc_id, year_end)]


#3.Merge all collapsed data together ####

collapsed_data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(lri, cough, cough_breathin_chest, cough_breathing_fever, cough_diff_breathing, cough_fever))

rm(lri, cough, cough_breathin_chest, cough_breathing_fever, cough_diff_breathing, cough_fever)

collapsed_data[collapsed_data == 'NaN'] <-  NA

#4. Check the correlations between antibiotic use for each set of symptoms ####
crosswalk_data <- data.frame(collapsed_data)
crosswalk_data <- crosswalk_data[colnames(crosswalk_data)[(grep('abx', colnames(crosswalk_data)))]]
vlist <- list()

for(i in 1:length(crosswalk_data)){
  vlist[[i]] <- as.vector(crosswalk_data[,i])
}

cor.matrix <- matrix(,nrow = length(crosswalk_data), ncol = length(crosswalk_data))

# calculate correlation matrix
cor.matrix <- matrix(,nrow = length(crosswalk_data), ncol = length(crosswalk_data))
for(i in 1:length(vlist)){
  for(j in 1:length(vlist)){
    r <- cor.test(vlist[[i]][!is.na(vlist[[i]]) & !is.na(vlist[[j]])], vlist[[j]][!is.na(vlist[[i]]) & !is.na(vlist[[j]])], method = 'pearson')
    cor.matrix[i, j] <- round(r$estimate,2)
    if(i==j){cor.matrix[i, j] <- NA}
  }
}

rownames(cor.matrix) <- names(crosswalk_data)
colnames(cor.matrix) <- names(crosswalk_data)

pdf('/share/homes/annieb6/AMR/antibiotic_use/LRI crosswalk/correlation.pdf',
    width = 15, height = 8)
print(
  ggtexttable(cor.matrix)
)
dev.off()

rm(cor.matrix, r, vlist, crosswalk_data)

#5. Fit linear regression models between antibiotic use for LRI and each set of symptoms
# and extract the coefficients.
cough <- lm(lri_abx~cough_abx, data = collapsed_data)$coefficients
cough_breathin_chest <- lm(lri_abx~cough_breathin_chest_abx, data = collapsed_data)$coefficients
cough_breathing_fever <- lm(lri_abx~cough_breathing_fever_abx, data = collapsed_data)$coefficients
cough_diff_breathing <- lm(lri_abx~cough_diff_breathing_abx, data = collapsed_data)$coefficients
cough_fever <- lm(lri_abx~cough_fever_abx, data = collapsed_data)$coefficients

coefs <- cbind(cough, cough_breathin_chest, cough_breathing_fever, cough_diff_breathing, cough_fever)
coefs <-  data.frame(coefs)

rownames(coefs)[2] <- 'beta'

write.csv(coefs, '/share/homes/annieb6/AMR/antibiotic_use/LRI crosswalk/lm_coefficients.csv')