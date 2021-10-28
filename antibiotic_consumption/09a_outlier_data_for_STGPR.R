#Identify outliers from the stacked ensemble model
rm(list = ls())
library(data.table)
library(stats)
library(MLmetrics)

#read in model estimate
model_est <- read.csv('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/2020_10_05/custom_stage1_df.csv', stringsAsFactors = F)
input_data <- read.csv('/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/STGPR_input_2020_10_05.csv', stringsAsFactors = F)

#unlog the estimates
model_est$cv_custom_stage_1 <- exp(model_est$cv_custom_stage_1)

#add a row ID onto the input daya
input_data$row_id <- row.names(input_data)

#merge together
mydata <- merge(model_est, input_data, all.x = F, all.y = F)
mydata <-  data.table(mydata)

#calculate te upper and lower bounds as 7.5* the MAD
MADs <-  mydata[,.(upper_bound = cv_custom_stage_1 + 7.2*mad(cv_custom_stage_1[!is.na(val)], val[!is.na(val)]),
                 lower_bound = cv_custom_stage_1 - 7.2*mad(cv_custom_stage_1[!is.na(val)],val[!is.na(val)]))]


#identify the outliers in the data
mydata <- cbind(mydata, MADs)
mydata <- mydata[!is.na(mydata$val)]
mydata$is_outlier[mydata$val<mydata$lower_bound |mydata$val>mydata$upper_bound] <- 1
mydata$is_outlier[mydata$val>mydata$lower_bound & mydata$val<mydata$upper_bound] <- 0
table(mydata$is_outlier)
outliers <- mydata[mydata$is_outlier == 1,]

input_data$is_outlier[input_data$row_id%in%outliers$row_id] <-  1
input_data$row_id <- NULL

write.csv(input_data, '/ihme/homes/annieb6/AMR/antibiotic_use/sales_data/STGPR_input_2020_10_05_outliered.csv', row.names = F)
