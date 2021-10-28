#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Compare WHO and IQVIA data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(data.table)
library(foreign)
library(ggplot2)
library(ggrepel)

rm(list = ls())
setwd('Z:/AMR/Covariates/antibiotic_use')

IQVIA <- data.table(read.csv('IQVIA/imputation/imputed_ddd_per_1000_2000_2018.csv', stringsAsFactors = F))
IQVIA <- IQVIA[IQVIA$ATC3 != 'J04A',]
IQVIA <- IQVIA[,.(ddd = sum(total_ddd),
                  ddd_per_1000 = sum(total_ddd_per_1000_pop)),
               by = c("super_region", "region", "country", "iso3", "loc_id", "GAUL_CODE", "year", "pop")]

who <- read.csv('WHO/J01_DDD.csv', stringsAsFactors = F)

#merge with the IQVIA and survey data and compare
mydata <- merge(who, IQVIA, by = c('iso3', 'year'), all.x = F, all.y = F)

# Add footnotes to the odd data points
mydata$iso3[mydata$iso3 == 'ESP' | 
                mydata$iso3== 'DEU' | 
                mydata$iso3== 'AUT' |
                mydata$iso3== 'NZL' | 
                mydata$iso3== 'CZE'] <- paste0(mydata$iso3[mydata$iso3 == 'ESP' | 
                                                               mydata$iso3== 'DEU' | 
                                                               mydata$iso3== 'AUT' |
                                                               mydata$iso3== 'NZL' | 
                                                               mydata$iso3== 'CZE'], '*')
mydata$iso3[mydata$iso3 == 'JOR'] <- 'JOR£'

png('comparing_sources/IQVIA_vs_WHO/DDDs_per_1000/IQVIA_vs_WHO.png',
    height = 20, width = 20, unit = 'cm', res = 300)

print(ggplot(mydata, aes(x = (ddd_per_1000.x), y = (ddd_per_1000.y)))+
        geom_point()+
        # annotate('text', x = quantile((mydata$ddd_per_1000.x),.01), y = quantile((mydata$ddd_per_1000.y), .99),
        #          label = paste0('r2=', round(cor((mydata$ddd_per_1000.x), (mydata$ddd_per_1000.y)),2)))+
        geom_abline(slope = 1, intercept = 0, colour = 'red')+
        theme_bw()+
        labs(x = 'WHO DDD/1000/year',
             y = 'IQVIA DDD/1000/year')+
        geom_text_repel(aes(label=iso3),hjust=-0.2, vjust=0, size = 2.5)+
        xlim(2000, 18000)+
        ylim(2000, 18000)
)
dev.off()

lm(ddd_per_1000.y ~ddd_per_1000.x, data = mydata)

# Correlation between all countries
round(cor((mydata$ddd_per_1000.x), (mydata$ddd_per_1000.y)),2)^2

#Remove countries with only community data and check correlation
mydata2 <- mydata[!(grepl('\\*', mydata$iso3)),]

round(cor(mydata2$ddd_per_1000.x, mydata2$ddd_per_1000.y),2)^2

#remove countries with <70%, not population adjusted and check correlation
mydata3 <- mydata2[!(grepl('\\£', mydata2$iso3)),]
round(cor(mydata3$ddd_per_1000.x, mydata3$ddd_per_1000.y),2)^2

#~~~~~#
# END #
#~~~~~#