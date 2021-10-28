#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Compare ESAC-NET and IQVIA data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(data.table)
library(ggplot2)
library(ggrepel)
rm(list = ls())

setwd('Z:/AMR/Covariates/antibiotic_use')

#Read in the cleaned, imputed datasets
IQVIA <- data.table(read.csv('IQVIA/imputation/imputed_ddd_per_1000_2000_2018.csv', stringsAsFactors = F))
IQVIA <- IQVIA[IQVIA$ATC3 != 'J04A',]
IQVIA <- IQVIA[,.(ddd = sum(total_ddd),
              ddd_per_1000 = sum(total_ddd_per_1000_pop)),
           by = c("super_region", "region", "country", "iso3", "loc_id", "GAUL_CODE", "year", "pop")]

ESAC <- read.csv('ESAC-NET/cleaned_datasets/J01_DDD.csv', stringsAsFactors = F)

#Merge the datasets
mydata <- merge(ESAC, IQVIA, by = c('iso3', 'year'), all.x = F, all.y = F)

#limit to required variables
mydata <- mydata[c('iso3', 'year', 'ddd_per_1000.x', 'ddd_per_1000.y')]

#select one year to plot
mydata <- mydata[mydata$year == 2015,]

#plot out the data
png('comparing_sources/IQVIA_vs_ESAC_NET/DDDs_per_1000/IQVIA_vs_ESAC_NET.png',
    height = 20, width = 20, unit = 'cm', res = 300)

print(ggplot(mydata, aes(x = (ddd_per_1000.x), y = (ddd_per_1000.y)))+
        geom_point()+
        # annotate('text', x = quantile((mydata$ddd_per_1000.x),.01), y = quantile((mydata$ddd_per_1000.y), .99),
        #          label = paste0('r2=', round(cor((mydata$ddd_per_1000.x), (mydata$ddd_per_1000.y)),2)))+
        geom_abline(slope = 1, intercept = 0, colour = 'red')+
        theme_bw()+
        labs(x = 'ESAC-NET DDD/1000/year',
             y = 'IQVIA DDD/1000/year')+
        geom_text_repel(aes(label=iso3),hjust=-0.2, vjust=0, size = 2.5)+
        xlim(2000, 18000)+
        ylim(2000, 18000)
)
dev.off()

# Correlation between all countries
round(cor((mydata$ddd_per_1000.x), (mydata$ddd_per_1000.y)),2)^2
lm(ddd_per_1000.y ~ddd_per_1000.x, data = mydata)

#~~~~~#
# END #
#~~~~~#