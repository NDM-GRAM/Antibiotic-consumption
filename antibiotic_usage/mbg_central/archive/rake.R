# Combine model outputs to useful summaries, and Rake

# clear workspace
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
hroot <- ifelse(Sys.info()[1]=="Windows", "H:/", "/homes/ngraetz")
setwd(paste0(hroot,'/repos/education/geospatial/mbg'))

source('/homes/ngraetz/repos/education/geospatial/mbg/functions.R')

defaultOptions(resolution = 5,        # raster resolution
               location = 'seattle',  # location for final run 
               cores = 30,            # number of cores to use
               start = Sys.time())    # start time

# load packages
library(raster)
library(reldist)
library(plyr)
library(data.table)
library(rgdal)

# load misc functions
edu_data_dir <- paste0(root,'/WORK/01_covariates/02_inputs/education/update_2017/data/geospatial_data')
geo_dir <- paste0(root,'/temp/geospatial')
indicators <- indicator

## ~~~~~~~~~~~~~~~~~~~~~
# Load data

# a template/masking layer
template <- raster(paste0(edu_data_dir,'/data/',target_country,'/template',getOption('resolution'),'kdata.gri'))
cell_idx <- cellIdx(template) # get cell indexes

# bring in population data and mask it
if(getOption('resolution')==5) {
  #pop<-brick(paste0(geo_dir,'/U5M_africa/data/raw/covariates/new_20160421/pop_stack.tif'))[[4]]
  pop<-brick(paste0(root,'/temp/learl/U5M/covs_20160617/pop_stack.tif'))[[4]]
  pop <- setExtent(pop,template)
  pop<-crop(pop,template)
  pop <- resample(pop, template, method="ngb")
  pop<-mask(pop,template)
}

if(getOption('resolution')==1) {
  pop<-raster(paste0(ifelse(.Platform$OS.type=="unix",'/home/j/','J:/'),'DATA/SHAPE_FILES/AFRIPOP/UGA_AFRIPOP_2010/apuga10v2.flt'))
  pop<-aggregate(pop,fact=10,fun=sum,na.rm=T)
  pop = setExtent(pop,extent(template)[1:4])
  pop<-crop(pop,template)
  pop=resample(pop,template) # FOR NOW... go back and make template and all covariates match pop raster later
  pop<-mask(pop,template)
  
}


# admin raster
#ad2 <- raster(paste0(geo_dir,'/vaccine/data/uga/clean/ad2_raster_',getOption('resolution'),'k.grd'))

# TEST GEO SHAPEFILE
setwd(paste0(root,'WORK/01_covariates/02_inputs/education/update_2017/data/geospatial_data/data'))
admin2.sp <- readOGR(dsn=target_country,layer=paste0(target_country,"_adm2"))
# admin2.sp <- admin2.sp[admin2.sp@data$country_id!="YEM",]
# admin2.sp <- admin2.sp[admin2.sp@data$country_id!="CPV",]
admin2 <- rasterize_check_coverage(admin2.sp, template, "gaul_code")

# get samples of national, population-weighted mortality rates in each fraction.
# admin codes, matched up with names
#ad2_code <- extract(ad2, cell_idx)
ad2_code <- extract(admin2, cell_idx)

# load in draw level data
load(paste0(edu_data_dir,'/output/data/',indicator,'_',target_country,'_cell_draws_eb.RData'))
assign(paste0(indicator,'_cell_pred'),cell_pred)
rm(cell_pred)

#test_mean <- raster("J:/WORK/01_covariates/02_inputs/education/update_2017/data/geospatial_data/output/rasters/edu_mean_Uganda_prediction_eb.gri")




## ~~~~~
# ADMIN 2 POPULATION WEIGHTS

# cell populations
pop_cell <- extract(pop, cell_idx) # using 2015 for now



# set to zero if the population or admin code is NA
pop_cell[is.na(pop_cell)] <- 0
pop_cell[is.na(ad2_code)] <- 0  


# give it a tiny homogenous populationn where there is 0 population.
tmp <- tapply(pop_cell, ad2_code, sum) 
pop_cell[ad2_code %in% names(tmp)[tmp==0]] = .0001 


# get population totals in all cells
pop_totals_ad2 <- tapply(pop_cell, ad2_code, sum)


# replicate for all cells in area
pop_totals_ad2_cell <- as.vector(pop_totals_ad2)[match(ad2_code,
                                                       names(pop_totals_ad2))]


# for all cells with some population, but unknown country, set to 0/1
disowned_ad2 <- which(is.na(pop_totals_ad2_cell))
pop_totals_ad2_cell[disowned_ad2] <- 1
pop_cell[disowned_ad2] <- 0


# get  population weights for each cell
pop_wt_ad2 <- pop_cell / pop_totals_ad2_cell

# make sure these sum to one or zero
wt_sum_ad2 <- tapply(pop_wt_ad2, ad2_code, sum)
stopifnot(all.equal(wt_sum_ad2, round(wt_sum_ad2)))

# check number of periods 
df = fread(paste0(root,'/WORK/01_covariates/02_inputs/education/update_2017/data/geospatial_data/africa_fully_processed_dhs_mean.csv'))
data_periods <- unique(df$year)
i <- length(data_periods)

# replicate for multiple years 
prs=i
pop_wt_all_ad2 <- rep(pop_wt_ad2, prs)
pop_cell_all <- rep(pop_cell, prs)

#periods <- rep(c(2001, 2006, 2011, 2015), each = length(ad2_code))
periods <- rep(data_periods, each = length(ad2_code))

ad2_code_all <- paste(ad2_code, periods, sep = '_')
ad2_code_all[ad2_code_all == 'NA'] <- NA





### ~~~~
## RAW DRAW CONDSIM
# # summarise the draws from these component mortalities
message('/nRunning  conditional simulation for fractions:')
for(v in indicators){
  message(v)
  
  # name to summarise, and summary object names
  name_from <- sprintf('%s_cell_pred', v)
  name_to_rate_ad2 <- sprintf('%s_cond_sim_ad2', v)

  # get object (draws)
  x <- get(name_from)
    
  # get NA mask
  good_cells <- which(!is.na(x[, 1]))
    
    
  # run conditional simulation for probs
  cond_sim_ad2 <- condSim(x[good_cells, ],
                        weights = pop_wt_all_ad2[good_cells],
                       group = ad2_code_all[good_cells])

  assign(name_to_rate_ad2, cond_sim_ad2)
  
  rm(x)
   
}


# # save these
save(list = grep('*_cond_sim_ad2', ls(), value = TRUE),
     file = paste0(edu_data_dir,'/output/condsim/conditional_simulation_ad2_',target_country,'_',indicator,'_',getOption('resolution'),'k.RData'))










## ~~~
# Rake to Laura's estimates
message('RAKE')

splitGeoNames<-function (geo,vac) {
  splits <- names(geo)
  splits <- strsplit(splits,"_")
  id    <- unlist(splits)[ c(TRUE,FALSE) ]
  year  <- unlist(splits)[ c(FALSE,TRUE) ]
  geo   <- data.table(ID=splits,
                      area = as.numeric(id),
                      t = as.numeric(year),
                      mean.geo = geo,
                      indicator = gsub('_cov','',vac))
  rownames(geo) <- NULL
  return (geo)
}



# process geo/gavi and merge them
geo=data.table()
for(v in indicators){
  d=(apply(get(paste0(v,'_cond_sim_ad2')), 1, mean))
  geo=rbind(geo,splitGeoNames(d,v))
}
geo = subset(geo,!is.na(area))

# DONT RAKE EDUCATION

# sae <- fread('data/uga/raw/all_antigens_coverage_preds.csv')
# sae<-sae[sae$t%in%c(2001,2006,2011,2015),]
# sae<-subset(sae,level=="dist112")
# sae$mean.sae = sae$mean
# sae=sae[,c('area','area_name','mean.sae','t','vac'),with=FALSE]
# sae=subset(sae,vac %in% unique(geo$vac))
# sae = subset(sae,!is.na(area))
# 
# 
# # merge and get raking factors for each area-vaccine-year
# m<-merge(geo,sae,by=c('area','t','vac'),all=T)
# m[,raking_factor :=  mean.sae/mean.geo]
# 
# m=m[,c('ID','area','area_name','t','vac','mean.geo','mean.sae','raking_factor'),with=FALSE]
# write.csv(m,'output/raking_factors.csv')


# TRY PLOTTING BY MERGING TO GEO SHAPEFILE
require(sp)
library(rgeos)
library(maptools)
library(ggplot2)
library(grid)
names(geo)[names(geo) == "area"] = "gaul_code"
setwd(paste0(root,'WORK/01_covariates/02_inputs/education/update_2017/data/geospatial_data/data'))
admin0 <- readOGR(dsn=target_country,layer=paste0(target_country,"_adm0"))
admin0 <- admin0[admin0@data$country_id!="YEM",]
admin0 <- admin0[admin0@data$country_id!="CPV",]
admin0.dt <- data.table(fortify(admin0)) 
gLegend<-function(a.gplot){
  pdf(NULL) # Workaround for bug in ggplot_gtable causing empty Rplots.pdf to be created
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  graphics.off()
  return(legend)
}

# Loop over periods
gg.count <- 1
for(period in sort(unique(geo$t))) {

  admin2.preds.p1 <- merge(admin2.sp,geo[geo$t==period], by="gaul_code")
  admin2.preds.p1@data$id = rownames(admin2.preds.p1@data)
  admin2.pts <- fortify(admin2.preds.p1, region="id")
  admin2.df = join(admin2.pts, admin2.preds.p1@data, by="id")
  admin2.df$predict.mean[is.na(admin2.df$mean.geo)] <- 0
  admin2.df$predict.mean <- as.numeric(admin2.df$mean.geo)
  
  # Plot
  #source(paste0(hroot,"/repos/education/geospatial/woodson_pallettes.r"))
  #color_list <- woodson_pallettes("black_to_light_10")
  color_list <- c("#000000","#00281D","#07425B","#38499A","#8149B9","#C653AF","#EB7190","#EC9F7D","#DCCF91","#DBF0C6")
  admin2.gg <- ggplot(admin2.df,aes(long,lat,group=group)) + 
    geom_polygon(aes(fill=predict.mean)) + 
    geom_path(data=admin0.dt, aes(x=long, y=lat, group=group), color='grey90', size=.05)
    #geom_path(color="white", lwd=.05) 
    if(indicator == 'edu_mean') {admin2.gg <- admin2.gg + scale_fill_gradientn(colours=rev(color_list), limits=c(0, 12), na.value = "#000000") + guides(fill=guide_colorbar(title="Years of\neducation", label=TRUE, ticks=FALSE))}
    if(indicator == 'edu_0') { admin2.gg <- admin2.gg + scale_fill_gradientn(colours=(color_list), limits=c(0, 1), na.value = "white") + guides(fill=guide_colorbar(title="Proportion \n0 years", label=TRUE, ticks=FALSE))}
    if(indicator == 'wasting') { admin2.gg <- admin2.gg + scale_fill_gradientn(colours=rev(color_list), limits=c(0, .5), na.value = "#000000") + guides(fill=guide_colorbar(title="Proportion wasted", label=TRUE, ticks=FALSE))}
    if(indicator == 'stunting') { admin2.gg <- admin2.gg + scale_fill_gradientn(colours=rev(color_list), limits=c(0, .5), na.value = "#000000") + guides(fill=guide_colorbar(title="Proportion stunted", label=TRUE, ticks=FALSE))}
    if(indicator == 'underweight') { admin2.gg <- admin2.gg + scale_fill_gradientn(colours=rev(color_list), limits=c(0, .5), na.value = "#000000") + guides(fill=guide_colorbar(title="Proportion underweight", label=TRUE, ticks=FALSE))}
    admin2.gg <- admin2.gg + ggtitle(period) +
    scale_x_continuous("", breaks=NULL) +
    scale_y_continuous("", breaks=NULL) +
    #theme_tufte() +
    coord_equal() 
  # pdf(paste0(edu_data_dir,"/output/plots/",target_country,"_admin2_mean_edu.pdf"))
  # admin2.gg
  # dev.off()
  # grab your legends using the predefined functions, then state their grid location
  p.legend <- gLegend(admin2.gg)
  assign(paste0("ggplot.",gg.count),admin2.gg)
  gg.count <- gg.count + 1
  
}


# Save plot 
plot_repo <- '/snfs1/WORK/01_covariates/02_inputs/education/update_2017/data/geospatial_data/data/Africa/plots'
pdf(paste0(plot_repo,"/2_means_data_",target_country,"_",indicator,".pdf"),onefile=FALSE)
# grab your legends using the predefined functions, then state their grid location
p.legend <- gLegend(ggplot.1)
p.legend$vp <- viewport(layout.pos.row = 7:14, layout.pos.col = 11:12)
# Initialize plot with master title
grid.newpage()
pushViewport(viewport(layout = grid.layout(19, 12, heights=c(.25,.25,.25,.25), widths=c(.25,.25,.25,.25))))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.text("Average by 5*5 km", vp = vplayout(1,1:10), gp = gpar(fontsize = 18, fontface = "bold"))
# Plot all data coverage maps
print(ggplot.1 + theme(legend.position="none"), vp = vplayout(2:10, 1:5))
print(ggplot.2 + theme(legend.position="none"), vp = vplayout(2:10, 6:10))
print(ggplot.3 + theme(legend.position="none"), vp = vplayout(11:19, 1:5))
print(ggplot.4 + theme(legend.position="none"), vp = vplayout(11:19, 6:10))
# Plot master legend
grid.draw(p.legend)
dev.off()

