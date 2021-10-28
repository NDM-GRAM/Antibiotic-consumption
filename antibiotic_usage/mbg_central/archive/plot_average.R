
  library(ggplot2, lib.loc = package_lib)
  library(rgdal, lib.loc = package_lib)
  
# Settings
  color_list <- c("#000000","#00281D","#07425B","#38499A","#8149B9","#C653AF","#EB7190","#EC9F7D","#DCCF91","#DBF0C6")

  admin0 <- readOGR(dsn=template_dir,layer=paste0(target_country,"_adm0"),verbose = FALSE)
  admin0.dt <- data.table(fortify(admin0)) 

  ## Logit functions
  logit <- function(x) {
    log(x/(1-x))
  }
  invlogit <- function(x) {
    exp(x)/(1+exp(x))
  }
  
# Load actual data
  if(time_stamp==TRUE) output_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date)
  if(time_stamp==FALSE) output_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/scratch')
  load(paste0(output_dir, '/', indicator,'_trainingdata.RData'))
  
  df <- subset(df, !is.na(latitude))
  df <- subset(df, latitude!=0)
  
  periods <- data.frame(group = rep(1:length(unique(df$year)),5),years = rep(sort(unique(df$year)),5))
  df$period <- match(df$year, periods$years) # add these to df

# Plot data cluster means for each year
  plot.data <- function(x) {
    df.period <- subset(df, period == x)
    df.year <- df$year[df$period==x][1]
    loop.data.gg <- ggplot() +
      geom_polygon(data=admin0.dt, aes(x=long, y=lat, group=group), fill='grey90', color='grey') +
      geom_point(data=df.period, aes(x=longitude, y=latitude, color=to_map), pch=16, size=1) +
      scale_fill_gradientn(colours=rev(color_list), limits=c(min(df[, to_map]), max(df[, to_map])), na.value = "white") + 
      guides(fill=guide_colorbar(title=to_map, label=TRUE, ticks=FALSE)) +
      coord_fixed() +
      ggtitle(df.year) +
      guides(size=FALSE) +
      theme_minimal() +
      theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank()) + theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0,0,0,0),"lines"))
    return(loop.data.gg)
  }
  for(period in sort(unique(df$period))) {
    assign(paste("data.gg", period, sep="."),plot.data(period))
  }
  
# Load predictions raster
  f <- paste0(root,'WORK/01_covariates/02_inputs/education/update_2017/data/geospatial_data/output/rasters/',indicator,'_',target_country,'_prediction_eb')
  preds <- brick(f)
  preds <- setExtent(preds, admin0)

# Convert raster to SpatialPointsDataFrame
  preds.sp <- rasterToPoints(preds, spatial=TRUE)
  projection <- proj4string(preds.sp)

# reproject sp object  
  preds.sp <- spTransform(preds.sp, CRS(projection)) 
  preds.sp@data <- data.frame(preds.sp@data, long=coordinates(preds.sp)[,1],lat=coordinates(preds.sp)[,2]) 
  preds.dt <- data.table(preds.sp@data)

## Plot preds of proportion with 0 years of education
  plot_repo <- paste0(root,"/WORK/01_covariates/02_inputs/education/update_2017/data/geospatial_data/output/plots/")
  names(preds.dt)[names(preds.dt) == "lat"] = "latitude"
  names(preds.dt)[names(preds.dt) == "long"] = "longitude" 

# Plot predictions for all periods
  plot.preds <- function(x) {
    period <- gsub("period_", "", x)
    df.year <- df$year[df$period==period][1]
    loop.preds.gg <- ggplot(preds.dt,aes(longitude,latitude)) +
    geom_raster(aes(fill=get(x))) +
    coord_fixed() + 
    theme_minimal() +
    #geom_path(data=admin1.dt, aes(x=long, y=lat, group=group), color='white', size=.1) +
    geom_path(data=admin0.dt, aes(x=long, y=lat, group=group), color='white', lwd=.1)
    if(indicator == 'edu_mean') {loop.preds.gg <- loop.preds.gg + scale_fill_gradientn(colours=rev(color_list), limits=c(0, 12), na.value = "#000000") + guides(fill=guide_colorbar(title="Years of\neducation", label=TRUE, ticks=FALSE))}
    if(indicator == 'edu_0') { loop.preds.gg <- loop.preds.gg + scale_fill_gradientn(colours=(color_list), limits=c(0, 1), na.value = "white") + guides(fill=guide_colorbar(title="Proportion \n0 years", label=TRUE, ticks=FALSE))}
    if(indicator == 'wasting') { loop.preds.gg <- loop.preds.gg + scale_fill_gradientn(colours=rev(color_list), limits=c(0, .5), na.value = "#000000") + guides(fill=guide_colorbar(title="Proportion wasted", label=TRUE, ticks=FALSE))}
    if(indicator == 'stunting') { loop.preds.gg <- loop.preds.gg + scale_fill_gradientn(colours=rev(color_list), limits=c(0, .5), na.value = "#000000") + guides(fill=guide_colorbar(title="Proportion stunted", label=TRUE, ticks=FALSE))}
    if(indicator == 'underweight') { loop.preds.gg <- loop.preds.gg + scale_fill_gradientn(colours=rev(color_list), limits=c(0, .5), na.value = "#000000") + guides(fill=guide_colorbar(title="Proportion underweight", label=TRUE, ticks=FALSE))}
    loop.preds.gg <- loop.preds.gg +
    scale_x_continuous("", breaks=NULL) +
    scale_y_continuous("", breaks=NULL) +
    theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0,0,0,0),"lines"))
    return(loop.preds.gg)
  }
  for(i.period in sort(unique(df$period))) {
    assign(paste("preds.gg", i.period, sep="."),plot.preds(paste0('period_',i.period)))
  }

# Pull legend for final plot
  gLegend<-function(a.gplot){
    pdf(NULL) # Workaround for bug in ggplot_gtable causing empty Rplots.pdf to be created
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    graphics.off()
    return(legend)
  }

# Save plot 
plot_repo <- 'J:/WORK/01_covariates/02_inputs/education/update_2017/data/geospatial_data/data/Africa/plots'
pdf(paste0(plot_repo,"/1_means_data_",target_country,"_",indicator,".pdf"),onefile=FALSE)

## Save gg objects for plotting in Rmd script
  # assign(paste0(target_iso3,'figure_1',fig,'_data_1'),data.gg.1)
  # assign(paste0(target_iso3,'figure_1',fig,'_data_2'),data.gg.2)
  # assign(paste0(target_iso3,'figure_1',fig,'_data_3'),data.gg.3)
  # assign(paste0(target_iso3,'figure_1',fig,'_preds_1'),preds.gg.1)
  # assign(paste0(target_iso3,'figure_1',fig,'_preds_2'),preds.gg.2)
  # assign(paste0(target_iso3,'figure_1',fig,'_preds_3'),preds.gg.3)
  # figures <- c(paste0(target_iso3,'figure_1',fig,'_data_1'),paste0(target_iso3,'figure_1',fig,'_data_2'),paste0(target_iso3,'figure_1',fig,'_data_3'),paste0(target_iso3,'figure_1',fig,'_preds_1'),paste0(target_iso3,'figure_1',fig,'_preds_2'),paste0(target_iso3,'figure_1',fig,'_preds_3'))
  # save(list=figures, file=paste0("C:/Users/ngraetz/Documents/THESIS/final_rmd/figures/",target_iso3,"_figure1",fig,".RData"))
  
# grab your legends using the predefined functions, then state their grid location
  p.legend <- gLegend(preds.gg.1)
  p.legend$vp <- viewport(layout.pos.row = 7:14, layout.pos.col = 11:12)
# Initialize plot with master title
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(19, 12, heights=c(.25,.25,.25,.25), widths=c(.25,.25,.25,.25))))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  grid.text("Average by 5*5 km", vp = vplayout(1,1:10), gp = gpar(fontsize = 18, fontface = "bold"))
# Plot all data coverage maps
  print(data.gg.1 + theme(legend.position="none"), vp = vplayout(2:10, 1:5))
  print(data.gg.2 + theme(legend.position="none"), vp = vplayout(2:10, 6:10))
  print(data.gg.3 + theme(legend.position="none"), vp = vplayout(11:19, 1:5))
  print(data.gg.4 + theme(legend.position="none"), vp = vplayout(11:19, 6:10))
# Plot master legend
  grid.draw(p.legend)
  dev.off()
  
pdf(paste0(plot_repo,"/1_means_preds_",target_country,"_",indicator,".pdf"),onefile=FALSE)    
# grab your legends using the predefined functions, then state their grid location
  p.legend <- gLegend(preds.gg.1)
  p.legend$vp <- viewport(layout.pos.row = 7:14, layout.pos.col = 11:12)
# Initialize plot with master title
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(19, 12, heights=c(.25,.25,.25,.25), widths=c(.25,.25,.25,.25))))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  grid.text("Average by 5*5 km", vp = vplayout(1,1:10), gp = gpar(fontsize = 18, fontface = "bold"))
# Plot all data coverage maps
  print(preds.gg.1 + theme(legend.position="none"), vp = vplayout(2:10, 1:5))
  print(preds.gg.2 + theme(legend.position="none"), vp = vplayout(2:10, 6:10))
  print(preds.gg.3 + theme(legend.position="none"), vp = vplayout(11:19, 1:5))
  print(preds.gg.4 + theme(legend.position="none"), vp = vplayout(11:19, 6:10))
# Plot master legend
  grid.draw(p.legend)
  dev.off()

  
