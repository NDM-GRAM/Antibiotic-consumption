# Plot covariate layers
library(ggplot2, lib.loc = package_lib)

color_list <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')

# Load predictions raster
library(raster, lib.loc = package_lib)
nt_covs <- brick(paste0(cov_dir, '/covs_transformed.grd')) # non-varying

# Get templates
library(rgdal, lib.loc = package_lib)
admin0 <- readOGR(dsn=template_dir,layer=paste0(target_country,"_adm0"),verbose = FALSE)
admin0.dt <- data.table(fortify(admin0)) 

# Make plot directory
plot_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/new_model/plots/')
dir.create(plot_dir, showWarnings = FALSE)

# Plot non-varying covariates
for(cov in names(nt_covs)) {
  if(cov=='access') covs <- nt_covs$access
  if(cov=='irrigation') covs <- nt_covs$irrigation
  covs.sp <- rasterToPoints(covs, spatial=TRUE)
  projection <- proj4string(covs.sp)
  
  # reproject sp object  
  covs.sp <- spTransform(covs.sp, CRS(projection)) 
  covs.sp@data <- data.frame(covs.sp@data, long=coordinates(covs.sp)[,1],lat=coordinates(covs.sp)[,2]) 
  covs.dt <- data.table(covs.sp@data)
  
  ## Plot preds of proportion with 0 years of education
  names(covs.dt)[names(covs.dt) == "lat"] = "latitude"
  names(covs.dt)[names(covs.dt) == "long"] = "longitude" 
  
  covs.gg <- ggplot(covs.dt,aes(longitude,latitude)) +
    geom_raster(aes(fill=get(cov))) +
    coord_fixed() + 
    theme_minimal() +
    geom_path(data=admin0.dt, aes(x=long, y=lat, group=group), color='white', lwd=.1) +
    scale_fill_gradientn(colours=(color_list), limits=c(minValue(covs), maxValue(covs)), na.value = "grey") + 
    guides(fill=guide_colorbar(title=cov, label=TRUE, ticks=FALSE)) +
    scale_x_continuous("", breaks=NULL) +
    scale_y_continuous("", breaks=NULL) +
    theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0,0,0,0),"lines"))
  
    png(paste0(plot_dir,'nt_cov_',cov,'.png'),width=1200)
    print(covs.gg)
    dev.off()
}

# Plot varying covariates
gLegend<-function(a.gplot){
  pdf(NULL) # Workaround for bug in ggplot_gtable causing empty Rplots.pdf to be created
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  graphics.off()
  return(legend)
}
selected_covs <- strsplit(fixed_effects," ")
selected_covs <- selected_covs[[1]][selected_covs[[1]] != "+"]
library(grid)
for(c in c('lights_new','evi','LST_day','total_pop','edu_0','edu_mean')) {
  if(c %in% selected_covs) {
    
    tv_cov <- brick(paste0(cov_dir, '/time_varying_covs_transformed_',c,'.grd'))
    
    # Convert raster to SpatialPointsDataFrame
    preds.sp <- rasterToPoints(tv_cov, spatial=TRUE)
    projection <- proj4string(preds.sp)
    
    # reproject sp object  
    preds.sp <- spTransform(preds.sp, CRS(projection)) 
    preds.sp@data <- data.frame(preds.sp@data, long=coordinates(preds.sp)[,1],lat=coordinates(preds.sp)[,2]) 
    preds.dt <- data.table(preds.sp@data)
    
    ## Plot preds of proportion with 0 years of education
    names(preds.dt)[names(preds.dt) == "lat"] = "latitude"
    names(preds.dt)[names(preds.dt) == "long"] = "longitude" 
    
    # Plot predictions for all periods
    plot.preds <- function(x) {
      period <- gsub(paste0(c,'.'), "", x)
      loop.preds.gg <- ggplot(preds.dt,aes(longitude,latitude)) +
        geom_raster(aes(fill=get(x))) +
        coord_fixed() + 
        theme_minimal() +
        geom_path(data=admin0.dt, aes(x=long, y=lat, group=group), color='white', lwd=.1) +
        scale_fill_gradientn(colours=rev(color_list), limits=c(min(minValue(tv_cov)), max(maxValue(tv_cov))), na.value = "grey") + 
        guides(fill=guide_colorbar(title=c, label=TRUE, ticks=FALSE)) +
        scale_x_continuous("", breaks=NULL) +
        scale_y_continuous("", breaks=NULL) +
        theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0,0,0,0),"lines")) + 
        ggtitle(paste0("Period ",period))
      return(loop.preds.gg)
    }
    for(i.period in 1:4) {
      assign(paste("preds.gg", i.period, sep="."),plot.preds(paste0(c,'.',i.period)))
    }
    
    # grab your legends using the predefined functions, then state their grid location
    png(paste0(plot_dir,'tv_cov_',c,'.png'),width=1200)
    p.legend <- gLegend(preds.gg.1)
    p.legend$vp <- viewport(layout.pos.row = 7:14, layout.pos.col = 11:12)
    # Initialize plot with master title
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(19, 12, heights=c(.25,.25,.25,.25), widths=c(.25,.25,.25,.25))))
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    grid.text(paste0("Time-varying ",c), vp = vplayout(1,1:10), gp = gpar(fontsize = 18, fontface = "bold"))
    # Plot all data coverage maps
    print(preds.gg.1 + theme(legend.position="none"), vp = vplayout(2:10, 1:5))
    print(preds.gg.2 + theme(legend.position="none"), vp = vplayout(2:10, 6:10))
    print(preds.gg.3 + theme(legend.position="none"), vp = vplayout(11:19, 1:5))
    print(preds.gg.4 + theme(legend.position="none"), vp = vplayout(11:19, 6:10))
    # Plot master legend
    grid.draw(p.legend)
    dev.off()
    
  }
}

# Combine all ntv and tv covariates dynamically depending on which ones we have
tv_cov_list <- as.list(ls.str(pos = -1, pattern = "tv_cov_"))


  
  