
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
  

# Load actual data (df already in memory)
if(time_stamp==TRUE) output_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date)
if(time_stamp==FALSE) output_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/scratch')

  df <- subset(df, !is.na(latitude))
  df <- subset(df, latitude!=0)
  
  periods <- data.frame(group = rep(1:length(unique(df$year)),5),years = rep(sort(unique(df$year)),5))
  df$period <- match(df$year, periods$years) # add these to df

  # Make quantity of interest
    if(indicator_family=="binomial") df <- df[, to_map := get(indicator) / N]


# Plot data cluster means for each year
  plot.data <- function(x) {
    df.period <- subset(df, period == x)
    df.year <- df$year[df$period==x][1]
    loop.data.gg <- ggplot() +
      geom_polygon(data=admin0.dt, aes(x=long, y=lat, group=group), fill='grey90', color='grey') +
      geom_point(data=df.period, aes(x=longitude, y=latitude, color=to_map), pch=16, size=1) +
      scale_color_gradientn(colours=(color_list), limits=c(min(df[, to_map]), max(df[, to_map])), na.value = "white") +
      guides(fill=guide_colorbar(title=indicator, label=TRUE, ticks=FALSE)) +
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

 library(INLA, lib.loc = package_lib)
  library(raster, lib.loc = package_lib)
  library(seegMBG, lib.loc = package_lib)
  library(data.table, lib.loc = package_lib)
  library(rgdal, lib.loc = package_lib)
  
  f <- paste0(output_dir,'/',indicator,'_prediction_eb')
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
    geom_path(data=admin0.dt, aes(x=long, y=lat, group=group), color='white', lwd=.1) +
    scale_fill_gradientn(colours=(color_list), limits=c(min(minValue(preds)), max(maxValue(preds))), na.value = "white") +
    guides(fill=guide_colorbar(title=indicator, label=TRUE, ticks=FALSE)) +
    scale_x_continuous("", breaks=NULL) +
    scale_y_continuous("", breaks=NULL) +
    ggtitle(df.year)
    theme(panel.margin = unit(0, "lines"), plot.margin = unit(c(0,0,0,0),"lines"))
    return(loop.preds.gg)
  }
  for(i.period in sort(unique(df$period))) {
    assign(paste("preds.gg", i.period, sep="."),plot.preds(paste0('period_',i.period)))
  }

# Make data and preds pngs for Shiny
  png(paste0(plot_dir,'/data1.png'),width=400)
  data.gg.1
  dev.off()
  png(paste0(plot_dir,'/data2.png'),width=400)
  data.gg.2
  dev.off()
  png(paste0(plot_dir,'/data3.png'),width=400)
  data.gg.3
  dev.off()
  png(paste0(plot_dir,'/data4.png'),width=400)
  data.gg.4
  dev.off()
  png(paste0(plot_dir,'/preds1.png'),width=400)
  preds.gg.1
  dev.off()
  png(paste0(plot_dir,'/preds2.png'),width=400)
  preds.gg.2
  dev.off()
  png(paste0(plot_dir,'/preds3.png'),width=400)
  preds.gg.3
  dev.off()
  png(paste0(plot_dir,'/preds4.png'),width=400)
  preds.gg.4
  dev.off()
  
# Make data coverage scatter
  df[, clusters:=1]
  total_clusters <- df[, list(clusters=sum(clusters)), by=c('original_year','country','source')]
  total_cluster_num <- total_clusters[, list(clusters=sum(clusters))]
  png(paste0(plot_dir,'/data_scatter.png'),width=1200,height=800)
  clusters.gg <- ggplot() +
    geom_point(data=total_clusters, aes(x=original_year, y=country, size=clusters, shape=factor(source), color=factor(source))) +
    guides(color=FALSE) + 
    scale_size(guide = guide_legend(title = "Clusters/polygons"), range=c(1,10)) + 
    ggtitle(paste0("Clusters/polygons by country/year, total points = ", total_cluster_num[1, clusters])) +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank()) 
  clusters.gg
  dev.off()
  
