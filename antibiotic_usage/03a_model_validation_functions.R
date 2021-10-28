#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Functions to analyses model fits and results #
# Code are either written by Annie Browne, or  #
# adapted from specified staff at IHME         #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create datasets for the estimates from DHS aggregated to admin 0, 1 and 2  #
# Input data set should have the columns:                                    #
# source; latitude; longitude; weight; shapefile; location_code              #
# Takes in the cleaned, uncollapsed data, subsets to DHS as a gold standard  #
# and aggregated this to admin 0, 1 and 2 levels (weighted means), outputing #
# the collapsed datasets                                                     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

aggregate_dhs <- function(inputdir = inputdir,
                          inputfile = inputfile,
                          outputdir = outputdir,
                          core_repo = core_repo,
                          indicator = indicator) {

  # Load require packages
  commondir      <- paste(core_repo, 'mbg_central/share_scripts/common_inputs', sep = '/')
  package_list <- c(t(read.csv(paste(commondir, 'package_list.csv', sep = '/'), header = FALSE)))
  source(paste0(core_repo, '/mbg_central/setup.R'))
  mbg_setup(package_list = package_list, repos = core_repo)
    
  # Read in uncollapsed data
  if(grepl(".csv", inputfile) == TRUE){
    mydata <- read.csv(paste0(inputdir, inputfile), stringsAsFactors = F)
  } else if(grepl(".rds", inputfile) == TRUE) {   
  mydata <- readRDS(paste0(inputdir, inputfile))
  }
  
  # Use as a dataframe
  mydata <- data.frame(mydata)
  mydata$shapefile <- as.character(mydata$shapefile) 
  
  #change name of indicator to generic name (this will become obsolete once rate is calculated)
  colnames(mydata)[colnames(mydata) == indicator] <- 'indicator_name'
  
  # restrict to DHS
 # mydata <- mydata[mydata$source == 'MACRO_DHS',]
  
  # drop data without antibiotic usage or weights
  mydata <- mydata[which(!is.na(mydata$indicator_name)),]
  mydata <- mydata[which(!is.na(mydata$weight)),]
  
  # For Polygons 
  # Seperate out polygons from the dataset
  polygons <- mydata[!is.na(mydata$shapefile),]
  polygons <- polygons[polygons$shapefile!= "",]
  
  #read in admin 2 shapefile
  shp <- readRDS('/share/geospatial/rds_shapefiles/g_2015_2014_2_modified/g_2015_2014_2_modified.rds')
  
  # find the centrods of polygons for points and plot them over the GAUL shapefile to get the codes
  polys <- unique(polygons[c("shapefile",  "location_code")])
  polys$location_code <- as.numeric(polys$location_code)
  polys <- polys[order(polys$shapefile, polys$location_code),]
  polys$ID <- 1:length(polys$shapefile)
  polys$ADM0 <- NA
  polys$ADM1 <- NA
  polys$ADM2 <- NA
  
  shapefile.list <- unique(polys$shapefile)
  
  for(ss in shapefile.list) {
    message(paste0("Processing shapefile ", ss))
    #read in each shapefile
    myshapefile <- readRDS(paste0("/share/geospatial/rds_shapefiles/", ss, ".rds"))
    
    #select the polygons which are samples & limit the shapefile to these, ensure the order matches the polygon dataframe
    my.polygons <- polys[polys$shapefile == ss,]
    limitedshp <- myshapefile[myshapefile$GAUL_CODE %in% my.polygons$location_code,]
    limitedshp$GAUL_CODE <- as.numeric(limitedshp$GAUL_CODE)
    limitedshp <- limitedshp[order(limitedshp$GAUL_CODE),]
    
    # Find the centroid & covnert to spatial points
    centroid <- getSpPPolygonsLabptSlots(limitedshp)
    centroid <- SpatialPoints(centroid, proj4string = CRS(projection(shp)))
    
    # plot over the shapefile and grap the admin codes, attach to the dataframe
    my.polygons$ADM0 <- over(centroid, shp)$ADM0_CODE
    my.polygons$ADM1 <- over(centroid, shp)$ADM1_CODE
    my.polygons$ADM2 <- over(centroid, shp)$ADM2_CODE

    #merge the dataframe with the admin codes back onto the one with all of the polygons locations
    polys$ADM0[polys$ID %in% my.polygons$ID] <- my.polygons$ADM0
    polys$ADM1[polys$ID %in% my.polygons$ID] <- my.polygons$ADM1
    polys$ADM2[polys$ID %in% my.polygons$ID] <- my.polygons$ADM2
  }
  
  #merge this information back onto the polygons dataframe
  polygons <- merge(polygons, polys, by = c("shapefile", "location_code"), all.x = T)
  
  # If the shapefile was admin1 original, change the admin2 to NA
  polygons$ADM2[polygons$admin_level == 1] <- NA
  
  # Remove ID column
  polygons$ID <- NULL
  
  # Fill in any rows with missing info (likely due to polygon centroids in the water)
  polygons$ADM0[polygons$location_code == 47586 & polygons$shapefile == 'admin2013_1'] <- 217
  polygons$ADM1[polygons$location_code == 47586 & polygons$shapefile == 'admin2013_1'] <- 47586
  polygons$ADM0[polygons$location_code == 1373 & polygons$shapefile == 'admin2013_1'] <- 217
  polygons$ADM1[polygons$location_code == 1373 & polygons$shapefile == 'admin2013_1'] <- 1373
  polygons$ADM0[polygons$location_code == 138 & polygons$shapefile == 'CMR_adm3_v2'] <- 45
  polygons$ADM1[polygons$location_code == 138 & polygons$shapefile == 'CMR_adm3_v2'] <- 818

  # Check for any rows without attached admins
  check <- polygons[is.na(polygons$ADM0),]
  check <- unique(check[c("location_name", "shapefile", "location_code")])
  
  if(length(check$location_name)>0){
    message('CHECK THESE LOCATIONS AND FILL IN THE ADMIN DETAILS, LIKELY CENTROID WAS IN WATER')
    check
  }
  
  # For points 
  # Seperate out the points
  points <- mydata[!is.na(mydata$latitude),]
  
  # Convert points to spatial points
  sppoints <- SpatialPoints(points[c("longitude", "latitude")], proj4string = CRS(projection(shp)))
  
  # plot over the admin 2 shapefile and extract admin codes
  points$ADM0 <-over(sppoints, shp)$ADM0_CODE
  points$ADM1 <-over(sppoints, shp)$ADM1_CODE
  points$ADM2 <-over(sppoints, shp)$ADM2_CODE
  
  #remove unwanted column
  points$optional <- NULL
  
  # Check for any rows without attached admins
  check <- points[is.na(points$ADM0),]
  
  if(length(check$country)>0){
    message('Selecting the closest polygon to the points as they likely lay in water')
  
    
    pts <- check[c("longitude", "latitude")]
    pts <- SpatialPoints(pts, proj4string = CRS(projection(shp)))
    
    # Crop shapfile to the extent ot the coords (should help speed this up)
    shp <- crop(shp, extent(pts))
    
    suppressWarnings(
      for (i in 1:nrow(check)) {
        message(paste0("Processing coords ", i, ' of ', nrow(check)))
        check$ADM0[i] <- shp$ADM0_CODE[which.min(gDistance(pts[i,], shp, byid=TRUE))]
        check$ADM1[i] <- shp$ADM1_CODE[which.min(gDistance(pts[i,], shp, byid=TRUE))]
        check$ADM2[i] <- shp$ADM2_CODE[which.min(gDistance(pts[i,], shp, byid=TRUE))]
      }
    )
  }
  
  # Remove data without admin data from the points dataset
  points <- points[!is.na(points$ADM0),]
  
  # Merge all datasets together
  new.data <- rbind(points, check, polygons)
  new.data <- data.table(new.data)
  
  # collapse to country level
  admin0 <- new.data[,.(rate = weighted.mean(indicator_name, weight, na.rm = T),
                        N = ((sum(weight))**2)/sum(weight**2),
                        year = max(year)),
                     by = .(nid, ADM0)]
  
  admin1 <- new.data[,.(rate = weighted.mean(indicator_name, weight, na.rm = T),
                        N = ((sum(weight))**2)/sum(weight**2),
                        year = max(year)),
                     by = .(nid, ADM0, ADM1)]
  
  admin2 <- new.data[,.(rate = weighted.mean(indicator_name, weight, na.rm = T),
                        N = ((sum(weight))**2)/sum(weight**2),
                        year = max(year)),
                     by = .(nid, ADM0, ADM1, ADM2)]
  
  
  # Save collapsed data sets
  write.csv(admin2, paste0(outputdir, 'Aggregated admin2- all data.csv'), row.names = F)
  write.csv(admin0, paste0(outputdir, 'Aggregated admin0- all data.csv'), row.names = F)
  write.csv(admin1, paste0(outputdir, 'Aggregated admin1- all data.csv'), row.names = F)
  # saveRDS(admin0, paste0(outputdir, 'DHS_ADMIN_0_aggregated.rds'))
  # saveRDS(admin1, paste0(outputdir, 'DHS_ADMIN_1_aggregated.rds'))
  # saveRDS(admin2, paste0(outputdir, 'DHS_ADMIN_2_aggregated.rds'))
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Scatter plots with linear regression line    #
# and coefficients comparing the aggregated    #
# DHS data with the aggregated predications    #
# at specified admin levels.                   #
# must have run aggregate_dhs prior to running #
# this function.                               #
# input dir specifies where you have saved the #
# aggregated DHS rds files                     #  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

scatter_compare <- function(run_date = run_date,
                            admin_level = c('0', '1', '2'),
                            indicator = indicator,
                            indicator_group = indicator_group,
                            in_dir = inputdir,
                            out_dir = outputdir){
  dir.create(out_dir, showWarnings = F)
  
  for(admin in admin_level){
    
    # Read in aggregated data
    mydata <- readRDS(paste0(in_dir, 'input_admin', admin, '.RDS'))

    # Read in aggregated model output
    results <- read.csv(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/pred_derivatives/admin_summaries/', indicator, '_admin_', admin, '_unraked_summary.csv'))

    # Merge predicted data onto raw data
    mydata <- merge(mydata, results, by = c(paste0('ADM', admin, '_CODE'), 'year'))
    
    # Get linear regression coefficients
    coefs <- lm(mean ~ outcome, data = mydata)
    summary(coefs)
    intercept <- round(coefs$coefficients[[1]],2)
    slope <- round(coefs$coefficients[[2]],2)
    p <- summary(coefs)$coefficients[2,4]
    if(p<0.001){
      p <- '<0.001'
    }
    r2 <- round(summary(coefs)$r.squared,2)
    
    # Plot data
    jpeg(paste0(out_dir, '/input_data_preds_comparison_admin', admin, '.jpeg'))
    print(
      ggplot(mydata, 
             aes(x = outcome, y = mean))+
        geom_point(color='blue', alpha = 0.5) +
        geom_smooth(method='lm', colour = 'red', show.legend = TRUE)+
        theme_bw()+
        theme(panel.grid=element_blank())+
        xlab("Input data mean")+  
        ylab("Predicted mean") + 
        geom_text(aes(0.1, 0.9, label = paste("R2 = ", r2)))+
        geom_text(aes(0.1, 0.87, label = paste("Intercept =",intercept)))+
        geom_text(aes(0.1, 0.84, label = paste("Slope =", slope)))+
        geom_text(aes(0.1, 0.81, label = paste("p =", p)), fontface = 'plain', size = 4)+
        ggtitle(paste0('Comparison between input and predicted data - admin ', admin))+
        theme(plot.title = element_text(hjust = 0.5))
    )
    dev.off()
  }
}   



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot time series maps of output raster #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

plot_all_regions <- function(run_date = run_date,
                             indicator = indicator,
                             indicator_group = indicator_group,
                             outputdir = outputdir,
                             my.palette =  brewer.pal(n = 10, name = "RdYlGn"),
                             summstats = 'mean',
                             years = 2000:2017,
                             title = indicator){

  
  for(measure in summstats){
  myraster  <- brick(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/', indicator, '_', measure, '_raster.tif'))
  
  jpeg(paste0(outputdir, 'time_series_maps_', measure,'.jpeg'),
       height = 20, width = 20, units = 'cm', res = 300)
  print(spplot(myraster, 
         names.attr = c(seq(min(years), max(years), 1)),
         col.regions = my.palette,
         cuts = 8,
         at = seq(0, 1, 0.1),
         maxpixels = 100000,
         main = list(label = title)))
  dev.off()

  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot INLA hyperparameters         #
# Adapted code written for HIV       #
# Laura Dwyer-Lindgren/Michael Cork #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
plot_hyperparameters <- function(indicator, 
                                 indicator_group,
                                 run_date, 
                                 age, 
                                 holdout, 
                                 save_file = NULL,
                                 regions = Regions) {
  
  # get regions
  run_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, "/")
  config <- fread(paste0(run_dir, 'config.csv'))

  # extract prior and posterior distributions from INLA model objects
  message("Load models & extract priors and posteriors for hyper-parameters")
  dist <- rbindlist(lapply(regions, function(r) {
    
    # load model
    message(paste0('...', r))
    load(paste0(run_dir, indicator, "_model_eb_bin", age, "_", r, "_", holdout, ".RData"))
    
    # extract hyper-priors from INLA (based on plot.inla() code)
    all.hyper <- INLA:::inla.all.hyper.postprocess(res_fit$all.hyper)
    hyper <- res_fit$marginals.hyperpar
    id <- strsplit(sapply(hyper, attr, 'hyperid'), split = '\\|')
    prior <- rbindlist(lapply(names(id), function(x) {
      print(x)
      if (grepl("Theta. for", x)) range <- c(-5, 5)
      if (grepl("GroupRho for", x)) range <- c(-0.999, 0.999)
      if (grepl("Group PACF. for", x)) range <- c(-0.999, 0.999)
      if (grepl("Precision for", x)) range <- c(1, 1000)
      p <- INLA:::inla.get.prior.xy(section = tolower(id[[x]][2]), hyperid = id[[x]][1], all.hyper = all.hyper, range = range, intern = F)
      if (grepl("Precision for", x)) {
        p <- inla.tmarginal(function(x) sqrt(1/x), p, method = 'linear')
        x <- gsub("Precision for", "SD for", x)
      }
      data.table(region = r, type = 'prior', name = x, x = p$x, y = p$y)
    }))
    
    # extract corresponding posteriors from INLA
    post <- rbindlist(lapply(names(hyper), function(x) {
      p <- hyper[[x]]
      if (grepl("Precision for", x)) {
        p <- inla.tmarginal(function(x) sqrt(1/x), p, method = 'linear')
        x <- gsub("Precision for", "SD for", x)
      }
      # if (x == "Theta1 for space") {
      #   x <- "Nominal range"
      #   p[, 'x'] <- sqrt(8) / exp(p[, 'x'])
      # }
      # if (x == "Theta2 for space") {
      #   x <- "Nominal variance"
      #   t1 <- hyper[["Theta1 for space"]]
      # }
      data.table(region = r, type = 'posterior', name = x, x = p[, 'x'], y = p[, 'y'])
    }))
    
    # combine
    all <- rbind(prior, post)
    all[, name := factor(name, unique(name))]
    return(all)
  }))
  
  # make plots
  message("Plotting hyper-parameters")
  
  if (is.null(save_file)) save_file <- paste0(run_dir, "/inla_hyperparameters.pdf")
  pdf(save_file, width = 14, height = 8)
  gg <- ggplot(dist[y > 1e-8,], aes(x = x, y = y, color = region, linetype = type)) +
    facet_wrap(~ name, scales = "free") +
    geom_line() +
    labs(x = '', y = '', title = "Hyper-parameter prior and posterior distributions") +
    theme_bw()
  print(gg)
  dev.off()
  
  return(dist)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot  covariate "importance" scores     #
# Adapted code written for HIV            #
# Laura Dwyer-Lindgren/Michael Cork       #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

get_cov_weights <- function(indicator, indicator_group, run_date, regions, outdir) {
  
  dir.create(outdir, showWarnings = F)
  
  # use mbg_central functions to calculate covariate weights and make plots
  all_plots <- lapply(regions, function(r) {
    # calculate weights (this auto-saves the output)
    cov.wts <- get.cov.wts(rd = run_date,
                           ind = indicator,
                           ind_gp = indicator_group,
                           reg = r,
                           age = 0,
                           holdout = 0)
    
    # make plots
    cov.plots <- plot.cov.wts(rd = run_date,
                              ind = indicator,
                              ind_gp = indicator_group,
                              reg = r,
                              age = 0,
                              holdout = 0)
    cov.plots <- cov.plots + labs(x="", y="", title=r)
    return(cov.plots)
  })
  
  # save plots (combined, and individually)
  pdf(paste0(outdir, "/cov_wts_all_regions.pdf"), width=10, height=7)
  do.call("grid.arrange", all_plots)
  for (ii in 1:length(regions)) print(all_plots[[ii]])
  dev.off()
  
  return("Plots saved!")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot child stackers                              #
# Code adapted from that provided by Ani Deshpande #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Aggregate raster by a shapefile polygon(s)
agg_raster <- function(raster,
                       shapefile,
                       field,
                       id,
                       weights = NULL) {
  
  # Subset and convert shapefile to sf object
  shapefile <- shapefile[shapefile[[field]] == id,]
  
  #Create weights raster to use in aggregation
  if(is.null(weights)) {
    wts <- raster
    wts[!is.na(wts)] <- 1
  } else {
    wts <- weights
  }
  
  # Calculate vector of weighted average 
  raster <- crop(raster, shapefile)
  wts <- crop(wts, shapefile)
  
  raster <- mask(raster, shapefile)
  wts <- mask(wts, shapefile)
  
  raster <- as.numeric(as.matrix(raster))
  wts <- as.numeric(as.matrix(wts))
  
  output <- sum(raster*wts, na.rm = T)/sum(wts, na.rm = T)
  return(output)
}

plot_child_stackers <- function(indicator = indicator,
                                indicator_group = indicator_group,
                                run_date = run_date,
                                regions = regions,
                                out_dir = outputdir,
                                pop_measure = pop_measure,
                                start_year = 2000,
                                end_year = 2017) {
  
  dir.create(out_dir, showWarnings = F)    
  for(reg in regions) {
    
    # Load child stacker model outputs and simple polygon
    load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/",
                run_date, "_bin0_", reg, "_0.RData"))
    
    
    # message(paste0('gam OOS correlation (r2): ', round(cor(df$rate, df$gam_cv_pred)^2,2)))
    # message(paste0('XGB OOS correlation (r2): ', round(cor(df$rate, df$xgboost_cv_pred)^2,2)))
    # message(paste0('Ridge OOS correlation (r2): ', round(cor(df$rate, df$ridge_cv_pred)^2,2)))
            
    child_mod_ras <- cov_list[child_model_names]
    
    gaul_list <- get_adm0_codes(reg,
                                shapefile_version = modeling_shapefile_version)
    
    simple_polygon_list <- load_simple_polygon(gaul_list = gaul_list, buffer = 1, tolerance = 0.4,
                                               shapefile_version = modeling_shapefile_version)
    subset_shape        <- simple_polygon_list[[1]]
    simple_polygon      <- simple_polygon_list[[2]]
    
    # Define population as a rater brick
    pop <- load_and_crop_covariates_annual(covs            = 'worldpop',
                                           measures        = pop_measure,
                                           simple_polygon  = simple_polygon,
                                           start_year      = start_year,
                                           end_year        = end_year,
                                           interval_mo     = 12)
    
    pop <- pop[[1]]
    
    # Get the location information as a dataframe for aggregation
    subdata <- subset_shape@data
    
    ## Get input data, calculate the proportion with indicator and aggregate to survey level for adding survey data to the plot
    setwd(paste0('/share/geospatial/mbg/',indicator_group, '/', indicator,'/output/',run_date,'/')) 
    id_files <- list.files(pattern = '*input')
    id_files <- id_files[grepl("csv", id_files)]
    
    if(grepl("\\+", reg)){
      reg2 <- gsub('+', '\\+', reg, fixed = TRUE)
    } else {
      reg2 <- reg
    }
    
    if(length(id_files) == 1){
      id_data <- read.csv(id_files, stringsAsFactors = FALSE)
    } else {
      id_files <- id_files[grep(reg2, id_files)]
      id_data <- read.csv(id_files, stringsAsFactors = FALSE)
    }
    
    id_data <- data.table(id_data)
    id_data$prop <- id_data[[indicator]]/id_data[["N"]]
    # id_data <- id_data %>% 
    #   group_by(nid, country, year) %>% 
    #   summarize(value = weighted.mean(x = prop, w = weighted_n),
    #             weight = sum(weighted_n))
    id_data <- id_data[,.(value = weighted.mean(x = prop, w = weighted_n),
                           weight = sum(weighted_n)),
                        by = c('nid', 'country', 'year')]
    
    #link the input data to the adm0 name
    # Add country name (not just 3 letter abbreviation) to input data
    # Subset input data to given region defined by the regions argument 
    gaul_codes <- get_adm0_codes(reg, shapefile_version = modeling_shapefile_version) 
    
    gaul_to_loc_id <- 
      get_location_code_mapping(shapefile_version = modeling_shapefile_version) %>% 
      dplyr::select(GAUL_CODE, loc_name, ihme_lc_id) %>% 
      dplyr::rename(location_name = loc_name) %>%
      filter(GAUL_CODE %in% gaul_codes)
    
    id_data <-  merge(id_data, gaul_to_loc_id, by.x = ('country'), by.y = 'ihme_lc_id', all.x = T)
    colnames(id_data)[colnames(id_data) == 'location_name'] <- 'ADM0_NAME'
    
    # Initialize a list to store yearly results
    yrly_data <- list()
    
    # Loop over rasterbricks and within them years as represented
    # by layers and output population weighted aggregates
    for (j in 1:length(child_mod_ras)) {
      cov <- names(child_mod_ras)[j]
      message(cov)
      raster <- child_mod_ras[[j]]
      
      cov_data <- list()
      for (i in 1:(end_year-start_year+1)) {
        answers <- lapply(subdata[['ADM0_CODE']], agg_raster, 
                          raster = raster[[i]], 
                          weights = pop[[i]],
                          shapefile = subset_shape,
                          field = 'ADM0_CODE')
        value <- unlist(answers)
        
        # Define year where rasterbrick is organized in increasing years and the
        # first layer is assumed to be associated with year 2000
        year <- i + start_year-1
        cov_data[[i]] <- cbind(value, year, cov, subdata)
      }
      yrly_data[[j]] <- do.call(rbind, cov_data)
    }
    
    # Bind rows of yrly_data to create a data frame of results and write to file
    output_data <- do.call(rbind, yrly_data)
    
    stacker_lines <- list()
    
    for (i in unique(output_data$ADM0_NAME)) {
      
      stacker_lines[[length(stacker_lines)+1]] <- ggplot() + 
        geom_line(data = filter(output_data, ADM0_NAME == i),
                  aes(x = year, y = value, col = cov)) + 
        geom_point(data = filter(id_data, ADM0_NAME == i), alpha = 0.75, colour = '#fdae6b',
                   aes(x = year, y = value,
                       size = weight)) +
        #	geom_text_repel(data = filter(id_data, ADM0_NAME == i), 
        #		aes(x = year, y = value,
        #			label = nid)) +
        ylim(0, 1) +
        theme_bw() +
        ggtitle(i)
      
    }
    
    
    pdf(paste0(out_dir, '/stackers_', reg, '.pdf'), width = 12.44, height = 7)
    print(
      ggarrange(plotlist = stacker_lines, nrow = 2, ncol = 3)
    )
    dev.off()
    
  }
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Calculate the correlation between      # 
# covariates and output the R as a table #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

covariate_corr_matrix <- function(indicator = indicator,
                                  indicator_group = indicator_group,
                                  run_date = run_date,
                                  regions = regions,
                                  out_dir = outputdir){
  
  dir.create(out_dir, showWarnings = F)
  for(reg in regions){
    
    #read in covariates
    load(paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/model_image_history/",
                run_date, "_bin0_", reg, "_0.RData"))
    
    #restrict to covariates
    covlist <- cov_list[5:length(cov_list)]
    
    #convert to matrices
    vlist <- list()
    
    for(i in 1:length(covlist)){
      vlist[[i]] <- as.vector(covlist[[i]][[1]])
    }
    
    # calculate correlation matrix
    cor.matrix <- matrix(,nrow = length(covlist), ncol = length(covlist))
    for(i in 1:length(vlist)){
      for(j in 1:length(vlist)){
        r <- cor.test(vlist[[i]], vlist[[j]], method = 'pearson')
        cor.matrix[i, j] <- round(r$estimate,2)
        if(i==j){cor.matrix[i, j] <- NA}
      }
    }
    
    rownames(cor.matrix) <- names(covlist)
    colnames(cor.matrix) <- names(covlist)
    
    pdf(paste0(out_dir, '/', reg, '.pdf'),
        width = 20, height = 7)
    print(
      ggtexttable(cor.matrix)
    )
    dev.off()
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot out admin level maps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
admin_maps <- function(adm_levels = c(0, 1, 2),
                       run_date,
                       indicator,
                       indicator_group,
                       years = seq(1990, 2015, 5),
                       outdir = outputdir, 
                       high_bad = T){
  
  library(ggplot2)
  library(viridis)
  background <- st_read('/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_0.shp')
  background <- st_simplify(background, dTolerance = 0.1, preserveTopology = T)
  
  for(i in adm_levels){
    message(paste0('Plotting admin ', i, ' maps'))
    mydata <- read.csv(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/pred_derivatives/admin_summaries/', indicator, '_admin_', i, '_unraked_summary.csv'), stringsAsFactors = F)
    mydata <- mydata[!is.na(mydata$mean),]
    shp <- st_read(paste0('/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_', i, '.shp'))
    shp <- st_simplify(shp, dTolerance = 0.1, preserveTopology = T)
    
    plot_data <- merge(shp, mydata, all.x = F, all.y = T)
    plot_data <- plot_data[plot_data$year %in% years,]
    
    ## Plot 5 year estimates 
    png(paste0(outdir, '/admin_', i, '_map.png'),
        height = 30, width = 20, units = 'cm', res = 300)
   print(
     ggplot()+
      geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0)+
      geom_sf(data = plot_data, aes(fill = mean),colour = 'black', size = 0)+
      geom_sf(data = background, fill = NA, colour = 'black', size = 0.15)+
      theme_bw()+
      theme(line = element_blank(),
            axis.text = element_blank())+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_fill_viridis(option='inferno', discrete = F, direction = ifelse(high_bad == T, -1, 1), limits = c(0, 1))+
      labs(fill = indicator)+
      facet_wrap(~year, ncol = floor(sqrt(length(years))))) 
    dev.off()
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                         END                                                                #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
