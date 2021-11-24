## R code to calculate average weather in geographic region from ECMWF ERA5 
## daily files
##
## 2021-03-05
##
## To do:
##
## Sources:
## ERA5
## Shapfiles
## From my project with Namrata


# Preliminaries -----------------------------------------------------------
rm(list = ls())
packages <- c('sp','rgeos','grid','sf','reticulate',
              'stringr','rgdal','raster','maptools',
              'parallel','lubridate','data.table',
              'tictoc','pbmcapply','haven')
new_packages <- packages[!(packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

# Toggles and Switches -----------------------------------------------------
# Set these to match what you downloaded from Copernicus in the previous step
country_name <- "india"
# If you want to create graphs to check that your output files make sense, set
# this to TRUE. Otherwise FALSE.
check_file <- FALSE

# Define Directories  -------------------------------------------------------
# Many of these might be the same thing depending on your environment
# Project directory (top level)
base_dir <- "~/Dropbox/research/projects/active/vertical_integration_uncertainty/"
# ERA data directory
era_dir <- '/media/jgs/datadrive/data/weather/ecmwf/era5/'
# Place where shapefiles are stored
map_dir <- paste0(base_dir, "data/district_shapefiles/SDE_DATA_IN_F7DSTRBND_2011/SDE_DATA_IN_F7DSTRBND_2011/")
# Place where you want maps (for debugging and data checking) to go
graph_dir <- paste0(base_dir, "output/graph/data_creation/")
# Place where output data should go
data_dir <- paste0(base_dir, "data/") 

# Import Shape Files  -----------------------------------------------------
setwd(map_dir)
# Import the Shape File for district
dist_shp <- shapefile(paste0(map_dir,"SDE_DATA_IN_F7DSTRBND_2011.shp"))

# Load data on grid membership -------------------------------------------
fname <- paste0(data_dir,"era5/2m_temperature_ensemble_spread_india_grid_membership.rds")
mem <- readRDS(file=fname)

# Import Climate File ----------------------------------------------------
'2m_temperature_ensemble_spread',
variables <- c('total_precipitation_ensemble_spread')
for (i in variables){ 
  folder <- i
  file_list <- list.files(paste0(era_dir,folder,"_india"), pattern=".grib", full.names=TRUE)
  #for (n in 1:length(file_list)){
  ## Function starts here
  loop_f <- function(n, file_list, mem, bounds, dist_shp){
    file <- file_list[n]
    print(file)
    data <- brick(paste0(file))
    crsData <- crs(data)
    resData <- res(data)
    projection(data) <- dist_shp@proj4string
    temp <- as.data.table(as.data.frame(data, xy=TRUE))
    varcols <- grep(pattern="era5",x=names(temp),value=TRUE)
    # Get cell numbers for merge. I would like to use x,y, but it taking forever
    # to do the relevant conversion.
    temp[, cell:=1:.N]
    # Merge with admin area membership
    temp <- merge(mem, temp, by=c("cell"))
    # ERA5 is hourly or 3 hourly. We want a daily dataset
    ym <- str_extract(pattern="\\d\\d\\d\\d\\d\\d", string=file)
    date <- ymd(paste(substr(ym, start=1, stop=4), substr(ym, start=5, stop=6), "01", sep="-"))
    days <- as.character(1:days_in_month(date))
    dates <- ymd(paste(substr(ym, start=1, stop=4), substr(ym, start=5, stop=6), days, sep="-"))
    times <- paste0(rep(dates, each=24/3), " ", rep(as.character(seq(0,23, by=3)), times=length(dates)),":00:00")
    dh <- ymd_hms(times)
    names(temp)[8:(7+length(dh))] <- as.character(dh)

    # Method 1: reshape and use by
    idvars <- c("cell","ID","id","district_name","c_code11","x","y")
    temp_m <- melt(temp, id.vars=idvars)
    temp_m[, date:=substr(variable, 1, 10)]
    if(i == "total_precipitation_ensemble_spread"){
      # ERA5 precip for this dataset is in cumulative meters over an hour. We
      # want everything in mm/hour, then we will calculate hourly cumulative
      temp_m[, value:=value]
    }
    temp_a <- temp_m[, .(max=max(value, na.rm=TRUE), min=min(value, na.rm=TRUE), avg=mean(value, na.rm=TRUE)), by=c(idvars,"date")]
    temp_a[max==-Inf, max:=NA]
    temp_a[min==Inf, min:=NA]
    temp_a[, avg_sr:=(max+min)/2]
    # Method 2: Loop over days
    # Interestingly, using this loop method with pmax/min, there is only about 40% overhead
    # compared with by calls on the reshaped dataset. If you weren't going to reshape the data,
    # this method would be much faster.
    #test2 <- copy(temp)
    #tic()
    #for(d in 1:days_in_month(date)){
    #  pat <- as.character(ymd(paste(substr(ym, start=1, stop=4), substr(ym, start=5, stop=6), d, sep="-")))
    #  varcols_day <- grep(pattern=pat,x=names(temp),value=TRUE)
    #  test2[, paste0("max",pat):=do.call(pmax, .SD), .SDcols=varcols_day]
    #}
    #toc()
    if(i == "total_precipitation_ensemble_spread"){      
      temp_a[, ppt:=avg]
      temp_a[, avg:=NULL]
    } else if (i == "2m_temperature_ensemble_spread"){
    }
    temp_a[, avg_sr:=NULL]    
    # Keep just the aggregates
    temp_a[, c("cell","id"):=NULL]
    temp_a[, c("district_name","c_code11"):=NULL]
    # Area average
    avg_weather <- temp_a[, lapply(.SD, mean, na.rm=TRUE), by=c("ID","date")]
    return(avg_weather)
  }

  Lf <- length(file_list)
  ncores <- 30
  tic(paste0("Code for ",i))
  outw <- rbindlist(pbmclapply(1:Lf, FUN=loop_f, file_list=file_list, mem=mem, bounds=bounds, dist_shp=dist_shp, mc.cores=ncores, ignore.interactive=TRUE))
  toc()
  # Rename
  if(i == "2m_temperature_ensemble_spread"){
    setnames(outw, old="avg", new="tavg_es")
    setnames(outw, old="max", new="tmax_es")
    setnames(outw, old="min", new="tmin_es")
  } else if (i == "total_precipitation_ensemble_spread"){
    setnames(outw, old="max", new="ppt_hourlymax_es")
    setnames(outw, old="min", new="ppt_hourlymin_es")
    setnames(outw, old="ppt", new="ppt_es")
  }
  #fwrite(outw, file = paste0(era_dir,"output/pakistan_daily_",i,'.csv'))
  write_dta(outw, path = paste0(era_dir,"output_india/era5_india_daily_",i,'.dta'))
}

# EOF
# Testing aggregate data map
library('ggplot2')
ggplot(data=outw[date=="1993-01-05"], aes(x=x,y=y, color=ppt_es)) + geom_point() +
  scale_color_gradient(low = "yellow", high = "red") + theme_void(base_size=20)
