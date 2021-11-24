## R code to calculate average weather in geographic region from ECMWF ERA5 
## daily files
##
## Code by Gabriel Gonzolez Sutil and Jeff Shrader
## 2020-12-17
##
## To do:
## . Population or other weighting.
## . Currently, max and min values are calculated from UTC midnight to midnight. It might make
##   more sense to use some local time. India is all in one time zone, 5.5 hours ahead, so
##   the current code is like calculating an early morning day t to early morning day t+1 value.
##   The min will either be from that morning (day t) or possibly the next morning (midnight to
##   5:30 am day t+1). Max should be recorded on the correct day.
## . For routine updates to this code, it would be more efficient to produce
##   single year files. Currently, all files from all months are processed at
##   once and put into a big file.
##
## Sources:
## ERA5
## https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview
## Muñoz Sabater, J., (2019): ERA5-Land hourly data from 1981 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). (<date of access>), 10.24381/cds.e2161bac
## Shapfiles
## From my project with Namrata


# Preliminaries -----------------------------------------------------------
rm(list = ls())
packages <- c('sp','rgeos','grid','sf','reticulate',
              'stringr','rgdal','raster','maptools',
              'parallel','lubridate','data.table','tictoc','pbmcapply','haven')
new_packages <- packages[!(packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

# Bounds for GDD  ---------------------------------------------------------
bounds <- c(0, 10, 20, 30, 32, 34, 36, 37, 38, 39, 40, 41, 42)

# Define Directory  -------------------------------------------------------
era_dir <- '/media/jgs/datadrive/data/weather/ecmwf/era5land/'
base_dir <- "~/Dropbox/research/projects/active/"
proj_dir <- paste0(base_dir, "vertical_integration_uncertainty/")
map_dir <- paste0(proj_dir, "data/district_shapefiles/SDE_DATA_IN_F7DSTRBND_2011/SDE_DATA_IN_F7DSTRBND_2011/")
graph_dir <- paste0(proj_dir, "output/graph/data_creation/")
setwd(era_dir)

# Import Shape Files  -----------------------------------------------------
setwd(map_dir)
# Import the Shape File for district
dist_shp <- shapefile(paste0(map_dir,"SDE_DATA_IN_F7DSTRBND_2011.shp"))

# Load data on grid membership -------------------------------------------
mem <- readRDS(file=paste0(era_dir,"output_india/era5land_india_grid_membership.rds"))

# Import Climate File ----------------------------------------------------
variables <- c('2m_temperature','total_precipitation')
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
    varcols <- grep(pattern="era5land",x=names(temp),value=TRUE)
    # Convert to celscius
    if( i == "2m_temperature" | i == "2m_dewpoint_temperature"){
      temp <- cbind(temp[, .(x,y)], temp[, .SD, .SDcols=varcols] - 272.15)
    }
    # Get cell numbers for merge. I would like to use x,y, but it taking forever
    # to do the relevant conversion.
    temp[, cell:=1:.N]
    # Merge with admin area membership
    temp <- merge(mem, temp, by=c("cell"))
    # ERA5-Land is hourly. We want a daily dataset with max, min, mean temp. I download
    # the files (in ERA1_...) in month month blocks.
    ym <- str_extract(pattern="\\d\\d\\d\\d\\d\\d", string=file)
    date <- ymd(paste(substr(ym, start=1, stop=4), substr(ym, start=5, stop=6), "01", sep="-"))
    days <- as.character(1:days_in_month(date))
    dates <- ymd(paste(substr(ym, start=1, stop=4), substr(ym, start=5, stop=6), days, sep="-"))
    times <- paste0(rep(dates, each=24), " ", rep(as.character(0:23), times=length(dates)),":00:00")
    dh <- ymd_hms(times)
    names(temp)[8:(7+length(dh))] <- as.character(dh)

    # Method 1: reshape and use by
    idvars <- c("cell","ID","id","district_name","c_code11","x","y")
    temp_m <- melt(temp, id.vars=idvars)
    temp_m[, date:=substr(variable, 1, 10)]
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
    if(i == "total_precipitation"){
      # Units are meters per hour. We could convert to mm by multiplying by 1000
      temp_a[, ppt:=avg]
      #temp_a[, min:=min*60*60]
      #temp_a[, max:=max*60*60]
      temp_a[, avg:=NULL]
    } else if (i == "2m_temperature"){
      # Growing degree days two ways
      for(b in bounds){
        # Schlenker-Roberts method
        temp_a[max <= b, paste0("dday_sr_",b):=0]
        temp_a[b <= min, paste0("dday_sr_",b):=avg_sr - b]
        temp_a[(min < b) & (max > b), temp_sr:=acos((2*b - max - min)/(max - min))]
        temp_a[(min < b) & (max > b), paste0("dday_sr_",b):=((avg_sr - b)*temp_sr + (max - min)*sin(temp_sr)/2)/pi]
        temp_a[, temp_sr:=NULL]
        
        # Direct summation
        # Given that we have hourly data, we could also just sum the hours above given thresholds.
        # The SR method is nice in that it works on reduced dimension data, and they have shown
        # that it is more accurate for predicting agricultural outcomes. For higher frequency
        # outcomes, direct summation might be better.
        # We need to make copies because we do multiple bounds
        #tt <- melt(temp[, .SD, .SDcols=c("x","y",varcols)], id.vars=c("x","y"))
        #tt[value <= b, nvalue:=0]
        #tt[value > b, nvalue:=value-b]
        #tt <- tt[, sum(nvalue)/24, by=c("x","y")]
        #names(tt)[3] <- paste0("dday_",b)
        #temp <- merge(temp, tt, by=c("x","y"), all.x=TRUE)
      }
    }
    temp_a[, avg_sr:=NULL]    
    # Keep just the aggregates
    temp_a[, c("cell","id"):=NULL]
    temp_a[, c("x","y","district_name","c_code11"):=NULL]
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
  if(i == "2m_temperature"){
    setnames(outw, old="avg", new="tavg")
    setnames(outw, old="max", new="tmax")
    setnames(outw, old="min", new="tmin")
  } else if (i == '2m_dewpoint_temperature'){
    setnames(outw, old="avg", new="dewtavg")
    setnames(outw, old="max", new="dewtmax")
    setnames(outw, old="min", new="dewtmin")
  } else if (i == "total_precipitation"){
    setnames(outw, old="max", new="ppt_hourlymax")
    setnames(outw, old="min", new="ppt_hourlymin")
  }
  #fwrite(outw, file = paste0(era_dir,"output/pakistan_daily_",i,'.csv'))
  write_dta(outw, path = paste0(era_dir,"output_india/era5land_india_daily_",i,'.dta'))
}

# EOF
# Testing aggregate data map
#library('ggplot2')
#ggplot(data=outw[date=="1990-01-04"], aes(x=x,y=y, color=ppt)) + geom_point() +
#  geom_text(aes(label=district_name),hjust=0, vjust=0) + 
#  scale_color_gradient(low = "yellow", high = "red") + theme_void(base_size=20)
