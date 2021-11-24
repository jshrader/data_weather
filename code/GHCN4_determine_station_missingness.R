## R code to determine how many missing values there are in each station.
##
## Jeff Shrader
## First: 2021-11-24
## Latest: Time-stamp: "2021-11-24 18:17:11 jgs"
##
## This borrows heavily from GHCN2_daily_process.R
rm(list = ls())
## Switches
debug <- FALSE
rebuild <- TRUE

## Set the year range you want to process
begin_year <- 2000
end_year <- 2020

## Preliminaries
options(echo=TRUE)
ptm_total <- proc.time()
packages <- c("sp","rgeos","stringr","rgdal","raster","haven","data.table",
  "readr","iotools","phylin","plm","lubridate","geosphere","collapse",
  "tictoc","parallel","pbmcapply")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))
## Directories
external_dir <- "/media/jgs/datadrive/data/weather/ghcn/daily/"
data_dir <- "~/Dropbox/research/data/weather/data/ghcn/"
##data_dir <- "C:/Users/jgs/Dropbox/research/data/weather/data/ghcn/"
map_dir <- paste0('~/Dropbox/research/data/maps/usa/')
##map_dir <- paste0('C:/Users/jgs/Dropbox/research/data/maps/usa/')
county_dir <- paste0(map_dir,'county/gz_2010_us_050_00_500k/')
tz_dir <- paste0(map_dir,'time_zone/tz_us/')

# Read the station data prepared by download_ghcn.R
st <- fread(paste0(data_dir,"ghcnd_stations_usa_countytz.csv"),
  colClasses=list(character=c("id","state","tzid","state_fips","county_fips","csafp","cbsafp"),
    numeric=c("lat","lon","elev","county_centroid_lon","county_centroid_lat","dist_to_centroid")))
st <- st[lat > 21 & lat < 53 & lon < -61 & lon > -127]
setkey(st,id)
st[, c("gsn_flag","hcn_crn_flag","wmo_id","name","lsad","censusarea"):=NULL]
st[, c("state","tzid","county_centroid_lon","county_centroid_lat","dist_to_centroid","csafp","cbsafp"):=NULL]

## unzip the relevant files
files <- seq(begin_year,end_year,by=1)
## For a more nuanced list of files (if you have already run some years)
if(rebuild == FALSE){
  already_run <-  as.numeric(substr(list.files(path=paste0(external_dir, "station_missing/"), pattern=".rds"), start=1, stop=4))
  files <- files[which(!files %in% already_run)]
}
setwd(paste0(external_dir,"station_missing/"))
getwd()
## For each year, find the missing values from all stations, recoding county location
func <- function(i, file=files){
    cat(paste("Starting ",file[i],"\n"))
    system(paste0("gunzip ",external_dir,"raw/",file[i],".csv.gz"), intern = FALSE, ignore.stderr = FALSE)
    data <- fread(paste0(external_dir,"raw/",file[i],".csv"),header=FALSE,colClasses=c('character','character','character','numeric','character','character','character','integer'))
    ## I want to keep prcp, tmax, tmin, tavg, snow
    # Other variables are included but are uncommon. Humidity and dew point are not
    # included in the file for some reason.
    # For a description of the data elements, see
    # https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
    names(data) <- c('id','date','element','value','mflag','qflag','sflag','obs_time')
    setkey(data,element)
    data <- data[c('PRCP','TMAX','TMIN')]
    data[, date_fmt:=ymd(date)]
    data[, date_string:=date]
    data[, date:=date_fmt]
    ## Drop observations that have quality flags
    data <- data[qflag=='']
    data[,c("qflag","sflag","mflag","obs_time","date_fmt"):=NULL]
    # Temperature is recorded without decimal points
    data[element %in% c('TMAX','TMIN'),value:=value/10]
    setkey(data,id)
    # Observations should be daily, but this would check it
    # dim(data)[1] == dim(unique(data[, .(id, date, element)]))[1]
    # saveRDS(data_full, file=paste0(external_dir, "station_missing/", file[i],"_county.rds"))
    # Expand the panel to make sure we observe every day
    data_w <- dcast(data, formula=id + date ~ element)
    # Cross joins to balance the table. data.table is so cool...
    # test <- data.table(ID=c(1,1,1,2,2,3), date=c(1,2,3,1,2,3), val=c(3,2,4,1,4,2))
    data_w <- setDT(data_w, key = c("id", "date"))[CJ(id, date, unique=TRUE)]
    data_w <- merge(data_w,st,by='id')
    names(data_w) <- tolower(names(data_w))
    # Non-missing obs
    data_w[!(is.na(tmax) | is.na(tmin) | is.na(prcp)), obs:=1]
    # In some cases, we have two weather stations at the exact same lon/lat.
    # Let's keep the one with the more complete records.
    data_w[, id2:=.GRP, by=c("lon","lat")]
    data_w[, count:=.N, by=c("id2","date")]
    max_count <- max(data_w$count)
    dim(data_w)
    if(max_count > 1){
      data_w[, good:=1]
      data_w[, good:=ifelse(is.na(prcp), 0, 1)]
      data_w[, good:=ifelse(is.na(tmax) | good==0, 0, 1)]
      data_w[, good:=ifelse(is.na(tmin) | good==0, 0, 1)]
      data_w[, good_obs:=ifelse(obs==max(obs), 1, 0), by=c("id2","date")]
      data_w[, quality:=(mean(good,na.rm=TRUE)+mean(good_obs,na.rm=TRUE))/2, by=c("id")]
      data_w[, max_quality:=max(quality), by=c("id2")]
      # Keep the better one
      data_w <- data_w[count == 1 | quality==max_quality]
      # If there are any ties left, keep a random one
      data_w[, count:=.N, by=c("id2","date")]
      max_count <- max(data_w$count)
      if(max_count > 1){
        rand <- runif(length(unique(data_w$id)))
        select <- data.table(cbind(rand, unique(data_w$id)))
        data_w <- merge(data_w, select, by.x=c("id"), by.y=c("V2"))
        data_w[, max_rand:=max(rand), by=c("id2")]
        data_w <- data_w[count == 1 | rand==max_rand]
        data_w[, c("rand", "max_rand"):=NULL]
      }
      data_w[, c("good", "good_obs", "quality", "max_quality"):=NULL]
    }
    data_w[, c("count","id2"):=NULL]
    dim(data_w)
    ## If you want, at this point you can calculate spatial autocorrelation between
    # stations. This is slow, even using the "moranfast" function. I suggest
    # subsetting the data to <100,000 rows, which should take roughly 1 minute.
    #library(moranfast)
    #rand_dates <- sample(unique(data_w$date), 3)
    #mor <- data_w[(date %in% rand_dates)  & !(is.na(tmax) | is.na(tmin) | is.na(prcp)), .(tmax, tmin, prcp, lon, lat)]
    #moranfast(mor$tmax, mor$lon, mor$lat)
    #moranfast(mor$tmin, mor$lon, mor$lat)
    #moranfast(mor$prcp, mor$lon, mor$lat)

    ## Find the number of missing and non-missing values in each county
    data_out <- data_w[,.(tmax_miss=sum(is.na(tmax)),tmax_nomiss=sum(!is.na(tmax)),
      tmin_miss=sum(is.na(tmin)),tmin_nomiss=sum(!is.na(tmin)),
      ppt_miss=sum(is.na(prcp)),ppt_nomiss=sum(!is.na(prcp))),
      by="date,state_fips,county_fips"]
    data_out[, tmax_frac_miss:=tmax_miss/(tmax_miss+tmax_nomiss)]
    data_out[, tmin_frac_miss:=tmin_miss/(tmin_miss+tmin_nomiss)]
    data_out[, ppt_frac_miss:=ppt_miss/(ppt_miss+ppt_nomiss)]
    saveRDS(data_out, file=paste0(external_dir,"station_missing/annual/",file[i],"_county.rds"))
    # Zip it back up
    system(paste0("gzip ",external_dir,"raw/",file[i],".csv"), intern = FALSE, ignore.stderr = TRUE)
}

# Run for all years
tic()
i <- 1:length(files)
dt <- pbmclapply(i, func, mc.cores = 16)
toc()

## Put all of the annual files together
file_list <- list.files(paste0(external_dir,"station_missing/annual/"),pattern="\\d{4}.county.rds",full.names=TRUE)
tic()
setattr(file_list, "names", basename(file_list))
dt <- rbindlist(pbmclapply(file_list, readRDS, mc.cores = 16))
toc()
dt[, fips := paste0(state_fips, county_fips)]
dt[, c("state_fips","county_fips"):=NULL]
## Save
write_dta(dt,path=paste0(data_dir,"ghcn_missing_county_daily.dta"))
# Note that the resulting dataset won't necessarily be a balanced panel. It should be
# balanced within year, but if a station is introduced across years, then it won't show
# up for the years before introduction. 
# EOF
