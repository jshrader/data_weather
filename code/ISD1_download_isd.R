## R code to download and process NOAA weather station data. The code downloads
## a list of all current and former NOAA weather stations, selects data for
## available stations, downloads it, and prepares it for use with other code
## like spatial averaging files.
##
## Jeff Shrader
## 2016/7/12
## Time-stamp: "2016-07-23 21:41:39 jgs"

## Set options
begin_year <- 2004
end_year <- 2015
external_dir <- "/media/jgs/datadrive/data/weather/isd/"

## Initialize and load packages
options(echo=TRUE)
ptm <- proc.time()
packages <- c("sp","rgeos","rgdal","spdep","fields","MBA","data.table")
lapply(packages, library, character.only = TRUE)

setwd("~/Dropbox/research/data/weather/data/isd")

## Get list of stations
file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
repeat {
    try(download.file(file, "isd-history.csv",
                      quiet = TRUE))
    if (file.info("isd-history.csv")$size > 0) {
        break
    }
}

st <- data.table(read.csv("isd-history.csv"))
## Clean up stations
names(st)[c(3, 9)] <- c('name','elev')
names(st) <- tolower(names(st))
## Keep just the US
st <- st[ctry=='US']
## Format variables
st[elev < -900, elev := NA]
## Month and day information is irrelevant for downloading files since NOAA
## stores them annually
st[, begin := as.numeric(substr(begin, 1, 4))]
st[, end := as.numeric(substr(end, 1, 4))]
## This is still missing a few state records. Visual inspection shows that many
## of them are bouy or coast guard stations. Some seem to be land-based records
## that have simply been miscoded, but I will exclude all of them to cut down
## on processing time.
st <- st[state!='']

## Get the weather data for each station
## I will be building a dataset for each year, since that is the temportal
## resolution of NOAA's decision to include a station in a file or not.
setkey(st,state,begin,end)
write.csv(st, "isd_history_usa.csv",row.names=FALSE)

cat("Getting ready to download files.")
for(y in seq(begin_year,end_year,by=1)){
    ## Get a list of all stations to query this year
    station_list <- st[begin <= y & end >= y]
    ## Preallocate a file download status matrix
    outputs <- as.data.table(cbind(matrix("", dim(station_list)[1], 1),matrix(99, dim(station_list)[1], 1)))
    names(outputs) <- c("file","status")
    for(s in 1:dim(station_list)[1]){
        ## Write the wget calls
        outputs[s, file:=paste(sprintf("%06d", station_list[s]$usaf), "-", sprintf("%05d", station_list[s]$wban), "-", y, ".gz", sep="")] 
        wget <- paste0("wget -P ", external_dir, " ftp://ftp.ncdc.noaa.gov/pub/data/noaa/",
                       y, "/", outputs[s]$file)
        outputs[s, status:=try(system(wget, intern=FALSE, ignore.stderr=TRUE))]
    }
    outputs$year <- y
    if(y == begin_year){
        outputs_full <- outputs
    } else {
        outputs_full <- rbind(outputs, outputs_full)
    }
    print(y)
}
saveRDS(outputs_full, file='isd_download_status.rds')
##outputs_full
cat("All done. Check the status of downloads in isd_download_status.rds.")

