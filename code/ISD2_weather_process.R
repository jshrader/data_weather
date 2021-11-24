## R code to find average weather from ISD stations inside different spatial
## boundaries. ISD data are global, station records at an hourly frequency.
## The data are particularly good from 1975 to the present, and the 
##
## Jeff Shrader
## First: 2016-7-15
## Latest: Time-stamp: "2017-01-16 16:35:32 jgs"

## Preliminaries
## rm(list = ls())
## Load packages
options(echo=TRUE)
ptm_total <- proc.time()
packages <- c("sp","rgeos","stringr","rgdal","raster","foreign","data.table","readr")
lapply(packages, library, character.only = TRUE)

## Debug switch
debug <- TRUE
create_stations <- FALSE

## Directories
dir <- '~/Dropbox/research/'
external_dir <- '/media/jgs/datadrive/data/weather/isd'
map_dir <- paste0(dir,'data/maps/usa/')
county_dir <- paste0(map_dir,'county/gz_2010_us_050_00_500k/')
tz_dir <- paste0(map_dir,'time_zone/tz_us/')
weather_dir <- paste0(dir,'data/weather/data/')

setwd(paste0(weather_dir,"isd"))

if(create_stations == TRUE){
    ## Read timezone shapefile
    tz <- shapefile(paste0(tz_dir,"tz_us.shp"))
    ## Read station list for the united states
    ## this file is processed by download_noaa_station_data.R.
    st <- data.table(read_csv(paste0(weather_dir,"isd/isd_history_usa.csv")))
    dim(st)
    st <- st[is.na(lat)==FALSE & is.na(lon)==FALSE]
    dim(st)
    ## Add timezone
    sp <- SpatialPoints(cbind(st$lon,st$lat), proj4string=tz@proj4string)
    spst <- SpatialPointsDataFrame(sp, st)
    tzid <- over(spst, tz)
    st <- data.table(cbind(st,tzid))
    ## Drop locations with missing timezone ID. These are offshore or islands,
    ## as far as I can tell.
    st <- st[is.na(TZID)==FALSE]
    dim(st)
    
    ## Build lists of stations by county for use when reading the weather files
    ## Grab county spatial data
    county <- shapefile(paste0(county_dir,"gz_2010_us_050_00_500k.shp"))
    sp <- SpatialPoints(cbind(st$lon,st$lat), proj4string=county@proj4string)
    spst <- SpatialPointsDataFrame(sp, st)
    county_id <- over(spst, county)
    st <- data.table(cbind(st,county_id))
    ## Assess fit quality
    ##st[is.na(COUNTY)]
    dim(st)
    ## Some locations don't have counties. I checked, and the issue is largely
    ## due to low resolution on the station list. 
    st <- st[is.na(COUNTY)==FALSE]
    dim(st)
    ## Renaming
    names(st)[c(14,15)] <- c('state_fips','county_fips')
    names(st) <- tolower(names(st))
    st$name <- NULL
    ## geo_id is just fips with a common US prefix
    st$geo_id <- NULL
    write.csv(st, "isd_history_usa_countytz.csv",row.names=FALSE)
} else {
    st <- data.table(read_csv("isd_history_usa_countytz.csv"))
}

##trueCentroids = gCentroid(sids,byid=TRUE)
##plot(sids)
##points(coordinates(sids),pch=1)
##points(trueCentroids,pch=2)

## unzip the relevant files
if(debug == TRUE){
    ##file <- "999999-96406-2015"
    file <- "999999-96406-2014"
    file <- "722909-93115-2009"
}

## Pre-allocate the R formatted data
columns <- fwf_widths(c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                   7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6,
                   1, 1, 1, 5, 1, 5, 1, 5, 1))
## c = character, i = integer, n = number, d = double, l = logical, D = date, T =
## date time, t = time, ? = guess, or ‘_’/‘-’ to skip the column.
##                                    10                                  19
##4    10  15 19  21  23  25  27  28  34  41  46  51  56  60  63  64  65  69
##'-','c','i','i','i','i','i','i','-','i','i','-','i','-','-','i','-','-','i'
##70  75  76  77  78  84  85  86  87  92  93  98  99 104 105
##'-','i','-','-','-','i','-','-','-','i','-','i','-','i','-'
column_types <- cols(X1='-',X2='c',X3='i',X4='c',X5='c',X6='c',X7='c',X8='c',
                     X9='-',X10='-',X11='-',X12='-',X13='-',X14='-',X15='-',
                     X16='i',X17='-',X18='-',X19='i',X20='-',X21='i',X22='-',
                     X23='-',X24='-',X25='i',X26='-',X27='-',X28='-',X29='i',
                     X30='-',X31='i',X32='-',X33='i',X34='-')
##data$datetime <- as.POSIXct(paste0(data$year,"-",data$month,"-"))


columns <- fwf_widths(c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                   7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6,
                   1, 1, 1, 5, 1, 5, 1, 5, 1,
                   3, 3, 2, 4, 1, 1, 1))
column_types <- cols(X1='-',X2='c',X3='i',X4='c',X5='c',X6='c',X7='c',X8='c',
                     X9='-',X10='-',X11='-',X12='-',X13='-',X14='-',X15='-',
                     X16='i',X17='c',X18='-',X19='i',X20='c',X21='i',X22='c',
                     X23='-',X24='-',X25='i',X26='c',X27='-',X28='-',X29='i',
                     X30='c',X31='i',X32='c',X33='i',X34='c',
                     X35='c',X36='c',X37='c',X38='c',X39='c',X40='c',X41='-')

setwd(external_dir)
## Here, pattern can be a regular expression. In full version, make files equal
## the locations based on stations in each state
##files <- list.files(path=".", pattern=file)
ptm_files <- proc.time()
for(i in 1:length(file)){
    cat(paste(file[i],"\n"))
    system(paste0("gunzip ",file[i],".gz"), intern = FALSE, ignore.stderr = FALSE)
    ## readr's read_fwf is so fast. Amazing.
    ## A potentially slightly faster version is iotools
    ## http://stackoverflow.com/questions/24715894/faster-way-to-read-fixed-width-files-in-r
    data <- data.table(read_fwf(file[i], col_positions=columns, col_types=column_types))
    system(paste0("gzip ",file[i]), intern = FALSE, ignore.stderr = TRUE)
    names(data) <- c("usafid", "wban", "year", "month", "day", "hour",
                     "minute","wind_dir", "wind_dir_q", "wind_spd", "wind_spd_q",
                     "ceiling", "ceiling_q",
                     "visibility", "visibility_q",
                     "temperature", "temperature_q", "dew_point", "dew_point_q",
                     "atm_pres", "atm_pres_q", "ADD", "precip_obs", "precip_duration",
                     "precip", "precip_condition", "precip_qual")
    ## Keep hourly data only. I am keeping on the hour. An alternative would be to
    ## take averages, but I don't see a clear benefit to that relative to the
    ## added computation time.
    data <- data[minute == "00"]
    ## Recode missing values and put everything in correct units
    data[wind_dir==999, wind_dir:=NA]
    data[wind_spd==9999, wind_spd:=NA]
    data[ceiling==99999, ceiling:=NA]
    data[visibility==999999, visibility:=NA]
    data[temperature==9999, temperature:=NA]
    data[dew_point==9999, dew_point:=NA]
    data[atm_pres==99999, atm_pres:=NA]
    data[,wind_spd:=wind_spd/10]
    data[,temperature:=temperature/10]
    data[,dew_point:=dew_point/10]
    data[,atm_pres:=atm_pres/10]
    ##write.csv(data, file = paste(file[i],  ".csv", sep = ""), row.names = FALSE)
}
cat(paste0('All files took ',round((proc.time() - ptm_files)[3],3),' seconds.\n'))

write.csv(stations, file = "data/stations.csv", row.names = FALSE)
mi.state <- readOGR("data/gis/.", "michigan", verbose = FALSE)
mi <- mi.state[mi.state$AREA > 0.01, ]
plot(mi, xlab = "Degrees", ylab = "Degrees", axes = T)
points(stations$LONG, stations$LAT, pch = 16, col = "red")


cat(paste0('All code took ',round((proc.time() - ptm_total)[3],1),' seconds.'))


pb.txt <- "2009-06-03 19:30"
pb.date <- as.POSIXct(pb.txt, tz="Europe/London")
format(pb.date, tz="America/Los_Angeles",usetz=TRUE)
