## R code to find average weather from stations inside different spatial boundaries
##
## Jeff Shrader
## First: 2016-7-15
## Latest: Time-stamp: "2021-12-02 09:40:36 jgs"
##
## Citation
## The journal article describing GHCN-Daily is:
## Menne, M.J., I. Durre, R.S. Vose, B.E. Gleason, and T.G. Houston, 2012:  An overview
## of the Global Historical Climatology Network-Daily Database.  Journal of Atmospheric
## and Oceanic Technology, 29, 897-910, doi:10.1175/JTECH-D-11-00103.1.

## To acknowledge the specific version of the dataset used, please cite:
## Menne, M.J., I. Durre, B. Korzeniewski, S. McNeal, K. Thomas, X. Yin, S. Anthony, R. Ray,
## R.S. Vose, B.E.Gleason, and T.G. Houston, 2012: Global Historical Climatology Network -
## NOAA National Climatic Data Center. http://doi.org/10.7289/V5D21VHZ [access date].

## Preliminaries
rm(list = ls())
begin_year <- 1950
end_year <- 2020
options(echo=TRUE)
ptm_total <- proc.time()
packages <- c("sp","rgeos","stringr","rgdal","raster","haven","data.table","readr","iotools","phylin","plm","lubridate","geosphere","collapse","tictoc","parallel","pbmcapply")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))
# Lag function for use with data.table
lg <- function(x)c(NA, x[1:(length(x)-1)])

## Debug switch
debug <- FALSE
## If you want to rebuild the whole dataset from scratch, set to true
rebuild <- FALSE

## Directories
external_dir <- "/media/jgs/datadrive/data/weather/ghcn/daily/"
data_dir <- "~/Dropbox/research/data/weather/data/ghcn/"
##data_dir <- "C:/Users/jgs/Dropbox/research/data/weather/data/ghcn/"
map_dir <- paste0('~/Dropbox/research/data/maps/usa/')
##map_dir <- paste0('C:/Users/jgs/Dropbox/research/data/maps/usa/')
county_dir <- paste0(map_dir,'county/gz_2010_us_050_00_500k/')
tz_dir <- paste0(map_dir,'time_zone/tz_us/')

setwd(paste0(data_dir))
## Read the station data prepared by download_ghcn.R
st <- fread(paste0(data_dir,"ghcnd_stations_usa_countytz.csv"))
st <- st[lat > 21 & lat < 53 & lon < -61 & lon > -127]
setkey(st,id)
st[, gsn_flag:=NULL]
st[, hcn_crn_flag:=NULL]
st[, wmo_id:=NULL]
st[, name:=NULL]
st[, lsad:=NULL]
st[, censusarea:=NULL]
# Get the county centroid list
county <- shapefile(paste0(county_dir,"gz_2010_us_050_00_500k.shp"))
county <- subset(county, !(STATE %in% c("15", "60", "66", "69", "72", "78", "02")))
# Keep just the continental US
centroid <- as.data.frame(gCentroid(county, byid=TRUE))
names(centroid) <- c("c_lon","c_lat")
centroid_with_id <- as.data.table(cbind(county@data[, "GEO_ID"],centroid))
names(centroid_with_id)[1] <- "geo_id"
centroid_with_id[, index:=1:.N]
centroid <- as.matrix(centroid)
# county dataset
counties <- as.data.table(county@data[, c(1,2,3)])
names(counties) <- tolower(names(counties))
names(counties)[c(2,3)] <- c("state_fips", "county_fips")

## unzip the relevant files
files <- seq(begin_year,end_year,by=1)
if(debug == TRUE){
    file <- "test"
}
## For a more nuanced list of files (if you have already run some years)
if(rebuild == FALSE){
  already_run <-  as.numeric(substr(list.files(path=external_dir, pattern="_idw_daily.dta"), start=1, stop=4))
  files <- files[which(!files %in% already_run)]
}
setwd(paste0(external_dir,"raw/"))
getwd()
## Here, pattern can be a regular expression. In full version, make files equal
## the locations based on stations in each state
##files <- list.files(path=".", pattern=file)
#for(i in 1:length(files)){
#    file <- files
    # Parellel version
func <- function(i, file=files){
    cat(paste("Starting ",file[i],"\n"))
    system(paste0("gunzip ",external_dir,"raw/",file[i],".csv.gz"), intern = FALSE, ignore.stderr = FALSE)
    # Previous version used read.csv.raw, but fread is faster. Maintaining legacy code in case something
    # breaks. identical() said the reads are the same.
    #data <- data.table(read.csv.raw(paste0(file[i],".csv"),header=FALSE,colClasses=c('character','character','character','numeric','character','character','character','integer')))
    data <- fread(paste0(file[i],".csv"),header=FALSE,colClasses=c('character','character','character','numeric','character','character','character','integer'))

    ## I want to keep prcp, tmax, tmin, tavg, snow
    # Other variables are included but are uncommon. Humidity and dew point are not
    # included in the file for some reason.
    # For a description of the data elements, see
    # https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
    names(data) <- c('id','date','element','value','mflag','qflag','sflag','obs_time')
    setkey(data,element)
    data <- data[c('PRCP','TMAX','TMIN','SNOW')]
    data[, date_fmt:=ymd(date)]
    data[, date_string:=date]
    data[, date:=date_fmt]
    ## Drop observations that have quality flags
    data <- data[qflag=='']
    data[,qflag:=NULL]
    data[,sflag:=NULL]
    # Temperature is recorded without decimal points
    data[element %in% c('TMAX','TMIN'),value:=value/10]
    setkey(data,id)
    data_full <- merge(data,st,by='id')
    rm(data)
    data_out <- data_full[,.(mean(value,na.rm=TRUE), sum(is.na(value)==FALSE)),by="date,date_string,element,state_fips,county_fips"]
    names(data_out)[c(6,7)] <- c('value','num_obs')
    ##saveRDS(data_out, file=paste0(file[i],"_county.rds"))
    write_dta(data_out, path=paste0(external_dir,file[i],"_county.dta"))
    # Create county file following the Barreca et al. (2016) method:
    # 1. Keep only stations that have complete records within a year (should be ~2000 stations currently)
    # 2. Inverse distance weight to county centroid (squared weights, 300 km cutoff)
    data_mort <- data_full[element %in% c("PRCP","TMAX","TMIN"), c("id","date","element","value","lat","lon")]
    data_mort <- dcast(data_mort, formula=id + date ~ element)
    # Test the max/min method versus average
    data_mort[, tavg:=(TMAX+TMIN)/2]
    data_mort[, obs:=.N, by=c("id","date")]
    #data_mort <- data_mort[!is.na(tavg) & !is.na(PRCP)]
    data_mort <- data_mort[!is.na(tavg)]
    data_mort[, obs:=.N, by=c("id")]
    # table(data_mort$obs)/365
    # In Barreca et al, they say that they have about 2000 stations per year with
    # all non-missing observations by the year 2000. To get to that number, I
    # need to include stations that have 1 or 2 missing values. Not a huge deal, but
    # I wonder where the discrepancy comes from?
    #
    # Allow for different cutoff during leap years
    max_obs <- max(data_mort$obs)
    data_mort <- data_mort[obs>=(max_obs-2)]
    # Interpolate the missing values
    # If you run this regression, you will find, of course, that the autocorrelation
    # is extremely high
    data_mort[, idn:=as.numeric(factor(id))]
    #data_mort <- pdata.frame(data_mort, index=c("idn","date"))
    #test <- pdata.frame(data_mort, index=c("id","date"), replace.non.finite=T)
    #pdim(data_mort)
    
    data_mort_fill <- data.table(make.pbalanced(data_mort, balance.type=c("fill"), index=c("idn","date")))
    # Fill id
    id <- unique(data_mort[, .(id, idn)])
    data_mort_fill[, id:=NULL]
    data_mort_fill <- merge(data_mort_fill, id, by=c("idn"))
    data_mort_fill[, tavg_bu:=tavg]
    # Fill missing values
    # I don't know why panel data types in R are so fickle?!?!
    # If the value that needs to be filled is at the beginning, use the lead
    min_date <- min(data_mort_fill$date)
    #missing_first <- data_mort_fill[date==min_date & is.na(tavg)]
    for(k in c(2,1,0)){
      data_mort_fill[date==(min_date+k), tavg1:=data_mort_fill[date==(min_date+k+1)]$tavg]
      data_mort_fill[date==(min_date+k) & is.na(tavg), tavg:=tavg1]
      data_mort_fill[date==(min_date+k), TMIN1:=data_mort_fill[date==(min_date+k+1)]$TMIN]
      data_mort_fill[date==(min_date+k) & is.na(TMIN), TMIN:=TMIN1]
      data_mort_fill[date==(min_date+k), TMAX1:=data_mort_fill[date==(min_date+k+1)]$TMAX]
      data_mort_fill[date==(min_date+k) & is.na(TMAX), TMAX:=TMAX1]
      data_mort_fill[date==(min_date+k), PRCP1:=data_mort_fill[date==(min_date+k+1)]$PRCP]
      data_mort_fill[date==(min_date+k) & is.na(PRCP), PRCP:=PRCP1]
    }
    data_mort_fill[, c("tavg1","TMIN1","TMAX1","PRCP1"):=NULL]

    missing_first <- data_mort_fill[date==min_date & is.na(tavg)]
    # Check if we need to fill any other values
    setkey(data_mort_fill, id, date)
    need_fill <- any(is.na(data_mort_fill$tavg))
    loop_i <- 1
    while(need_fill == TRUE | loop_i < 10){
      # This older, pdata method is nice because of the use of "lag" but it is dreadfully slow.
      #data_mort_fill <- pdata.frame(data_mort_fill, index=c("idn","date"))
      #data_mort_fill$LTMAX <- lag(data_mort_fill$TMAX)
      #data_mort_fill$LTMIN <- lag(data_mort_fill$TMIN)
      #data_mort_fill$Ltavg <- lag(data_mort_fill$tavg)
      #data_mort_fill$LPRCP <- lag(data_mort_fill$PRCP)
      #data_mort_fill <- data.table(data_mort_fill)
      for(k in c("TMAX","TMIN","tavg","PRCP")){
        # This method is nice for reference by variable, but it is slower
        #data_mort_fill[, c(paste0("L",k)):=data_mort_fill[list(id,date-1)][[k]]]
        data_mort_fill[, c(paste0("L",k)):=lg(get(k)), by=id]
        data_mort_fill[is.na(get(k)), c(k):=get(paste0("L",k))]
      }
      need_fill <- any(is.na(data_mort_fill$tavg))
      loop_i <- loop_i + 1
    }
    # You can check the interpolation quality by using
    # summary(data_mort_fill$tavg_bu)
    # summary(data_mort_fill$tavg)
    data_mort_fill[, tavg_bu:=NULL]
    # Drop all lag variables
    data_mort_fill[, c("LTMAX","LTMIN","Ltavg","LPRCP"):=NULL]
    data_mort_fill[, idn:=NULL]
    # If you wanted something more nuanced, you could use
    #plm(tavg ~ lag(tavg), data=data_mort)
    # This is equivalent to 
    #felm(tavg ~ lag(tavg, k=1) | id, data=data_mort)

    ## Calculate temperature bins
    bin_int <- (seq(from=10, to=90, by=10) - 32)*(5/9)
    data_mort_fill[, c(paste0("tbin",1)):=ifelse(tavg<bin_int[1], 1, 0)]
    for(k in 2:9){
      data_mort_fill[, c(paste0("tbin",k)):=ifelse(tavg>=bin_int[(k-1)] & tavg<bin_int[(k)], 1, 0)]
    }
    data_mort_fill[, c(paste0("tbin",10)):=ifelse(tavg>=bin_int[9], 1, 0)]
    
    ## Do inverse distance weighting
    # Get distance between all stations and centroids
    #data_mort_fill <- merge(data_mort_fill, unique(data_full[, .(id, date, lon, lat)]), by=c("id","date"), all.x=TRUE, all.y=FALSE)
    data_mort_fill <- merge(data_mort_fill, unique(data_full[, .(id, lon, lat)]), by=c("id"), all.x=TRUE, all.y=FALSE)
    # In some cases, we have two weather stations at the exact same lon/lat.
    # Let's keep the one with the more complete records.
    data_mort_fill[, id2:=.GRP, by=c("lon","lat")]
    data_mort_fill[, count:=.N, by=c("id2","date")]
    max_count <- max(data_mort_fill$count)
    dim(data_mort_fill)
    if(max_count > 1){
      data_mort_fill[, good:=1]
      data_mort_fill[, good:=ifelse(is.na(PRCP), 0, 1)]
      data_mort_fill[, good:=ifelse(is.na(TMAX) | good==0, 0, 1)]
      data_mort_fill[, good:=ifelse(is.na(TMIN) | good==0, 0, 1)]
      data_mort_fill[, good_obs:=ifelse(obs==max(obs), 1, 0), by=c("id2","date")]
      data_mort_fill[, quality:=(mean(good,na.rm=TRUE)+mean(good_obs,na.rm=TRUE))/2, by=c("id")]
      data_mort_fill[, max_quality:=max(quality), by=c("id2")]
      # Keep the better one
      data_mort_fill <- data_mort_fill[count == 1 | quality==max_quality]
      # If there are any ties left, keep a random one
      data_mort_fill[, count:=.N, by=c("id2","date")]
      max_count <- max(data_mort_fill$count)
      if(max_count > 1){
        rand <- runif(length(unique(data_mort_fill$id)))
        select <- data.table(cbind(rand, unique(data_mort_fill$id)))
        data_mort_fill <- merge(data_mort_fill, select, by.x=c("id"), by.y=c("V2"))
        data_mort_fill[, max_rand:=max(rand), by=c("id2")]
        data_mort_fill <- data_mort_fill[count == 1 | rand==max_rand]
        data_mort_fill[, c("rand", "max_rand"):=NULL]
      }
      data_mort_fill[, c("good", "good_obs", "quality", "max_quality"):=NULL]
    }
    data_mort_fill[, c("count","id2"):=NULL]
    dim(data_mort_fill)
    mort_loc <- as.matrix(unique(data_mort_fill[, .(lon, lat)]))
    dist <- as.data.table(distm(mort_loc, centroid)/1000)
    names(dist) <- unique(centroid_with_id$geo_id)
    dist[, id:=unique(data_mort_fill[, .(id)])]
    # Centroids are the columns, stations are the rows. Let's make
    # this long so we have 1:m centroid:station crosswalk.
    dist_m <- melt(as.data.table(dist), id.vars=c("id"))
    names(dist_m) <- c("id", "geo_id", "dist_km")
    dist_m <- dist_m[dist_km <=300]
    # Merge together with county information
    # This is the most memory intensive bit of code
    counties_out <- merge(counties, dist_m, by=c("geo_id"), all.x=TRUE)
    # Bring in weather info
    data_mort_fill[, c("obs","lon","lat"):=NULL]
    names(data_mort_fill) <- tolower(names(data_mort_fill))
    data_out <- merge(counties_out, data_mort_fill, by=c("id"), allow.cartesian=TRUE)
    # We only need FIPS codes now
    data_out[, c("id","geo_id"):=NULL]
    # Barecca et al. use squared distance weights
    data_out[, weight:=(1/(dist_km^2))]
    #test <- fmean(data_out, g=c(data_out$state_fips, data_out$county_fips, data_out$date), w=data_out$weight)
    var_list <- c("tavg","tmin","tmax","prcp",paste0("tbin",1:10))
    for(v in var_list){
      data_out[, c(v):=get(v)*weight]
    }
    num <- data_out[, lapply(.SD, sum, na.rm=TRUE), by=c("state_fips","county_fips","date"), .SDcols=var_list]
    den <- data_out[, sum(weight), by=c("state_fips","county_fips","date")]
    num <- merge(num, den, by=c("state_fips","county_fips","date"))
    for(v in var_list){
      num[, c(v):=get(v)/V1]
    }
    num[, V1:=NULL]
    # Clean up and save the daily dataset
    num <- merge(num, unique(data_full[, .(date, date_string)]), by=c("date"), all.x=TRUE)
    num[, date:=ymd(date_string)]
    write_dta(num, path=paste0(external_dir,file[i],"_idw_daily.dta"))
    # Monthly
    num[, month:=month(date)]
    num[, year:=year(date)]
    num_month <- num[, lapply(.SD, sum, na.rm=TRUE), by=c("state_fips","county_fips","year","month"), .SDcols=paste0("tbin",1:10)]
    num_month_other <- num[, lapply(.SD, mean, na.rm=TRUE), by=c("state_fips","county_fips","year","month"), .SDcols=c("tavg","tmax","tmin","prcp")]
    data_out <- merge(num_month, num_month_other, by=c("state_fips","county_fips","year","month"))
    write_dta(data_out, path=paste0(external_dir,file[i],"_idw_monthly.dta"))

    #test <- data_out[, lapply(.SD, mean, na.rm=TRUE), by=c("state_fips","year","month"), .SDcols=c("tavg","tmax","tmin","prcp",paste0("tbin",1:10))]

    ## CBSA, simple average
    data_out <- data_full[is.na(cbsafp)==FALSE,.(mean(value,na.rm=TRUE), sum(is.na(value)==FALSE)),by="date,date_string,element,cbsafp"]
    names(data_out)[c(5,6)] <- c('value','num_obs')
    ##saveRDS(data_out, file=paste0(file[i],"_county.rds"))
    write_dta(data_out, path=paste0(external_dir,file[i],"_cbsa.dta"))

    data_out <- data_full[,.(mean(value,na.rm=TRUE), sum(is.na(value)==FALSE)),by="date,date_string,element,state_fips"]
    names(data_out)[c(5,6)] <- c('value','num_obs')
    ##saveRDS(data_out, file=paste0(file[i],"_county.rds"))
    write_dta(data_out, path=paste0(external_dir,file[i],"_state.dta"))
    rm(data_full)
    ##write.csv(data, file = file[i],  ".csv", sep = ""), row.names = FALSE)
    system(paste0("gzip ",external_dir,"raw/",file[i],".csv"), intern = FALSE, ignore.stderr = TRUE)
    #cat(paste0('File took ',round((proc.time() - ptm_files)[3],3),' seconds.\n'))
}

tic()
i <- 1:length(files)
dt <- pbmclapply(i, func, mc.cores = 4)
toc()

if(debug==TRUE){
    ## http://www.kevjohnson.org/making-maps-in-r/
    ## Check that output makes sense
    library('maps')
    library('RColorBrewer')
    library('maptools')
    library('ggmap')

    county <- shapefile(paste0(county_dir,"gz_2010_us_050_00_500k.shp"))
    county <- crop(county, extent(-127, -61, 21, 53))
    county <- fortify(county, region="GEO_ID")

    weather <- data.table(readRDS(paste0(external_dir,"2014_county.rds")))
    weather$id <- paste0('0500000US',weather$state_fips,weather$county_fips)

    ## Check how many counties got weather observations
    county <- as.data.table(county)
    county_list <- county[,.(id)]
    county_list <- unique(county_list)
    weather_list <- unique(weather[,.(id,state_fips,county_fips)])
    hasstation <- merge(county_list, weather_list, by="id", all.x=TRUE)
    sum(is.na(hasstation$state_fips))

    setkey(weather,element)
    weather_tmin <- weather['TMAX']
    weather1 <- weather_tmin[,.(mean(value,na.rm=TRUE)),by=id]
    ##weather1 <- weather_tmin[date=='20040102',.(id,value)]
    names(weather1)[2] <- 'value'
    plotData <- merge(county, weather1, by="id")
    plotData$vc <- cut(plotData$value, quantile(plotData$value, probs=c(0,.20,.4,.60,.8,1 )))

    ggplot() +
        geom_polygon(data = plotData, aes(x=long, y=lat, group=group,fill=vc),
                     color = "white", size = 0.15) +
        scale_fill_brewer(palette = "Greens") + 
        coord_map() + theme_nothing(legend=TRUE)

    ## Time series of weather in a given location
    weather$date1 <- as.POSIXct(paste0(substr(weather$date,1,4),"-",substr(weather$date,5,6),"-",substr(weather$date,7,8)))
    plot(x=weather[fips=='08069',date1],y=weather[fips=='08069',value])

    ## Look at station locations
    st <- st[lat > 21 & lat < 53 & lon < -61 & lon > -127]
    st$fips <- paste0(st$state_fips,st$county_fips)
    sp <- SpatialPoints(cbind(st$lon,st$lat), proj4string=county@proj4string)
    spst <- SpatialPointsDataFrame(sp, st)
    map(county, lty = 1)
    plot(spst,add=TRUE, pch='.')

}
