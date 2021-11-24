## R code to download and process GHCN weather station data. The code downloads
## a list of all current and former weather stations, selects data for
## available stations, downloads it, and prepares it for use with other code
## like spatial averaging files.
##
## Jeff Shrader
## 2016/7/23
## Time-stamp: "2021-11-24 13:02:54 jgs"

## Set options
#begin_year <- 1950
#end_year <- 1989
begin_year <- 2010
end_year <- 2020
station_list <- TRUE

external_dir <- "/media/jgs/datadrive/data/weather/ghcn/daily/"
data_dir <- "~/Dropbox/research/data/weather/data/ghcn/"
map_dir <- paste0('~/Dropbox/research/data/maps/usa/')
county_dir <- paste0(map_dir,'county/gz_2010_us_050_00_500k/')
cbsa_dir <- paste0(map_dir,'cbsa/')
tz_dir <- paste0(map_dir,'time_zone/tz_us/')

## Initialize and load packages
options(echo=TRUE)
ptm <- proc.time()
packages <- c("sp","rgeos","rgdal","spdep","raster","fields","MBA","data.table","readr","geosphere")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

setwd(data_dir)
if(station_list == TRUE){
  ## Get list of stations
  file <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"
  repeat {
    try(download.file(file, "ghcnd-stations.txt", quiet = FALSE))
    if (file.info("ghcnd-stations.txt")$size > 0) {
      break
    }
  }

  ## IV. FORMAT OF "ghcnd-stations.txt"
  ## ------------------------------
  ##     Variable   Columns   Type
  ## ------------------------------
  ## ID            1-11   Character
  ## LATITUDE     13-20   Real
  ## LONGITUDE    22-30   Real
  ## ELEVATION    32-37   Real
  ## STATE        39-40   Character
  ## NAME         42-71   Character
  ## GSN FLAG     73-75   Character
  ## HCN/CRN FLAG 77-79   Character
  ## WMO ID       81-85   Character
  ## ------------------------------
  columns <- fwf_widths(c(11, 9, 10, 7, 3, 31, 4, 4, 6))
  columns$col_names <- c('id','lat','lon','elev','state','name','gsn_flag','hcn_crn_flag','wmo_id')
  column_types <- cols(id='c',lat='d',lon='d',elev='d',state='c',name='c',
    gsn_flag='c',hcn_crn_flag='c',wmo_id='c')

  st <- data.table(read_fwf("ghcnd-stations.txt",col_positions=columns,col_types=column_types))
  st$cntry <- substr(st$id, 1, 2)
  ## Keep only the US locations
  st <- st[cntry=='US']
  st[, cntry:=NULL]
  ## Format variables
  st[elev < -998, elev := NA]

  ## Add timezone and county IDs
  ## Read timezone shapefile
  tz <- shapefile(paste0(tz_dir,"tz_us.shp"))
  st <- st[is.na(lat)==FALSE & is.na(lon)==FALSE]
  dim(st)
  ## Add timezone
  sp <- SpatialPoints(cbind(st$lon,st$lat), proj4string=tz@proj4string)
  spst <- SpatialPointsDataFrame(sp, st)
  tzid <- over(spst, tz)
  st <- data.table(cbind(st,tzid))
  ## Drop locations with missing timezone ID. These are offshore or islands,
  ## as far as I can tell.
  dim(st)
  st <- st[is.na(TZID)==FALSE]
  dim(st)

  ## Build lists of stations by county for use when reading the weather files
  ## Grab county spatial data
  county <- shapefile(paste0(county_dir,"gz_2010_us_050_00_500k.shp"))
  #county <- st_read(paste0(county_dir,"gz_2010_us_050_00_500k.shp"))
  #cent <- st_centroid(county)
  # After some looking around, I think gCentroid is the best way to calculate
  # centroids for complex geometries like counties.
  # https://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
  centroid <- as.data.frame(gCentroid(county, byid=TRUE))
  centroid <- cbind(county@data[, "GEO_ID"],centroid)
  #test <- data.frame(geosphere::centroid(county))
  names(centroid) <- c("GEO_ID","county_centroid_lon","county_centroid_lat")
  # Prepare for merge
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
  ## Merge centroid
  st <- merge(st, centroid, by=c("GEO_ID"), all.x=TRUE)
  ## Find distance to centroid for IDW. We will calculate distance in km because
  ## lat/lon is changing size over the globe.
  st[, dist_to_centroid:=distGeo(st[, c('lon','lat')], st[, c('county_centroid_lon','county_centroid_lat')])]
  ## Renaming
  names(st)[c(12,13)] <- c('state_fips','county_fips')
  names(st) <- tolower(names(st))
  st$name <- NULL
  ## geo_id is just fips with a common US prefix
  st$geo_id <- NULL
  

  ## Add CBSA
  # Note that this is the oldest CBSA shapefile offered by the Census (on tigerline at least)
  cbsa <- shapefile(paste0(cbsa_dir,"cb_2013_us_cbsa_500k.shp"))
  sp <- SpatialPoints(cbind(st$lon,st$lat), proj4string=cbsa@proj4string)
  spst <- SpatialPointsDataFrame(sp, st)
  cbsa_id <- over(spst, cbsa)
  st <- data.table(cbind(st,cbsa_id))
  ## Remove extraneous
  st$AFFGEOID <- NULL
  ## In this case, GEOID==CBSAFP
  st$GEOID <- NULL
  st$NAME <- NULL
  st$LSAD <- NULL
  st$ALAND <- NULL
  st$AWATER <- NULL
  names(st) <- tolower(names(st))

  write_csv(st, "ghcnd_stations_usa_countytz.csv")
}

cat("Getting ready to download files.")
i <- 1
for(y in seq(begin_year,end_year,by=1)){
  print(y)
  ## Preallocate a file download status matrix
  outputs <- as.data.table(cbind(matrix("", end_year-begin_year+1, 1),
    matrix(NA, end_year-begin_year+1, 1)))
  names(outputs) <- c("file","status")
  ## Write the wget calls
  outputs[i, file:=y]
  wget <- paste0("wget -N -nc --wait=1 -P ", paste0(external_dir,"raw/"), " ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/",
    y, ".csv.gz")
  outputs[i, status:=try(system(wget, intern=FALSE, ignore.stderr=TRUE))]
}
saveRDS(outputs, file='ghcn_download_status.rds')
##outputs_full
cat("All done. Check the status of downloads in ghcn_download_status.rds.")

# EOF
