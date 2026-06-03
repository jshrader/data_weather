## R code to find county membership for each grid cell in PRISM.
## This only needs to be run once for a single file. It can then be applied to 
## all files. 
##
## Jeff Shrader
## First: 2018-11-29

## Preliminaries
rm(list = ls())
## Debug switch
debug <- FALSE
options(echo=TRUE)
ptm_total <- proc.time()
pacman::p_load(sf,rgeos,stringr,raster,parallel,colorRamps,lubridate,
  data.table,tictoc,exactextractr)
if(Sys.info()["nodename"]=="scrivener"){
  dir <- "/home/jgs/Dropbox/research/"
  datadrive_dir <- paste0('/media/jgs/datadrive/data/weather/prism/prism_daily/')
} else {
}
map_dir <- paste0(dir,'data/maps/usa/county/')
proj_dir <- paste0(dir,'projects/active/Weather_Forecasts_and_Mortality/Data/')

## Get the county polygons
setwd(paste0(map_dir,"gz_2010_us_050_00_500k/"))
county <- st_read(".")
## Keep only continental US
county <- county[!(county$STATE %in% c("02","15")),]
county <- county[!(county$STATE %in% as.character(seq(57,78))),]
if( debug == TRUE ){
  county <- county[county$STATE %in% c("01"),]
}
## County data.table for later merging with weather
county_fips <- as.data.table(as.data.frame(county,xy=TRUE))
county_fips[,ID:=1:.N]
county_fips[,GEO_ID:=NULL]
county_fips[,NAME:=NULL]
county_fips[,LSAD:=NULL]
county_fips[,CENSUSAREA:=NULL]
names(county_fips) <- c("state_fips","county_fips","geometry","ID")

## Baseline population data
pop_temp <- raster(paste0(dir,"data/population_gridded_ciesin/usgrid_data_2010/uspop10.tif"))
# Bring in representative weather file (new PRISM naming + COG .tif)
zip_dir <- paste0(datadrive_dir,"tmax/zip/")
tmp_dir <- paste0(datadrive_dir,"metadata/tmp")
if (!dir.exists(tmp_dir)) {
  dir.create(tmp_dir, recursive = TRUE)
}
zips <- list.files(zip_dir, pattern="\\.zip$", full.names=TRUE)
if (length(zips) == 0) {
  stop("No PRISM zip files found in ", zip_dir)
}
unzip(zips[1], exdir = tmp_dir)
rasters <- list.files(tmp_dir, pattern="tmax.*\\.(tif|bil)$", full.names=TRUE, ignore.case=TRUE)
if (length(rasters) == 0) {
  stop("No PRISM raster found in ", tmp_dir, " after unzip")
}
weather <- raster(rasters[1])
crs <- crs(weather)
# pop1 <- projectRaster(pop_temp,crs=crs,res=res(weather),method="bilinear")
pop0 <- projectRaster(pop_temp,crs=crs)
pop <- resample(pop0,weather,method="bilinear")
extent(pop) == extent(weather)

## Get the population to use as a weight for each grid point
den0 <- as.data.table(as.data.frame(pop,xy=TRUE))
den0[,cell:=1:.N]
den <- den0[is.na(uspop10)==FALSE,]
## Get the county membership and population weight for each
## PRISM grid point (NOTE: this requires that the PRISM grid
## is stable over the full history).
cell_member <- rbindlist(exact_extract(weather, county, include_cell=TRUE), idcol="ID")
cell_member[,value:=NULL]
## Merge area_weather with county_fips to get county membership of each grid cell
cell_member_county <- merge(cell_member,county_fips,by=c("ID"))
cell_member_county <- merge(cell_member_county, den, by=c("cell"), all.x=TRUE)
setnames(cell_member_county, "uspop10", "pop2010")
cell_member_county[is.na(pop2010), pop2010:=0]

if(debug==TRUE){
  pdf(paste0(proj_dir,"../Output/Figures/data_creation_notes/PRISM_and_population_grid.pdf"),width=7,height=5)
  plot(weather,col=matlab.like(20)[20],alpha=0.5)
  plot(pop,col=matlab.like(20)[1:10],alpha=0.2,add=TRUE)
  dev.off()
}

## Save the two files recording population and county membership of each
## grid point
## We can drop geometry
cell_member_county[, geometry:=NULL]
saveRDS(cell_member_county,paste0(datadrive_dir,'prism_grid_to_county.rds'))
#saveRDS(den,paste0(datadrive_dir,'prism_grid_population.rds'))

# Clean up
# Delete the temporary weather file used for reference grid
setwd(paste0(datadrive_dir,"ppt/zip/"))
unlink("../metadata/tmp",recursive=TRUE)
# EOF
