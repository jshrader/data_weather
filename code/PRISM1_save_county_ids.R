## R code to find county membership for each grid cell in PRISM.
##
## Jeff Shrader
## First: 2018-11-29

## Preliminaries
rm(list = ls())
## Debug switch
debug <- FALSE
options(echo=TRUE)
ptm_total <- proc.time()
packages <- c("sp","rgeos","stringr","rgdal","raster","parallel","colorRamps","lubridate","data.table","tictoc")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)
if(Sys.info()["nodename"]=="ALBATROSS"){
} else {
  dir <- '~/Dropbox/research/'
  datadrive_dir <- paste0('/media/jgs/datadrive/data/weather/prism/prism_daily/')
  tmp_dir <- "~/tmp/"
}
map_dir <- paste0(dir,'data/maps/usa/county/')
proj_dir <- paste0(dir,'projects/active/Weather_Forecasts_and_Mortality/Data/')

## Get the county polygons
setwd(paste0(map_dir,"gz_2010_us_050_00_500k/"))
county <- readOGR(dsn = ".", layer = "gz_2010_us_050_00_500k", encoding="latin1")
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
names(county_fips) <- c("state_fips","county_fips","ID")

## Baseline population data
pop_temp <- raster(paste0(dir,"data/population_gridded_ciesin/usgrid_data_2010/uspop10.tif"))
# Bring in representative weather file
setwd(paste0(datadrive_dir,"ppt/2000/"))
f <- "PRISM_ppt_stable_4kmD2_20000101_bil.zip"
unzip(f,exdir="../tmp")
weather <- raster("../tmp/PRISM_ppt_stable_4kmD2_20000101_bil.bil")
crs <- crs(weather)
# pop1 <- projectRaster(pop_temp,crs=crs,res=res(weather),method="bilinear")
pop0 <- projectRaster(pop_temp,crs=crs)
pop <- resample(pop0,weather,method="bilinear")
extent(pop) == extent(weather)
if(debug==TRUE){
  pdf(paste0(proj_dir,"../Output/Figures/data_creation_notes/PRISM_and_population_grid.pdf"),width=7,height=5)
  plot(weather,col=matlab.like(20)[20],alpha=0.5)
  plot(pop,col=matlab.like(20)[1:10],alpha=0.2,add=TRUE)
  dev.off()
}

## Get the population to use as a weight for each grid point
den0 <- as.data.table(as.data.frame(pop,xy=TRUE))
den <- den0[is.na(uspop10)==FALSE,]

## Get the county membership and population weight for each
## PRISM grid point (NOTE: this requires that the PRISM grid
## is stable over the full history).
cell_member <- as.data.table(extract(weather, county, cellnumbers=TRUE, df=TRUE))
names(cell_member) <- c("ID","cell","x")
cell_member[,x:=NULL]
## Merge area_weather with county_fips to get county membership of each grid cell
cell_member_county <- merge(cell_member,county_fips,by=c("ID"))

## Save the two files recording population and county membership of each
## grid point
saveRDS(cell_member_county,paste0(datadrive_dir,'prism_grid_to_county.rds'))
saveRDS(den,paste0(datadrive_dir,'prism_grid_population.rds'))

# Clean up
# Delete the temporary weather file used for reference grid
setwd(paste0(datadrive_dir,"ppt/2000/"))
unlink("../tmp",recursive=TRUE)