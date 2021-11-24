## R code to calculate average weather in geographic region from ECMWF ERA5 
## daily files
##
## Code by Gabriel Gonzolez Sutil and Jeff Shrader
## 2020-12-17
##
## To do:
##
## Sources:
## ERA5
## https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview
## Muñoz Sabater, J., (2019): ERA5-Land hourly data from 1981 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS). (<date of access>), 10.24381/cds.e2161bac
## Shapfiles
## https://gadm.org/download_country_v3.html


# Preliminaries -----------------------------------------------------------
rm(list = ls())
packages <- c('sp','rgeos','grid','ggplot2','sf','reticulate',
              'stringr','rgdal','maptools','raster','maptools',
              'parallel','colorRamps','lubridate','data.table','tictoc','pbmcapply','haven')
new_packages <- packages[!(packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

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
counties <- as.data.table(as.data.frame(dist_shp@data[, c(1,2,3)], xy=TRUE))
names(counties) <- tolower(names(counties))
names(counties)[c(2)] <- c("district_name")
#counties <- counties[!is.na(c_code11)]
counties[,ID:=1:.N]

# Load data ----------- ---------------------------------------------------
folder <- '2m_temperature'
file_list <- list.files(paste0(era_dir,folder,'_india/'), pattern=".grib", full.names=TRUE)
# Get the grid for a representative file (one of the benefits of ERA5 is the use
# of a consistent grid over time).
file <- file_list[1] # Choose one file
data <-  raster(paste0(file))
crsData <- crs(data) # Define System of Data
# We expect this to be 0.1 by 0.1 for ERA5-Land
resData <- res(data) # Define resolution of data
# Project Shape File into Data System
## If want to change coordinates: extent(rasterdata) <- c(36,37,-3,-2)
## Assign Projection: WGS84 (World Geodetic System - standard for cartography.
## 84 is the lastest version which is from 1984) - Use same as shape file
projection(data) <- dist_shp@proj4string

# Find spatial membership for each point ---------------------------------
cell_member <- as.data.table(extract(data, dist_shp, cellnumbers=TRUE, df=TRUE, small=TRUE))
# To verify that the hand-coded cell numbers are working, run:
#temp <- as.data.table(as.data.frame(data, xy=TRUE))
#temp[, cell:=1:.N]
#setnames(temp, old='X19790101', new='test')
#m <- merge(test, temp, by=c("cell"))
#identical(m$X19790101, m$test)
# Check that all locations received at least a few grid points. Small locations like
# Washington DC can slip through the cracks.
if(identical(length(unique(counties$ID)), length(unique(cell_member$ID)))==FALSE){
  stop("Some geographic units were not matched with any grid points.")
}
# Count of matched grid points
cell_member[, count:=.N, by=ID]
few_cells <- unique(cell_member[count<4][, .(ID,count)])
look_into <- merge(counties[ID%in%few_cells$ID], few_cells, by=c("ID"), all=TRUE)
look_into
# Check that the weather values make sense (warmer closer to equator, eg)
#if(check_file==TRUE){
names(cell_member)[3] <- "weather"
centroid <- as.data.frame(gCentroid(dist_shp, byid=TRUE))
names(centroid) <- c("c_lon","c_lat")
centroid_with_id <- as.data.table(cbind(dist_shp@data[, "ID"],centroid))
names(centroid_with_id)[1] <- "id"
test <- merge(centroid_with_id, counties, by=c("id"))
test <- merge(test, cell_member, by=c("ID"))
summary(lm(test$weather ~ test$c_lat))
test[, .(mean(weather, na.rm=TRUE), mean(count)), by=c("district_name")][is.na(V1)]
test_c <- test[, mean(weather, na.rm=TRUE), by=c("district_name", "c_lon","c_lat")]
#ggplot(data=test_c, aes(x=c_lon,y=c_lat, color=V1)) + geom_point() +
#  geom_text(aes(label=district_name),hjust=0, vjust=0) + 
#  scale_color_gradient(low = "yellow", high = "red") + theme_void(base_size=20)
#ggsave(filename=paste0(graph_dir, "temperature_centroid_match_check.pdf"))
# Drop weather: we just want the spatial membership
cell_member <- cell_member[, c(-3)]
cell_member <- cell_member[, c(-3)]

## Merge with admin boundaries to get county membership of each grid cell
## This is what needs to be saved to merge with each weather file.
mem <- merge(cell_member, counties, by=c("ID"))
# To check that you have grid points in each member, you can use
# cell_member_admin[, .N, by=.(NAME_3)][1:141]
# Save the membership file for use when calculating averages for the other
# locations.
saveRDS(object=mem, file=paste0(era_dir,"output_india/era5land_india_grid_membership.rds"))

# Make a dataset that contains the names of locations, IDs, and other info
locs <- merge(centroid_with_id, counties, by=c("id"))
locs[, id:=NULL]
write_dta(data=locs, path=paste0(era_dir,"output_india/india_districts.dta"))

# EOF
