## R code to find the state membership of data from Berkeley Earth.
##
## Code by Jeff Shrader
## 2021-05-20
##
## To do:
##
## Sources and citations:


# Preliminaries -----------------------------------------------------------
rm(list = ls())
packages <- c('sp','rgeos','grid','ggplot2','sf','reticulate',
              'stringr','rgdal','maptools','raster','maptools',
              'parallel','colorRamps','lubridate','data.table',
              'tictoc','pbmcapply','haven')
new_packages <- packages[!(packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

# Toggles and Switches -----------------------------------------------------
# If you want to create graphs to check that your output files make sense, set
# this to TRUE. Otherwise FALSE.
check_file <- TRUE

# Define Directories  -------------------------------------------------------
# Many of these might be the same thing depending on your environment
# Project directory (top level)
base_dir <- "~/Dropbox/research/projects/active/elites/"
# data directory
data_dir <- '/media/jgs/datadrive/data/weather/berkeley_earth/'
# Place where shapefiles are stored
map_dir <- paste0("/home/jgs/Dropbox/research/data/maps/usa/state/cb_2014_us_state_500k/")
# Place where you want maps (for debugging and data checking) to go
graph_dir <- paste0(base_dir, "output/graph/data_creation/")
# Place where output data should go
out_dir <- paste0(base_dir, "data/") 

# Import Shapefiles  -----------------------------------------------------
setwd(map_dir)
# Import the Shapefile for your geographic level
shp <- shapefile(paste0(map_dir,"cb_2014_us_state_500k.shp"))
# Generate a data.frame from the polygons
poly <- as.data.table(as.data.frame(shp@data, xy=TRUE))
names(poly) <- tolower(names(poly))
poly[,ID:=1:.N]

# Load data --------------------------------------------------------------
# Load a single, representative file (which for nicely formatted ERA data is any one of the)
# so we can determine grid membership
#file <- paste0(data_dir,"Complete_TAVG_LatLong1.nc")
file <- paste0(data_dir,"daily/Complete_TMAX_Daily_LatLong1_1920.nc")
# The file is a brick in reality (one slice per month) but we just need
# one of the slices to find spatial membership.
# We will take a layer from one of the post 1880 years because that is the p
# period on which we will focus. It shouldn't matter, but I just want to be
# safe in case something weird happens with missing values.
data <-  raster(paste0(file), varname=c("temperature"))
clim <-  brick(paste0(file), varname=c("climatology"))
#data <-  brick(paste0(file))
crsData <- crs(data) # Define System of Data
# We expect this to be 1 by 1 for the base data and 0.25 by 0.25 for the CONUS data
resData <- res(data) # Define resolution of data
# Project Shape File into Data System
## If want to change coordinates: extent(rasterdata) <- c(36,37,-3,-2)
## Assign Projection: WGS84 (World Geodetic System - standard for cartography.
## 84 is the lastest version which is from 1984) - Use same as shape file
projection(data) <- shp@proj4string

# Find spatial membership for each point ---------------------------------
cell_member <- as.data.table(suppressWarnings(raster::extract(data, shp, cellnumbers=TRUE, df=TRUE, small=TRUE, weights=TRUE, normalizeWeights=FALSE)))
cell_member <- cell_member[!is.na(cell)]
# To verify that the hand-coded cell numbers are working, run:
if(check_file==TRUE){
  temp <- as.data.table(as.data.frame(data, xy=TRUE))
  temp[, cell:=1:.N]
  m <- merge(cell_member, temp, by=c("cell"))
  identical(m[, 3], m[, 3])
  rm(m)
  rm(temp)
}
# Check that all locations received at least a few grid points. Small locations like
# Washington DC can slip through the cracks.
if(identical(length(unique(poly$ID)), length(unique(cell_member$ID)))==FALSE){
  warning("Some geographic units were not matched with any grid points. This might be benign (for example, due to CONUS data and a complete USA shapefile), but make sure.")
}
# Count of matched grid points
cell_member[, count:=.N, by=ID]
few_cells <- unique(cell_member[count<3][, .(ID,count)])
look_into <- merge(poly[ID%in%few_cells$ID], few_cells, by=c("ID"), all=TRUE)
## These locations only had a few grid points (1 or 2). Maybe make sure that
# you don't need to densify your grid or use something like centroid matching.
look_into

# Check that the weather values make sense (warmer closer to equator, eg)
names(cell_member)[3] <- "weather"
centroid <- as.data.frame(gCentroid(shp, byid=TRUE))
names(centroid) <- c("c_lon","c_lat")
centroid_with_id <- as.data.table(cbind(shp@data[, "GEOID"],centroid))
names(centroid_with_id)[1] <- "geoid"
test <- merge(centroid_with_id, poly, by=c("geoid"))
test <- merge(test, cell_member, by=c("ID"))
# If this is the level of temperature, the slope coef should be negative.
# If it is another variable, all bets are off. 
summary(lm(test$weather ~ test$c_lat))
# Look at unmatched locations
test[, .(mean(weather, na.rm=TRUE), mean(count)), by=c("name")][is.na(V1)]
test_c <- test[, mean(weather, na.rm=TRUE), by=c("name", "c_lon","c_lat")]
test_c <- test_c[!is.nan(V1)]
ggplot(data=test_c, aes(x=c_lon,y=c_lat, color=V1)) + geom_point() +
  geom_text(aes(label=name),hjust=0, vjust=0) + 
  scale_color_gradient(low = "yellow", high = "red") + theme_void(base_size=20)
ggsave(filename=paste0(graph_dir, "temperature_centroid_match_check_tmax_daily.pdf"))

# Drop weather: we just want the spatial membership
cell_member <- cell_member[, weather:=NULL][
  , count := NULL]

## Merge with admin boundaries to get county membership of each grid cell
## This is what needs to be saved to merge with each weather file.
mem <- merge(cell_member, poly, by=c("ID"))
# To check that you have grid points in each member, you can use
# cell_member_admin[, .N, by=.(NAME_3)][1:141]
# Save the membership file for use when calculating averages for the other
# locations.
fname <- paste0(data_dir,"daily_1by1_grid_membership.rds")
saveRDS(object=mem, file=fname)

# Make a dataset that contains the names of locations, IDs, and other info
locs <- merge(centroid_with_id, poly, by=c("geoid"))
fname <- paste0(data_dir,"location_names.dta")
write_dta(data=locs, path=fname)

# EOF
