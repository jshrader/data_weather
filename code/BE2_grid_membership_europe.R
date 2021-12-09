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
pacman::p_load('sp','rgeos','grid','ggplot2','sf','reticulate',
              'stringr','rgdal','maptools','raster','maptools',
              'parallel','colorRamps','lubridate','data.table',
              'tictoc','pbmcapply','haven','countrycode','exactextractr',
              'purrr')
# Toggles and Switches -----------------------------------------------------
# If you want to create graphs to check that your output files make sense, set
# this to TRUE. Otherwise FALSE.
check_file <- TRUE

# Define Directories  -------------------------------------------------------
# Many of these might be the same thing depending on your environment
# Project directory (top level)
base_dir <- "~/Dropbox/research/projects/active/feeding_the_future/"
# data directory
data_dir <- '/media/jgs/datadrive/data/weather/berkeley_earth/'
# Place where you want maps (for debugging and data checking) to go
graph_dir <- paste0(base_dir, "output/graph/data_creation/")
# Place where output data should go
out_dir <- paste0(base_dir, "data/") 

# Import Shapefiles  -----------------------------------------------------
# Import the Shapefile for your geographic level
shp <- shapefile(paste0(base_dir,"data/2021/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp"))
# List of countries for which we have data can be found in the europe_sample.dta file
keep_list <- c("Albania","Armenia","Austria","Belarus","Belgium","Bulgaria","Cyprus","Denmark",
  "Finland","Czechoslovakia","Yugoslavia","France","Georgia","Germany","Greece","Hungary",
  "Iceland","Ireland","Italy","Latvia","Liechtenstein","Lithuania","Luxembourg","Malta",
  "Moldova","Netherlands","Norway","Poland","Portugal","Romania","Russia","Spain","Sweden",
  "Switzerland","Turkey","Ukraine","United Kingdom","Montenegro","Serbia","Kosovo",
  "Czech Republic","Slovakia")
code <- countrycode(keep_list, origin="country.name", destination="iso3c")
code <- code[!is.na(code)]
shp <- subset(shp, ADM0_A3 %in% code)
# Generate a data.frame from the polygons
poly <- as.data.table(as.data.frame(shp@data, xy=TRUE))
names(poly) <- tolower(names(poly))
poly[,ID:=1:.N]

# Load data --------------------------------------------------------------
# Load a single, representative file (which for nicely formatted ERA data is any one of the)
# so we can determine grid membership
file <- paste0(data_dir,"monthly/Complete_TAVG_LatLong1.nc")
# The file is a brick in reality (one slice per month) but we just need
# one of the slices to find spatial membership.
# We will take a layer from one of the post 1880 years because that is the p
# period on which we will focus. It shouldn't matter, but I just want to be
# safe in case something weird happens with missing values.
data <-  raster(paste0(file), varname=c("temperature"), band=3000)
clim <-  brick(paste0(file), varname=c("climatology"))
#data <-  brick(paste0(file))
crsData <- crs(data) # Define System of Data
# We expect this to be 1 by 1 for the base data and 0.25 by 0.25 for the CONUS data
resData <- res(data) # Define resolution of data
# Project Shape File into Data System
projection(data) <- shp@proj4string

# Find spatial membership for each point ---------------------------------
cell_member <- rbindlist(suppressWarnings(exact_extract(data, shp, include_cell=TRUE)), idcol="ID")
cell_member <- cell_member %>%
  rename(weight=coverage_fraction)
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
# Remove extra elements from poly
poly <- poly[, .(ID,name,adm0_a3)]

# Count of matched grid points
cell_member[, count:=.N, by=ID]
few_cells <- unique(cell_member[count<3][, .(ID,count)])
look_into <- merge(poly[ID%in%few_cells$ID], few_cells, by=c("ID"), all=TRUE)
## These locations only had a few grid points (1 or 2). Maybe make sure that
# you don't need to densify your grid or use something like centroid matching.
look_into

# Drop weather: we just want the spatial membership
cell_member[, value:=NULL]

## Merge with admin boundaries to get county membership of each grid cell
## This is what needs to be saved to merge with each weather file.

mem <- merge(cell_member, poly, by=c("ID"))
# To check that you have grid points in each member, you can use
# cell_member_admin[, .N, by=.(NAME_3)][1:141]
# Save the membership file for use when calculating averages for the other
# locations.
fname <- paste0(base_dir,"data/2021/berkeley_earth/Complete_TAVG_LatLong1_grid_membership_europe.rds")
saveRDS(object=mem, file=fname)

# Get info about locations
centroid <- as.data.frame(gCentroid(shp, byid=TRUE))
names(centroid) <- c("c_lon","c_lat")
centroid_with_id <- as.data.table(cbind(shp@data[, c("ISO_A3","NAME")],centroid))
names(centroid_with_id) <- str_to_lower(names(centroid_with_id))

# Make a dataset that contains the names of locations, IDs, and other info
fname <- paste0(base_dir,"data/2021/berkeley_earth/Complete_TAVG_LatLong1_location_names_europe.dta")
write_dta(data=centroid_with_id, path=fname)

# EOF
