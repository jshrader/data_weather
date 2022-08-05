## R code to find population weights for each grid point in the PRISM/Schlenker-Roberts files.
## NOTE: This file only needs to be run once for each population grid. 
##
## Code by Gabriel Gonzalez Sutil and Jeffrey Shrader
##
## Sources:
## http://www.columbia.edu/~ws2162/links.html

# Preliminaries -----------------------------------------------------------
rm(list = ls())
packages <- c('stringr','raster', 'rgdal','sp','haven','colorRamps','data.table','labelled')
new_packages <- packages[!(packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

# Directories -------------------------------------------------------------
if(Sys.info()["sysname"]=="Linux"){
  dir <- "/media/jgs/datadrive/data/weather/schlenker_roberts/"
  maps <- "/home/jgs/Dropbox/research/data/population_gridded_ciesin/usgrid_data_2010/"
} else if(Sys.info()["sysname"]=="Windows"){
  dir <- ""
  maps <- "C:/Users/jgs/Dropbox/research/data/population_gridded_ciesin/usgrid_data_2010/"
}else {
  dir <- '~/Users/Gabriel Gonzalez Sut/Documents/Research/Jeff/Schlenker-Roberts/Data/'
  maps <- paste0(dir,'population/')
}


# Import Link FIPS Code ---------------------------------------------------
setwd(dir)
# This file is created by Wolfram, and it is not stated on his website what shapefile is used for the counties
county_fips <- as.data.table(read_dta('linkGridnumberFIPS.dta'))
county_fips <- remove_labels(county_fips)

# Unzip a Relevant, Representative File --------------------------------------
setwd(paste0(dir))
y <- 2018
untar(paste0('year',y,'.tgz'), compressed=TRUE)

# Grid to Raster ----------------------------------------------------------
folder <- paste0('year',y,'/')
setwd(paste0(dir, folder))
file_list <- list.files()
grids <- data.frame()
for (file in file_list){
  ## Import Data
  # print(file)
  df <- as.data.frame(read_dta(paste0(file)))
  df <- df[df$dateNum == paste0(y,'-01-01'),]
  grids <- rbind(grids,df)
  rm(df)
}  
## Define Coordinates
grids <- grids[c('gridNumber')]
x <- (grids[c('gridNumber')] - 1) %% 1405
grids[c('x')] <- -125 +  x/24
grids[c('y')] <- 49.9375 + 1/48 - ceiling(grids[c('gridNumber')]/1405)/24
pixels <- copy(grids)
coordinates(pixels) <- ~ x + y
# Coerce to SpatialPixelsDataFrame
#plot(pixels)
gridded(pixels) <- TRUE
rasterPixels <- raster(pixels)
## If want to change coordinates: extent(rasterdata) <- c(36,37,-3,-2)
## Assign Projection: WGS84 (World Geodetic System - standar for cartography. 84 is the lastest version
## which is from 1984)
projection(rasterPixels) <- CRS("+proj=longlat +datum=WGS84")
#plot(rasterPixels)
res(rasterPixels)
# Put FIPS codes with lat/lon for use in putting other files on PRISM grid
out <- merge(county_fips, grids, by=c("gridNumber"))

# Project Population ------------------------------------------------------
setwd(paste0(maps))
#pop <- raster('gpw_v4_population_count_rev11_2010_30_sec.tif')
pop <- raster("uspop10.tif")
#pop <- raster('ushu10.tif')
#plot(pop)
crs <- crs(rasterPixels)
res <- res(rasterPixels)
pop <- projectRaster(pop,rasterPixels, res, crs, method = 'bilinear')
#plot(pop)
pop <- resample(pop,rasterPixels,method="bilinear")
#plot(pop)
#if((extent(pop) == extent(rasterPixels)) ==TRUE){
#  plot(rasterPixels,col=matlab.like(20)[20],alpha=0.5)
#  plot(pop,col=matlab.like(20)[1:10],alpha=0.2,add=TRUE)
#}

# Define population for grid points ---------------------------------------
den <- as.data.table(as.data.frame(pop,xy=TRUE))
colnames(den) <- c('x','y','popLevel')
den <- den[is.na(popLevel)==FALSE,]
den <- merge(grids,den,by=c('x','y'))
saveRDS(den,paste0(dir,'prism_grid_population.rds'))

# Check to make sure matching looks good
# Does this look like a population map of the US? It should!
plot(den$x,den$y)
library('ggplot2')
d <- ggplot(den, aes(x=x, y=y, z = log(1+popLevel)))
d + stat_summary_hex(bins=100)
