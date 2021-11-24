## R code to calculate average weather in geographic region from Berkeley
## Earth
##
## Code by Jeff Shrader
## 2021-05-13
##
## To do:
##
## Sources:
## Berkeley Earth
## http://berkeleyearth.org/data/
## 
## Shapfiles
## 


# Preliminaries -----------------------------------------------------------
rm(list = ls())
packages <- c('sp','rgeos','grid','sf','reticulate',
              'stringr','rgdal','raster','maptools','dplyr',
              'parallel','lubridate','data.table','tictoc','pbmcapply','haven')
new_packages <- packages[!(packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

# Define Directories  -------------------------------------------------------
# Many of these might be the same thing depending on your environment
# Project directory (top level)
base_dir <- "~/Dropbox/research/projects/active/elites/"
# data directory
data_dir <- '/media/jgs/datadrive/data/weather/berkeley_earth/'
# Place where you want maps (for debugging and data checking) to go
graph_dir <- paste0(base_dir, "output/graph/data_creation/")
# Place where output data should go
out_dir <- paste0(base_dir, "data/") 

# Load data on grid membership -------------------------------------------
mem <- readRDS(file=paste0(data_dir,"daily_1by1_grid_membership.rds"))

# Import Climate File ----------------------------------------------------
proc_data <- function(year , var){
  file <- paste0(data_dir,"daily/Complete_",var,"_Daily_LatLong1_",year,".nc")
  temp_sp <-  brick(paste0(file), varname=c("temperature"))
  climatology_sp <-  brick(paste0(file), varname=c("climatology"))
  temp <- as.data.table(as.data.frame(temp_sp, xy=TRUE))
  temp[, cell:=1:.N]
  clim <- as.data.table(as.data.frame(climatology_sp, xy=TRUE))
  # Bring in relevant population grid
  pop <- raster(paste0(base_dir,"data/population_grids/USA_HistoricalPopulationDataset/pop_m4_",year,"/w001001.adf"))
  crs <- crs(temp_sp)
  res <- res(temp_sp)
  pop <- projectRaster(pop,temp_sp, res, crs, method = 'bilinear')
  pop <- resample(pop,temp_sp,method="bilinear")
  den <- as.data.table(as.data.frame(pop,xy=TRUE))
  colnames(den) <- c('x','y','popLevel')
  den <- den[is.na(popLevel)==FALSE,]
  # Make climatology dataset long for merge
  clim_long <- melt(clim, id.vars=c("x","y")) %>%
    .[!is.nan(value)] %>%
    .[, doy:=as.numeric(substr(variable, start=2,stop=4))] %>%
    .[, variable:=NULL] %>%
    rename(climatology=value)
  # Make temperature dataset long and merge climatology to get the level of temperature
  temp_long <- melt(temp, id.vars=c("x","y","cell")) %>%
    .[!is.nan(value)] %>%
    .[, elapse:=as.numeric(substr(variable,2,5))] %>%
    .[, variable := NULL] %>%
    .[, date:=ymd(paste0(year,"01","01"))+elapse-1] %>%
    .[, doy:=yday(date)] %>%
    .[, elapse:=NULL] %>%
    rename(t=value)
  # Merge in population
  temp_long <- merge(temp_long, den, by=c("x","y"))
  # Merge climatology
  temp_long <- merge(temp_long, clim_long, by=c("x","y","doy")) %>%
    .[, t:=t+climatology] %>%
    .[, c("x","y","doy"):=NULL]
  # Put the final variable in the appropriate name
  names(temp_long)[names(temp_long) == "t"] <- tolower(var)
  return(temp_long)
}
years <- seq(1880, 2010, by=10)
tmax <- rbindlist(pbmclapply(years, FUN=proc_data, var="TMAX", mc.cores=10))
tmin <- rbindlist(pbmclapply(years, FUN=proc_data, var="TMIN", mc.cores=10))
# A little cleanup
tmax <- rename(tmax, tmax_clim=c("climatology"))
tmin <- rename(tmin, tmin_clim=c("climatology"))
tmin <-rename(tmin, tmin_pop=popLevel)
# Merge both weather records
tall <- merge(tmax,tmin,by=c("cell","date"))
# The population values should be identical
identical(tall$popLevel, tall$tmin_pop)
tall[, tmin_pop:=NULL]
# Merge in state membership
# Note that we need the cartesian join because we allocate each grid cell to
# a state based on the amount of the grid cell that falls into the state polygon.
# Thus, a single grid cell can fall partially into two states. 
tall <- merge(tall, mem, by=c("cell"), allow.cartesian=TRUE)
tall <- tall %>%
  .[, c("statens","affgeoid","geoid","stusps","lsad","aland","awater"):=NULL] %>%
  rename(state_fips=statefp)
# Average as mean of min and max, as is standard
tall[, tavg:=.5*(tmax+tmin)]
# At this point, the dataset is ready for averaging.

## Schlenker-Roberts GDD calculations
# Bounds for GDD  
# Make sure that these are reasonable (like don't go above your highest max temp)
#summary(tall$tmax)
# This set of bounds is what Wolfram uses for US ag
bounds <- c(0, 5, 8, 10, 12, 15, 20, 25, 29, 30, 31, 32, 33, 34)
#bounds <- c(0, 10, 20, 25, 28, 30, 32, 34, 36, 37, 38, 39, 40, 41, 42)
for(b in bounds){
  # Schlenker-Roberts method
  tall[tmax <= b, paste0("degday_",b):=0]
  tall[b <= tmin, paste0("degday_",b):=tavg - b]
  tall[(tmin < b) & (tmax > b), temp_sr:=acos((2*b - tmax - tmin)/(tmax - tmin))]
  tall[(tmin < b) & (tmax > b), paste0("degday_",b):=((tavg - b)*temp_sr + (tmax - tmin)*sin(temp_sr)/2)/pi]
  tall[, temp_sr:=NULL]
  print(paste(b, "done."))
}

## Day bins (Deschenes and Greenstone style)
bin_bounds <- seq(0, 25, by=5)
tall[, tbin0:=(tavg<0)*1]
for(b in bin_bounds){
  tall[, paste0("tbin_",b):=(tavg>=b & tavg<(b+5))*1]
}
tall[, tbin_30:=(tavg>=30)*1]

##### TO DO: 1. calculate weighted averages where we weight by both population
# and cell overlap fraction
# 2. Calculate averages over the full year and over the growing season (April (04) to September (09) in Wolfram's code)
vars <- c("tmax","tmin","tavg",grep(pattern="degday|tbin", names(tall), value=TRUE))
tall[, year:=lubridate::year(date)]
## Area Average
annual_area <- tall[, lapply(.SD, weighted.mean, w=weight, na.rm=TRUE), by = list(state_fips, name, year), .SDcols=vars]
## Population Average
tall[, weight2 := weight*popLevel]
annual_pop <- tall[, lapply(.SD, weighted.mean, w=weight2, na.rm=TRUE), by = list(state_fips, name, year), .SDcols=vars]
## Growing season
tall[, month:=lubridate::month(date)]
grow_area <- tall[month>=4 & month<=9, lapply(.SD, weighted.mean, w=weight, na.rm=TRUE), by = list(state_fips, name, year), .SDcols=vars]
grow_pop <- tall[month>=4 & month<=9, lapply(.SD, weighted.mean, w=weight2, na.rm=TRUE), by = list(state_fips, name, year), .SDcols=vars]

# Output
write_dta(annual_area, path = paste0(base_dir,"data/temperature_1880_2019_annual_area.dta"))
write_dta(annual_pop, path = paste0(base_dir,"data/temperature_1880_2019_annual_pop.dta"))
write_dta(grow_area, path = paste0(base_dir,"data/temperature_1880_2019_growing_season_area.dta"))
write_dta(grow_pop, path = paste0(base_dir,"data/temperature_1880_2019_growing_season_pop.dta"))


# Show aggregate data in a map
library('ggplot2')
library('maps')
library('mapproj')
library('ggthemes')
us_states <- map_data("state")
for(i in c("annual_area","annual_pop","grow_area","grow_pop")){
  outw <- get(i)
  outw[, region:=str_trim(tolower(name))]
  to_plot <- left_join(us_states, outw)
  p <- ggplot(data = to_plot,
    aes(x = long, y = lat,
      group = group, fill = tavg))
  p + geom_polygon(color = "gray90", size = 0.4) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
    scale_fill_gradient(low = "yellow", high = "red") + theme_map(base_size=14) 
  ggsave(paste0(base_dir, "output/graph/data_creation/map_", i, "_tavg.png"))
}


# EOF
