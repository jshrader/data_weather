## R code to calculate average weather in geographic region from Berkeley
## Earth
##
## Code by Jeff Shrader
## 2021-05-13
##
## Sources:
## Berkeley Earth
## http://berkeleyearth.org/data/
## 
## Shapfiles
## 


# Preliminaries -----------------------------------------------------------
rm(list = ls())
pacman::p_load('sp','rgeos','grid','sf','reticulate',
              'stringr','rgdal','raster','maptools','dplyr',
              'parallel','lubridate','data.table','tictoc','pbmcapply','haven')

# Define Directories  -------------------------------------------------------
# Many of these might be the same thing depending on your environment
# Project directory (top level)
base_dir <- "~/Dropbox/research/projects/active/feeding_the_future/"
# data directory
data_dir <- '/media/jgs/datadrive/data/weather/berkeley_earth/'
pop_dir <- '/media/jgs/datadrive/data/hyde/baseline/zip/'
# Place where you want maps (for debugging and data checking) to go
graph_dir <- paste0(base_dir, "output/graph/data_creation/")
# Place where output data should go
out_dir <- paste0(base_dir, "data/") 

# Load data on grid membership -------------------------------------------
mem <- readRDS(file=paste0(data_dir,"Complete_TAVG_LatLong1_grid_membership.rds"))

# Import Climate File ----------------------------------------------------
# There is just one file for the monthly data
var <- "TAVG"
file <- paste0(data_dir,"monthly/Complete_",var,"_LatLong1.nc")
temp_sp <-  brick(paste0(file), varname=c("temperature"))
climatology_sp <-  brick(paste0(file), varname=c("climatology"))
temp <- as.data.table(as.data.frame(temp_sp, xy=TRUE))
temp[, cell:=1:.N]
clim <- as.data.table(as.data.frame(climatology_sp, xy=TRUE))
# Bring in relevant population grid
# The U.S.-specific grids from Fang and Jawitz (2018) are decadal
# and start in 1790.
crs <- crs(temp_sp)
res <- res(temp_sp)
years <- seq(1790,2010, by=10)
pop_frame <- function(year){
  pop <- raster(paste0(pop_dir,year,"AD_pop/popc_",year,"AD.asc"))
  # raster is eating the crs of the HYDE dataset, but luckily it is the same
  # as the BE data
  crs(pop) <- crs
  # If you are working 1790 to present, you have more options for pop grids
  #pop <- raster(paste0(base_dir,"data/population_grids/USA_HistoricalPopulationDataset/pop_m4_",y,"/w001001.adf"))
  pop <- projectRaster(pop,temp_sp, res, crs, method = 'bilinear')
  pop <- resample(pop,temp_sp,method="bilinear")
  den <- as.data.table(as.data.frame(pop,xy=TRUE))
  colnames(den) <- c('x','y','popLevel')
  den <- den[is.na(popLevel)==FALSE,]
  den[, year:=year]
  return(den)
}
den <- rbindlist(pbmclapply(years, pop_frame))
# Make climatology dataset long for merge
clim_long <- melt(clim, id.vars=c("x","y")) %>%
  .[!is.nan(value)] %>%
  .[, month:=as.numeric(substr(variable, start=2,stop=4))] %>%
  .[, variable:=NULL] %>%
  rename(climatology=value)
# Make temperature dataset long and merge climatology to get the level of temperature
temp_long <- melt(temp, id.vars=c("x","y","cell")) %>%
  .[!is.nan(value)] %>%
  .[, year:=as.numeric(substr(variable,2,5))] %>%
  .[, month:=ceiling(as.numeric(substr(variable,6,60))*12)] %>%
  .[, variable := NULL] %>%
  rename(t=value)
# Get the closest value from the set of possible population years using a data.table
# binary search hack. http://stackoverflow.com/questions/20133344/find-closest-value-in-a-vector-with-binary-search
temp_long[, val:=year]
setkey(temp_long, val)
year_dt <- data.table(years, val=years)
setkey(year_dt, val)
# binary search and "roll" to the nearest neighbour
# In the final expression the val column will have the you're looking for.
temp_long <- year_dt[temp_long, roll = "nearest"]
temp_long <- temp_long %>% rename(pop_year=years)
temp_long[, val:=NULL]
# Merge in population
den <- den %>% rename(pop_year=year)
temp_long <- merge(temp_long, den, by=c("x","y","pop_year"), all.x=TRUE)
# Merge climatology
temp_long <- merge(temp_long, clim_long, by=c("x","y","month")) %>%
  .[, t:=t+climatology] %>%
  .[, c("x","y"):=NULL]
# Put the final variable in the appropriate name
names(temp_long)[names(temp_long) == "t"] <- tolower(var)
# A little cleanup
temp_long <- rename(temp_long, tavg_clim=c("climatology"))
# Merge both weather records
#tall <- merge(tmax,tmin,by=c("cell","date"))
# Merge in state membership
# Note that we need the cartesian join because we allocate each grid cell to
# a state based on the amount of the grid cell that falls into the state polygon.
# Thus, a single grid cell can fall partially into two states. 
temp_long <- merge(temp_long, mem, by=c("cell"), allow.cartesian=TRUE)
temp_long <- temp_long %>%
  .[, c("statens","affgeoid","geoid","stusps","lsad","aland","awater"):=NULL] %>%
  rename(state_fips=statefp)
# At this point, the dataset is ready for averaging.

## Schlenker-Roberts GDD calculations
# We need min and max temperature for this.
# Bounds for GDD  
# Make sure that these are reasonable (like don't go above your highest max temp)
#summary(tall$tmax)
# This set of bounds is what Wolfram uses for US ag
#bounds <- c(0, 5, 8, 10, 12, 15, 20, 25, 29, 30, 31, 32, 33, 34)
#bounds <- c(0, 10, 20, 25, 28, 30, 32, 34, 36, 37, 38, 39, 40, 41, 42)
#for(b in bounds){
  # Schlenker-Roberts method
#  tall[tmax <= b, paste0("degday_",b):=0]
#  tall[b <= tmin, paste0("degday_",b):=tavg - b]
#  tall[(tmin < b) & (tmax > b), temp_sr:=acos((2*b - tmax - tmin)/(tmax - tmin))]
#  tall[(tmin < b) & (tmax > b), paste0("degday_",b):=((tavg - b)*temp_sr + (tmax - tmin)*sin(temp_sr)/2)/pi]
#  tall[, temp_sr:=NULL]
#  print(paste(b, "done."))
#}

## Day bins (Deschenes and Greenstone style)
bin_bounds <- seq(0, 30, by=5)
b <- bin_bounds[1]
b_name <- ifelse(b>=0, as.character(b), paste0("n",as.character(abs(b))))
temp_long[, paste0("tbin_le_",b_name):=(tavg<bin_bounds[1])*1]
for(b in bin_bounds){
  b_name <- ifelse(b>=0, as.character(b), paste0("n",as.character(abs(b))))
  if (b == bin_bounds[length(bin_bounds)]){
    temp_long[, paste0("tbin_",b_name):=(tavg>=b)*1]
  } else {
    temp_long[, paste0("tbin_",b_name):=(tavg>=b & tavg<(b+5))*1]  
  }
}
temp_long[is.na(popLevel), popLevel:=0]

vars <- c("tavg",grep(pattern="tbin", names(temp_long), value=TRUE))
## Area Average
annual_area <- temp_long[, lapply(.SD, weighted.mean, w=weight, na.rm=TRUE), by = list(state_fips, name, year), .SDcols=vars]
## Population Average
temp_long[, weight2 := weight*popLevel]
annual_pop <- temp_long[, lapply(.SD, weighted.mean, w=weight2, na.rm=TRUE), by = list(state_fips, name, year), .SDcols=vars]
## Growing season
grow_area <- temp_long[month>=4 & month<=9, lapply(.SD, weighted.mean, w=weight, na.rm=TRUE), by = list(state_fips, name, year), .SDcols=vars]
grow_pop <- temp_long[month>=4 & month<=9, lapply(.SD, weighted.mean, w=weight2, na.rm=TRUE), by = list(state_fips, name, year), .SDcols=vars]
## Monthly
monthly_area <- temp_long[, lapply(.SD, weighted.mean, w=weight, na.rm=TRUE), by = list(state_fips, name, year, month), .SDcols=vars]
monthly_pop <- temp_long[, lapply(.SD, weighted.mean, w=weight2, na.rm=TRUE), by = list(state_fips, name, year, month), .SDcols=vars]

# Output
write_dta(annual_area, path = paste0(base_dir,"data/temperature_1750_2020_annual_area.dta"))
write_dta(annual_pop, path = paste0(base_dir,"data/temperature_1750_2020_annual_pop.dta"))
write_dta(grow_area, path = paste0(base_dir,"data/temperature_1750_2020_growing_season_area.dta"))
write_dta(grow_pop, path = paste0(base_dir,"data/temperature_1750_2020_growing_season_pop.dta"))
write_dta(monthly_area, path = paste0(base_dir,"data/temperature_1750_2020_monthly_area.dta"))
write_dta(monthly_pop, path = paste0(base_dir,"data/temperature_1750_2020_monthly_pop.dta"))


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
  p1 <- p + geom_polygon(color = "gray90", size = 0.4) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
    scale_fill_gradient(low = "yellow", high = "red") + theme_map(base_size=14) 
  ggsave(p1, paste0(base_dir, "output/graph/data_creation/map_", i, "_tavg.png"))
}


# EOF
