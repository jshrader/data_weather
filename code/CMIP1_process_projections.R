# Load CMIP6 projection data
rm(list = ls())
pacman::p_load(downloader,plyr,sp,rgeos,stringr,rgdal,raster,epwshiftr,tigris,spData,sf,exactextractr,data.table,lubridate,tictoc,furrr,purrr,fixest,pbmcapply)
# Directories
if(Sys.info()["sysname"]=="Windows"){
  dir <- paste0('C:/Users/jgs/Dropbox/research/projects/active/Weather_Forecasts_and_Mortality/Data/weather/')
} else {
  dir <- paste0('~/Dropbox/research/projects/active/Weather_Forecasts_and_Mortality/Data/weather/')
  map_dir <- paste0('~/Dropbox/research/data/maps/usa/national')
}
cmip_dir <- paste0(dir,'/cmip6/')
data_dir <- paste0('~/Dropbox/research/data/weather/data/cmip6/')

# Keep data for grid cells in the US (or another area if you want to check that)
# us_states is loaded by spData
country <- sf::st_union(us_states)
## Read a CMIP file, keeping grid cells that overlap the country boundaries
country_cmip <- function(n, .file_list, .country, debug=FALSE){
  file <- .file_list[n]
  if(debug == TRUE){
    print(file)
  }
  
  cmip <- raster(file)
  #
  cmip <- raster(paste0(cmip_dir,"tas_ssp245_BCC-CSM2-MR_2015-01-01-2039-12-31.nc"))
  cmip1 <- raster(paste0(data_dir,"tas_day_GFDL-ESM4_ssp585_r1i1p1f1_gr1_20150101-20991231_v20180701.nc"))
  # put in -180 to 180 longitude
  # https://stackoverflow.com/questions/25730625/how-to-convert-longitude-from-0-360-to-180-180
  cmip0 <- raster::rotate(cmip)
  # Reproject shapefile
  country0 <- st_transform(.country, crs(cmip))
  # Test
  #plot(cmip0)
  #plot(usa0, lwd = 4, add=TRUE)
  
  # Get overlapping grid points
  cell_member <- as.data.table(exactextractr::exact_extract(cmip0, country0, include_cell=T))
  ## If we have lots of files, we can use the old trick of just finding
  # spatial membership for one file, then re-using that membership for other
  # files with the same grid. Here, however, I am not sure that the files
  # all share consistent grid defs, and it isn't too slow just to read every
  # file.
  cell_member[, value:=NULL]
  # Drop obs with low coverage fraction
  cell_member <- cell_member[coverage_fraction>.5][, coverage_fraction:=NULL]
  # Now read in all layers from file, finding values that lie inside USA
  cmip2 <- brick(file)
  cmip2 <- rotate(cmip2)
  wp <- as.data.table(as.data.frame(cmip2, xy=TRUE))
  wp[,cell:=1:.N]
  wpm <- merge(wp, cell_member, by=c("cell"))
  wpm_l <- melt(wpm, id.vars=c("cell","x","y"))
  wpm_l[, date:=ymd(substr(variable, 2, 11))]
  wpm_l[, variable:=NULL]
  wpm_l <- wpm_l %>%
    rename(c("value"="tmean")) %>%
    rename(c("x"="longitude")) %>%
    rename(c("y"="latitude")) %>%
    transform(tmean = tmean-273.15)
  # Keep only dates we care about
  #wpm_l <- wpm_l[(date < ymd("2018-01-01")) |
  #                 (date>=ymd("2048-01-01") & date<ymd("2051-01-01")) |
  #                 (date>=ymd("2097-01-01"))]
  wpm_l[, model:=str_split(file, pattern="_")[[1]][6]]
  return(wpm_l)
}

# Run over all files
file_list <- list.files(paste0(cmip_dir), pattern=".nc", full.names=TRUE)
# Make sure you can read each file without error
for(i in file_list){
  print(i)
  cmip <- raster(i)
}
Lf <- length(file_list)
ncores <- future::availableCores()-8
tic()
#plan(multisession, workers = ncores)
#outw <- rbindlist(future_map(1:Lf, ~country_cmip(.x), .file_list=file_list, .country=country, .options = furrr_options(seed = TRUE)))
outw <- rbindlist(pbmclapply(1:Lf, FUN=country_cmip, .file_list=file_list, .country=country, mc.cores=ncores, ignore.interactive=TRUE))
toc()

# Save output file
outw[, cell:=NULL]
outw <- outw[!is.na(date)]
# This model's tmean values are messed up (almost always 0 in 2015).
outw <- outw[model!="BCC-CSM2-MR"]
# Some years are not as well-represented as others because not all models produce their
# files in the same format. We will keep the 2 or 3 best-represented years
outw[, year:=year(date)]
# table(outw$year)
outw <- outw[!(year %in% c(2017,2018,2097))]
outw[, year:=NULL]
# Save resulting set of projections
saveRDS(object=outw, file=paste0(cmip_dir, "cmip6_tmean_projections.rds"))
# EOF
