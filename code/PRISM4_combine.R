## R code to combine the annual PRISM files.
##
## Jeff Shrader
## First: 2018-11-29
## Recent: 2022-12-14

## Preliminaries
rm(list = ls())
## Debug switch
debug <- FALSE
options(echo=TRUE)
ptm_total <- proc.time()
pacman::p_load("haven","parallel","tictoc","tidyverse","data.table","lubridate","weathermetrics","pbmcapply")
if(Sys.info()["nodename"]=="ALBATROSS"){
} else {
  dir <- '~/Dropbox/research/'
  datadrive_dir <- paste0('/media/jgs/datadrive/data/weather/prism/prism_daily/')
  tmp_dir <- "~/tmp/"
  proj_dir <- "/home/jgs/Dropbox/research/projects/active/Construction/"
}


## Get all of the different weather variables into their own file lists
# Future improvements would take in a pattern based on years so that we don't have
# to process all files. 
dt <- list()
for(w in c("ppt","tmp")){
  file_list <- list.files(paste0(datadrive_dir,w),pattern=paste0(".*",w,".*rds"),full.names=TRUE)
  tic(w)
  setattr(file_list, "names", basename(file_list))
  dt[[w]] <- rbindlist(pbmclapply(file_list, readRDS, mc.cores = 16), idcol = "id")
  dt[[w]]$id <- NULL
  dt[[w]]$id <- NULL
  toc()
}
## Merge the list of data tables
dt_m <- dt %>% reduce(left_join, by = c("state_fips","county_fips","date"))
dt_m <- data.table(dt_m)
dt_m[,d:=ymd(date)]
dt_m[,date:=d]
dt_m[,d:=NULL]
dt_m <- unique(dt_m)
setnames(dt_m, "pop2010.x","pop2010")
dt_m[, pop2010.y:=NULL]

## We need to add some quality control here -- compare to known values, compare to a well vetted reference set, etc.
cor(dt_m$ppt_area, dt_m$ppt_pop)
cor(dt_m$tmax_area, dt_m$tmin_pop)
#cor(dt_m$atmax_area, dt_m$tmax_area, use='pairwise.complete.obs')

# Output the file that you need
dt_m[, fips:=paste0(state_fips, county_fips)]
dt_m[, c("state_fips", "county_fips"):=NULL]
dt_m <- dt_m[date>=ymd("1990-01-01")]
# Aggregate to the year level
dt_m[, year:=year(date)]
dt_a <- dt_m[, lapply(.SD, FUN="mean"), by=c("fips","year")]
# Basic quality checks
dt_a[ppt_bin_area>.75]
# Both of these observations are in Washington right along the Pacific.
dt_a[tavg_area==max(tavg_area)]
# Hottest average temp is in Florida (max is in Texas)
write_dta(data=dt_a,path=paste0(proj_dir,"data/prism_fips_year.dta"))

# EOF
