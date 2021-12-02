## R code to combine the annual PRISM files.
##
## Jeff Shrader
## First: 2018-11-29

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
}


## Get all of the different weather variables into their own file lists
# Future improvements would take in a pattern based on years so that we don't have
# to process all files. 
dt <- list()
for(w in c("ppt","tmax","tmin","atmax","atmin")){
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

## We need to add some quality control here -- compare to known values, compare to a well vetted reference set, etc.
cor(dt_m$ppt_area, dt_m$ppt_pop)
cor(dt_m$tmax_area, dt_m$tmin_pop)
cor(dt_m$atmax_area, dt_m$tmax_area, use='pairwise.complete.obs')

# Output the file that you need
# For forecast-mortality project: 2004 to the present
dt_out <- dt_m[date>=ymd("2004-01-01")]
proj_dir <- paste0(dir,'projects/active/Weather_Forecasts_and_Mortality/Data/')
write_dta(data=dt_out,path=paste0(proj_dir,"weather/prism_2004_2020.dta"))
# EOF
