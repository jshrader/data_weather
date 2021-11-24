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
packages <- c("foreign","parallel","tictoc","tidyverse","data.table","lubridate","weathermetrics","pbmcapply")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)
if(Sys.info()["nodename"]=="ALBATROSS"){
} else {
  dir <- '~/Dropbox/research/'
  datadrive_dir <- paste0('/media/jgs/datadrive/data/weather/prism/prism_daily/')
  tmp_dir <- "~/tmp/"
}
proj_dir <- paste0(dir,'projects/active/Weather_Forecasts_and_Mortality/Data/')

## Get all of the different weather variables into their own file lists
dt <- list()
for(w in c("ppt","tmax","tmin","atmax","atmin")){
  file_list <- list.files(paste0(datadrive_dir,w),pattern=".*rds",full.names=TRUE)
  tic(w)
  setattr(file_list, "names", basename(file_list))
  dt[[w]] <- rbindlist(pbmclapply(file_list, readRDS, mc.cores = 3), idcol = "id")
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

# Output the file that we need (1999 to the present)
dt_out <- dt_m[date>ymd("1998-01-01")]
## svp <- function(T){
##   s <- ifelse(T>0,
##     0.61121*exp((18.678 - (T/234.5))*(T/(257.14+T))),
##     0.61115*exp((23.036 - (T/333.7))*(T/(279.82+T))))
## }
## # Calculate saturation vapor pressure
## dt_out[,smin_area:=svp(tmin_area)]
## dt_out[,smax_area:=svp(tmax_area)]
## dt_out[,smin_pop:=svp(tmin_pop)]
## dt_out[,smax_pop:=svp(tmax_pop)]
## # Calculate relative humidity
## dt_out[,rhmin_area:=100*(vpdmin_area+smin_area)*smin_area]
## dt_out[,rhmax_area:=svp(tmax_area)]
## dt_out[,rhmin_pop:=svp(tmin_pop)]
## dt_out[,rhmax_pop:=svp(tmax_pop)]

## dt_out[,atmax_pop:=heat.index(t=tmax_pop,dp=tdmean_pop)]
## dt_out[,atmax_area:=heat.index(t=tmax_area,dp=tdmean_area)]
## dt_out[,atmin_pop:=heat.index(t=tmin_pop,dp=tdmean_pop)]
## dt_out[,atmin_area:=heat.index(t=tmin_area,dp=tdmean_area)]
write.dta(dt_out,file=paste0(proj_dir,"weather/prism_1999_2017.dta"),convert.factors="string")
