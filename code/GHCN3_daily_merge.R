## R code to combine the annual GHCN files.
## The input files are created by code that lives in my personal weather data
## folder.
##
## Jeff Shrader
## First: 2018-11-29

## Preliminaries
rm(list = ls())
## Debug switch
debug <- FALSE
options(echo=TRUE)
ptm_total <- proc.time()
packages <- c("haven","parallel","tictoc","tidyverse","data.table","lubridate","pbmcapply")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)
if(Sys.info()["nodename"]=="ALBATROSS"){
} else {
  dir <- '~/Dropbox/research/'
  datadrive_dir <- "/media/jgs/datadrive/data/weather/ghcn/daily/"
  data_dir <- "~/Dropbox/research/data/weather/data/ghcn/"
}

## Get all of the different weather variables into their own file lists
file_list <- list.files(datadrive_dir,pattern="\\d{4}.county.dta",full.names=TRUE)
tic()
setattr(file_list, "names", basename(file_list))
dt <- rbindlist(pbmclapply(file_list, read_dta, mc.cores = 16))
toc()

## Reshape and clean up fields
dt <- dt[element!="TAVG"]
dt[,num_obs:=NULL]
# I don't get why my numeric dates keep getting unformatted.
dt[,date:=ymd(date_string)]
# Clean up numeric FIPS codes
dt[,state_fips:=as.character(state_fips)]
dt[nchar(state_fips)==1, state_fips:=paste0("0",state_fips)]
dt[,county_fips:=as.character(county_fips)]
dt[nchar(county_fips)==2, county_fips:=paste0("0",county_fips)]
dt[nchar(county_fips)==1, county_fips:=paste0("00",county_fips)]
dt_w <- dcast(dt,state_fips + county_fips + date + date_string ~ element)
names(dt_w) <- c("state_fips","county_fips","date","date_string","ppt_ghcn","snow_ghcn","tmax_ghcn","tmin_ghcn")

## Put everything in PRISM units
# Precipitation is in tenths of mm in GHCN and in mm in PRISM
dt_w[,ppt_ghcn:=ppt_ghcn/10]

## Save
write_dta(dt_w,path=paste0(data_dir,"ghcn_county_daily.dta"))


## Combine the inverse distance weighted files
file_list <- list.files(datadrive_dir,pattern="\\d{4}.idw.monthly.dta",full.names=TRUE)
tic()
setattr(file_list, "names", basename(file_list))
dt <- rbindlist(pbmclapply(file_list, read_dta, mc.cores = 16))
toc()
## Put everything in PRISM units
# Precipitation is in tenths of mm in GHCN and in mm in PRISM
# The average rainfall in the US is about 760 mm annually, so daily average
# rain should be just north of 2mm
dt[,prcp:=prcp/10]
## Save
write_dta(dt,path=paste0(data_dir,"ghcn_county_idw_monthly.dta"))

# And daily
file_list <- list.files(datadrive_dir,pattern="\\d{4}.idw.daily.dta",full.names=TRUE)
tic()
setattr(file_list, "names", basename(file_list))
dt <- rbindlist(pbmclapply(file_list, read_dta, mc.cores = 16))
toc()
dt[,prcp:=prcp/10]
dt[,date:=ymd(date_string)]
write_dta(dt,path=paste0(data_dir,"ghcn_county_idw_daily.dta"))
