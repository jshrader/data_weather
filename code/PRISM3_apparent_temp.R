## R code to calculate apparent temperature (heat index) from PRISM files. This
## needs to be done before averaging because the formula is nonlinear in
## temperature and dewpoint temperature. I have chosen to make this a different
## program than "prism_county_average_annual.R" because I wanted to maintain
## the simplicity of that program. The trade-off is that this program will
## perform some redundant operations like unzipping and reading daily files.
##
## Jeff Shrader
## First: 2018-11-29

## Preliminaries
rm(list = ls())
## Debug switch
debug <- FALSE
# Packages
packages <- c("stringr","raster","readstata13","parallel","lubridate","data.table","tictoc","weathermetrics","pbmcapply")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)
# Directories
if(Sys.info()["nodename"]=="ALBATROSS"){
} else {
  dir <- '~/Dropbox/research/'
  datadrive_dir <- paste0('/media/jgs/datadrive/data/weather/prism/prism_daily/')
  tmp_dir <- "~/tmp/"
}

## Bring in the PRISM grid to county and population cross walks
cell_member_county <- readRDS(paste0(datadrive_dir,'prism_grid_to_county.rds'))
den <- readRDS(paste0(datadrive_dir,'prism_grid_population.rds'))

## Process each weather field for all PRISM files
if(debug==TRUE){

}
# The Buck Equation for saturation vapor pressure
svp <- function(T){
  s <- ifelse(T>0,
    0.61121*exp((18.678 - (T/234.5))*(T/(257.14+T))),
    0.61115*exp((23.036 - (T/333.7))*(T/(279.82+T))))
}

func_at <- function(file_name,w,y){
  # Get the temperature raster
  weather1 <- raster(file_name)
  wp <- as.data.table(as.data.frame(weather1,xy=TRUE))
  wp[,cell:=1:.N]
  ## Match it with the dewpoint temp data
  # Grab the date
  date <- str_match(file_name,"\\d{8}")
  # Read in the appropriate dewpoint temperature file
  ## dew_file <- paste0(datadrive_dir,"tdmean_hold/PRISM_tdmean_stable_4kmD1_",date,"_bil.bil")
  ## dew1 <- raster(dew_file)
  ## dew <- as.data.table(as.data.frame(dew1,xy=TRUE))
  ## dew[,cell:=1:.N]
  # Read in the appropriate vapor pressure deficit
  m_or_m <- substr(w,2,4)
  vpd_name <- paste0("vpd",m_or_m)
  at_name <- paste0("at",m_or_m)
  vpd_file <- paste0(datadrive_dir,"vpd_hold/PRISM_vpd",m_or_m,"_stable_4kmD1_",date,"_bil.bil")
  vpd1 <- raster(vpd_file)
  vpd <- as.data.table(as.data.frame(vpd1,xy=TRUE))
  vpd[,cell:=1:.N]
  vpd$x <- NULL
  vpd$y <- NULL
  # Merge
  ## wp <- merge(wp,dew,by="cell")
  ## wp$x.y <- NULL
  ## wp$y.y <- NULL
  wp <- merge(wp,vpd,by="cell")
  ## names(wp) <- c("cell","x","y",w,"tdmean","vpdmax")
  names(wp) <- c("cell","x","y",w,vpd_name)
  # Calculate apparent temperature
  # Recall that get() allows us to use variable names in column reference
  # wp[,at_from_dewpoint:=heat.index(t=get(w),dp=tdmean,temperature.metric="celsius")]

  # Calculate saturation vapor pressure
  wp[,s:=svp(get(w))]
  # Calculate relative humidity
  wp[,rh:=(get(vpd_name)+s)*s]
  wp[rh>100,rh:=100]
  wp[,(at_name):=heat.index(t=get(w),rh=rh,temperature.metric="celsius")]
  wp[,s:=NULL]
  wp[,rh:=NULL]
  wp[,(vpd_name):=NULL]
  wp[,(w):=NULL]
  ## if(debug=TRUE){
  ##   # compare the values we get from dewpoint temp and RH. The RH-based measures
  ##   # are better for high values b/c we only have mean dewpoint temperature
  ##   png(paste0(proj_dir,"../Output/Figures/data_creation_notes/apparent_temp_from_RH_vs_dewpoint.png"),width=960,height=960)
  ##   plot(wp$at_from_dewpoint,wp$at_from_rh)
  ##   lines(x = c(-30,40), y = c(-30,40),col='red')
  ##   dev.off()
  ## }
  
  weather_with_county <- merge(cell_member_county,wp,by="cell")
  weather_with_county[,ID:=NULL]
  weather_with_county[,cell:=NULL]
  weather_with_county_pop <- merge(den,weather_with_county,by=c("x","y"))
  weather_with_county[,x:=NULL]
  weather_with_county[,y:=NULL]
  weather_with_county_pop[,x:=NULL]
  weather_with_county_pop[,y:=NULL]
  
  # Simplify the names
  outnames <- c("state_fips","county_fips",paste(at_name,"area",sep="_"))
  names(weather_with_county) <- outnames
  outnames <- c("pop","state_fips","county_fips",paste(at_name,"pop",sep="_"))
  names(weather_with_county_pop) <- outnames
  
  ## Average the raster inside each county, area-weighted (implicit) and weighted by population
  avg_weather <- weather_with_county[,lapply(.SD, mean, na.rm=TRUE),by=c("state_fips","county_fips")]
  pop_avg_weather <- weather_with_county_pop[,lapply(.SD, weighted.mean,w=pop, na.rm=TRUE),by=c("state_fips","county_fips")]
  pop_avg_weather[,pop:=NULL]
  
  ## Merge it all together and export
  wo <- merge(avg_weather,pop_avg_weather,by=c("state_fips","county_fips"))
  wo[,date:=date]
}

for(y in 1998:2017){
  tic(y)
  ww <- c("tmax","tmin")
  for(w in ww){
    mm <- ifelse(w=="tmax","max","min")
    ## First get humidity inputs ready
    # Unzip all the files to a dewpoint holding bay
    ## setwd(paste0(datadrive_dir,"tdmean/",y,"/"))
    ## system(paste("cd",getwd(), "&& unzip -o '*.zip' -d ../../tdmean_hold"),ignore.stdout=TRUE)
    setwd(paste0(datadrive_dir,"vpd",mm,"/",y,"/"))
    system(paste("cd",getwd(), "&& unzip -o '*.zip' -d ../../vpd_hold"),ignore.stdout=TRUE)

    setwd(paste0(datadrive_dir,w,"/",y,"/"))
    system(paste("cd",getwd(), "&& unzip -o '*.zip' -d ../../at_hold"),ignore.stdout=TRUE)
    # List of files to process
    files_all <- list.files(path=paste0(datadrive_dir,"at_hold"),pattern=paste0(w,".*bil$"),full.names=TRUE)
    
    tic(w)
    setattr(files_all, "names", basename(files_all))
    dt <- rbindlist(pbmclapply(files_all, func_at,w=w,y=y, mc.cores = 3))
    toc()

    fname <- paste0(datadrive_dir,"at",mm,"/prism_county_at",mm,"_",y)
    saveRDS(dt,paste0(fname,'.rds'))
    #write.dta13(wo,paste0(fname,'.dta'),convert.factors=c("string"))
    #fwrite(wo,paste0(fname,'.csv'))
    # Delete the unzipped files
    setwd(datadrive_dir)
    unlink("./vpd_hold",recursive=TRUE)
    unlink("./at_hold",recursive=TRUE)
  }
  toc()
}

