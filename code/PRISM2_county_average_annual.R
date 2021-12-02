## R code to calculate average weather in county from PRISM daily files
##
## Then, to run the code, you can use Rscript:
##  Rscript county_weather.R
##
## Jeff Shrader
## First: 2018-6-4

## Cites
## for gridded population data:
## Global grids
## Center for International Earth Science Information Network - CIESIN - Columbia University. 2017. Gridded Population of the World, Version 4 (GPWv4): Population Count, Revision 10. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H4PG1PPM. Accessed DAY MONTH YEAR.
## Census grids
## Center for International Earth Science Information Network - CIESIN - Columbia University. 2017. U.S. Census Grids (Summary File 1), 2010. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/H40Z716C. Accessed DAY MONTH YEAR.

## Preliminaries
rm(list = ls())
## Debug switch
debug <- FALSE
# Packages
packages <- c("stringr","raster","readstata13","parallel","lubridate","data.table","tictoc")
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
func <- function(file_name,w,y){
  weather1 <- raster(file_name)
  wp <- as.data.table(as.data.frame(weather1,xy=TRUE))
  wp[,cell:=1:.N]
  
  weather_with_county <- merge(cell_member_county,wp,by="cell")
  weather_with_county[,ID:=NULL]
  weather_with_county[,cell:=NULL]
  # I thought this was dropped in PRISM1, but apparently not
  weather_with_county[, value:=NULL]
  weather_with_county_pop <- merge(den,weather_with_county,by=c("x","y"))
  weather_with_county[,x:=NULL]
  weather_with_county[,y:=NULL]
  weather_with_county_pop[,x:=NULL]
  weather_with_county_pop[,y:=NULL]
    
  ## Grab the date
  date <- str_match(file_name,"\\d{8}")
  #date <- str_match(names(weather_with_county)[4],"\\d{8}")
  # Simplify the names
  outnames <- c("coverage_fraction","state_fips","county_fips",paste(w,"area",sep="_"))
  names(weather_with_county) <- outnames
  outnames <- c("pop","coverage_fraction","state_fips","county_fips",paste(w,"pop",sep="_"))
  names(weather_with_county_pop) <- outnames
  # Make combined weights for the pop-by-coverage weighting
  weather_with_county_pop[, pop:=pop*coverage_fraction]
  weather_with_county_pop[, coverage_fraction:=NULL]
  
  ## Average the raster inside each county, area-weighted (implicit) and weighted by population
  avg_weather <- weather_with_county[,lapply(.SD, weighted.mean,w=coverage_fraction, na.rm=TRUE),by=c("state_fips","county_fips")]
  pop_avg_weather <- weather_with_county_pop[,lapply(.SD, weighted.mean,w=pop, na.rm=TRUE),by=c("state_fips","county_fips")]
  pop_avg_weather[,pop:=NULL]
  avg_weather[, coverage_fraction:=NULL]
  
  ## Merge it all together and export
  wo <- merge(avg_weather,pop_avg_weather,by=c("state_fips","county_fips"))
  wo[,date:=date]
}

# Loop through all weather variables and years, producing annual .rds files that can be merged
# as needed for your project.
for(w in c("ppt","tmax","tmin","vpdmax","vpdmin")){
  tic(w)
  for(y in 1981:2020){
    # Unzip all the files to a common holding bay
    setwd(paste0(datadrive_dir,w,"/zip"))
    #setwd(paste0(datadrive_dir,"test"))
    # The wildcard statement has some subtleties:
    # We are matching on year, but some years can also appear as days, so we need to include
    # the "_" before the year so that we only pick up values that match _<y>.
    system(paste0("cd ",getwd(), " && unzip -o '*_",y,"*.zip' -d ../../hold"),ignore.stdout=TRUE)
    # List of files to process
    files_all <- list.files(path=paste0(datadrive_dir,"hold"),pattern=paste0(w,".*bil$"),full.names=TRUE)
    #files_all <- files_all[1:10]
    
    tic(y)
    setattr(files_all, "names", basename(files_all))
    dt <- rbindlist(mclapply(files_all, func,w=w,y=y, mc.cores = 16))
    #print("Saving")
    fname <- paste0(datadrive_dir,w,"/prism_county_",w,"_",y)
    saveRDS(dt,paste0(fname,'.rds'))
    #write.dta13(wo,paste0(fname,'.dta'),convert.factors=c("string"))
    #fwrite(wo,paste0(fname,'.csv'))
    toc()
    # Delete the unzipped files
    setwd(datadrive_dir)
    unlink("./hold",recursive=TRUE)
  }
  toc()
}

# EOF
