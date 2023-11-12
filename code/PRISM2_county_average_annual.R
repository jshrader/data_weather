## R code to calculate average weather in county from PRISM daily files
##
## As of 2020, Rscript fails becuase of an Rcpp issue (new version should resolve the problem)
## To run the code, you can use Rscript:
##  Rscript ~/Dropbox/research/data/weather/code/PRISM2_county_average_annual.R
##
## Jeff Shrader
## First: 2018-6-4
## Latest: 2023-01-13

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
pacman::p_load(stringr,raster,parallel,lubridate,data.table,tictoc)
# Directories
if(Sys.info()["nodename"]=="scrivener"){
  # This is the path to the full data. If you want to set this to use the project-
  # specific test data subset, direct it to <project dir>/data/prism
  #datadrive_dir <- paste0('/media/jgs/datadrive/data/weather/prism/prism_daily/')
  proj_dir <- '/home/jgs/Dropbox/research/projects/active/Construction/'
  datadrive_dir <- paste0(proj_dir,'data/prism/')
} else {
}

## A total kludge, but as of 2021, rcpp is throwing a bunch of errors when I 
# first load a raster. So I just need to load one raster here at the beginning, 
# catch the error, then the later rasters will load without problem. 
test <- raster(paste0(datadrive_dir,"kludge/PRISM_ppt_stable_4kmD2_19810101_bil.bil"))
test2 <- as.data.table(as.data.frame(test,xy=TRUE))
rm(test)
rm(test2)

## Save spline basis for predictions
weather <- raster(paste0(datadrive_dir,"metadata/tmp/PRISM_tmax_stable_4kmD2_20000101_bil.bil"))
wp <- as.data.frame(weather, xy=TRUE)
k <- c(0,15,30)
# WARNING: This spline call must match the one later in the code exactly!!!
sp <- bSpline(x=wp[,3], knots=k,degree = 3, Boundary.knots=c(-10,40))
temps <- seq(-10,40,by=.1)
sp_temps <- predict(sp, temps)
saveRDS(sp_temps, paste0(datadrive_dir,'metadata/prism_spline_basis.rds'))


## Bring in the PRISM grid to county and population cross walks
cell_member_county <- readRDS(paste0(datadrive_dir,'metadata/prism_grid_to_county.rds'))

## Process each weather field for all PRISM files
func <- function(file_name,w,y){
  # Read in raster, merge the location information generated in PRISM1 code
  weather1 <- raster(file_name)
  # If we are doing temperature, read in both tmax and tmin
  if(w == "tmp"){
    weather2 <- raster(str_replace(file_name,"tmax","tmin"))
  }
  wp <- as.data.table(as.data.frame(weather1,xy=TRUE))
  wp[,cell:=1:.N]
  if(w == "tmp"){
    wp2 <- as.data.table(as.data.frame(weather2,xy=TRUE))
    wp <- merge(wp,wp2,by=c("x","y"))
    names(wp)[c(3,5)] <- c("tmax","tmin")
    wp[, tavg:=.5*(tmax+tmin)]
  } else {
    names(wp)[c(3)] <- w
  }
  weather_with_county <- merge(cell_member_county,wp,by="cell")
  weather_with_county[,c("ID","cell","x.x","x.y","y.y","y.x"):=NULL]

  ## Calculate non-linear transformations
  if(w == "tmp"){
    ## Schlenker-Roberts GDD calculations
    # We need min and max temperature for this.
    # Bounds for GDD  
    # Make sure that these are reasonable (like don't go above your highest max temp)
    # This set of bounds is what Wolfram uses for US ag
    #bounds <- c(0, 5, 8, 10, 12, 15, 20, 25, 29, 30, 31, 32, 33, 34)
    # A smaller set but that includes more extreme heat values
    #bounds <- c(0, 10, 20, 25, 30, 35, 40)
    bounds <- c(0, 35)
    for(b in bounds){
      weather_with_county[tmax <= b, paste0("degday_",b):=0]
      weather_with_county[b <= tmin, paste0("degday_",b):=tavg - b]
      weather_with_county[(tmin < b) & (tmax > b), temp_sr:=acos((2*b - tmax - tmin)/(tmax - tmin))]
      weather_with_county[(tmin < b) & (tmax > b), paste0("degday_",b):=((tavg - b)*temp_sr + (tmax - tmin)*sin(temp_sr)/2)/pi]
      weather_with_county[, temp_sr:=NULL]
      print(paste(b, "done."))
    }
    ## Splines
    # NOTE! The bSpline call must exactly match the call used in PRISM1...R 
    # or else you won't be able to get the correct predictions from the 
    # aggregated data.
    # The knots and boundary knots must both be specified for the spline basis 
    # to be invariant to x.
    # Setting the boundary knots equal to internal knots imposes 0 slope on the 
    # predictions beyond those boundaries. Be careful!
    k <- c(0,15,30)
    sp <- bSpline(x=weather_with_county$tavg, knots=k,degree = 3, Boundary.knots=c(-10,40))
    sp <- as.data.table(sp)
    names(sp) <- paste0("tavg_s",colnames(sp))
    weather_with_county <- cbind(weather_with_county,sp)
    ## Polynomials
    weather_with_county[, tavg_p2:=tavg^2]
    weather_with_county[, tavg_p3:=tavg^3]
    weather_with_county[, tavg_p4:=tavg^4]
    weather_with_county[, tavg_p5:=tavg^5]
  } else if(w == "ppt"){
    # NOAA defines chance of precip based on chance of >0.1 inches or 2.54 mm
    # https://psl.noaa.gov/data/usclimate/glossary.html
    weather_with_county[, ppt_bin:=if_else(ppt>2.54,1,0)]
    weather_with_county[, ppt_p2:=ppt^2]
  }

  ## Grab the date
  date <- str_match(file_name,"\\d{8}")
  # Make combined weights for the pop-by-coverage weighting
  weather_with_county[, pop2010:=pop2010*coverage_fraction]
  ## Average the raster inside each county, area-weighted (implicit) and weighted by population
  avg_weather <- weather_with_county[,lapply(.SD, weighted.mean,w=coverage_fraction, na.rm=TRUE),by=c("state_fips","county_fips")]
  pop_avg_weather <- weather_with_county[,lapply(.SD, weighted.mean,w=pop2010, na.rm=TRUE),by=c("state_fips","county_fips")]
  pop_avg_weather[,c("pop2010","coverage_fraction"):=NULL]
  avg_weather[, coverage_fraction:=NULL]
  # Rename
  in_names <- names(avg_weather)[!names(avg_weather)%in%c("state_fips","county_fips","pop2010")]
  out_names <- c(paste(in_names,"area",sep="_"))
  setnames(avg_weather, in_names, out_names)
  in_names <- names(pop_avg_weather)[!names(pop_avg_weather)%in%c("state_fips","county_fips","pop2010")]
  out_names <- c(paste(in_names,"pop",sep="_"))
  setnames(pop_avg_weather, in_names, out_names)
  ## Merge it all together and export
  wo <- merge(avg_weather,pop_avg_weather,by=c("state_fips","county_fips"))
  wo[,date:=date]
}

# Loop through all weather variables and years, producing annual .rds files that can be merged
# as needed for your project.
# "tmp","vpdmax","vpdmin"
for(w in c("tmp","ppt")){
  tic(w)
  # Earliest start is 1981 in full data (unless we download older files) and 
  # for shared data the range is 2017 to 2018
  for(y in 2017:2018){
    if(w == "tmp"){
      ws <- c("tmax")
    } else {
      ws <- w
    }
    # Unzip all the files to a common holding bay
    setwd(paste0(datadrive_dir,ws,"/zip"))
    # The wildcard statement has some subtleties:
    # We are matching on year, but some years can also appear as days, so we need to include
    # the "_" before the year so that we only pick up values that match _<y>.
    system(paste0("cd ",getwd(), " && unzip -o '*_",y,"*.zip' -d ../../hold"),ignore.stdout=TRUE)
    if(w == "tmp"){
      setwd(paste0(datadrive_dir,"tmin","/zip"))
      system(paste0("cd ",getwd(), " && unzip -o '*_",y,"*.zip' -d ../../hold"),ignore.stdout=TRUE)
    }
    files_all <- list.files(path=paste0(datadrive_dir,"hold"),pattern=paste0(ws,".*bil$"),full.names=TRUE)
    #files_all <- files_all[1:10]
    setattr(files_all, "names", basename(files_all))
    tic(y)
    dt <- rbindlist(mclapply(files_all, func,w=w,y=y, mc.cores = 16))
    #print("Saving")
    fname <- paste0(datadrive_dir,w,"/prism_county_",w,"_",y)
    # File formats are frustrating. RDS is small but slow to read and write 
    # because R only uses a single thread? DTA is kinda small, can be read in 
    # Stata, but I don't always want to use Stata. CSV is fast to read and 
    # write but big. Other formats like feather are great but more experimental.
    saveRDS(dt,paste0(fname,'.rds'))
    # I stopped using the following, and `haven` is the newer package now if 
    # you really want to work with Stata.
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
