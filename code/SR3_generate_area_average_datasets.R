## R code to calculate average weather in county from PRISM (Schenkler-Roberts) daily files
## To add a new year to the data, simply change the year list at the beginning (years).
## This will run the averaging code for the near year and add it to the files from previous
## years. 
##
## Code by Gabriel Gonzalez Sutil and Jeffrey Shrader
##
## Sources:
## http://www.columbia.edu/~ws2162/links.html


# Preliminaries -----------------------------------------------------------
rm(list = ls())
packages <- c('stringr','data.table','haven','labelled','parallel','tictoc','pbmcapply')
new_packages <- packages[!(packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)
  
if(Sys.info()["nodename"]=="scrivener"){
  dir <- '~/Dropbox/research/'
  datadrive_dir <- "/media/jgs/datadrive/data/weather/schlenker_roberts/"
} else {
  dir <- '~/Users/Gabriel Gonzalez Sut/Documents/Research/Jeff/Schlenker-Roberts/Data/'
  datadrive_dir <- dir
}
proj_dir <- paste0(dir,'data/weather/data/schlenker_roberts/')

# Specify the years to process---------------------------------------------
# This allows you to quickly update the data. Simply specify the next year only.
years <- as.character(seq(1950,2019))
#years <- as.character(c(1950,2009))

## Prepare for parallel 
num_cores <- detectCores()-1
#cl = makeCluster(num_cores, outfile="")

# Import Population -------------------------------------------------------
pop_dir <- paste0(datadrive_dir)
setwd(pop_dir)
pop <- readRDS('prism_grid_population.rds')
pop <- pop[c('gridNumber','popLevel')]

# Import Link FIPS Code ---------------------------------------------------
setwd(datadrive_dir)
county_fips <- read_dta('linkGridnumberFIPS.dta')
county_fips <- remove_labels(county_fips)

# Unzip Files -------------------------------------------------------------
unzip <- 'Yes'
if (unzip == 'Yes'){
  directory_list <- character()
  for(y in years){
    directory_list <- c(directory_list,
      list.files(path=datadrive_dir, pattern=paste0(y,".tgz")))
  }
  # Untar all files
  # parallel untar needs full path names
  # Be careful because you also need to supply full target directory names
  files_to_untar <- paste0(datadrive_dir,directory_list)
  to_dir <- paste0(datadrive_dir)
  parLapply(cl, X=files_to_untar, fun='untar', exdir=to_dir)
}

# Import S/R Data -------------------------------------------------------
# Get the full list of files that are in the date range we want to process
file_list <- list()
dir_list <- character()
for(y in years){
  dir_list <- c(dir_list,grep(x=list.dirs(path=datadrive_dir, full.names=FALSE), pattern=paste0("year",y), value=TRUE))
}
for (y in dir_list){
  ## Set Working Directory
  name <- paste0(datadrive_dir,y,"/")
  file_list[[y]] <- paste0(name,list.files(path=name))
}
# This vector now contains all the files that need to be processed--good for parallel processing
files <- unlist(file_list, use.names=FALSE)

func <- function(file=file_name){
  #for (file in files){
  # Check whether the file has already been processed
  y <- str_extract(pattern="[0-9][0-9][0-9][0-9]", string=file)
  state <- str_match(pattern="(state|fips)([0-9]+)", string=file)[, 3]
  out_file <- paste0(y,state,".Rda")
  f1 <- paste0(datadrive_dir,"daily/sp",out_file)
  f2 <- paste0(datadrive_dir,"daily/wg",out_file)
  f3 <- paste0(datadrive_dir,"monthly/sp",out_file)
  f4 <- paste0(datadrive_dir,"monthly/wg",out_file)
  file_done <- ifelse(file.exists(f1) & file.exists(f2) & file.exists(f3) & file.exists(f4), TRUE, FALSE)

  if(!file_done){
    print(file)
    ## Import Data
    prism <- as.data.table(read_dta(file))
    prism <- remove_labels(prism)
    ## Merge County Code and Population
    prism <- merge(prism,county_fips,by=c('gridNumber'))
    prism_pop <- merge(prism,pop,by=c('gridNumber'))
    ## Grouping
    prism[ , c('year','month','day') := list(substr(prism$dateNum, 1, 4), 
      substr(prism$dateNum, 6, 7),
      substr(prism$dateNum, 9, 10))]
    prism_pop[ , c('year','month','day') := list(substr(prism_pop$dateNum, 1, 4), 
      substr(prism_pop$dateNum, 6, 7),
      substr(prism_pop$dateNum, 9, 10))]
    prism2 <- copy(prism)
    prism2$day <- NULL 
    prism_pop2 <- copy(prism_pop)
    prism_pop2$day <- NULL 
    ## Simple Average
    daily <- prism[, lapply(.SD, mean, na.rm=TRUE), by = list(fips, year, month, day, dateNum)]
    monthly <- prism2[, lapply(.SD, mean, na.rm=TRUE), by = list(fips, year, month)]
    ## Population-weighted Average
    daily_wg <- prism_pop[,lapply(.SD, weighted.mean, w=popLevel , na.rm=TRUE), by = list(fips, year, month, day, dateNum)]
    monthly_wg <- prism_pop2[,lapply(.SD, weighted.mean, w=popLevel , na.rm=TRUE), by = list(fips, year, month)]
    ## Save Files
    folder <- 'daily/'
    setwd(paste0(datadrive_dir,folder))
    # "sp1990state1.Rda"
    save(daily, file = paste0("sp",out_file))
    save(daily_wg, file = paste0('wg',out_file)) 

    # Save monthly
    folder <- 'monthly/'
    setwd(paste0(datadrive_dir,folder))
    save(monthly, file = paste0('sp',out_file))
    save(monthly_wg, file = paste0('wg',out_file))
  }
}
  
tic()
dt <- pbmclapply(files, func, mc.cores = num_cores)
toc()


# Append Daily Area Weighted into Unique Dataset ----------------------------------
app_files <- function(file){
  load(file) 
  return(daily)
}
for(i in c("area","pop")){
  print(paste0("Saving daily ", i," average."))
  if(i == "area"){
    suffix <- "sp"
  } else {
    suffix <- "wg"
  }
  folder <- 'daily/'
  setwd(paste0(datadrive_dir,folder))
  file_list <- list.files(pattern = suffix)[1:20]
  tic()
  schlenker_daily <- rbindlist(mclapply(file_list, FUN=app_files, mc.cores=num_cores))
  toc()
  # Bring in previous years
  if (exists(paste0(proj_dir, 'schlenker_daily_',i,'.dta'))){
    prev <- as.data.table(read_dta(file = paste0(proj_dir, 'schlenker_daily_',i,'.dta')))
  }
  schlenker_daily[, fips:=as.character(fips)]
  schlenker_daily[nchar(fips)<5, fips:=paste0("0",fips)]
  #schlenker_daily[, year:=NULL]
  #schlenker_daily[, month:=NULL]
  #schlenker_daily[, day:=NULL]
  schlenker_daily[, gridNumber:=NULL]
  colnames(schlenker_daily) <- c("fips","year","month","day","date",paste0("tmin_sr_",i),paste0("tmax_sr_",i),paste0("prec_sr_",i))
  if (exists(paste0(proj_dir, 'schlenker_daily_',i,'.dta'))){
    outfile <- rbind(prev, schlenker_daily)
  } else {
    outfile <- copy(schlenker_daily)
  }
  outfile <- unique(outfile)
  write_dta(data=outfile, path=paste0(proj_dir,"schlenker_daily_",i,".dta"))
  # Take out garbage
  rm(outfile)
  rm(schlenker_daily)
}


# Merge Monthly Simple into Unique Dataset ----------------------------------
print("Saving monthly area average.")
folder <- 'monthly/'
setwd(paste0(datadrive_dir,folder))
file_list <- list.files(pattern = 'sp')
for (file in file_list){
  print(file)
  # if the merged dataset doesn't exist, create it
  if (!exists('schlenker_monthly_sp')){
    load(file)
    schlenker_monthly_sp <- copy(monthly)
    rm(monthly)}
  # if the merged dataset does exist, append to it
  if (exists('schlenker_monthly_sp')){
    load(file) 
    schlenker_monthly_sp <- rbind(schlenker_monthly_sp, monthly)
    rm(monthly)
  }
  file.remove(file)
}
if (exists(paste0(proj_dir, 'schlenker_monthly_area.dta'))){
  prev <- as.data.table(read.dta13(file = paste0(proj_dir, 'schlenker_monthly_area.dta')))
}
schlenker_monthly_sp[, fips:=as.character(fips)]
schlenker_monthly_sp[nchar(fips)<5, fips:=paste0("0",fips)]
schlenker_monthly_sp[, year:=as.numeric(year)]
schlenker_monthly_sp[, month:=as.numeric(month)]
schlenker_monthly_sp[, gridNumber:=NULL]
schlenker_monthly_sp[, dateNum:=NULL]
colnames(schlenker_monthly_sp) <- c("fips","year","month","tmin_sr_area","tmax_sr_area","prec_sr_area")
if (exists(paste0(proj_dir, 'schlenker_monthly_area.dta'))){
  outfile <- rbind(prev, schlenker_monthly_sp)
} else {
  outfile <- copy(schlenker_monthly_sp)
}
write_dta(data=outfile, path=paste0(proj_dir,"schlenker_monthly_area.dta"))
# Take out garbage
rm(outfile)
rm(schlenker_monthly_sp)
rm(prev)

# Merge Monthly Weighted into Unique Dataset ----------------------------------
print("Saving monthly population average.")
folder <- 'monthly/'
setwd(paste0(datadrive_dir,folder))
file_list <- list.files(pattern = 'wg')
for (file in file_list){
  print(file)
  # if the merged dataset doesn't exist, create it
  if (!exists('schlenker_monthly_wg')){
    load(file)
    schlenker_monthly_wg <- copy(monthly_wg)
    rm(monthly_wg)}
  # if the merged dataset does exist, append to it
  if (exists('schlenker_monthly_wg')){
    load(file) 
    schlenker_monthly_wg <- rbind(schlenker_monthly_wg, monthly_wg)
    rm(monthly_wg)
  }
}
if (exists(paste0(proj_dir, 'schlenker_monthly_pop.dta'))){
  prev <- as.data.table(read.dta13(file = paste0(proj_dir, 'schlenker_monthly_pop.dta')))
}
schlenker_monthly_wg[, fips:=as.character(fips)]
schlenker_monthly_wg[nchar(fips)<5, fips:=paste0("0",fips)]
schlenker_monthly_wg[, year:=as.numeric(year)]
schlenker_monthly_wg[, month:=as.numeric(month)]
schlenker_monthly_wg[, gridNumber:=NULL]
schlenker_monthly_wg[, popLevel:=NULL]
schlenker_monthly_wg[, dateNum:=NULL]
colnames(schlenker_monthly_wg) <- c("fips","year","month","tmin_sr_pop","tmax_sr_pop","prec_sr_pop")
if (exists(paste0(proj_dir, 'schlenker_monthly_pop.dta'))){
  outfile <- rbind(prev, schlenker_monthly_wg)
} else {
  outfile <- copy(schlenker_monthly_wg)
}
write_dta(data=outfile, path=paste0(proj_dir,"schlenker_monthly_pop.dta"))
# Take out garbage
rm(outfile)
rm(schlenker_monthly_wg)
rm(prev)

# Delete unneeded files
setwd(paste0(datadrive_dir,'daily/'))
file_list <- list.files(pattern = 'wg')
unlink(file_list)
file_list <- list.files(pattern = 'sp')
unlink(file_list)
setwd(paste0(datadrive_dir,'monthly/'))
file_list <- list.files(pattern = 'wg')
unlink(file_list)
file_list <- list.files(pattern = 'sp')
unlink(file_list)


#file.remove(file)
#setwd(dir)
#unlink(paste0(dir,paste0('year',y,'/')), recursive = TRUE)
#file.remove(file)


# EOF
