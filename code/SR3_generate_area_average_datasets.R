## R code to calculate average weather in county from PRISM (Schenkler-Roberts)
## daily files. To add a new year to the data, simply change the year list at
## the beginning (years). This will run the averaging code for the new year
## and add it to the files from previous years. 
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

## Prepare for parallel 
num_cores <- detectCores()-1

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
#unzip <- 'No'
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
  cl = makeCluster(num_cores, outfile="")
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


# Append Daily Data into Unique Dataset ----------------------------------
app_files <- function(file, suffix){
  load(file)
  if(suffix=="sp"){
    return(daily)
  } else {
    return(daily_wg)
  }
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
  file_list <- list.files(pattern = suffix)
  tic()
  schlenker_daily <- rbindlist(pbmclapply(file_list, FUN=app_files, suffix=suffix, mc.cores=num_cores))
  toc()
  # Bring in previous years
  if (file.exists(paste0(proj_dir, 'schlenker_daily_',i,'.dta'))){
    prev <- as.data.table(read_dta(file = paste0(proj_dir, 'schlenker_daily_',i,'.dta')))
  }
  schlenker_daily[, fips:=as.character(fips)]
  schlenker_daily[nchar(fips)<5, fips:=paste0("0",fips)]
  #schlenker_daily[, year:=NULL]
  #schlenker_daily[, month:=NULL]
  #schlenker_daily[, day:=NULL]
  schlenker_daily[, gridNumber:=NULL]
  if(suffix == "wg"){
    schlenker_daily[, popLevel:=NULL]
  }
  colnames(schlenker_daily) <- c("fips","year","month","day","date",paste0("tmin_sr_",i),paste0("tmax_sr_",i),paste0("prec_sr_",i))
  if (file.exists(paste0(proj_dir, 'schlenker_daily_',i,'.dta'))){
    outfile <- rbind(prev, schlenker_daily)
  } else {
    outfile <- copy(schlenker_daily)
  }
  outfile <- unique(outfile)
  outfile <- outfile[order(fips, date), ]
  write_dta(data=outfile, path=paste0(proj_dir,"schlenker_daily_",i,".dta"))
  # Take out garbage
  rm(outfile)
  rm(schlenker_daily)
}

# Append Monthly Data into Unique Dataset ----------------------------------
app_files <- function(file, suffix){
  load(file)
  if(suffix=="sp"){
    return(monthly)
  } else {
    return(monthly_wg)
  }
}
for(i in c("area","pop")){
  print(paste0("Saving monthly ", i," average."))
  if(i == "area"){
    suffix <- "sp"
  } else {
    suffix <- "wg"
  }
  folder <- 'monthly/'
  setwd(paste0(datadrive_dir,folder))
  file_list <- list.files(pattern = suffix)
  tic()
  schlenker_monthly <- rbindlist(pbmclapply(file_list, FUN=app_files, suffix=suffix, mc.cores=num_cores))
  toc()
  # Bring in previous years
  if (file.exists(paste0(proj_dir, 'schlenker_monthly_',i,'.dta'))){
    prev <- as.data.table(read_dta(file = paste0(proj_dir, 'schlenker_monthly_',i,'.dta')))
  }
  schlenker_monthly[, fips:=as.character(fips)]
  schlenker_monthly[nchar(fips)<5, fips:=paste0("0",fips)]
  schlenker_monthly[, year:=as.numeric(year)]
  schlenker_monthly[, month:=as.numeric(month)]
  schlenker_monthly[, dateNum:=NULL]
  schlenker_monthly[, gridNumber:=NULL]
  if(suffix == "wg"){
    schlenker_monthly[, popLevel:=NULL]
  }
  colnames(schlenker_monthly) <- c("fips","year","month",paste0("tmin_sr_",i),paste0("tmax_sr_",i),paste0("prec_sr_",i))
  if (file.exists(paste0(proj_dir, 'schlenker_monthly_',i,'.dta'))){
    outfile <- rbind(prev, schlenker_monthly)
  } else {
    outfile <- copy(schlenker_monthly)
  }
  outfile <- unique(outfile)
  outfile <- outfile[order(fips, year, month), ]
  write_dta(data=outfile, path=paste0(proj_dir,"schlenker_monthly_",i,".dta"))
  # Take out garbage
  rm(outfile)
  rm(schlenker_monthly)
}

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
