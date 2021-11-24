* Stata program to clean up the various weather datasets
* 
* Jeff Shrader
* First: 2018/11/30
* Time-stamp: <2019-07-05 12:50:12 jgs>

* Preliminaries
if c(hostname) == "albatross" {
   local dir "C:/Users/jgs/Dropbox/research/projects/active/Weather_Forecasts_and_Mortality"
}
else if {
   local dir "~/Dropbox/research/projects/active/Weather_Forecasts_and_Mortality"
}
local data "`dir'/Data/weather"
cd "`data'"

* R saves all numeric variables as double.
use ghcn_1999_2017.dta, clear
compress
local degC = ustrunescape("\u2103")
label var ppt_ghcn "Daily cumulative precipitation GHCN (mm)"
label var snow_ghcn "Snowfall GHCN (mm)"
label var tmax_ghcn "Max temperature GHCN (`degC')"
label var tmin_ghcn "Min temperature GHCN (`degC')"
order state_fips county_fips date
sort state_fips county_fips date
save ghcn_1999_2017.dta, replace

use prism_1999_2017.dta, clear
compress
label var ppt_area "Daily cumulative precipitation PRISM, area weighted (mm)"
label var ppt_pop "Daily cumulative precipitation PRISM, pop weighted (mm)"
label var tmax_area "Max temperature PRISM, area weighted (`degC')"
label var tmax_pop "Max temperature PRISM, pop weighted (`degC')"
label var atmax_area "Max apparent temperature PRISM, area weighted (`degC')"
label var atmax_pop "Max apparent temperature PRISM, pop weighted (`degC')"
label var tmin_area "Min temperature PRISM, area weighted (`degC')"
label var tmin_pop "Min temperature PRISM, pop weighted (`degC')"
label var atmin_area "Min apparent temperature PRISM, area weighted (`degC')"
label var atmin_area "Min apparent temperature PRISM, pop weighted (`degC')"
order state_fips county_fips date
sort state_fips county_fips date
save prism_1999_2017.dta, replace

foreach i in pop area {
   use schlenker_daily_`i'.dta, clear
   capture drop year month day
   capture rename dateNum date
   gen year = year(date)
   drop if year <= 1998
   capture drop gridNumber
   capture drop popLevel
   compress
   capture rename tMin sr_tmin_`i'
   capture rename tMax sr_tmax_`i'
   capture rename prec sr_prec_`i'
   capture tostring fips, replace
   replace fips = "0" + fips if length(fips) < 5
   replace fips = "0" + fips if length(fips) < 5
   label var sr_tmin_`i' "Schlenker-Roberts min temp, `i' weighted (`degC')"
   label var sr_tmax_`i' "Schlenker-Roberts min temp, `i' weighted (`degC')"
   label var sr_prec_`i' "Schlenker-Roberts precipitation, `i' weighted (mm)"
   save schlenker_daily_`i'.dta, replace
}
