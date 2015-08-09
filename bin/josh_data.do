* Stata program to collapse and combine daily weather data for the US
*
* Jeff Shrader
* First: 2015/07/21
* Time-stamp: <2015-07-22 16:05:41 jgs>

* Preliminaries
local dir "~/google_drive/research/data/weather"
local data_ext "/Volumes/Data 2TB/data/weather"
cd "`data_ext'"
local download = 0
capture log close
log using "`dir'/bin/josh_weather_data.log", append

* This is kludgey, but I need a datelist. This should be generated from dir
local datelist "201403 201404 201405 201406 201407 201408 201409 201410 201411 201412 201501 201502 201503 201504 201505 201506"

* We can add some code eventually to download the files directly.
* Just modify this code:
/*
if `download’ == 1 {
   cd "`dir'/data/raw"
   foreach i of local pollutants  {
      foreach t of local years {
         shell /usr/local/bin/wget -Odaily_`i'_`t'.zip "http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_`i'_`t'.zip"
      }
   }
}
*/

* Unpack the data, collapse the hourly, and combine with the daily
* Unzip
* local filelist: dir . files "QCLCD*.zip"
/*foreach i of local filelist {
   di "`i'"
   * shell gzip -d `i'
   shell unzip `i'
   * unzipfile `i'
}*/

* Iterate through, grabbing humidity and cloud cover from hourly
foreach i of local datelist {
   di "`i'"
   import delimited using "`i'hourly.txt", clear
   keep wban date time stationtype skycondition visibility relativehumidity
   destring relativehumidity, replace ignore("M")
   gen clear = (substr(skycondition,1,3) == "CLR" | substr(skycondition,1,2) == "VV" | substr(skycondition,1,3) == "FEW")
   replace clear = . if skycondition == "M"
   gen clouds = 1 - clear
   * Destring visibility as alternative to clouds
   destring visibility, replace ignore("M")
   * There are repeat observations for reasons that aren't totally clear
   duplicates drop
   bysort wban date time: gen count = _N
   tab count
   tostring date, replace
   gen date1 = date(date, "YMD")
   format date1 %d
   drop date
   rename date1 date
   collapse (mean) visibility relativehumidity clouds, by(wban date stationtype)

   sort wban date
   save hourly_to_daily.dta, replace

   * Now process the daily data
   import delimited using "`i'daily.txt", clear
   replace preciptotal = strtrim(preciptotal)
   replace preciptotal = "0.001" if preciptotal == "T"
   replace preciptotal = "" if preciptotal == "err"
   foreach v in tmin tavg tmax preciptotal {
      destring `v', replace ignore("M")
   }

   keep wban yearmonthday tmax tmin tavg preciptotal
   tostring yearmonthday, replace
   gen date = date(yearmonthday, "YMD")
   format date %d
   drop yearmonthday
   sort wban date
   save daily.dta, replace

   * Put it all together with station data
   import delimited using "`i'station.txt", clear delimiters("|")
   keep wban wmo callsign name state latitude longitude groundheight timezone
   drop if wban == .
   merge 1:m wban using daily.dta
   keep if _merge == 3
   drop _merge

   merge 1:1 wban date using hourly_to_daily.dta
   keep if _merge == 1 | _merge == 3
   drop _merge
   save weather_station_`i'.dta, replace
}

*32.6887236,-117.1402145

* Append them all together
local dtalist: dir . files "weather_station*.dta"
gen drop = 1
append using `dtalist'
drop if drop == 1
drop drop 
saveold weather_station.dta, replace

* clean up
rm hourly_to_daily.dta
rm daily.dta
foreach i of local datelist {
   * rm "weather_station_`i'.dta"
   rm "`i'daily.txt"
   rm "`i'hourly.txt"
   rm "`i'monthly.txt"
   rm "`i'precip.txt"
   rm "`i'remarks.txt"
   rm "`i'station.txt"
}

* Move the data to drive
zipfile weather_station.dta, saving(weather_station.dta.zip, replace)
* copy weather_station.dta "`dir'/data/weather_station.dta"

log close
