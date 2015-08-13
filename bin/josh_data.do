* Stata program to collapse and combine daily weather data for the US
*
* Jeff Shrader
* First: 2015/07/21
* Time-stamp: <2015-08-09 20:15:02 jgs>

* Preliminaries
local dir "~/google_drive/research/data/weather"
local data_ext "/Volumes/Data 2TB/data/weather"
cd "`data_ext'"
local download 0
capture log close
log using "`dir'/bin/josh_weather_data.log", append

* This is kludgey, but I need a datelist. This should be generated from dir
local datelist "201403 201404 201405 201406 201407 201408 201409 201410 201411 201412 201501 201502 201503 201504 201505 201506"

* We can add some code eventually to download the files directly.
* Just modify this code:
if "`download’" == "1" {
   cd "`dir'/data/raw"
   foreach i of local pollutants  {
      foreach t of local years {
         shell /usr/local/bin/wget -Odaily_`i'_`t'.zip "http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_`i'_`t'.zip"
      }
   }
}

* Unpack the data, collapse the hourly, and combine with the daily
* Unzip
/*
local filelist: dir . files "QCLCD*.zip"
foreach i of local filelist {
   di "`i'"
   * shell gzip -d `i'
   shell unzip `i'
   * unzipfile `i'
}
*/
* Iterate through, grabbing humidity and cloud cover from hourly
foreach i of local datelist {
   di "`i'"
   import delimited using "QCLCD`i'/`i'hourly.txt", clear
   * keep wban date time stationtype drybulbcelsius hourlyprecip skycondition visibility relativehumidity
   keep wban date time drybulbcelsius hourlyprecip skycondition visibility relativehumidity
   destring drybulbcelsius, replace ignore("M")
   replace hourlyprecip = strtrim(hourlyprecip)
   replace hourlyprecip = "0.001" if hourlyprecip == "T"
   destring hourlyprecip, replace ignore("M")
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
   drop count clear skycondition
   rename hourlyprecip precipitation
   rename drybulbcelsius temp_celsius
   * Collapsing to hourly
   replace time = time/100
   gen hour = floor(time)
   collapse (mean) temp_celsius precipitation visibility relativehumidity clouds, by(wban date hour)
   bysort wban date: egen max_temp = max(temp)
   bysort wban date: egen min_temp = min(temp)
   
   export delimited using "`i'hourly_proc.csv", replace
}

log close
