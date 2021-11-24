= Weather Data = 

This document describes various weather data sources.

== Summary ==
 
=== Skeleton ===
Type:
Directory:
File names:
Source:
Notes: 

== Datasets ==


==== USCRN  ====
Type:
    Station
    USA
Directory:
File names:
Source:
Citation:
Variables:
Description:

=== GHCN ===
==== GHCN-Daily station data: Annual  ====
Type:
    Global weather stations
    Global
    
Directory:
    Data 2TB/data/weather/data/ghcn/    
File names:
      
Source:
    ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/
Citation:

Variables:
    See http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/readme.txt

Description:
    General description can be found here:
      https://www.ncdc.noaa.gov/oa/climate/ghcn-daily/

    Documentation is provided in doc/ghcn-daily-by_year-format.rtf. This file
    describes variables and labels.

    A more comprehensive documentation for the daily dataset can be found here:
      http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt


==== GHCND Station information ====
Directory: 
File names:
Source:
Citation:
Variables:
    Variables and labels are under section IV of
      http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt

Description:
    GHCND weather station metadata. 

Notes: 
    Humidity and cloud data are more sparse than precipitation or temperature data. For weather
    stations, I have pulled what exists, so far as I can tell. There are satellite
    products, but the one I can find only has data through 2009. It is ISCCP Cloud
    Data here:
      http://www.ncdc.noaa.gov/data-access/satellite-data/satellite-data-access-datasets


=== NCEP Reanalysis ===
Type:
    Gridded, 2.5x2.5 degree
    Global
    4-times daily, daily, monthly
    1948/01/01 to present (2015/10/13)
Directory:
    Data 2TB/data/weather/data/ncep_reanalysis/
File names:
    air.sig995.YYY.nc -- Near surface temperature (.995 sigma level)
    air.mon.mean.nc -- Monthly average surface temperature
    pr_wtr.mon.mean.nc -- Monthly average precipitable water at surface
Source:
    http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html
Notes:
    Files are in netCDF4-classic format.
Reference:
    Kalnay et al.,The NCEP/NCAR 40-year reanalysis project, Bull. Amer. Meteor. Soc., 77, 437-470, 1996.
    If you acquire NCEP Reanalysis data products from PSD, we ask that you acknowledge us in your use of the data. This may be done by including text such as NCEP Reanalysis data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at http://www.esrl.noaa.gov/psd/ in any documents or publications using these data. We would also appreciate receiving a copy of the relevant publications.

=== NCEP Reanalysis II ===
Type:
    Gridded, 2.5x2.5 degree
    Global
    4-times daily, daily, monthly
    1979/01/01 to present (2015/07/31)
Directory:
    Data 2TB/data/weather/data/ncep_reanalysis_2/
File names:
    pr_wtr.eatm.mon.mean.nc -- Monthly average precipitable water at surface
    skt.sfc.mon.mean.nc -- Monthly Skin temperature
    tmin.2m.mon.mean.nc -- Monthly mean minimum temperature
    tmax.2m.mon.mean.nc -- Monthly mean maxmimum temperature
    air.2m.mon.mean.nc -- Monthly mean air temperature
Source:
    http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis2.html
Notes:
    Files are in netCDF4-classic format.
Reference:
    NCEP-DOE AMIP-II Reanalysis (R-2): M. Kanamitsu, W. Ebisuzaki, J. Woollen, S-K Yang, J.J. Hnilo, M. Fiorino, and G. L. Potter. 1631-1643, Nov 2002, Bulletin of the American Meterological Society.
    If you acquire NCEP/DOE 2 Reanalysis data products from PSD, we ask that you acknowledge us in your use of the data. This may be done by including text such as NCEP/DOE 2 Reanalysis data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at http://www.esrl.noaa.gov/psd/ in any documents or publications using these data. We would also appreciate receiving a copy of the relevant publications. 

=== CPC Merged Analysis of Precipitation (CMAP) ===
Type:
    Gridded, 2.5x2.5 degree (144x72)
    Global
    Monthly
    1979/01 to present (2015/09)
Directory:
    data/weather
File names:
    precip.mon.mean.nc -- Monthly average precipitation
    precip.mon.ltm.nc -- Long-term monthly average precipitation, 1981-2010 climatology
Source:
    http://www.esrl.noaa.gov/psd/data/gridded/data.cmap.html        
Notes:
    Files are in netCDF format.
    This is the precipitation file used for verification of the IRI seasonal
    forecasts. The climatology is not correct, however.
Reference:
    Xie, P., and P.A. Arkin, 1997: Global precipitation: A 17-year monthly analysis based on gauge observations, satellite estimates, and numerical model outputs. Bull. Amer. Meteor. Soc., 78, 2539 - 2558.
    If you acquire data products from PSD, we ask that you acknowledge us in your use of the data. This may be done by including text such as NCEP/DOE 2 Reanalysis data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at http://www.esrl.noaa.gov/psd/ in any documents or publications using these data. We would also appreciate receiving a copy of the relevant publications. 

=== GHCN CAMS 2m Temperature (Land) ===
Type:
    Gridded, 0.5x0.5 degree 
    Global
    Monthly
    1950/01 to present (2015/09)
Directory:
    data/weather
File names:
    air.mon.mean.nc -- Monthly average precipitation
    air.mon.ltm.nc -- Long-term monthly average precipitation, 1981-2010 climatology
Source:
    http://www.esrl.noaa.gov/psd/data/gridded/data.ghcncams.html
Notes:
    Files are in netCDF format.
    This is the precipitation file used for verification of the IRI seasonal
    forecasts. The climatology is not correct, however.
Reference:
    Fan, Y., and H. van den Dool (2008), A global monthly land surface air temperature analysis for 1948-present, J. Geophys. Res., 113, D01103, doi:10.1029/2007JD008470.
    If you acquire data products from PSD, we ask that you acknowledge us in your use of the data. This may be done by including text such as NCEP/DOE 2 Reanalysis data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at http://www.esrl.noaa.gov/psd/ in any documents or publications using these data. We would also appreciate receiving a copy of the relevant publications. 



=== PRISM ===
Type:
Directory:
File names:
    
Source:
    http://www.prism.oregonstate.edu/recent/
Notes:
    http://www.prism.oregonstate.edu/documents/PRISM_datasets.pdf




=== Quality Controlled Local Climatological Data (QCLCD) ===
Type:
    Station data for ~1,600 quality controlled locations
    USA
    Hourly
    2005-01-01 to present (2015-10-13)
Directory:
    Data 2TB/data/weather/data/qclcd/
File names:
    QCLCDYYYYMM.zip contains
        YYYYMMdaily.txt
        YYYYMMhourly.txt
        YYYYMMmonthly.txt
        YYYYMMprecip.txt
        YYYYMMremarks.txt
        YYYYMMstation.txt
Source:
    http://www.ncdc.noaa.gov/qclcd/QCLCD

Variables:
   For list, see the documentation at the source url.

Description:
    General description can be found here:
      http://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/quality-controlled-local-climatological-data-qclcd
    Note this heartbreaking answer to an FAQ on precipitation:
      Q:  Is it possible to distinguish between missing precipitation
           observations and 0 precipitation?
      A:  0 is not a reportable value in METAR; thus we cannot distinguish.  


