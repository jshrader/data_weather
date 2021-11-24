# R code to take gridded University of Delaware (UDEL) weather data--used in
# DJO, BHM, etc.-- 
#
# Citations:
# Willmott, C. J. and K. Matsuura (2001) Terrestrial Air Temperature and Precipitation: Monthly and Annual Time Series (1950 - 1999), http://climate.geog.udel.edu/~climate/html_pages/README.ghcn_ts2.html.
# If you acquire UDel_AirT_Precip data products from PSL, we ask that you acknowledge us in your use of the data. This may be done by including text such as UDel_AirT_Precip data provided by the NOAA/OAR/ESRL PSL, Boulder, Colorado, USA, from their Web site at https://psl.noaa.gov/ in any documents or publications using these data. We would also appreciate receiving a copy of the relevant publications. This will help PSL to justify keeping the UDel_AirT_Precip data set freely available online in the future. Thank you!
#
# To do:
# . Use GADM or other global boundary files to generate area averages
# . Output files at different levels of aggregation: county, state, country
# .. We would ideally population weight using NASA data
# . For projects that can use gridded data, we could also indicate how to merge those data
