# Weather Data
Code to process different weather data. The different programs have prefixes that indicate which weather data source they process. 

Currently the code covers: 
1. GHCN station data (hypothetically global, but current code is focused on US)
2. PRISM daily and monthly gridded data (just US)
3. Berkeley Earth gridded data of various types (global coverage back to 1750 for some regions)
4. Schlenker-Roberts version of PRISM data with consistent underlying weather stations (US)
5. ERA5 gridded data (global)

Partially implemented 
1. (ISD)[https://www.ncei.noaa.gov/products/land-based-station/integrated-surface-database]
2. (University of Delaware)[https://psl.noaa.gov/data/gridded/data.UDel_AirT_Precip.html] (used in many global climate analyses, but only available from 1900-2014)
