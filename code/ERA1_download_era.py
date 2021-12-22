# File to Import Climate Date
# Hourly ERA5-Land data (0.1 degree resolution)
# Source: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview
# ERA5 single level data (0.25 degree resolution but maybe more accurate)
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form
#
# Original authors (but based largely on the Copernicus API template)
# Gabriel Gonzalez Sutil and Jeff Shrader 07/2019
#
# To run this code:
# Install cdsapi ('pip install cdsapi' should do it)
# Make sure you are registered to use the Copernicus API: https://cds.climate.copernicus.eu/api-how-to
# Edit the variables 1 through 6 below

# Preliminaries
import os
import cdsapi
import re

## Stuff to change to fit what you want
## 1. Weather fields 
variables = ['2m_temperature', 'total_precipitation']
## 2. Years
#     Be careful here. The top of the range should be 1 more than you want because 
#     Python is weird like that. You can go back to 1979.
years = [str(x) for x in range(1990,2020)]
## 3. Suffix you want to put on output files (like the country name). This is totally
#     up to you and not part of the API call
loc_suff = 'india'
## 4. Directory where you want to save files
directory = '/media/jgs/datadrive/data/weather/ecmwf/era5/'
## 5. ERA5 or ERA5-land?
#     For ERA5
dataset_name = 'reanalysis-era5-single-levels'
#     For ERA5-land
#dataset_name = 'reanalysis-era5-land'
## 6. If you want to download ERA5 (not land), then you need to specify
#     what product types you want. Options are 'ensemble_mean', 'ensemble_members', 'ensemble_spread', 'reanalysis'
ptypes = ['ensemble_spread',]

## Everything below here should run just fine without edits
# Only make changes if you want to download a subset of months, subset of days in month,
# or subset of hours in day. Right now it downloads everything and saves one file per month.
#
# The files have daily or sub-daily data but are saved monthly. You shouldn't need to edit
# this unless you only want specific months of the year.
months = ['01','02','03',
          '04','05','06',
          '07','08','09',
          '10','11','12']
## Bounding box (in N,W,S,E order, -180 to 180 longitude format)
# I tend to be a bit generous with the bounding boxes (round at least to next bigger lat/lon)
# Lots of sources for these bounding boxes. Here are some:
# https://gist.github.com/graydon/11198540
# https://boundingbox.klokantech.com/
# Global
# [90, -180, -90, 180,]
# India bounding box
# N: 36.5, E: 103.2, S: 5.3, W: 66.7
# [36.5, 66.7, 5.3, 103.2,]
# Continental US
# [50, -126, 23, -66,]
# Mexico
# [33, -118, 14, -86,]
if loc_suff is 'india':
    area_box = [36.5, 66.7, 5.3, 103.2,]
elif loc_suff is 'mexico':
    area_box = [33, -118, 14, -86,]

# Change Directory and create folders (if needed)
os.chdir(directory)
for i in variables: 
    print(i)
    for j in ptypes:
        fold = i + "_" + j + "_" + loc_suff
        try:
            os.mkdir(fold)
            print('Directory' ,fold,  'Created') 
        except FileExistsError:
            print('Directory' ,fold,  'already exists')

# Get the dataset type
ds = re.search(r"land", dataset_name)
if ds is None:
    data_prefix = "era5"
else:
    data_prefix = "era5land"


# Retrieve Data
c = cdsapi.Client()
for i in variables:
    for j in ptypes:
        # Generate API call set
        if data_prefix is "era5":
            ptype = {'product_type': j}
        folder = directory + i + "_" + j + "_" + loc_suff
        os.chdir(folder)
        for y in years:
            for m in months:
                file = data_prefix + '_' + y + m + '.grib'
                path = folder + '/' + file 
                print(file)
                print(path)
                exists = os.path.isfile(path)
                call_set = {
                    'format': 'grib',
                    'variable': i,
                    'year': y,
                    'month': m,
                    'day': [
                        '01', '02', '03',
                        '04', '05', '06',
                        '07', '08', '09',
                        '10', '11', '12',
                        '13', '14', '15',
                        '16', '17', '18',
                        '19', '20', '21',
                        '22', '23', '24',
                        '25', '26', '27',
                        '28', '29', '30',
                        '31',
                    ],
                    'time': [
                        '00:00', '01:00', '02:00',
                        '03:00', '04:00', '05:00',
                        '06:00', '07:00', '08:00',
                        '09:00', '10:00', '11:00',
                        '12:00', '13:00', '14:00',
                        '15:00', '16:00', '17:00',
                        '18:00', '19:00', '20:00',
                        '21:00', '22:00', '23:00',
                    ],
                    'area': area_box,
                }
                if data_prefix is "era5":
                    call_set.update(ptype)
                if exists:
                    print('File already exists')
                else:
                    print('Retrieving file')
                    c.retrieve(
                        dataset_name,
                        call_set,
                        file)


