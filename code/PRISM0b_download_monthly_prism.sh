#!/usr/bin/bash

# Guidance on how to download
# https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf


declare -a arr=("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12")

# Get the 1981 to present data
for t in `seq 1981 2020`
do
    echo "$t"
    for m in "${arr[@]}"
    do
        echo "$m"
        cd /media/jgs/datadrive/data/weather/prism/prism_monthly/tmean/zip
        wget --content-disposition http://services.nacse.org/prism/data/public/4km/tmean/$t$m
        cd /media/jgs/datadrive/data/weather/prism/prism_monthly/tmin/zip
        wget --content-disposition http://services.nacse.org/prism/data/public/4km/tmin/$t$m
        cd /media/jgs/datadrive/data/weather/prism/prism_monthly/tmax/zip
        wget --content-disposition http://services.nacse.org/prism/data/public/4km/tmax/$t$m
        cd /media/jgs/datadrive/data/weather/prism/prism_monthly/ppt/zip
        wget --content-disposition http://services.nacse.org/prism/data/public/4km/ppt/$t$m
    done
done

# Note that there is a structural break in the way PRISM handles data prior to 1981
# Get the older data
#for t in `seq 1974 1980`
#do
#    echo "$t"
#    cd ~/Dropbox/research/data/weather/data/prism/ppt_1980/zip
#    wget --content-disposition http://services.nacse.org/prism/data/public/4km/ppt/$t
#    cd ~/Dropbox/research/data/weather/data/prism/tmean_1980/zip
#    wget --content-disposition http://services.nacse.org/prism/data/public/4km/tmean/$t
#done

