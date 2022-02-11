#!/usr/bin/perl 
use strict; 
use warnings; 
use DateTime;
use File::chdir;

# Guidance on how to download
# https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf
# This code is taken practically verbatim from that document

# Get the 1981 to present data
# 'ppt',
my $day; 
#my @clim_var = ('ppt','tmax','tmin','atmax','atmin','tdmean','tmean','vpdmax','vpdmin');
my @clim_var = ('ppt'); 
my $base_url = 'http://services.nacse.org/prism/data/public/4km'; 
my $stop = DateTime->new( day => 31, month => 12, year => 2021 );
for my $var (@clim_var){
    my $start = DateTime->new( day => 1, month => 1, year => 2021 ); 
    while($start <= $stop) { 
        $day = $start->strftime('%Y%m%d');  #place date in proper format
        chdir("/media/jgs/datadrive/data/weather/prism/prism_daily/$var/zip/") or die "$!";
        system("wget -N --content-disposition $base_url/$var/$day"); 
        sleep 1;  #to be nice to our server 
        $start->add(days => 1);
    }
}
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

