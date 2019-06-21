


# Download NOAA-provided zipped data
wget https://www.spc.noaa.gov/gis/svrgis/zipped/1950-2017-torn-aspath.zip -P data/


# Unzip the data
unzip -j data/1950-2017-torn-aspath.zip -d data/1950-2017-tornado-aspath


# Delete the remaining .zip
rm data/1950-2017-torn-aspath.zip


