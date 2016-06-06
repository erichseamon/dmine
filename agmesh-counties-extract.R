library("maptools")
library("sirad")
library("reshape")
library("reshape2")
library("RNetCDF")
require(rgdal)
require(rgeos)

library(raster)
setwd("/nethome/erichs/counties/")


counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


countyfiplist <- counties@data$FIPS

for (i in countyfiplist) {
subset_county <- counties[counties@data$FIPS == i,]
}




b <- brick("/reacchspace/obj1/netcdf/MET/data/bi_1979.nc")
ee <- counties[counties$FIPS == i, ]


                    
e <- extract(b, counties) 

(where p is a SpatialPolygons* object) 