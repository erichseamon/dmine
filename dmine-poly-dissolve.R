library(rgeos)
library(rgdal)
library(dplyr)
library(maptools)


setwd("/nethome/erichs/aezpoly3")
aezpoly <- readShapePoly('aezpoly3.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

aezpoly_dissolve <- gUnaryUnion(aezpoly, id = aezpoly@data$GRIDCODE)
