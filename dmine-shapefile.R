require("rgdal")
require("rgeos")
require("dplyr")

setwd("/nethome/erichs/aezpoly3/")

aezpoly <- readShapePoly('aezpoly3.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#region <- gUnaryUnion(region, id = region@data$country)

require(sp) 
a = aggregate(counties, counties$GRIDCODE, dissolve = TRUE) 
