#------------------------------------------------------------------------#
# TITLE:        agmesh-commodityplot.R
# AUTHOR:       Erich Seamon
# INSTITUITON:  College of Natural Resources
#               University of Idaho
# DATE:         May-June. 2016
# STAGE:        DMINE extraction and subsetting of UI GRIDMET data for machine
#               learning analysis
#
# COMMENTS:     function that generates a commodity plot using the following 
#               inputs: 
#               commodity_plot(<commodity>,<year>,<month>)
#               example: commodity_plot("Wheat",2003,4)
#                
#               More on the dmine model: dmine.io
#------------------------------------------------------------------------#

library(raster)
library(rgdal)
library(leaflet)
library(maptools)
library(png)
library(jpeg)
library(ncdf)

commodity_plot <- function(commodity_var,year_var,month_var) {
  rpre <- paste("/waf/USDA/crop_indemnity_raster_commodity/", year_var, ".", month_var, ".", commodity_var, "_raster.grd", sep="")  
  r <- raster(rpre)
  setwd("/nethome/erichs/counties/")
  counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  WA <- counties[grep("Washington", counties@data$STATE_NAME),]
  rgb.palette <- colorRampPalette(c("blue", "green"))
  levelplot(r, att='ACRES', col.regions=rgb.palette(120)) + layer(sp.polygons(WA))
}

