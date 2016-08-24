library(raster)
library(rgdal)
library(leaflet)
library(maptools)

#pick your commodity raster

commodity_plot <- function(commodity_var,year_var,month_var) {
  
  rpre <- paste("/agmesh-scenarios/scenario_52177/raster_commodity/", year_var, ".", month_var, ".", commodity_var, "_raster.grd", sep="")  
  r <- raster(rpre)
  #r <- raster("/agmesh-scenarios/scenario_52177/raster_commodity/2003.3.Wheat_raster.grd")

  setwd("/nethome/erichs/counties/")
  counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  WA <- counties[grep("Washington", counties@data$STATE_NAME),]

  rgb.palette <- colorRampPalette(c("blue", "green"))
  levelplot(r, att='ACRES', col.regions=rgb.palette(120)) + layer(sp.polygons(WA))
}


