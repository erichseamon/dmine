library(raster)
library(rgdal)
library(leaflet)

r <- raster("/agmesh-scenarios/scenario_52177/raster_commodity/2003.3.Wheat_raster.grd")
#r <- raster("/agmesh-scenarios/scenario_52177/raster_commodity/bi_nov_2008.nc")
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                    na.color = "transparent")


setwd("/nethome/erichs/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
WA <- counties[grep("Washington", counties@data$STATE_NAME),]


#proj4string(r) <- CRS("+init=epsg:3857") # OSM Mercator projection

#leaflet() %>% addTiles() %>%
#  addRasterImage(r, colors = pal, opacity = 0.8) %>%
#  addLegend(pal = pal, values = values(r),
#            title = "Surface temp")



rgb.palette <- colorRampPalette(c("blue", "green"))
levelplot(r, att='ACRES', col.regions=rgb.palette(120) + layer(sp.polygons(WA)))



