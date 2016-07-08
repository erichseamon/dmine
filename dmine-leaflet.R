library(raster)
library(rgdal)
library(leaflet)

r <- raster("/agmesh-scenarios/scenario_52177/bi_nov_2008.nc")
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                    na.color = "transparent")


proj4string(r) <- CRS("+init=epsg:3857") # OSM Mercator projection

leaflet() %>% addTiles() %>%
  addRasterImage(r, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(r),
            title = "Surface temp")
