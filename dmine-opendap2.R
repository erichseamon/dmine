# Load the packages:
library("ncdf4")
#library("ncdf") ## oude versie

library(geoknife)
library(maptools)

#---counties
scen_state = "Idaho"

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]

counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

#--loop list for county by fip
countyfiploop <- counties@data$FIPS

#--data frame of county fip list
countyfiplist <- data.frame(counties@data$FIPS)

#--data frame of county names
countynames <- data.frame(counties@data$NAME)

#combo of county names and fip for this list
countylist <- cbind(countynames, countyfiplist)

#--number of rows in county list
countylistrows <- 12 * nrow(countylist)

list <- list.files(path = dirname)
list2 = list
list2 = substr(list2,1,nchar(list2)-3)
list3 <- data.frame(list2)

listcols <- nrow(list)




#-----

counties_p <- polygons(counties)

stencil <- simplegeom(Srl = list(counties@polygons[[4]]), proj4string = CRS("+proj=longlat +datum=WGS84"))



stencil <- simplegeom(Srl = list(Srs1,Srs2,Srs3), proj4string = CRS("+proj=longlat +datum=WGS84"))



WFSServer?request=GetCapabilities



stencil <- webgeom()
stencil <- webgeom(url = "https://tigerweb.geo.census.gov/arcgis/rest/services/Generalized_ACS2016/State_County/MapServer/")
#stencil <- webgeom('state::Washington')

query(counties,'geoms')
geom(stencil) <- 'sample:States'  
fabric <- webdata(url='dods://cida.usgs.gov/thredds/dodsC/UofIMETDATA')
query(fabric,'variables')


query(stencil, 'attributes')

attribute(stencil) <- 'FIPS'
query(stencil, 'values')
values(stencil) <- "16021"  






variables(fabric) <- 'precipitation_amount'
query(fabric,'times')
times(fabric) <- c('2006-01-01', '2007-01-01')


job <- geoknife(stencil, fabric, wait=TRUE)

data.out <- result(job, with.units=TRUE)
data.out


url_grid <- "https://cida.usgs.gov/thredds/dodsC/UofIMETDATA"

## open connection
VOLKRK02_O2 <- nc_open(url_grid)

## Look what's inside
str(ncatt_get(VOLKRK02_O2, "O2"))
str(ncatt_get(VOLKRK02_O2, "time"))

## Get time series data
G.time <- ncvar_get(VOLKRK02_O2,"time")
G.O2 <- ncvar_get(VOLKRK02_O2,'precipitation_amount')

## Convert G.time to real date
date <- as.Date(G.time, origin = "1970-01-01 00:00:00 +01:00")

## Plot data against time
plot(date, G.O2,"l", main = "Oxygen in VOLKERAK02")

##close connection
nc_close(VOLKRK02_O2)