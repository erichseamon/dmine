library(maptools)
library(leaflet)
library(ggplot2)
library(gridExtra)

#--add Norwest boundary processing units

setwd("/dmine/data/norwest/")

projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

streamtemp <- readShapePoints('NorWeST_ObservedTempPoints_WACoast_wgs84/NorWeST_ObservedTempPoints_WACoast_wgs84.shp', proj4string = projection)

setwd("/dmine/data/norwest/NorWeST_WBD_ProcessingUnits_wgs84/")

units <- readShapePoly('NorWeST_WBD_ProcessingUnits_wgs84.shp', 
                        proj4string=CRS
                        ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#--add Norwest Predicted stream temperature points for the WA Coast region
setwd("/dmine/data/norwest/NorWeST_PredictedStreamTempPoints_WACoast_wgs84/")

WACoast_points_pred <- readShapePoints('NorWeST_PredictedStreamTempPoints_WACoast_wgs84.shp', 
                       proj4string=CRS
                       ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#--add Norwest Predicted stream temperature lines for the WA Coast region
setwd("/dmine/data/norwest/NorWeST_PredictedStreamTempLines_WACoast_wgs84/")

WACoast_streams_pred <- readShapeLines('NorWeST_PredictedStreamTempLines_WACoast_wgs84.shp', 
                                       proj4string=CRS
                                       ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#--add the US states boundary
setwd("/dmine/data/states/")

states <- readShapePoly('states.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#--add the US streams data if necessary
#setwd("/dmine/data/streams/")

#streams <- readShapeLines('USAstreams.shp', 
#                        proj4string=CRS
#                        ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#load the WACoast observed temperature locations - all locations.
setwd("/dmine/data/norwest/")
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
streamtemp <- readShapePoints('NorWeST_ObservedTempPoints_WACoast_wgs84/NorWeST_ObservedTempPoints_WACoast_wgs84.shp', proj4string = projection)

#load the WACoas attribute data file for the WACoast - Monthly Summaries
streamtemp_att <- read.csv("NorWeST_ObservedStreamTemp_TempMonthlySummaries_WashingtonCoast_AllDays.csv")

#-the range of years and months for our loop of every time step of stream temperature data
monthyearspan <- unique(streamtemp_att$MonthYear)


for (i in monthyearspan) {
stm <- subset(streamtemp_att, MonthYear == i)
streamtemp_joined <-  merge(streamtemp, stm, by='OBSPRED_ID')
                            
stmm <- as.data.frame(streamtemp_joined)
stmm2 <- as.numeric((stmm[,"MonthlyMax"]))
stmm3 <- as.data.frame(stmm2)

lengthh <- length(streamtemp_joined)
data_seq = seq(min(na.omit(as.numeric(stmm3$stmm2))), max(na.omit(as.numeric(stmm3$stmm2))), length=length(streamtemp_joined))
col_pal = colorRampPalette(c('blue', 'red'))(lengthh)
cols = col_pal[ cut(as.numeric(stmm3$stmm2), data_seq, include.lowest=T) ]

sta_unique <- unique(streamtemp_att$MonthlyMean)
col_pal2 = colorRampPalette(c('blue', 'red'))(length(sta_unique))
cols2 = col_pal2[ cut(streamtemp_att$MonthlyMean, data_seq, include.lowest=T) ]
cols3 <- na.omit(cols2)
length(cols3)
cols4 <- cols3[1:length(cols3)]

layout(matrix(2:1,nrow=2), width = c(1,2),height = c(1,1))
plotpath <- paste("/dmine/data/norwest/WACoast_monthly/", "WACoast_", i, ".png", sep="")
png(filename=plotpath)
plotz <- plot(subset(units, GNLCCunit == "WACO"))


cols2 <- na.omit(cols)
ll <- length(cols2)
cols3 <- cols2[1:ll]
lines(WACoast_streams_pred, col = 'lightgray')
points(streamtemp_joined, col = cols, pch=16, cex=1)

#legend_image <- as.raster(matrix(cols4), ncol=1)
#plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'legend title')
#text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
#rasterImage(legend_image, 0, 0, .25 ,1)

 MonthYearz <- streamtemp_att$MonthYear
streamtemp_att$sort_order <- streamtemp_att$SampleYear *100 + streamtemp_att$SampleMonth
p <- ggplot(streamtemp_att) + geom_boxplot(aes(x=reorder(MonthYear, sort_order), y=MonthlyMean))
#p <- ggplot(streamtemp_att) + geom_boxplot(aes(x = MonthYear, y = MonthlyMean, group = MonthYear, las=3))
p1 <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))


dev.off()



   
}


#streamtemp_joined <- data.frame(streamtemp_joined)

library(raster)

units_WA <- subset(units, NORWEST == "WA Coast")

inputfile <- "/dmine/data/USDA/agmesh-scenarios/Washington/netcdf/pdsi_jun_2006.nc"
ipf <- nc_open(inputfile)
ipflat <- ipf$dim$lat$vals
ipflon <- ipf$dim$lon$vals
ipfl <- cbind(ipflon, ipflat)

# Grab the lat and lon from the data
lat <- raster(inputfile, dimname="lat")


# Convert to points and match the lat and lons
lonlat <- rasterToPoints(lat)
lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))

lonlat_pnw <- lonlat[units_WA,]


lonlat_pnw <- lonlat[lonlat$x < -117 & lonlat$x > -125,]
lonlat_pnw <- lonlat_pnw[lonlat_pnw$y > 45 & lonlat_pnw$y < 52,]

#----

ncin <- open.ncdf(inputfile)
lon <- get.var.ncdf(ncin, "lon")
nlon <- dim(lon)

lat <- get.var.ncdf(ncin, "lat")
nlat <- dim(lat)

xystream_nc <- cbind(lat,lon)


#---

library(dplyr)
library(tidyr)
library(geoknife) #order matters because 'query' is masked by a function in dplyr
library(RColorBrewer)
library(maps) 

lonlat_pnw2 <- lonlat_pnw@coords[,1:2]
xystream <- data.frame(t(data.frame(lonlat_pnw2)))
colnames(xystream_nc) <- c("lat", "long")

#--

stencil <- simplegeom(data.frame(t(lonlat_pnw@coords[,1:2])))

fabric <- webdata(list(url = 'https://cida.usgs.gov/thredds/dodsC/macav2metdata_monthly_historical', variables = "huss_BNU-ESM_r1i1p1_historical", 
                       times = c('2016-06-01','2016-07-01')))

job <- geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
check(job)
running(job)
error(job)
successful(job)
tmmxData_result <- result(job, with.units=TRUE)
tmmxData_result_frame <- data.frame(colMeans(tmmxData_result[sapply(tmmxData_result, is.numeric)]))
colnames(tmmxData_result_frame) <- c("specific_humidity")


streamtemp_combined <<- cbind(streamtemp, tmmxData_result_frame$specific_humidity)
streamtemp_combined_frame <<- data.frame(streamtemp_combined)

xystream_transformed <- data.frame(t(xystream))
colnames(xystream_transformed) <- c("long", "lat")


xystream_transformed$start.date <- rep(2007, nrow(xystream_transformed))
xystream_transformed$end.date <- rep(2015, nrow(xystream_transformed))



#--access modis data from grid points

library(MODISTools)

GetDates(Product = "MOD17A3", Lat = xystream_nc[2,1], Long = xystream_nc[1,1])
uniquexy <- data.frame(unique(xystream_nc))
uniquexy$start.date <- rep(2001, nrow(uniquexy))
uniquexy$end.date <- rep(2008, nrow(uniquexy))

MODISSubsets(LoadDat = uniquexy, Products = "MOD17A3",
             Bands = c("Npp_1km"),
             Size = c(1,1))
