library(maptools)
library(leaflet)

setwd("/dmine/data/norwest/NorWeST_WBD_ProcessingUnits_wgs84/")

units <- readShapePoly('NorWeST_WBD_ProcessingUnits_wgs84.shp', 
                        proj4string=CRS
                        ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

setwd("/dmine/data/norwest/NorWeST_PredictedStreamTempPoints_WACoast_wgs84/")

WACoast_points_pred <- readShapePoints('NorWeST_PredictedStreamTempPoints_WACoast_wgs84.shp', 
                       proj4string=CRS
                       ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


setwd("/dmine/data/norwest/NorWeST_PredictedStreamTempLines_WACoast_wgs84/")

WACoast_streams_pred <- readShapeLines('NorWeST_PredictedStreamTempLines_WACoast_wgs84.shp', 
                                       proj4string=CRS
                                       ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


setwd("/dmine/data/states/")

states <- readShapePoly('states.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


setwd("/dmine/data/streams/")

streams <- readShapeLines('USAstreams.shp', 
                        proj4string=CRS
                        ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")







setwd("/dmine/data/norwest/")
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
streamtemp <- readShapePoints('NorWeST_ObservedTempPoints_WACoast_wgs84/NorWeST_ObservedTempPoints_WACoast_wgs84.shp', proj4string = projection)

streamtemp_att <- read.csv("NorWeST_ObservedStreamTemp_TempMonthlySummaries_WashingtonCoast_AllDays.csv")
streamtemp_ts <- ts(streamtemp_att)

monthyearspan <- unique(streamtemp_att$MonthYear)

for (i in monthyearspan) {
 stm <- subset(streamtemp_att, MonthYear == i)
streamtemp_joined <-  merge(streamtemp, stm, by='OBSPRED_ID')
  
lengthh <- length(streamtemp_joined)
#streamtemp_joined$MonthlyMean[is.na(streamtemp_joined$MonthlyMean)] <- 0
data_seq = seq(min(na.omit(streamtemp_joined$MonthlyMean)), max(na.omit(streamtemp_joined$MonthlyMean)), length=length(streamtemp_joined))
col_pal = colorRampPalette(c('blue', 'red'))(lengthh)
cols = col_pal[ cut(streamtemp_joined$MonthlyMean, data_seq, include.lowest=T) ]

sta_unique <- unique(streamtemp_att$MonthlyMean)
col_pal2 = colorRampPalette(c('blue', 'red'))(length(sta_unique))
cols2 = col_pal2[ cut(streamtemp_att$MonthlyMean, data_seq, include.lowest=T) ]
cols3 <- na.omit(cols2)
length(cols3)
cols4 <- cols3[1:length(cols3)]

layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
plotpath <- paste("/dmine/data/norwest/WACoast_monthly/", "WACoast_", i, ".png", sep="")
png(filename=plotpath)
plot(subset(units, GNLCCunit == "WACO"))


cols2 <- na.omit(cols)
ll <- length(cols2)
cols3 <- cols2[1:ll]
lines(WACoast_streams_pred, col = 'lightgray')
points(streamtemp_joined, col = cols, pch=16, cex=1)

legend_image <- as.raster(matrix(cols4), ncol=1)
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'legend title')
text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
rasterImage(legend_image, 0, 0, .25 ,1)


dev.off()



   
}


#streamtemp_joined <- data.frame(streamtemp_joined)


