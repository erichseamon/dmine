library(rnoaa)
library(maptools)

setwd("/dmine/data/ghcnd/ghcnd_admin/")
ghcnd_stations <- read.csv("ghcnd-stations_revised.csv")

coordinates(ghcnd_stations) <- ~long + lat
summary(points)

xy <- ghcnd_stations[,c(3,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data = ghcnd_stations,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

columns <- c("ID", "YEAR", "MONTH", "ELEMENT", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN", "EVAP", "MNPN", "MXPN", "SN*#", "SX*#", "TOBS", "WDMV", "WESF", "WT**", "WTEQ", "VALUE1", "MFLAG1", "QFLAG1", "SFLAG1", "VALUE2", "MFLAG2", "QFLAG2", "SFLAG2")


setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                         proj4string=CRS
                         ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
counties <- counties[grep("Latah", counties@data$NAME),]

subset_stations <- spdf[counties, ]

subset_stations_data <- meteo_pull_monitors(subset_stations$station)

datetxt <- subset_stations_data$date
datetxt <- as.Date(datetxt)
df <- data.frame(year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))

subset_stations_data <- cbind(subset_stations_data, df)

subsetstations_month <- subset(subset_stations_data, month == 1)

library(doBy)
out <- summaryBy(prcp ~ year, data = subsetstations_month, 
          FUN = list(mean, max, min, median, sd))

plot(out$year, out$prcp.mean)

lines(stats::lowess(out), col = "red")






newz1 <- mean(na.omit(subset_stations_data$prcp))
newz2 <- mean(na.omit(subset_stations_data$snow))
newz3 <- mean(na.omit(subset_stations_data$snwd))
#newz4 <- mean(na.omit(subset_stations_data$wesd))
#newz5 <- mean(na.omit(subset_stations_data$wesf))
#newz6 <- mean(na.omit(subset_stations_data$evap))
#newz7 <- mean(na.omit(subset_stations_data$mnpn))
#newz8 <- mean(na.omit(subset_stations_data$mxpn))
#newz9 <- mean(na.omit(subset_stations_data$sn32))
#newz10 <- mean(na.omit(subset_stations_data$sx32))
newz11 <- mean(na.omit(subset_stations_data$tmax))
newz12 <- mean(na.omit(subset_stations_data$tmin))
#newz13 <- mean(na.omit(subset_stations_data$tobs))
#newz14 <- mean(na.omit(subset_stations_data$wdmv))
newz15 <- mean(na.omit(subset_stations_data$tavg))

newzfinal <- cbind(newz1, newz2, newz3, newz11, newz12, newz15)









barplot(newzfinal)

setwd("/dmine/data/ghcnd/ghcnd_all/")


x <- read.fwf('KSW00043216.dly',widths = c(11, 4, 2, 4, rep(c(5, 1, 1, 1),31)))


