N1 <- "2001"
N2 <- "2015"
state1 <- "Washington"
state2 <- "Oregon"
state3 <- "Idaho"

idlist1 <- c("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai")
walist1 <- c("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin")
orlist1 <- c("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa")
statelist <- c("Washington", "Oregon", "Idaho")


iddlist <- c("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", "Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa")
#colnames(iddlist) <- "countyname"



idlist <- paste("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
walist <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", sep="|")
orlist <- paste("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")
#fulllist <- list(c(idlist, walist, orlist))

onedata <- paste("/dmine/data/USDA/agmesh-scenarios/", state1, "/summaries/", N1, "_", N2, "_usda_gridmet_", state1, sep="")
twodata <- paste("/dmine/data/USDA/agmesh-scenarios/", state2, "/summaries/", N1, "_", N2, "_usda_gridmet_", state2, sep="")
threedata <- paste("/dmine/data/USDA/agmesh-scenarios/", state3, "/summaries/", N1, "_", N2, "_usda_gridmet_", state3, sep="")

stater1 <- as.data.frame(read.csv(onedata, strip.white = TRUE))
stater2 <- as.data.frame(read.csv(twodata, strip.white = TRUE))
stater3 <- as.data.frame(read.csv(threedata, strip.white = TRUE))

threestate_summary <- rbind(stater1, stater2, stater3)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- counties[grep(paste(statelist[1], sep=""), counties@data$STATE_NAME),]
wacounties <- counties[grep(walist, counties@data$NAME),]

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


counties <- counties[grep(paste(statelist[3], sep=""), counties@data$STATE_NAME),]
idcounties <- counties[grep(idlist, counties@data$NAME),]

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


counties <- counties[grep(paste(statelist[2], sep=""), counties@data$STATE_NAME),]
orcounties <- counties[grep(orlist, counties@data$NAME),]



bb <- data.frame(wacounties$FIPS)
names(bb) <- "FIPS"
cc <- data.frame(idcounties$FIPS)
names(cc) <- "FIPS"
dd <- data.frame(orcounties$FIPS)
names(dd) <- "FIPS"

alll <- rbind(bb, cc, dd)
all2 <- t(alll)
all3 <- paste(all2, sep="|", collapse="|")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


allcounties <- counties[grep(all3, counties@data$FIPS),]

palouse <- subset(threestate_summary, county = iddlist )

#iddlist_v <- c(iddlist)

wapalouse <- subset(threestate_summary, state = "WA")
wapalouse <- subset(wapalouse, county %in% walist1)

idpalouse <- subset(threestate_summary, state = "ID")
idpalouse <- subset(idpalouse, county %in% idlist1)

orpalouse <- subset(threestate_summary, state = "OR")
orpalouse <- subset(orpalouse, county %in% orlist1)

palouse <- rbind(wapalouse, idpalouse, orpalouse)

palouse2 <- with(palouse, palouse[order(year, monthcode),])

setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/summary"))
#palouse2$acres <- as.numeric(palouse2$acres)
write.csv(palouse2, file = '2001_2015_palouse_summary')



palouse3 <- data.table(palouse2)

setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/summary"))
palouse3 <- read.csv("2001_2015_palouse_summary")
palouse3 <- data.table(palouse3)
palouse3$date <-as.numeric(paste(palouse3$year, ".", palouse3$monthcode, sep=""))


#-by year
palouse_loss_year <- palouse3[,list(loss=sum(loss)), by = year]
barplot(palouse_loss_year$loss)

#-by county
palouse_loss_county <- palouse3[,list(loss=sum(loss)), by = county]
library(datasets)
library(rgdal)
library(leaflet)
library(png)
library(jpeg)
library(ncdf)  
library(data.table)
library(raster)
library(maptools)
tt <- colorRampPalette(c("light blue", "blue", "red"))
orderedcolors2 <- tt(length(palouse_loss_county$loss))[order(palouse_loss_county$loss)]

names(allcounties)[1] <- "county"
palouse_counties <- merge(allcounties, palouse_loss_county, by = 'county')
plot(palouse_counties, col = orderedcolors2)


barplot(palouse2$loss)

