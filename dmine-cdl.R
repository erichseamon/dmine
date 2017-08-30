library(rgdal)
library(raster)
library(ncdf)
library("RNetCDF")
library(maptools)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

statez = c("Idaho", "Washington", "Oregon")
Idaho_list1 <- paste("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
Washington_list1 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", sep="|")
Oregon_list1 <- paste("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")


combinedlist2 <- paste("Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", "Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep=",")
combinedlist3 <- c("Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", "Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai")
c4 <- as.data.frame(rep(combinedlist3, times = 9))

combinedlist <- c(Idaho_list1, Washington_list1, Oregon_list1)

palouse_counties <- counties[grep("Idaho|Oregon|Washington", counties@data$STATE_NAME),]

yearspan <- 2007:2015
countyspan <- combinedlist3
commodityspan <- c(23,24,31,21)

matrixx <- matrix(NA, nrow = nrow(c4), ncol = 1)
matrixxx <- cbind(matrixx, c4, rep(yearspan, each = 26))

p=1
for (i in yearspan) {
  
  setwd(paste('/dmine/data/CDL', sep=""))
  cdl <- raster(paste("CDL_", i, ".grd", sep=""))

 for (j in countyspan) {
   
   palouse_counties2 <- palouse_counties[grep(j, palouse_counties@data$NAME),]
   
   #for (k in commodityspan) {
    
    wintercdl <- cdl == 24 #winter wheat
    wintercdl <- crop(wintercdl, palouse_counties2)
    fr <- rasterize(palouse_counties2, wintercdl)   
    lr <- mask(x=wintercdl, mask=fr)
    
    lr[lr==0] <- NA
    lrr <- as.data.frame(lr)
    lrr2 <- subset(lrr, layer != "NA")
    matrixxx[p,1]   <- nrow(lrr2)
    p=p+1
 }
  #}
}


matrixxx2 <- as.data.frame(matrixxx)
colnames(matrixxx2) <- c("WHEAT_totalacres_CDL", "county", "year")

ID <- rep("Idaho", times = 7)
OR <- rep("Oregon", times = 7)
WA <- rep("Washington", times = 12)

statess <- as.data.frame(rep(c(WA, OR, ID), times = 9))
colnames(statess) <- "state"

m3 <- cbind(matrixxx2, statess)

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/")
write.csv(m3, file = "palouse_2007-2015_NASS_WHEAT_acres.csv")
