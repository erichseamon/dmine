library("ncdf")
library("raster")
library("sp")
library("rgeos")
library("rgdal")
library("proj4")
library("RNetCDF")
library("ncdf4")
library("RColorBrewer")
library("raster")
library("rasterVis")
library("latticeExtra")
library("maptools")
library("parallel")
library("Evapotranspiration")
#library("plyr")
library("data.table")
library("sirad")
library("rgeos")
library("MASS")
library("stringr")
library("car")
library("sp")
detach(package:tidyr)



library(raster)


#--Oregon

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

or_counties <- counties[grep("Oregon", counties@data$STATE_NAME),]
palouse_Oregon_counties <- or_counties[grep(Oregon_list1, or_counties@data$NAME),]
kk="Oregon"

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Oregon_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

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



dirname <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, sep = "")
dirname2 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/summaries2", sep="")
dirname3 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/cdl", sep="")
setwd(dirname)
varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
yearspan = c(2010:2015)


for (i in yearspan) { 
  varspannumber = 0
  for (j in varspan) { 
    varspannumber = varspannumber + 1
    jj=0
    for (k in monthspan) {
      for (l in countyfiploop) {
        jj = jj + 1
        subset_county <- counties[counties@data$FIPS == l,]
        name_county <- subset_county$NAME
        ee <- raster(paste(dirname3, "/", "CDL_", kk, "-", i, "-", k, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
        sp <- SpatialPoints(ee)
        #eee <- extract(ee, sp, method='bilinear')
        newmatrix[jj,varspannumber] <- mean(ee, na.rm=TRUE)
        newmatrix[jj,16] <- l
        newmatrix[jj,17] <- k #--month
        newmatrix[jj,18] <- i #--yeari
        #writeRaster(ee, paste(dirname3, "/", "CDL_", kk, "-", i, "-", k, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
        print(paste("creating matrix of climate variables for:", kk, "-", i, "-", k, "-", name_county, "-", j,  sep=""))
      }  
    } 
  }
  setwd(dirname2)
  name <- paste(kk, "_", i, "_palouse_summary", sep="") #--used for individual states
  #name <- paste(dirname, "/", variable, "_", month, "_", year, "_summary", sep="")
  write.matrix(newmatrix, file=name, sep=",")
}
