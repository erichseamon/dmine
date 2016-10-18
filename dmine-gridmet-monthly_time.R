#------------------------------------------------------------------------#
# TITLE:        dmine-gridmet-monthly.R
# AUTHOR:       Erich Seamon
# INSTITUITON:  College of Natural Resources
#               University of Idaho
# DATE:         May-June. 2016
# STAGE:        DMINE extraction and subsetting of UI GRIDMET data for machine
#               learning analysis
#
# COMMENTS:     The purpose of this script is to allow a user to input a range
#               of years, and a lat/long bounding box - and the code will extract all variables
#               for that time period and location.  The second portion of the script steps thru
#               all counties within the US (or a subset), and extracts daily data from each county
#               and averages it for the month.  Each monthly average, for all variables, 
#               for each county, by the month, is then put into a matrix for machine learning analysis.
#                
#               More on the dmine design: dmine.io
#------------------------------------------------------------------------#


#----SETUP SECTION----------------------------------------------------#

#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists------#
cat("\14")

#----supress warnings

options(warn=0)

#----set packages--------------------------------------------------------#

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
library("plyr")
library("data.table")
library("sirad")
library("rgeos")
library("MASS")
library("stringr")
library("car")
library("sp")


#-------------------------------------------------

#dmineplots <- function(scen_state, N1, N2) {
  
  scen_state = "Idaho"
  N1 = "2001"
  N2 = "2002"
  startyear = "2001"
  
  
  
  #N1 is beginning year
  #N2 is ending year
  #scen_state is the state to plot
#--bringing in county shapefile
setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% scen_state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, sep="")
#unique <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/netcdf/pdsi_apr_", N2, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_positive/", sep=""))
#system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
unique <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_positive", sep=""))


#as.data.frame(text) %>% separate(text, into = paste("V", 1:4, sep = "_"))

setwd(monthdir)

i <- paste(startyear, "_monthly_usda_gridmet_post2001_", scen_state, sep="")

#for (i in unique) {
  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp', 
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% scen_state)
  #counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/summaries", sep=""))
  x <- as.data.frame(read.csv(i, strip.white = TRUE))
  x <- subset(x, damagecause == "Drought")
  #colnames(x) <- c("UNIQUEID", "YEAR", "COUNTY", "COMMODITYCODE", "MONTHCODE", "COMMODITY", "DAMAGECAUSE", "ACRES", "LOSS")
  
  
  
  
  
  u <- data.frame(trimws(x$COUNTY))
  colnames(u) <- c("NAME")
  z <- cbind(x,u)
  m <- merge(counties, z, by='NAME')
  m$LOSS[is.na(m$LOSS)] <- 0
  m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
  m$ACRES[is.na(m$ACRES)] <- 0
  
  #shapefile(m)
  #--begin polygon work
  #length(na.omit(m$LOSS))
  #tt <- colorRampPalette(brewer.pal(11, "Spectral")
  tt <- colorRampPalette(c("blue", "orange", "red"), space = "Lab")
  mz <- subset(m, LOSS != 0)
  mzacres <- subset(m, ACRES > 0)
  lengacres <- length(m$ACRES)
  leng <- length(m$LOSS)
  len2 <- tt(len <- length(mz$LOSS))
  len2acres <- tt(len <- length(mzacres$ACRES))
  len2a <- length(mz$LOSS)
  len2a <- length(mzacres$ACRES)
  len3 <- tt(len <- length(m$LOSS))
  
  orderedcolors2 <- tt(length(mz$LOSS))[order(order(mz$LOSS))]
  orderedcolors3 <- tt(length(mzacres$ACRES))[order(order(mzacres$ACRES))]
  newframe <- data.frame(m$LOSS)
  
  xx <- 1
  newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
  
  for (jj in 1:leng){
    
    if (m$LOSS[jj] == 0) {
      #print("yes this worked, added 0")
      newmatrix[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj] 
      newmatrix[jj,] <- orderedcolors2[xx]
      xx <- xx + 1
    }
    
  }
  
  xx <- 1
  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)
  
  for (jj in 1:leng){
    
    if (m$ACRES[jj] == 0) {
      #print("yes this worked, added 0")
      newmatrix_acres[jj,] <- 0
    } else {
      #print("yes, this worked, added color")
      #newmatrix[jj,] <- len3[jj] 
      newmatrix_acres[jj,] <- orderedcolors3[xx]
      xx <- xx + 1
    }
    
  }
  
  newmatrix[newmatrix==0] <- NA
  newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix[newmatrix == NA] <- 0
  newmatrix <- c(newmatrix)
  
  newmatrix_acres[newmatrix_acres==0] <- NA
  newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix_acres[newmatrix_acres == NA] <- 0
  newmatrix_acres <- c(newmatrix_acres)
  
  
  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)
  #midpoints <- barplot(mz$LOSS)
  png(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/", "/month_png/", x$YEAR[1], "_", x$MONTHCODE[1], "_", x$COMMODITY[1],  "_plot.png", sep=""))
  par(mar=c(6,3,3,2)+1)
  par(mfrow=c(2,2))
  layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
  #--turn image horizontal
  
  plotmonth <- month.abb[x$MONTHCODE[1]]
  plotyear <- x$YEAR[1]
  plotcommodity <- x$COMMODITY[1]
  
  midpoint_loss <- (max(mz$LOSS) + min(mz$LOSS)/2)
  midpoint_acres <- (max(mzacres$ACRES) + min(mzacres$ACRES)/2)
  
  #b <- barplot(mz$LOSS, names.arg = mz$NAME, las=2, col = newmatrix2)
  #text(bb, midpoint_loss, labels=mz$LOSS, srt=90)
  plot(m, col = newmatrix, main = paste(scen_state, " crop loss $ \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""))
  
  #bb <- barplot(mzacres$ACRES, names.arg = mz$NAME, las=2, col = newmatrix2acres)
  #text(b, midpoint_acres, labels=mzacres$ACRES, xpd=NA, col = "White")
  #plot(m, col = newmatrix_acres, main = paste(scen_state, " crop loss acres \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""))
  
 
  dev.off()
}