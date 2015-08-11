#------------------------------------------------------------------------#
# TITLE:        PalouseET_Setup.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         October 17. 2014
#
# STAGE:        Setup - Stage 1
#
# COMMENTS:     This is the Setup code for Erich Seamon's Final Project 
#               for FOR 504,which uses weather parameters for the inland 
#               pacific northwest from 2007-2011 to calculate  
#               evapotranspirationon a daily timestep, as well as 
#               to compare crop yield toacross the study area to 
#               the aforementioned ET values. 
#
#--Setting the working directory and clearing the workspace-----------#

#----SETUP SECTION----------------------------------------------------#

#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists------#
cat("\14")

#----set the working directory.  In order to run this script from any UI-#
#----network location - mount \\CALS-DDVJ9YR1\climatevariables as -------#
#----your Z: drive.------------------------------------------------------#

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

#memory.size(10000)

#--SECTION 1: Load dataset, remove missing values, and extract data---- #

#-----loading shapefiles for study region - tri-state shapefile
#-----and the crop yield point data for 2011-2013------#

setwd("/climatevariables") 

#tristate <- readShapePoly('tristate.shp', 
#                          proj4string=CRS
#                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#cropyield <- readShapePoints('REACCHcropyield_allyears.shp', 
#                             proj4string=CRS
#                             ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#cropyield_df <- data.frame(cropyield)
#cropyield_df[cropyield_df==0] <- NA

#----Setting vectors for loops for years, variables, day, and rasters---#
#----associated with input datasets 2007-2011------#

#----input ranges of years to examine----------#
setwd("/reacchspace/obj1/netcdf/METREACCH/") 

rm(list = ls()) #--clears all lists------#

N1 <- readline("enter first year of data range: ")
N2 <- readline("enter last year of data range: ")
LAT1 <- readline("enter Lat min: ")
LAT2 <- readline("enter Lat max: ")
LON1 <- readline("enter Lon min: ")
LON2 <- readline("enter Lon max: ")

yearspan <- c(N1:N2)
#yearspan <- c(2007,2008,2009,2010,2011)
variablespan <- c("pr", "pdur", "vs", "srad", 
                  "rmin", "rmax", "tmmx", "tmmn")
rasterspan <- c("raster", "matrix", "raster_transposed", 
                "matrix_mean", "matrix_median", "matrix_sd", 
                "matrix_min", "matrix_max")
dayspan <- c(1:365)

ncolz <- c(1:109)
nrowz <- c(1:140)

#----writes subset variables to file for shell script operations--#

fileConn<-file("/tmp/agmesh-subset-vartmp.txt", "w")
writeLines(c(paste('yearstart=', N1, sep='')), fileConn)
writeLines(c(paste('yearend=', N2, sep='')), fileConn)
writeLines(c(paste('lat1=', LAT1, sep='')), fileConn)
writeLines(c(paste('lat2=', LAT2, sep='')), fileConn)
writeLines(c(paste('lon1=', LON1, sep='')), fileConn)
writeLines(c(paste('lon2=', LON2, sep='')), fileConn)
close(fileConn)

#write.table(data.frame(ID,N1,N2,LAT1,LAT2,LON1,LON2), "/tmp/agmesh-subset-vartmp.txt", sep="\t")
system("/agmesh-code/agmesh-subset.sh")
#system("rm /tmp/agmesh-subset-vartmp.txt")

#---Loop below that loads netcdf data files for each year, for each 
#---of the six variables ( 1) tmmx - max temp, 2) tmmn - min temp, 
#---3) rmin - min rel humidity, 4) max rel humidity, 5) srad - solar  
#---radiation, 6) vs - wind speed) needed to calculate evapotranspitation.  
#---This loop creates a raster brick for each variable, per year.  
#---For example: tmmx2007brick - raster brick for all max temp daily data.  
#---365 layers (days) across the geographic region.

#----SECTION 2: Load data and Derive Variables-------#
#yearspan=c(2000)
#variablespan=c("pdur")
for (i in yearspan) {
  for (j in variablespan) {
    
    #-----loads netcdfs one by one-------------------#
    
    dirname <- paste("/reacchspace/obj1/netcdf/METREACCH/", j, sep = "")
    aggmetnc <- file.path(paste("/reacchspace/obj1/netcdf/METREACCH/", j, sep = ""))
    agmetfullname <- file.path(dirname, paste(j, "_", i, ".nc", sep = ""))
    aggmetnc <- agmetfullname 
    
    #--creates Raster Bricks for each climate variable, for each year.---#
    
    b <- brick(aggmetnc, lvar=3, level=1, xmx=LAT2, xmn=LAT1)
    b <- t(b)
    b_mean <- calc(b, mean)
    assign(paste(j, i, "brickmean", sep=""), b_mean)
    assign(paste(j, i, "brick", sep=""), b)
    
    metvar <- open.ncdf(aggmetnc, write=FALSE)# Open a netcdf file 
    metvarname <- file.path(paste("MET", i, "_", j, sep = ""))
    
    names(metvar)<- paste("MET", i, "_", j, sep = "")
    aggmet_raster <- raster(aggmetnc, layer=1)
    aggmet_matrix <- as.matrix(aggmet_raster)
    aggmet_raster_transposed <- t(aggmet_raster)
    aggmet_matrix_mean <- mean(aggmet_matrix)
    aggmet_matrix_median <- median(aggmet_matrix)
    aggmet_matrix_sd <- sd(aggmet_matrix)
    aggmet_matrix_min <- min(aggmet_matrix)
    aggmet_matrix_max <- max(aggmet_matrix)
    assign(paste("aggmet", i, "_", j, "_", "raster", sep = ""),aggmet_raster)
    assign(paste("aggmet", i, "_", j, "_", "matrix",  sep = ""),aggmet_matrix)
    assign(paste("aggmet", i, "_", j, "_", "raster_transposed", sep = ""),aggmet_raster_transposed)
    assign(paste("aggmet", i, "_", j, "_", "matrix_mean", sep = ""),aggmet_matrix_mean)
    assign(paste("aggmet", i, "_", j, "_", "matrix_median", sep = ""),aggmet_matrix_median)
    assign(paste("aggmet", i, "_", j, "_", "matrix_sd", sep = ""),aggmet_matrix_sd)
    assign(paste("aggmet", i, "_", j, "_", "matrix_min", sep = ""),aggmet_matrix_min)
    assign(paste("aggmet", i, "_", j, "_", "matrix_max", sep = ""),aggmet_matrix_max)
  }
}

#---SECTION 2 CON'D - Deriving Variables------------------#

#--------re-calculates tmmx and tmmn from Kelvin to Celsius
#--------and also calc mean values 

tempspan <- c("tmmx","tmmn")
for (i in yearspan) {
  
  tmmnbrick   <- get(paste("tmmn", i, "brick", sep=""))
  tmmnbrick <- tmmnbrick - 273.15
  tmmxbrick   <- get(paste("tmmx", i, "brick", sep=""))
  tmmxbrick <- tmmxbrick - 273.15
  tmmxbrick_mean <- calc(tmmxbrick, mean)
  tmmnbrick_mean <- calc(tmmnbrick, mean)
  assign(paste("tmmn", i, "brick", sep=""), tmmnbrick)
  assign(paste("tmmx", i, "brick", sep=""), tmmxbrick)
  assign(paste("tmmn", i, "brickmean", sep=""), tmmnbrick_mean)
  assign(paste("tmmx", i, "brickmean", sep=""), tmmxbrick_mean)
  
}