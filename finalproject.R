#------------------------------------------------------------------------#
# TITLE:        Seamon_Final_Project.R
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
# COMMENTS:     This is Erich Seamon's Final Project for FOR 504, 
#               which uses weather parameters for the inland pacific 
#               northwest from 2007-2011 to calculate evapotranspiration 
#               on a daily timestep, as well as to compare crop yield to
#               across the study area to the aforementioned ET values. 
#
#--Setting the working directory and clearing the workspace-----------#

#----SETUP SECTION----------------------------------------------------#

#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists------#
cat("\14")

#----set the working directory.  In order to run this script from any UI--#
#----network location - mount \\CALS-DDVJ9YR1\climatevariables as --------#
#----your Z: drive.-------------------------------------------------------#

setwd("/climatevariables") 
options(warn=0)

#----set packages---------------------------------------------------------#

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

memory.size(10000)

#--SECTION 1: Load dataset, remove missing values, and extract data---- #

  #-----loading shapefiles for study region - tri-state shapefile
  #-----and the crop yield point data for 2011-2013------#

tristate <- readShapePoly('tristate.shp', 
            proj4string=CRS
            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

cropyield <- readShapePoints('REACCHcropyield_allyears.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
cropyield_df <- data.frame(cropyield)
#cropyield_df[cropyield_df==0] <- NA

  #----Setting vectors for loops for years, variables, day, and rasters---#
  #----associated with input datasets 2007-2011------#

setwd("/reacchspace/obj1/netcdf/METREACCH/") 

#----input ranges of years to examine----------#

rm(list = ls()) #--clears all lists------#

N <- readline("enter first year of data range: ")
N2 <- readline("enter last year of data range: ")
yearspan <- c(N:N2)
#yearspan <- c(2007,2008,2009,2010,2011)
variablespan <- c("tmmx", "tmmn", "pr", "pdur", "vs", "srad", 
                  "rmin", "rmax")
rasterspan <- c("raster", "matrix", "raster_transposed", 
                "matrix_mean", "matrix_median", "matrix_sd", 
                "matrix_min", "matrix_max")
dayspan <- c(1:365)
 
ncolz <- c(1:109)
nrowz <- c(1:140)

#---Loop below that loads netcdf data files for each year, for each 
#---of the six variables ( 1) tmmx - max temp, 2) tmmn - min temp, 
#---3) rmin - min rel humidity, 4) max rel humidity, 5) srad - solar  
#---radiation, 6) vs - wind speed) needed to calculate evapotranspitation.  
#---This loop creates a raster brick for each variable, per year.  
#---For example: tmmx2007brick - raster brick for all max temp daily data.  
#---365 layers (days) across the geographic region.

#----SECTION 2: Load data and Derive Variables-------#

for (i in yearspan) {
  for (j in variablespan) {
    
    #-----loads netcdfs one by one-------------------#
  
    dirname <- paste("/reacchspace/obj1/netcdf/METREACCH/", j, sep = "")
    aggmetnc <- file.path(paste("/reacchspace/obj1/netcdf/METREACCH/", j, sep = ""))
    agmetfullname <- file.path(dirname, paste(j, "_", i, ".nc", sep = ""))
    aggmetnc <- agmetfullname 
    
    #--creates Raster Bricks for each climate variable, for each year.---#
    
    b <- brick(aggmetnc, lvar=3, level=1)
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
                      
#tmmx2007brick <- tmmxbrick - 273.15
#tmmn2007brick <- (tmmn2007brick - 273.15)
#tmmx2007brickmean <- calc(tmmx2007brick, mean)
#tmmn2007brickmean <- calc(tmmn2007brick, mean)

#tmmx2008brick <- (tmmx2008brick - 273.15)
#tmmn2008brick <- (tmmn2008brick - 273.15)
#tmmx2008brickmean <- calc(tmmx2008brick, mean)
#tmmn2008brickmean <- calc(tmmn2008brick, mean)

#tmmx2009brick <- (tmmx2009brick - 273.15)
#tmmn2009brick <- (tmmn2009brick - 273.15)
#tmmx2009brickmean <- calc(tmmx2009brick, mean)
#tmmn2009brickmean <- calc(tmmn2009brick, mean)

#tmmx2010brick <- (tmmx2010brick - 273.15)
#tmmn2010brick <- (tmmn2010brick - 273.15)
#tmmx2010brickmean <- calc(tmmx2010brick, mean)
#tmmn2010brickmean <- calc(tmmn2010brick, mean)

#tmmx2011brick <- (tmmx2011brick - 273.15)
#tmmn2011brick <- (tmmn2011brick - 273.15)
#tmmx2011brickmean <- calc(tmmx2011brick, mean)
#tmmn2011brickmean <- calc(tmmn2011brick, mean)

  #---This subsection calculates evapotranspiration for years 
  #---selected.  The calculation creates an et value for each cell
  #---for each annual raster brick created  (mm/d-1).

  #-------Prepping for Raster calculations for evapotranspiration---------#

#extraTraster <- raster(extraT, layer=1)
dayspan_array <- array(data=NaN, c(109, 140, 365))
ETlat_array <- array(data=NaN, c(109, 140, 365))
extrat_array <- array(data=NaN, c(109, 140, 365))

extratncell <- c(1:15260)
for (i in dayspan) {
  #for (j in ncolz) {
    #for (k in nrowz) {
    dayspan_array[,,i] <- i
    ETlat_array[,,i] <- yFromRow(srad2007brick)
    #ETlat_array[j,k,i] <- yFromRow(srad2007brick, row=1:109(srad2007brick))
}
#}
#}

ETlat<- yFromRow(tmmx2007brick)
ETlat <- t(ETlat)
ETlat_matrix <- matrix(ETlat,nrow=140,ncol=length(ETlat),byrow=TRUE)
ETlat_matrix <- t(ETlat_matrix)
#ETlat_array <- as.array(ETlat_matrix)
ETlat_raster <- setValues(srad2007brick, ETlat_array)
ETlat_raster <- raster(ETlat_raster)
ETlatrasterbrick <- brick(ETlat_raster)
ETlatrasterbrickfinal <- brick(ETlat_raster, ETlat_raster, 
                         ETlat_raster, ETlat_raster,ETlat_raster, 
                         ETlat_raster, ETlat_raster, ETlat_raster)
ETlatrasterbrickSet <- setValues(tmmx2007brick, ETlat_array)

#-------Raster calculations for evapotranspiration - 2007---------#


meah=10

Tmean2007_rasterbrick <- (tmmx2007brick - tmmn2007brick)/2
RHmean2007_rasterbrick <- (rmax2007brick - rmin2007brick)/2
es2007 <- 0.6108 * exp(17.27 * Tmean2007_rasterbrick / (Tmean2007_rasterbrick+237.3))
ea2007 <- (RHmean2007_rasterbrick / 100) * es2007
deltaVP <- es2007 - ea2007
vappressure2007 <- ea2007
z = 1000

ETlatradians <- radians(ETlat_array)
#extraT = extrat(dayOfYear(dayspan), ETlatradians)$ExtraTerrestrialSolarRadiationDaily
#tal <- cst(srad2007brick, dayOfYear(dayspan), radians(ETlatradians))
extraT = 50
tal = 1

erichet2007 <- (0.408 * deltaVP(tmmx2007brick, tmmn2007brick) * 
(rns(srad2007brick, albedo = 0.23) - rnl(tmmx2007brick, tmmn2007brick, 
srad2007brick, vappressure2007, extraT, tal)) + psychC(tmmx2007brick, 
tmmn2007brick, z) * (900/(Tmean2007_rasterbrick + 273)) * 
wind2(vs2007brick, meah) * (es(tmmx2007brick,tmmn2007brick) - 
vappressure2007))/(deltaVP(tmmx2007brick, tmmn2007brick) + psychC(tmmx2007brick, 
tmmn2007brick, z) * (1 + 0.34 * wind2(vs2007brick, meah)))

erichet2007_mean <- calc(erichet2007, mean)
et2007m <- as.matrix(erichet2007)

   #-------Raster calculations for evapotranspiration - 2008---------#



meah=10

Tmean2008_rasterbrick <- (tmmx2008brick - tmmn2008brick)/2
RHmean2008_rasterbrick <- (rmax2008brick - rmin2008brick)/2
es2008 <- 0.6108 * exp(17.27 * Tmean2008_rasterbrick / (Tmean2008_rasterbrick+237.3))
ea2008 <- (RHmean2008_rasterbrick / 100) * es2008
deltaVP <- es2008 - ea2008
vappressure2008 <- ea2008
z = 1000

ETlatradians <- radians(ETlat_array)
#extraT = extrat(dayOfYear(dayspan), ETlatradians)$ExtraTerrestrialSolarRadiationDaily
#tal <- cst(srad2007brick, dayOfYear(dayspan), radians(ETlatradians))
extraT = 50
tal = 1

erichet2008 <- (0.408 * deltaVP(tmmx2008brick, tmmn2008brick) * 
(rns(srad2008brick, albedo = 0.23) - rnl(tmmx2008brick, 
tmmn2008brick, srad2008brick, vappressure2008, extraT, tal)) +
psychC(tmmx2008brick, tmmn2008brick, z) * (900/(Tmean2008_rasterbrick + 273)) * 
wind2(vs2008brick, meah) * (es(tmmx2008brick,tmmn2008brick) - 
vappressure2008))/(deltaVP(tmmx2008brick, tmmn2008brick) + 
psychC(tmmx2008brick, tmmn2008brick, z) * (1 + 0.34 * wind2(vs2008brick, meah)))

erichet2008_mean <- calc(erichet2008, mean)
et2008m <- as.matrix(erichet2008)

   #-------Raster calculations for evapotranspiration - 2009---------#



meah=10

Tmean2009_rasterbrick <- (tmmx2009brick - tmmn2009brick)/2
RHmean2009_rasterbrick <- (rmax2009brick - rmin2009brick)/2
es2009 <- 0.6108 * exp(17.27 * Tmean2009_rasterbrick / (Tmean2009_rasterbrick+237.3))
ea2009 <- (RHmean2009_rasterbrick / 100) * es2009
deltaVP <- es2009 - ea2009
vappressure2009 <- ea2009
z = 1000

ETlatradians <- radians(ETlat_array)
#extraT = extrat(dayOfYear(dayspan), ETlatradians)$ExtraTerrestrialSolarRadiationDaily
#tal <- cst(srad2007brick, dayOfYear(dayspan), radians(ETlatradians))
extraT = 50
tal = 1

erichet2009 <- (0.408 * deltaVP(tmmx2009brick, tmmn2009brick) * 
(rns(srad2009brick, albedo = 0.23) - rnl(tmmx2009brick, 
tmmn2009brick, srad2009brick, vappressure2009, extraT, tal)) +  
psychC(tmmx2009brick, tmmn2009brick, z) * (900/(Tmean2009_rasterbrick + 273)) * 
wind2(vs2009brick, meah) * (es(tmmx2009brick,tmmn2009brick) - 
vappressure2009))/(deltaVP(tmmx2009brick, tmmn2009brick) + 
psychC(tmmx2009brick, tmmn2009brick, z) * (1 + 0.34 * wind2(vs2009brick, meah)))

erichet2009_mean <- calc(erichet2009, mean)
et2009m <- as.matrix(erichet2009)

   #-------Raster calculations for evapotranspiration - 2010---------#



meah=10

Tmean2010_rasterbrick <- (tmmx2010brick - tmmn2010brick)/2
RHmean2010_rasterbrick <- (rmax2010brick - rmin2010brick)/2
es2010 <- 0.6108 * exp(17.27 * Tmean2010_rasterbrick / (Tmean2010_rasterbrick+237.3))
ea2010 <- (RHmean2010_rasterbrick / 100) * es2010
deltaVP <- es2010 - ea2010
vappressure2010 <- ea2010
z = 1000

ETlatradians <- radians(ETlat_array)
#extraT = extrat(dayOfYear(dayspan), ETlatradians)$ExtraTerrestrialSolarRadiationDaily
#tal <- cst(srad2007brick, dayOfYear(dayspan), radians(ETlatradians))
extraT = 50
tal = 1

erichet2010 <- (0.408 * deltaVP(tmmx2010brick, tmmn2010brick) * 
(rns(srad2010brick, albedo = 0.23) - rnl(tmmx2010brick, 
tmmn2010brick, srad2010brick, vappressure2010, extraT, tal)) +  
psychC(tmmx2010brick, tmmn2010brick, z) * (900/(Tmean2010_rasterbrick + 273)) * 
wind2(vs2010brick, meah) * (es(tmmx2010brick,tmmn2010brick) - 
vappressure2010))/(deltaVP(tmmx2010brick, tmmn2010brick) + psychC(tmmx2010brick, 
tmmn2007brick, z) * (1 + 0.34 * wind2(vs2007brick, meah)))

erichet2010_mean <- calc(erichet2010, mean)
et2010m <- as.matrix(erichet2010)

   #-------Raster calculations for evapotranspiration - 2011---------#



meah=10

Tmean2011_rasterbrick <- (tmmx2011brick - tmmn2011brick)/2
RHmean2011_rasterbrick <- (rmax2011brick - rmin2011brick)/2
es2011 <- 0.6108 * exp(17.27 * Tmean2011_rasterbrick / (Tmean2011_rasterbrick+237.3))
ea2011 <- (RHmean2011_rasterbrick / 100) * es2011
deltaVP <- es2011 - ea2011
vappressure2011 <- ea2011
z = 1000

ETlatradians <- radians(ETlat_array)
#extraT = extrat(dayOfYear(dayspan), ETlatradians)$ExtraTerrestrialSolarRadiationDaily
#tal <- cst(srad2007brick, dayOfYear(dayspan), radians(ETlatradians))
extraT = 50
tal = 1

erichet2011 <- (0.408 * deltaVP(tmmx2011brick, tmmn2011brick) * 
(rns(srad2011brick, albedo = 0.23) - rnl(tmmx2011brick, 
tmmn2011brick, srad2011brick, vappressure2011, extraT, tal)) +  
psychC(tmmx2011brick, tmmn2011brick, z) * (900/(Tmean2011_rasterbrick + 273)) * 
wind2(vs2011brick, meah) * (es(tmmx2011brick,tmmn2011brick) - 
vappressure2011))/(deltaVP(tmmx2011brick, tmmn2011brick) + psychC(tmmx2011brick, 
tmmn2007brick, z) * (1 + 0.34 * wind2(vs2007brick, meah)))

erichet2011_mean <- calc(erichet2011, mean)
et2011m <- as.matrix(erichet2011)

#------SECTION 3 - Plot Univariate and BiVariate Data ------#

  #------Plotting - year 2007 ET with climate variables ------#

win.graph(15,15)

lyt = c(1, 2, 3, 4, 5, 6,
        7, 7, 7, 7, 7, 7, 
        7, 7, 7, 7, 7, 7)

lytmtx = matrix(lyt,nrow=3,ncol=6,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' functio

hist(tmmx2007brickmean,   
     col = "gray",
     xlab = "max air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Max Temp")

  #-----Add summary statistics to plot
tmmx2007histmean <- mean(as.vector(tmmx2007brickmean))
tmmx2007histmedian <- median(as.vector(tmmx2007brickmean))
tmmx2007histsd <- sd(as.vector(tmmx2007brick))
tmmx2007histmin <- min(as.vector(tmmx2007brickmean))
tmmx2007histmax <- max(as.vector(tmmx2007brickmean))

abline(v=tmmx2007histmean,lwd=2)
abline(v=tmmx2007histmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmx2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmx2007histmin,100,pch = "*")
points(tmmx2007histmax,100,pch = "*")

  #----Add Legend
legend(-10,2000,
       #c("Mean","Median","St.Dev.","Min.","Max."),
       c("Mean","Median","Min.","Max."),
       #col = c("black","red","blue","black","black"),
       col = c("black","red","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.65)

hist(tmmn2007brickmean,   
     col = "gray",
     xlab = "min air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Min Air Temp")

  #---Add summary statistics to plot

tmmn2007histmean <- mean(as.vector(tmmn2007brickmean))
tmmn2007histmedian <- median(as.vector(tmmn2007brickmean))
tmmn2007histsd <- sd(as.vector(tmmn2007brickmean))
tmmn2007histmin <- min(as.vector(tmmn2007brickmean))
tmmn2007histmax <- max(as.vector(tmmn2007brickmean))


abline(v=tmmn2007histmean,lwd=2)
abline(v=tmmn2007histmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmn2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmn2007histmin,100,pch = "*")
points(tmmn2007histmax,100,pch = "*")

hist(rmin2007brickmean,   
     col = "gray",
     xlab = "min relative humidity (%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Min Rel Humidity")

  #---Add summary statistics to plot
rmin2007histmean <- mean(as.vector(rmin2007brickmean))
rmin2007histmedian <- median(as.vector(rmin2007brickmean))
rmin2007histsd <- sd(as.matrix(rmin2007brick))
rmin2007histmin <- min(as.vector(rmin2007brickmean))
rmin2007histmax <- max(as.vector(rmin2007brickmean))


abline(v=rmin2007histmean,lwd=2)
abline(v=rmin2007histmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmin2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rmin2007histmin,100,pch = "*")
points(rmin2007histmax,100,pch = "*")

hist(rmax2007brickmean,   
     col = "gray",
     xlab = "max relative humidity(%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Max Rel Humidity")

  #---Add summary statistics to plot
rmax2007histmean <- mean(as.vector(rmax2007brickmean))
rmax2007histmedian <- median(as.vector(rmax2007brickmean))
rmax2007histsd <- sd(as.matrix(rmax2007brick))
rmax2007histmin <- min(as.vector(rmax2007brickmean))
rmax2007histmax <- max(as.vector(rmax2007brickmean))


abline(v=rmax2007histmean,lwd=2)
abline(v=rmax2007histmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmax2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rmax2007histmin,100,pch = "*")
points(rmax2007histmax,100,pch = "*")

hist(srad2007brickmean,   
     col = "gray",
     xlab = "solar radiation (w/m2)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Solar Radiation")

  #---Add summary statistics to plot
srad2007histmean <- mean(as.vector(srad2007brickmean))
srad2007histmedian <- median(as.vector(srad2007brickmean))
srad2007histsd <- sd(as.matrix(srad2007brick))
srad2007histmin <- min(as.vector(srad2007brickmean))
srad2007histmax <- max(as.vector(srad2007brickmean))


abline(v=srad2007histmean,lwd=2)
abline(v=srad2007histmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= srad2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(srad2007histmin,100,pch = "*")
points(srad2007histmax,100,pch = "*")

hist(vs2007brickmean,   
     col = "gray",
     xlab = "Wind Speed Frequency (m-s-1)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2007 Wind Speed")

  #---Add summary statistics to plot
vs2007histmean <- mean(as.vector(vs2007brickmean))
vs2007histmedian <- median(as.vector(vs2007brickmean))
vs2007histsd <- sd(as.matrix(vs2007brick))
vs2007histmin <- min(as.vector(vs2007brickmean))
vs2007histmax <- max(as.vector(vs2007brickmean))


abline(v=vs2007histmean,lwd=2)
abline(v=vs2007histmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= vs2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(vs2007histmin,100,pch = "*")
points(vs2007histmax,100,pch = "*")

boxplot(erichet2007, 
        ylab = "Evapotranspiration (mm/d-1)",
        xlab = "Days (1-365)",
        notch = FALSE,
        main = "2007 Mean Daily Distributions of ET")

  #---comparing daily ET distributions for April 1st, Aug 1st, 
  #---and Dec 1st, 2007

  #---Creates the window size--------------------#
win.graph(3.5,8) 
  #---par function sets number or rows and columns for plotting---#
par(mfrow=c(3,1))  


     plot(subset(erichet2007, 90,   
     col = "gray",
     xlab = "min air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,240),
     #xlim = c(-0.5,6),
     main = "ET - April 1st 2007"))

    plot(tristate, bg="transparent", add=TRUE)

     plot(subset(erichet2007, 210,  
     col = "gray",
     xlab = "min air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "ET - August 1st 2007"))

     plot(tristate, bg="transparent", add=TRUE)

     plot(subset(erichet2007, 335,
     col = "gray",
     xlab = "min air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "ET - December 1st 2007"))

     plot(tristate, bg="transparent", add=TRUE)

  #---------Comparing ET - April 1st 2007 thru 2011

  #---Creates the window size---------------#
win.graph(11,7) 
  #---par function sets number or rows and columns for plotting--#
par(mfrow=c(1,5))  


boxplot(erichet2007$layer.90,
        ylim = c(2,25),
        col="aquamarine3",
        xlab="April1 ET 2007",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2008$layer.90,
        ylim = c(2,25),
        col="aquamarine3",
        xlab="April1 ET 2008",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2009$layer.90,
        ylim = c(2,25),
        col="aquamarine3",
        xlab="April1 ET 2009",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2010$layer.90,
        ylim = c(2,25),
        col="aquamarine3",
        xlab="April1 ET 2010",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2011$layer.90,
        ylim = c(2,25),
        col="aquamarine3",
        xlab="April1 ET 2011",
        ylab="ET Distribution (mm/d-1)")


  #---------Comparing ET - August 1st 2007 thru 2011

  #---Creates the window size-------------------#
win.graph(11,7) 
  #---par function sets number or rows and columns for plotting--#
par(mfrow=c(1,5))  


boxplot(erichet2007$layer.210,
        ylim = c(10,60),
        col="aquamarine3",
        xlab="Aug1 ET 2007",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2008$layer.210,
        ylim = c(10,60),
        col="aquamarine3",
        xlab="Aug1 ET 2008",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2009$layer.210,
        ylim = c(10,60),
        col="aquamarine3",
        xlab="Aug1 ET 2009",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2010$layer.210,
        ylim = c(10,60),
        col="aquamarine3",
        xlab="Aug1 ET 2010",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2011$layer.210,
        ylim = c(10,60),
        col="aquamarine3",
        xlab="Aug1 ET 2011",
        ylab="ET Distribution (mm/d-1)")

  #---------Comparing ET - December 1st 2007 thru 2011

  #---Creates the window size-----#
win.graph(11,7) 
  #---par function sets number or rows and columns for plotting----#
par(mfrow=c(1,5))  


boxplot(erichet2007$layer.335,
        ylim = c(0,15),
        col="aquamarine3",
        xlab="Dec1 ET 2007",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2008$layer.335,
        ylim = c(0,15),
        col="aquamarine3",
        xlab="Dec1 ET 2008",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2009$layer.335,
        ylim = c(0,15),
        col="aquamarine3",
        xlab="Dec1 ET 2009",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2010$layer.335,
        ylim = c(0,15),
        col="aquamarine3",
        xlab="Dec1 ET 2010",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2011$layer.335,
        ylim = c(0,15),
        col="aquamarine3",
        xlab="Dec1 ET 2011",
        ylab="ET Distribution (mm/d-1)")

#------crop yield plots-----#
win.graph(11,7) 
#---par function sets number or rows and columns for plotting----#
par(mfrow=c(2,2))  

hist(cropyield_df[,8],   
        col = "gray",
        xlab = "crop yield",
        ylab = "Count",
        #ylim = c(0,230),
        #xlim = c(-0.5,7),
        main = "Winter Wheat 2012 Yield")

hist(cropyield_df[,9],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Winter Wheat 2012 Yield")



hist(cropyield_df[,11],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Winter Wheat 2012 Yield")

hist(cropyield_df[,12],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Winter Wheat 2012 Yield")
  #------ET Correlations

#source ("D:/Dropbox/ES Research/ES Classwork/FOR504/scripts/week6exercise_crosscorrelation_function.R")
#x <- CrossCorrelationFunction(erichet2007[1,1])