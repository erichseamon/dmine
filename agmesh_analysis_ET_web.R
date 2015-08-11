#------------------------------------------------------------------------#
# TITLE:        agmesh_analysis_ET_web.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         November 14, 2014
#
# STAGE:        Raster Generation - Stage 3
#
# COMMENTS:     This is the agmesh analyis script for ET.  Agmesh  
#               uses weather parameters for the inland pacific northwest   
#               to calculateclimate parameter related outputs. This 
#               agmesh analysis script uses the data generated in 
#               stages 1 and 2 to create ET raster bricks for the
#               range of years input.
#
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------#


#-------Prepping for Raster calculations for evapotranspiration---------#
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



(scen <- read.table("/tmp/agmesh-subset-web-scenario.txt"))
scen <- t(scen)

setwd(paste("/agmesh_scenarios/", scen[1], sep="")) 
yearspan <- c(scen[2]:scen[3])

variablespan <- c("pdur", "vs", "srad", 
                  "rmin", "rmax", "tmmx", "tmmn")
rasterspan <- c("raster", "matrix", "raster_transposed", 
                "matrix_mean", "matrix_median", "matrix_sd", 
                "matrix_min", "matrix_max")
dayspan <- c(1:365)

ncolz <- c(1:109)
nrowz <- c(1:140)

#----SECTION 2: Load data and Derive Variables-------#
#yearspan=c(2000)
#variablespan=c("pdur")
for (i in yearspan) {
  for (j in variablespan) {
    
    #-----loads netcdfs one by one-------------------#
    
    dirname <- paste("/agmesh_scenarios/", scen[1], sep = "")
    aggmetnc <- file.path(paste("/agmesh_scenarios/", scen[1], sep = ""))
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

print("Converting temperature arrays from Kelvin to Celsius...")


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

print("Generating arrays for evapotranspiration calculation ...")


nrow <- nrow(tmmnbrick)
ncol <- ncol(tmmnbrick)
nlayers <- nlayers(tmmnbrick)

#extraTraster <- raster(extraT, layer=1)
dayspan <- c(1:nlayers)
dayspan_array <- array(data=NaN, c(nrow,ncol,nlayers))
ETlat_array <- array(data=NaN, c(nrow,ncol,nlayers))
extrat_array <- array(data=NaN, c(nrow,ncol,nlayers))

extratncell <- c(1:15260)

for (i in yearspan) {
  print(paste("Looping through yearly variable preparation for:", i))
    for (k in dayspan) {
      #for (j in ncolz) {
      #for (k in nrowz) {
      dayspan_array[,,k] <- i
      loopvalue <- tmmnbrick
      #loopvalue <- get(paste(j, i, "brick", sep=""))
      ETlat_array[,,k] <- yFromRow(loopvalue)
      #ETlat_array[j,k,i] <- yFromRow(srad2007brick, row=1:109(srad2007brick))

ETlat<- yFromRow(loopvalue)
ETlat <- t(ETlat)
ETlat_matrix <- matrix(ETlat,nrow=140,ncol=length(ETlat),byrow=TRUE)
ETlat_matrix <- t(ETlat_matrix)
#ETlat_array <- as.array(ETlat_matrix)
ETlat_raster <- setValues(loopvalue, ETlat_array)
ETlat_raster <- raster(ETlat_raster)
ETlatrasterbrick <- brick(ETlat_raster)
ETlatrasterbrickfinal <- brick(ETlat_raster, ETlat_raster, 
                               ETlat_raster, ETlat_raster,ETlat_raster, 
                               ETlat_raster, ETlat_raster, ETlat_raster)
ETlatrasterbrickSet <- setValues(loopvalue, ETlat_array)

   
    }
}