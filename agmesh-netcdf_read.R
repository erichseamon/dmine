#-----Rcode starts-----

#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists------#
cat("\14")

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

rm(list = ls()) #--clears all lists------#

N1 <- readline("enter the year of the netcdf file you want to read in: ")
N2 <- readline("enter the variable of interest (e.g: pdur, srad, rmax, rmin, tmmx, tmmn, vs):")

assign("N1", N1, envir = .GlobalEnv)

yearspan <- c(N1)
variablespan <- c(N2)
rasterspan <- c("raster", "matrix", "raster_transposed", 
                "matrix_mean", "matrix_median", "matrix_sd", 
                "matrix_min", "matrix_max")
dayspan <- c(1:365)

#--sets the working directory where the netcdf files live.----#

setwd(paste("/reacchspace/obj1/netcdf/MET/", sep="")) 


#---- Load data and Derive Variables-------#
j="pdsi"
i="may"
k="2006"
for (i in yearspan) {
  for (j in variablespan) {
    
    #-----loads netcdfs one by one.  Builds the url for a netcdf file - using the variables defined above-------------------#
    dirname <- paste("/dmine/data/USDA/agmesh-scenarios/Idaho/netcdf/", sep = "")
    agmetfullname <- file.path(dirname, paste(j, "_", i, "_", k, ".nc", sep = ""))
    aggmetnc <- agmetfullname 
    
    #--creates Raster Bricks for each climate variable, for each year.---#
    
    b <- brick(aggmetnc, lvar=3, level=1, xmx=LAT2, xmn=LAT1)
    b <- t(b)
    b_mean <- calc(b, mean)
    assign(paste(j, i, "brickmean", sep=""), b_mean)
    assign(paste(j, i, "brick", sep=""), b)
    
    #---opens the netcdf file after building the url above----
    
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

#----RCode ends-----
