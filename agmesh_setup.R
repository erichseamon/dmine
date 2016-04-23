#------------------------------------------------------------------------#
# TITLE:        agmesh_setup.R
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
# STAGE:        Agmesh Setup - Stage 2
#
# COMMENTS:     This is the agmesh setup - stage 2.  Agmesh uses weather 
#               parameters for the inland pacific northwest to calculate  
#               climate parameter related outputs. Stage 2 aggregates and 
#               subsets climatic grid data, based on user input.  The files
#               that are created are used for agmesh analytics.
#                
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------#


#----SETUP SECTION----------------------------------------------------#

#--clear the variable list and set the working directory----#

#rm(list = ls()) #--clears all lists------#
#cat("\14")

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



#----Setting vectors for loops for years, variables, day, and rasters---#
#----associated with input datasets 2007-2011------#

#agmeshtemp <- scan("/tmp/agmesh-subset-vartmp.txt")

#----input ranges of years to examine----------#
setwd(paste("/agmesh-scenarios/", scen, sep=""))

#rm(list = ls()) #--clears all lists------#

#---Loop below that loads netcdf data files for each year, for each 
#---of the six variables ( 1) tmmx - max temp, 2) tmmn - min temp, 
#---3) rmin - min rel humidity, 4) max rel humidity, 5) srad - solar  
#---radiation, 6) vs - wind speed) needed to calculate evapotranspitation.  
#---This loop creates a raster brick for each variable, per year.  
#---For example: tmmx2007brick - raster brick for all max temp daily data.  
#---365 layers (days) across the geographic region.

print("Generating raster arrays for analysis...")

yearspan <- c(N1:N2)
#yearspan <- c(2007,2008,2009,2010,2011)
variablespan <- c("pr", "vs", "srad", 
                  "rmin", "rmax", "tmmx", "tmmn", "bi", "erc", "fm1000", "fm1", "fm100", "th", "pet")
rasterspan <- c("raster", "matrix", "raster_transposed", 
                "matrix_mean", "matrix_median", "matrix_sd", 
                "matrix_min", "matrix_max")
dayspan <- c(1:365)

ncolz <- c(1:109)
nrowz <- c(1:140)

setwd(paste("/agmesh-scenarios/", scen, sep="")) 

#--SECTION 1: Load dataset, remove missing values, and extract data---- #

#-----loading shapefiles for study region - tri-state shapefile
#-----and the crop yield point data for 2011-2013------#
setwd("/agmesh-data/shapefiles/") 

tristate <- readShapePoly('tristate.shp', 
                          proj4string=CRS
                         ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

cropyield <- readShapePoints('REACCHcropyield_allyears.shp', 
                             proj4string=CRS
                             ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

mlra <- readShapePoly('/agmesh-data/shapefiles/mlra_contiguous.shp', 
                             proj4string=CRS
                             ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


cropyield_df <- data.frame(cropyield)
cropyield_df[cropyield_df==0] <- NA
cropyield_df_xyobs <- subset(cropyield_df, select = c(coords.x1, coords.x2))

setwd(paste("/agmesh-scenarios/", scen, sep="")) 


#----SECTION 2: Load data and Derive Variables-------#
#yearspan=c(2000)
#variablespan=c("pdur")
for (i in yearspan) {
  for (j in variablespan) {
    
    #-----loads netcdfs one by one-------------------#
    dirnamedata <- paste("/agmesh-data/shapefiles", sep = "")
    dirname <- paste("/agmesh-scenarios/", scen, sep = "")
    aggmetnc <- file.path(paste("/agmesh-scenarios/", scen, sep = ""))
    agmetfullname <- file.path(dirname, paste(j, "_", i, ".nc", sep = ""))
    aggmetnc <- agmetfullname 
    
    #--creates Raster Bricks for each climate variable, for each year.---#
    
    b <- brick(aggmetnc, lvar=3, level=1, xmx=LAT2, xmn=LAT1)
    b <- t(b)
    b_mean <- calc(b, mean)
    assign(paste(j, i, "brickmean", sep=""), b_mean)
    assign(paste(j, i, "brick", sep=""), b)
    
    #--rasterizes mlra data for the US.  commented out due to time for run
    #--data was created and loaded into /agmesh-data/rasters as a one time
    #--Used for grouping/categorization.
    
    #agmetfullnamemask <- file.path(dirnamedata, paste("tmmx_1979.nc", sep = ""))
    #rastermaskbrick <- raster(agmetfullnamemask)
    #extent(rastermaskbrick) <- extent(mlra)
    #mlra_raster <- raster(ncol=585, nrow=1386)
    #mlra_raster <- rasterize(mlra, rastermaskbrick, 'MLRA_ID')
    
    #--end of rasterization.  should be saved to grd for calling from calc_ET
    
    metvar <- open.ncdf(aggmetnc, write=FALSE)# Open a netcdf file 
    #metvarmask <- open.ncdf(agmetfullnamemask, write=FALSE)# Open a netcdf file 
    
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

