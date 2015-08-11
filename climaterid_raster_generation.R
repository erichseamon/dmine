#------------------------------------------------------------------------#
# TITLE:        climategrid_raster_generation.R
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
  