#------------------------------------------------------------------------#
# TITLE:        agmesh_analyze.R
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
# STAGE:        Agmesh  - Stage 5
#
# COMMENTS:     This is the agmesh subsetting and aggregation - stage 5.   
#               Agmesh uses weather parameters for the inland pacific northwest   
#               to calculate climate parameter related outputs. Stage 2 allows
#               input of latitude and longitude, as well as year ranges - which are
#               then passed to a bash shell script for aggregation and subsetting.
#                
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------


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

#----input ranges of years to examine----------#
setwd(paste("/reacchspace/obj1/netcdf/AGMESH/", scen, sep="")) 

print("Analysis Phase.  Please choose an analysis method:")
print("Evapotranspiration EDA compare years - Choose 1")
print("Evapotranspiration EDA compare days - Choose 2")

analyzechoice <- readline("Enter your choice: ")

if (analyzechoice=='1') {
  print("Evapotranspiration EDA")
  print("Enter the following information for ET EDA:")
  inputbrick <- (readline("Enter the ET dataset to examine:"))
  inputbrick <- get(inputbrick)
  inputcorrelation <- (readline("Enter correlation type. (i.e. spearson)"))
  source("/svn/REACCH/trunk/reacch-r-development/r-scripts/et-palouse/agmesh_gridcorrelation.R")
  output = gridcorrelation(rasterstack = inputbrick, method = inputcorrelation, type = "both")
  (print("Plotting ET EDA output..."))
  plot(output)
}

if (analyzechoice=='2') {
  inputyear <- readline("Enter a scenario year to anayze: (e.g. 2000)")
  inputday <- readline("Enter the day of the year number: (1-365)")
  inputday <- get(inputday)
  for (i in inputday:inputday(erichet2000brick)) {
    + x <- erichet2000brick[[i]]
}
}
