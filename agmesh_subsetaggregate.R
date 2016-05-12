#------------------------------------------------------------------------#
# TITLE:        agmesh_subsetaggregate.R
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
# STAGE:        Agmesh subsetting and aggregation - Stage 2
#
# COMMENTS:     This is the agmesh subsetting and aggregation - stage 2.   
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
library("jpeg")
library("png")

#memory.size(10000)

users <- read.csv("/agmesh-scenarios/dmine-users.csv")
friendsfile <- read.csv("/agmesh-scenarios/dmine-wppl-friends.csv", header=FALSE)

friendsfile2 <- friendsfile[-1,1:3]

frow <- nrow(friendsfile2)

RSCENARIO <- sample(50000:100000, 1, replace=F)


#myurl <- "http://hydro1.sci.gsfc.nasa.gov/daac-bin/access/timeseries.cgi?variable=GRACE:GRACEDADM_CLSM025NA_7D.1:gws_inst&startDate=2016-04-15T00&endDate=2014-04-25T00&location=GEOM:POINT(-106.625, 35.875)&type=plot&keep=1"
#z <- paste("/agmesh-scenarios/", scen, sep="")
#zz <- tempfile(pattern = "GRACEDADM_CLSM025NA_7D1", tmpdir = z, fileext = ".png")
#download.file(myurl,zz,mode="wb")

#----Setting vectors for loops for years, variables, day, and rasters---#
#----associated with input datasets 2007-2011------#

#----input ranges of years to examine----------#

#rm(list = ls()) #--clears all lists------#

N1 <- readline("enter first year of data range: ")
N2 <- readline("enter last year of data range: ")
LAT1 <- readline("enter Lat min: ")
LAT2 <- readline("enter Lat max: ")
LON1 <- readline("enter Lon min: ")
LON2 <- readline("enter Lon max: ")

assign("N1", N1, envir = .GlobalEnv)
assign("N2", N2, envir = .GlobalEnv)

#----writes subset variables to file for shell script operations--#

print(paste("Creating R Scenario ", RSCENARIO, sep=""))


fileConn<-file("/tmp/agmesh-subset-R-scenario.txt", "w")
#writeLines(c(paste('yearstart=', N1, sep='')), fileConn)
#writeLines(c(paste('yearend=', N2, sep='')), fileConn)
#writeLines(c(paste('lat1=', LAT1, sep='')), fileConn)
#writeLines(c(paste('lat2=', LAT2, sep='')), fileConn)
#writeLines(c(paste('lon1=', LON1, sep='')), fileConn)
#writeLines(c(paste('lon2=', LON2, sep='')), fileConn)
#writeLines(c(paste('scenario=', "scenario_", RSCENARIO, sep='')), fileConn)
close(fileConn)

for (i in 1:frow) {
  userid <- friendsfile2[i,1]
  scen <- paste("scenario_", RSCENARIO, "_", userid, sep="")
  dir.create(paste("/agmesh-scenarios/", scen, sep=""))
}

#--extraction for GRACE nasa data

#system("/agmesh-code/agmesh-dircreate.sh")

for (i in 1:frow) {
  lat <- friendsfile2[i,2]
  long <- friendsfile2[i,3]
  userid <- friendsfile2[i,1]
  scen <- paste("scenario_", RSCENARIO, "_", userid, sep="")
  fileConn<-file(paste("/tmp/agmesh-subset-R-scenario", "_userid_", userid, ".txt", "w"))
  myurl <- paste("http://hydro1.sci.gsfc.nasa.gov/daac-bin/access/timeseries.cgi?variable=GRACE:GRACEDADM_CLSM025NA_7D.1:gws_inst&startDate=2010-04-15T00&endDate=2015-04-15T00&location=GEOM:POINT(", long, ", ", lat, ")&type=plot&keep=1", sep="")
  z <- paste("/agmesh-scenarios/", scen, sep="")
  zz <- tempfile(pattern = "GRACEDADM_CLSM025NA_7D1", tmpdir = z, fileext = ".png")
  download.file(myurl,zz,mode="wb")
}


#write.table(data.frame(ID,N1,N2,LAT1,LAT2,LON1,LON2), "/tmp/agmesh-subset-vartmp.txt", sep="\t")
system("/agmesh-code/agmesh-subset.sh")
#system("rm /tmp/agmesh-subset-vartmp.txt")
