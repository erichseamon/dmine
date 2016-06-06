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
#colnames(friendsfile2) <- c("ID", "lat", "long")

frow <- nrow(friendsfile2)

RSCENARIO <- sample(200000:300000, 1, replace=F)

#----writes subset variables to file for shell script operations--#

print(paste("Creating R Scenario ", RSCENARIO, sep=""))

#--create folders for each user from DMINE.  These folders will hold resultant information, including images of probability

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
  z <- paste("/agmesh-scenarios/", scen, sep="")
  myurl <- paste("http://hydro1.sci.gsfc.nasa.gov/daac-bin/access/timeseries.cgi?variable=GRACE:GRACEDADM_CLSM025NA_7D.1:gws_inst&startDate=2010-05-15T00&endDate=2015-05-20T00&location=GEOM:POINT(", long, ", ", lat, ")&type=plot&keep=1", sep="")
  daymet <- paste("https://daymet.ornl.gov/data/send/saveData?lat=", lat, "&lon=", long, "&year=2012,2013", sep="")
  #daymet <- system(paste("wget 'https://daymet.ornl.gov/data/send/saveData?lat=", lat, "&lon=", long, "&year=2012,2013' " , z, scen, sep=""))
  zz <- tempfile(pattern = "GRACEDADM_CLSM025NA_7D1", tmpdir = z, fileext = ".png")
  zzdaymet <- tempfile(pattern = "scenario_", tmpdir = z, fileext = ".csv")
  download.file(myurl,zz,mode="wb")
  download.file(daymet,zzdaymet,mode="wb")
}



#--call script to subset the appropriate locational data for all the user location in the users file
#--and place in each respective user folder, created above.  Each folder is re-created when the scripts are run.

#write.table(data.frame(ID,N1,N2,LAT1,LAT2,LON1,LON2), "/tmp/agmesh-subset-vartmp.txt", sep="\t")
system("/home/git/nco/ncra-subset.sh")
#system("rm /tmp/agmesh-subset-vartmp.txt")
