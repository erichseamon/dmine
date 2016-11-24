library(rgdal)
library(raster)
library(ncdf)
library("RNetCDF")

setwd(paste('/dmine/data/CDL', sep=""))
cdl <- raster("CD07wgs841.tif")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/Idaho/netcdf/"))
nc <- brick("tmmx_apr_2001.nc", crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


temp <- get.var.ncdf(temp.nc)
temp[temp=="32767"] <- NA

temp.nc$dim$lon$vals -> lon
temp.nc$dim$lat$vals -> lat
temp.nc$dim$day$vals -> time



