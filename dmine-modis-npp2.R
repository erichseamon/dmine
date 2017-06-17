library(dplyr)
library(tidyr)
library(geoknife) #order matters because 'query' is masked by a function in dplyr
library(RColorBrewer)
library(maps) 

inputfile <- "/dmine/data/USDA/agmesh-scenarios/Washington/netcdf/pdsi_jun_2006.nc"

ncin <- open.ncdf(inputfile)
lon <- get.var.ncdf(ncin, "lon")
nlon <- dim(lon)

lat <- get.var.ncdf(ncin, "lat")
nlat <- dim(lat)

xystream_nc <- cbind(lat,lon)

#--access modis data from grid points

library(MODISTools)

GetDates(Product = "MOD17A3", Lat = xystream_nc[2,1], Long = xystream_nc[1,1])
uniquexy <- data.frame(unique(xystream_nc))
uniquexy$start.date <- rep(2001, nrow(uniquexy))
uniquexy$end.date <- rep(2002, nrow(uniquexy))

MODISSubsets(LoadDat = uniquexy, Products = "MOD17A3",
             Bands = c("Npp_1km"),
             Size = c(1,1))
