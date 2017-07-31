library(dplyr)
library(tidyr)
library(geoknife) #order matters because 'query' is masked by a function in dplyr
library(RColorBrewer)
library(maps) 
library(ncdf4)
library(maptools)
library(raster)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% "Idaho")

inputfile <- "/dmine/data/VIC/month/total_moisture_feb_2013.nc"

#ncin <- open.ncdf(inputfile)
#nc <- open.ncdf(inputfile)

library(sp)
library(raster)
library(ncdf4)

# read ncdf file
nc<-nc_open(inputfile)

# extract variable name, size and dimension
v <- nc$var[[1]]
size <- v$varsize
dims <- v$ndims
nt <- size[dims]              # length of time dimension
lat <- nc$dim$lat$vals   # latitude position
lon <- nc$dim$lon$vals  # longitude position

# read sst variable
r<-list()
for (i in 1:nt) {
  start <- rep(1,dims)     # begin with start=(1,1,...,1)
  start[dims] <- i             # change to start=(1,1,...,i) to read    timestep i
  count <- size                # begin with count=(nx,ny,...,nt), reads entire var
  count[dims] <- 1             # change to count=(nx,ny,...,1) to read 1 tstep
  

  dt<-ncvar_get(nc, varid = 'total_moisture', start = start, count = count)
  
  # convert to raster
  r[i]<-raster(dt)
}

image(t(flip(x, 1)))


# create layer stack with time dimension
r<-stack(r)

# transpose the raster to have correct orientation
rt<-t(r)
extent(rt)<-extent(c(range(lon), range(lat)))
extent(r)<-extent(c(range(lon), range(lat)))



# plot the result
spplot(rt)




























ncin_raster <- raster(ncin)

r2 <- crop(ncin, extent(counties))
r3 <- mask(r2, SPDF)



lon <- get.var.ncdf(ncin, "lon")
nlon <- dim(lon)

lat <- get.var.ncdf(ncin, "lat")
nlat <- dim(lat)

xystream_nc <- cbind(lat,lon)
colnames(xystream_nc) <- c("lat", "long")

#--access modis data from grid points

library(MODISTools)

GetDates(Product = "MOD17A3", Lat = xystream_nc[2,1], Long = xystream_nc[1,1])
uniquexy <- data.frame(unique(xystream_nc))
uniquexy$start.date <- rep(1989, nrow(uniquexy))
uniquexy$end.date <- rep(2000, nrow(uniquexy))

MODISSubsets(LoadDat = uniquexy, Products = "MOD17A3",
             Bands = c("Npp_1km"),
             Size = c(1,1)