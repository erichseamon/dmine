library(rasterVis)
library(data.table)
library(stringr)

setwd("/agmesh-scenarios/scenario_52177/summaries/")
combined.df <- data.frame(read.csv("2001_2015_usda_gridmet_WA"))

#-remove all other variables to allow for datasets based on year, month, county, and commodity - loss and acres

#combined.df <- subset(combined.df, select = -c(insuranceplancode,insurancename,stagecode,damagecausecode,damagecause,month,statecode,state,countyfips,countycode,bi,pr,th,pdsi,pet,erc,rmin,rmax,tmmn,tmmx,srad,sph,vs,fm1000,fm100) )
#combined.df <- subset(combined.df, select = -c(insuranceplancode,insurancename,stagecode,damagecausecode,damagecause,month,statecode,state,countyfips,countycode,bi,pr,th,pdsi,pet,erc,rmin,rmax,tmmn,tmmx,srad,sph,vs,fm1000,fm100) )

#-convert to a data table

combined.df <- data.table(combined.df)

#-order the columns by commodity, then year, month, and county

combined.df <- combined.df[with(combined.df, order(commoditycode,year,monthcode,county)), ]
setwd("/agmesh-scenarios/scenario_52177/commodity_csv/")
#function(commoditynames) sub("\\s+$", "", commoditynames)
#--strip white space off so file names are right
#lapply(combined.df$commodity, function(funct){sub("\\s+$", "", combined.df$commodity[[funct]])})
#commoditynames <- unique(combined.df$commodity)


for (i in commoditynames) {
 x <- subset(combined.df, commodity == i)
 write.csv(x, file=paste(i, ".csv", sep=""))
}

  
  #lapply(commoditynames, function(funct){write.csv(combined.df$commodity[[funct]], file = paste(funct, ".csv", sep = ""))})

#----

#-sum the acres and loss columns for all common rows  This merges all rows that have the same values exept for acres and loss.  We sum those to create a geographic
#-representation for each commodity - for each county, year, and month.  This will be use to convert to a raster for comparison to meterological data.

combined.df <- combined.df[, lapply(.SD, sum), by=list(year,county,commoditycode,monthcode)]

combined.yearmonth <- split(combined.df,list(combined.df$year,combined.df$monthcode, combined.df$commoditycode))
setwd("/agmesh-scenarios/scenario_52177/month/")
lapply(names(combined.yearmonth), function(funct){write.csv(combined.yearmonth[[funct]], file = paste(funct, ".csv", sep = ""))})



#--bringing in county shapefile
setwd("/nethome/erichs/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep("Washington", counties@data$STATE_NAME),]

unique <- list.files("/agmesh-scenarios/scenario_52177/yearmonth/")
maskraster <- raster("/agmesh-scenarios/scenario_52177/pdsi_apr_1996.nc")

for (i in unique) {
setwd("/agmesh-scenarios/scenario_52177/yearmonth/")
x <- as.data.frame(read.csv(i, strip.white = TRUE))
u <- data.frame(trimws(x$county, "r"))
colnames(u) <- c("NAME")
colnames(x) <- c("UNIQUEID", "YEAR", "COUNTY", "COMMODITYCODE", "MONTHCODE", "ACRES", "LOSS")
z <- cbind(x,u)
m <- merge(counties, z, by='NAME')
#shapefile(m)
extent(maskraster) <- extent(m)
r <- rasterize(m, maskraster)
i = substr(i,1,nchar(i)-4)
setwd("/agmesh-scenarios/scenario_52177/raster_commodity_plots/")
#writeRaster(r, filename=paste(i, "_raster", sep=""))
jpeg(paste(i, "_plot", sep=""))
print(levelplot(r, att='LOSS', col.regions=brewer.pal(8, 'Set2')) + 
  layer(sp.polygons(m, lwd=0.5)))
dev.off()
}


levelplot(r, att='LOSS', col.regions=brewer.pal(8, 'Set2')) + 
  layer(sp.polygons(m, lwd=0.5))


unique <- names(combined.yearmonth)

for (i in unique) {
    assign(paste(i, ".csv", sep=""), combined.yearmonth$`i`)
}




combined.split <- split(combined.df, combined.df$commodity)
setwd("/home/git/data/USDA/crop_indemnity_commodity/")
commodityspan = unique(combined.df$commodity)

lapply(names(combined.split), function(funct){write.csv(combined.split[[funct]], file = paste(funct, ".csv", sep = ""))})

