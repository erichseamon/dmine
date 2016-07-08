setwd("/agmesh-scenarios/scenario_52177")
combined.df <- data.frame(read.csv("2001_2015_usda_gridmet_WA"))

#-remove all other variables to allow for datasets based on year, month, county, and commodity - loss and acres

combined.df <- subset(combined.df, select = -c(insuranceplancode,insurancename,stagecode,damagecausecode,damagecause,month,statecode,commodity,state,countyfips,countycode,bi,pr,th,pdsi,pet,erc,rmin,rmax,tmmn,tmmx,srad,sph,vs,fm1000,fm100) )

#-convert to a data table

combined.df <- data.table(combined.df)

#-order the columns by commodity, then year, month, and county

combined.df <- combined.df[with(combined.df, order(commoditycode,year,monthcode,county)), ]

#-sum the acres and loss columns for all common rows  This merges all rows that have the same values exept for acres and loss.  We sum those to create a geographic
#-representation for each commodity - for each county, year, and month.  This will be use to convert to a raster for comparison to meterological data.

combined.df <- combined.df[, lapply(.SD, sum), by=list(year,county,commoditycode,monthcode)]


combined.split <- split(combined.df, combined.df$commodity)
setwd("/home/git/data/USDA/commodity/")
commodityspan = unique(combined.df$commodity)

lapply(names(combined.split), function(funct){write.csv(combined.split[[funct]], file = paste(funct, ".csv", sep = ""))})

