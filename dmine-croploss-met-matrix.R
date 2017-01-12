library(raster)

dirname <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, sep = "")
dirname2 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/summaries2", sep="")
dirname3 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/cdl", sep="")
setwd(dirname)
varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
yearspan = c(2010:2010)


for (i in yearspan) { 
  cdl <- raster(paste("/dmine/data/CDL/", "CDL_", i, ".grd", sep=""))
  #sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #cdl <- projectRaster(cdl, crs = sr)
  wintercdl <- cdl == 24 #spring wheat
  wintercdl <- crop(wintercdl, extent(counties))
  wintercdl[wintercdl==0] <- NA
  #writeRaster(wintercdl, paste(dirname3, "/", "CDL_", kk, "-", i, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
  #print(paste("cdl yearly winter wheat construction for:", kk, "-", i, "-", "-", name_county, "-", j,  sep=""))
  #--new matrix to contain variable, month, year, and county
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=18)
  colnames(newmatrix) <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100", "countyfips", "month", "year")
  varspannumber = 0
  for (j in varspan) { 
    varspannumber = varspannumber + 1
    jj=0
    for (k in monthspan) {
      ncfile <- paste(dirname, "/netcdf/", j, "_", k, "_", i, ".nc", sep="")
      rasterout <- brick(ncfile) #create a brick
      rasterout <- mean(rasterout) #get the mean of all 30 days for the month
      rasterout <- mask(rasterout, counties) #- mask just the raster for the state in question
      rasterout3 <- crop(rasterout, extent(counties)) #now crop it for the state
      r.new = resample(rasterout3, wintercdl, "bilinear")
      rasterout4 <- mask(r.new, wintercdl)
      #png(paste(dirname, "/", j, "_", k, "_", i, ".png", sep=""))
      #plot(rasterout, main = paste0("Monthly Plot for: ", j, ", ", k, ", ", i, sep=""))
      #plot(counties, add=TRUE)
      #dev.off() 
      #rasterout <- t(rasterout)
      #proj4string(rasterout) <- projection 
      for (l in countyfiploop) {
        jj = jj + 1
        subset_county <- counties[counties@data$FIPS == l,]
        name_county <- subset_county$NAME
        e <- crop(rasterout4, subset_county) 
        
        r <- raster(ncol=90, nrow=45)
        extent(r) <- extent(subset_county)
        r.polys <- rasterize(subset_county, r, field = subset_county@data[,1], fun = "mean", 
                             update = TRUE, updateValue = "NA")
        #plot(r.polys)
        
        extent(r.polys) <- extent(subset_county)
        r.new2 = resample(r.polys, e, "bilinear")
        
        
        ee <- mask(e, r.new2)
        ##sp <- SpatialPoints(ee)
        ##eee <- extract(ee, sp, method='bilinear')
        ##newmatrix[jj,varspannumber] <- mean(eee, na.rm=TRUE)
        ##newmatrix[jj,16] <- l
        ##newmatrix[jj,17] <- k #--month
        ##newmatrix[jj,18] <- i #--yeari
        writeRaster(ee, paste(dirname3, "/", "CDL_", kk, "-", i, "-", k, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
        print(paste("writing raster, creating matrix of climate variables for:", kk, "-", i, "-", k, "-", name_county, "-", j,  sep=""))
      }  
    } 
  }
  setwd(dirname2)
  #name <- paste(kk, "_", i, "_palouse_summary", sep="") #--used for individual states
  #name <- paste(dirname, "/", variable, "_", month, "_", year, "_summary", sep="")
  #write.matrix(newmatrix, file=name, sep=",")
}
