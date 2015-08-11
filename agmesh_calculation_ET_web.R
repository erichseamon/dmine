
#------------------------------------------------------------------------#
# TITLE:        agmesh_analysis_ET_web.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         November 14, 2014
#
# STAGE:        Raster Generation - Stage 3
#
# COMMENTS:     This is the agmesh analyis script for ET.  Agmesh  
#               uses weather parameters for the inland pacific northwest   
#               to calculateclimate parameter related outputs. This 
#               agmesh analysis script uses the data generated in 
#               stages 1 and 2 to create ET raster bricks for the
#               range of years input.
#
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------#

options(warn=-1)

#-------Prepping for Raster calculations for evapotranspiration---------#
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
#detach("package:hydroTSM")

scen <- read.table("/tmp/agmesh-subset-web-scenario.txt")
scen <- t(scen)

setwd(paste("/agmesh-scenarios/", scen[1], sep="")) 
yearspan <- c(scen[2]:scen[3])

variablespan <- c("pdur", "vs", "srad", 
                  "rmin", "rmax", "tmmx", "tmmn")
rasterspan <- c("raster", "matrix", "raster_transposed", 
                "matrix_mean", "matrix_median", "matrix_sd", 
                "matrix_min", "matrix_max")
dayspan <- c(1:365)

ncolz <- c(1:109)
nrowz <- c(1:140)

#----SECTION 2: Load data and Derive Variables-------#
#yearspan=c(2000)
#variablespan=c("pdur")
for (i in yearspan) {
  for (j in variablespan) {
    
    #-----loads netcdfs one by one-------------------#
    
    dirname <- paste("/agmesh-scenarios/", scen[1], sep = "")
    aggmetnc <- file.path(paste("/agmesh-scenarios/", scen[1], sep = ""))
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

#print("Converting temperature arrays from Kelvin to Celsius...")



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
  
}

#--print("Generating arrays for evapotranspiration calculation ...")--#



nrow <- nrow(tmmnbrick)
ncol <- ncol(tmmnbrick)
nlayers <- nlayers(tmmnbrick)

#extraTraster <- raster(extraT, layer=1)
dayspan <- c(1:nlayers)
dayspan_array <- array(data=NaN, c(nrow,ncol,nlayers))
ETlat_array <- array(data=NaN, c(nrow,ncol,nlayers))
extrat_array <- array(data=NaN, c(nrow,ncol,nlayers))

extratncell <- c(1:15260)

for (i in yearspan) {
  print(paste(i))
  for (k in dayspan) {
    #for (j in ncolz) {
    #for (k in nrowz) {
    dayspan_array[,,k] <- i
    loopvalue <- tmmnbrick
    #loopvalue <- get(paste(j, i, "brick", sep=""))
    ETlat_array[,,k] <- yFromRow(loopvalue)
    #ETlat_array[j,k,i] <- yFromRow(srad2007brick, row=1:109(srad2007brick))
    
    ETlat<- yFromRow(loopvalue)
    ETlat <- t(ETlat)
    ETlat_matrix <- matrix(ETlat,nrow=140,ncol=length(ETlat),byrow=TRUE)
    ETlat_matrix <- t(ETlat_matrix)
    #ETlat_array <- as.array(ETlat_matrix)
    ETlat_raster <- setValues(loopvalue, ETlat_array)
    ETlat_raster <- raster(ETlat_raster)
    ETlatrasterbrick <- brick(ETlat_raster)
    ETlatrasterbrickfinal <- brick(ETlat_raster, ETlat_raster, 
                                   ETlat_raster, ETlat_raster,ETlat_raster, 
                                   ETlat_raster, ETlat_raster, ETlat_raster)
    ETlatrasterbrickSet <- setValues(loopvalue, ETlat_array)
    
    
  }
}






#------------------------------------------------------------------------#
# TITLE:        agmesh_calculation_ET_web.R
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
# STAGE:        Agmesh Calculation for ET - Stage 4
#
# COMMENTS:     This is the agmesh setup - stage 4.  Agmesh uses weather 
#               parameters for the inland pacific northwest to calculate  
#               climate parameter related outputs. Stage 4 uses 
#               data created in stages 2 and 3 to calculate ET.
#                
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------

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



#-------Raster calculations for evapotranspiration ---------#

library("maptools")
library("sirad")
library("reshape")
library("reshape2")

setwd("/agmesh-data/shapefiles/") 


tristate <- readShapePoly('tristate.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

cropyield <- readShapePoints('REACCHcropyield_nonulls.shp', 
                             proj4string=CRS
                             ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

mlra <- readShapePoly('/agmesh-data/shapefiles/mlra_contiguous.shp', 
                      proj4string=CRS
                      ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#--set mlra mask raster
mlramaskraster <- raster("/agmesh-data/rasters/mlra_raster.grd")

croppedmlra <- crop(mlramaskraster, tmmxbrick, package="raster")

cropyield_df <- data.frame(cropyield)
cropyield_df[cropyield_df==0] <- NA
cropyield_df_xyobs <- subset(cropyield_df, select = c(coords.x1, coords.x2))
cropyield_df_yieldobs <- subset(cropyield_df, select = c(X12YieldWW))


cropyield_df_yieldobs2012winter <- data.frame(cropyield_df$X12YieldWW)
cropyield_df_yieldobs2012spring <- data.frame(cropyield_df$X12YieldS)
cropyield_df_yieldobs2013winter <- data.frame(cropyield_df$X13YieldWW)
cropyield_df_yieldobs2013spring <- data.frame(cropyield_df$X13YieldS)


obs_nrow <- nrow(cropyield_df)
obs <- c(1:obs_nrow)


setwd(paste("/agmesh-scenarios/", scen[1], sep="")) 
scenariodir <- setwd(paste("/agmesh-scenarios/", scen[1], sep="")) 


for (i in yearspan) {
  
  #print(paste("Generating ET for year:", i))
  
  
  meah=10
  tmmxETbrick <- get(paste("tmmx", i, "brick", sep=""))
  tmmxETbrickspring <- tmmxETbrick[[1:100]]
  tmmxETbrickwinter <- tmmxETbrick[[210:310]]
  tmmxETbrickspringmean <- calc(tmmxETbrickspring, mean)
  tmmxETbrickwintermean <- calc(tmmxETbrickwinter, mean)
  assign(paste("tmmxET", i, "brickspringmean", sep = ""),tmmxETbrickspringmean)
  assign(paste("tmmxET", i, "brickwintermean", sep = ""),tmmxETbrickwintermean)
  tmmnETbrick <- get(paste("tmmn", i, "brick", sep=""))
  tmmnETbrickspring <- tmmnETbrick[[1:100]]
  tmmnETbrickwinter <- tmmnETbrick[[210:310]]
  tmmnETbrickspringmean <- calc(tmmnETbrickspring, mean)
  tmmnETbrickwintermean <- calc(tmmnETbrickwinter, mean)
  assign(paste("tmmnET", i, "brickspringmean", sep = ""),tmmnETbrickspringmean)
  assign(paste("tmmnET", i, "brickwintermean", sep = ""),tmmnETbrickwintermean)
  rmaxETbrick <- get(paste("rmax", i, "brick", sep=""))
  rmaxETbrickspring <- rmaxETbrick[[1:100]]
  rmaxETbrickwinter <- rmaxETbrick[[210:310]]
  rmaxETbrickspringmean <- calc(rmaxETbrickspring, mean)
  rmaxETbrickwintermean <- calc(rmaxETbrickwinter, mean)
  assign(paste("rmaxET", i, "brickspringmean", sep = ""),rmaxETbrickspringmean)
  assign(paste("rmaxET", i, "brickwintermean", sep = ""),rmaxETbrickwintermean)
  rminETbrick <- get(paste("rmin", i, "brick", sep=""))
  rminETbrickspring <- rminETbrick[[1:100]]
  rminETbrickwinter <- rminETbrick[[210:310]]
  rminETbrickspringmean <- calc(rminETbrickspring, mean)
  rminETbrickwintermean <- calc(rminETbrickwinter, mean)
  assign(paste("rminET", i, "brickspringmean", sep = ""),rminETbrickspringmean)
  assign(paste("rminET", i, "brickwintermean", sep = ""),rminETbrickwintermean)
  sradETbrick <- get(paste("srad", i, "brick", sep=""))
  sradETbrickspring <- sradETbrick[[1:100]]
  sradETbrickwinter <- sradETbrick[[210:310]]
  sradETbrickspringmean <- calc(sradETbrickspring, mean)
  sradETbrickwintermean <- calc(sradETbrickwinter, mean)
  assign(paste("sradET", i, "brickspringmean", sep = ""),sradETbrickspringmean)
  assign(paste("sradET", i, "brickwintermean", sep = ""),sradETbrickwintermean)
  vsETbrick <- get(paste("vs", i, "brick", sep=""))
  vsETbrickspring <- vsETbrick[[1:100]]
  vsETbrickwinter <- vsETbrick[[210:310]]
  vsETbrickspringmean <- calc(vsETbrickspring, mean)
  vsETbrickwintermean <- calc(vsETbrickwinter, mean)
  assign(paste("vsET", i, "brickspringmean", sep = ""),vsETbrickspringmean)
  assign(paste("vsET", i, "brickwintermean", sep = ""),vsETbrickwintermean)
  Tmeanrasterbrick <- (tmmxETbrick - tmmnETbrick)/2
  RHmeanrasterbrick <- (rmaxETbrick - rminETbrick)/2
  es <- 0.6108 * exp(17.27 * Tmeanrasterbrick / (Tmeanrasterbrick+237.3))
  ea <- (RHmeanrasterbrick / 100) * es
  deltaVP <- es - ea
  vappressure <- ea
  z = 1000

  ETlatradians <- radians(ETlat_array)
  #extraT = extrat(dayOfYear(dayspan), ETlatradians)$ExtraTerrestrialSolarRadiationDaily
  #tal <- cst(srad2007brick, dayOfYear(dayspan), radians(ETlatradians))
  extraT = 50
  tal = 1

  erichet <- (0.408 * deltaVP(tmmxETbrick, tmmnETbrick) * 
                  (rns(sradETbrick, albedo = 0.23) - rnl(tmmxETbrick, tmmnETbrick, 
                  sradETbrick, vappressure, extraT, tal)) + psychC(tmmxETbrick, 
                  tmmnETbrick, z) * (900/(Tmeanrasterbrick + 273)) * 
                  wind2(vsETbrick, meah) * (es(tmmxETbrick,tmmnETbrick) - 
                  vappressure))/(deltaVP(tmmxETbrick, tmmnETbrick) + psychC(tmmxETbrick, 
                  tmmnETbrick, z) * (1 + 0.34 * wind2(vsETbrick, meah)))
  
  #--writes et grids to folder.  needs permissions fix
  
  assign(paste("erichet", i, "brick", sep=""), erichet)
  savedstackname <- paste("erichet", i, "brick_", scen[1], sep="")
  savedstack <- assign(paste("erichet", i, "brick_", scen[1], sep=""), erichet)
  fname <- paste("/agmesh-scenarios/", scen[1], "/ETgrids", "/ET", i, scen[1], sep="")
  writeRaster(savedstack, filename=fname, bandorder='BIL', overwrite=TRUE)
  
  erichetspring <- erichet[[1:100]]
  erichetwinter <- erichet[[210:310]]
  erichetspring_xyobs <- extract(erichetspring, cropyield_df_xyobs)
  erichetwinter_xyobs <- extract(erichetwinter, cropyield_df_xyobs)
  #erichetspring_mean <- rowMeans(erichetspring_xyobs)
  #erichetspring_mean <- data.frame(erichetspring_mean)
  #erichetwinter_mean <- rowMeans(erichetwinter_xyobs)
  #erichetwinter_mean <- data.frame(erichetwinter_mean)
  #erichetspring_mean <- mean(erichetspring)
  #erichetspring_mean <- data.frame(erichetspring_mean)
  #erichetwinter_mean <- rowMeans(erichetwinter)
  #erichetwinter_mean <- data.frame(erichetwinter_mean)
  
  #--writes et grids to folder.  needs permissions fix
  
  savedstacknamespring <- paste("erichet", i, "brick_", scen[1], sep="")
  savedstackspring <- assign(paste("erichet", i, "brick_", scen[1], sep=""), erichetspring)
  fnamespring <- paste("/agmesh-scenarios/", scen[1], "/ETgrids", "/ETspring", i, scen[1], sep="")
  writeRaster(savedstackspring, filename=fnamespring, bandorder='BIL', overwrite=TRUE)
  
  #--writes et grids to folder.  needs permissions fix
  
  savedstacknamewinter <- paste("erichet", i, "brick_", scen[1], sep="")
  savedstackwinter <- assign(paste("erichet", i, "brick_", scen[1], sep=""), erichetwinter)
  fnamewinter <- paste("/agmesh-scenarios/", scen[1], "/ETgrids", "/ETwinter", i, scen[1], sep="")
  writeRaster(savedstackwinter, filename=fnamewinter, bandorder='BIL', overwrite=TRUE)
  
  #print("past the raster stuff")
  
  #---point of work break es02232015 - data above ready for seasonal ET comparison---#
  
  assign(paste("erichet", i, sep=""), erichet)
  assign(paste("erichet", i, "spring", sep=""), erichetspring)
  assign(paste("erichet", i, "winter", sep=""), erichetwinter)
  #assign(paste("erichet", i, "spring", "_xyobs", sep=""), erichetspring_xyobs)
  #assign(paste("erichet", i, "winter", "_xyobs", sep=""), erichetwinter_xyobs)
  #assign(paste("erichet", i, "spring", "_mean", sep=""), erichetspring_mean)
  #assign(paste("erichet", i, "winter", "_mean", sep=""), erichetwinter_mean)
  
  #for (j in season) {
    #erichcrop_mean <- get(paste("cropyield_df_yieldobs", i, j, sep=""))
    #erichcrop_mean <- as.numeric(unlist(erichcrop_mean))
    #erichcrop_mean <- mean(erichcrop_mean)
    #assign(paste("cropyield_df_yieldobs_mean", i, j, sep=""), erichcrop_mean)
    #erichcrop_sd <- get(paste("cropyield_df_yieldobs", i, j, sep=""))
    #erichcrop_sd <- as.numeric(unlist(erichcrop_sd))
    #erichcrop_sd <- sd(erichcrop_sd)             
    #assign(paste("cropyield_df_yieldobs_sd", i, j, sep=""), erichcrop_sd)
    
  #}  

  
  ET_mean <- calc(erichet, mean)
  assign(paste("erichet", i, "brick", "mean", sep=""), ET_mean)
  ET_matrix <- as.matrix(ET_mean)
  assign(paste("ET", i, "brick", "matrix", sep=""), ET_matrix)
  
  
  mlrapoints <- rasterToPoints(croppedmlra)
  mlraframe <- as.data.frame(mlrapoints)
  mlrafactor <- as.factor(mlraframe$layer)
  mlracell <- nrow(mlraframe)
  
  combinerows <- ncol * nrow * nlayers
  #combinedallmatrix <- matrix(data=NA,nrow=combinerows,ncol=7) 
  varbrickspanrow <- nrow(tmmxETbrick)
  varbrickspancol <- ncol(tmmxETbrick)
  combinedallmatrixfinal <- c(1,1,1,1,1,1,1,1,1,1,1)
  for (j in 1:varbrickspanrow){
    for (k in 1:varbrickspancol){
      combined1 <- cbind(c(tmmxETbrick[j,k,,]))
      combined2 <- cbind(c(tmmnETbrick[j,k,,]))
      combined3 <- cbind(c(rminETbrick[j,k,,]))
      combined4 <- cbind(c(rmaxETbrick[j,k,,]))
      combined5 <- cbind(c(sradETbrick[j,k,,]))
      combined6 <- cbind(c(vsETbrick[j,k,,]))
      combined7 <- cbind(c(croppedmlra[j,k,]))
      combined9 <- cbind(c(erichet[j,k,]))
      combined8 <- mlraframe[mlracell, -3]
      combined7a <- cbind(c(rep(combined7, nlayers(tmmnETbrick))))
      list2 <- cbind(c(rep(i, nlayers(tmmnETbrick))))
      list3 <-cbind(c(rep(mlraframe[j*k,1], nlayers(tmmnETbrick))))
      list4 <-cbind(c(rep(mlraframe[j*k,2], nlayers(tmmnETbrick))))
      combinedallmatrix <- cbind(combined1, combined2, combined3, combined4, combined5, combined6, combined9, list2, combined7a, list3, list4)
      combinedallmatrixfinal <- rbind2(combinedallmatrixfinal, combinedallmatrix)
    }
  }
  
  assign(paste("climatematrix", i, sep=""), combinedallmatrixfinal)
  
  
  for (j in 1:nlayers(erichet)) {
    plotz <- subset(erichet, j)
    #plotz <- get(paste("erichet$layer.", j, sep=""))
    jpeg(paste("/agmesh-scenarios/", scen, "/ETtimelapse/", "ET", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
    plot(plotz)
    dev.off(); 
  }  
  
  #----end 
}

fclimatematrix1 <- c(1,1,1,1,1,1,1,1,1,1,1)
for (i in yearspan){
  fclimatematrix2 <- get(paste("climatematrix", i, sep=""))
  fclimatematrix1 <- rbind2(fclimatematrix1, fclimatematrix2)
}
#print("here")
setwd(paste("/agmesh-scenarios/", scen, "/ETtimelapse", sep="")) 
system("/usr/local/bin/ffmpeg -i '%*.jpg' -r 30 -q:v 2 timelapse.mp4")
#print("and here")
#------------------------------------------------------------------------#
# TITLE:        agmesh_plotting_ET.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         November 14, 2014
#
# STAGE:        Plotting - Stage 5
#
# COMMENTS:     This is the agmesh plotting script for ET.  Agmesh  
#               uses weather parameters for the inland pacific northwest   
#               to calculateclimate parameter related outputs. This 
#               agmesh plotting script uses the data generated in 
#               stages 1 and 2 to create plots.
#
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------#

library("MASS")
now <- Sys.time()
#------SECTION 3 - Plot Univariate and BiVariate Data ------#

#------Plotting - ET with climate variables ------#

#win.graph(15,15)

#dev.off()

#--spring plotting

for (i in yearspan) {
  
  jpeg(paste("/agmesh-scenarios/", scen, "/tmmximages", "/springplot", "tmmx", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  
  tmmxbrickspringmean <- get(paste("tmmxET", i, "brickspringmean", sep=""))
  tmmnbrickspringmean <- get(paste("tmmnET", i, "brickspringmean", sep=""))
  rmaxbrickspringmean <- get(paste("rmaxET", i, "brickspringmean", sep=""))
  rminbrickspringmean <- get(paste("rminET", i, "brickspringmean", sep=""))
  sradbrickspringmean <- get(paste("sradET", i, "brickspringmean", sep=""))
  vsbrickspringmean <- get(paste("vsET", i, "brickspringmean", sep=""))
  
  
  #tmmxbrickmean2012 <- get(paste("tmmx", 2012, "brickmean", sep=""))
  #tmmnbrickmean2012 <- get(paste("tmmn", 2012, "brickmean", sep=""))
  #rminbrickmean2012 <- get(paste("rmin", 2012, "brickmean", sep=""))
  #rmaxbrickmean2012 <- get(paste("rmax", 2012, "brickmean", sep=""))
  #sradbrickmean2012 <- get(paste("srad", 2012, "brickmean", sep=""))
  #vsbrickmean2012 <- get(paste("vs", 2012, "brickmean", sep=""))      
  
  #tmmxbrickmean2013 <- get(paste("tmmx", 2013, "brickmean", sep=""))
  #tmmnbrickmean2013 <- get(paste("tmmn", 2013, "brickmean", sep=""))
  #rminbrickmean2013 <- get(paste("rmin", 2013, "brickmean", sep=""))
  #rmaxbrickmean2013 <- get(paste("rmax", 2013, "brickmean", sep=""))
  #sradbrickmean2013 <- get(paste("srad", 2013, "brickmean", sep=""))
  #vsbrickmean2013 <- get(paste("vs", 2013, "brickmean", sep="")) 
  
  
  hist(tmmxbrickspringmean,   
       col = "gray",
       xlab = "Celsius",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Max Air Temp - ", "Spring ", i, sep=""))
  
  #-----Add summary statistics to plot
  tmmxhistspringmean <- mean(as.vector(tmmxbrickspringmean))
  tmmxhistmedianspring <- median(as.vector(tmmxbrickspringmean))
  tmmxhistsdspring <- sd(as.vector(tmmxbrickspringmean))
  tmmxhistminspring <- min(as.vector(tmmxbrickspringmean))
  tmmxhistmaxspring <- max(as.vector(tmmxbrickspringmean))
  
  abline(v=tmmxhistspringmean,lwd=2)
  abline(v=tmmxhistmedianspring,col = "red",lwd = 2,lty = "dashed")
  #abline(v= tmmx2007histsd,col = "blue", lwd = 2,lty = "dashed")
  points(tmmxhistminspring,100,pch = "*")
  points(tmmxhistmaxspring,100,pch = "*")
  
  #----Add Legend
  legend(-10,2000,
         #c("Mean","Median","St.Dev.","Min.","Max."),
         c("Mean","Median","Min.","Max."),
         #col = c("black","red","blue","black","black"),
         col = c("black","red","black","black"),
         pch = c(NA,NA,NA,"*","*"),
         lwd = c(2,2,2,NA,NA),
         lty = c(1,2,2,NA,NA),
         bty = "n",
         cex = 0.65)
  
  plot(subset(tmmxbrickspringmean), 
       #col = "gray",
       xlab = "Longitude", 
       ylab = "Latitude",
       #ylim = c(0,240),
       #xlim = c(-0.5,6),
       main = paste("Max Air Temp - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  dev.off()
  
  
  
  jpeg(paste("/agmesh-scenarios/", scen, "/tmmnimages", "/springplot", "tmmn", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(tmmnbrickspringmean,   
       col = "gray",
       xlab = "Celsius",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Min Air Temp - ", "Spring ", i, sep=""))
  
  #---Add summary statistics to plot
  
  tmmnhistspringmean <- mean(as.vector(tmmnbrickspringmean))
  tmmnhistmedianspring <- median(as.vector(tmmnbrickspringmean))
  tmmnhistsdspring <- sd(as.vector(tmmnbrickspringmean))
  tmmnhistminspring <- min(as.vector(tmmnbrickspringmean))
  tmmnhistmaxspring <- max(as.vector(tmmnbrickspringmean))
  
  
  abline(v=tmmnhistspringmean,lwd=2)
  abline(v=tmmnhistmedianspring,col = "red",lwd = 2,lty = "dashed")
  #abline(v= tmmn2007histsd,col = "blue", lwd = 2,lty = "dashed")
  points(tmmnhistminspring,100,pch = "*")
  points(tmmnhistmaxspring,100,pch = "*")
  
  
  plot(subset(tmmnbrickspringmean),  
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Min Air Temp - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  
  
  jpeg(paste("/agmesh-scenarios/", scen, "/rminimages", "/springplot", "rmin",  i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(rminbrickspringmean,   
       col = "gray",
       xlab = "%",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Min Rel Humidity - ", "Spring ", i, sep=""))
  
  #---Add summary statistics to plot
  rminhistspringmean <- mean(as.vector(rminbrickspringmean))
  rminhistmedianspring <- median(as.vector(rminbrickspringmean))
  rminhistsdspring <- sd(as.matrix(rminbrickspringmean))
  rminhistminspring <- min(as.vector(rminbrickspringmean))
  rminhistmaxspring <- max(as.vector(rminbrickspringmean))
  
  
  abline(v=rminhistspringmean,lwd=2)
  abline(v=rminhistmedianspring,col = "red",lwd = 2,lty = "dashed")
  #abline(v= rmin2007histsd,col = "blue", lwd = 2,lty = "dashed")
  points(rminhistminspring,100,pch = "*")
  points(rminhistmaxspring,100,pch = "*")
  
  
  
  plot(subset(rminbrickspringmean),  
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Min Rel Humidity - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  jpeg(paste("/agmesh-scenarios/", scen, "/rmaximages", "/springplot", "rmax", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(rmaxbrickspringmean,   
       col = "gray",
       xlab = "%",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Max Rel Humidity - ", "Spring ", i, sep=""))
  
  #---Add summary statistics to plot
  rmaxhistspringmean <- mean(as.vector(rmaxbrickspringmean))
  rmaxhistmedianspring <- median(as.vector(rmaxbrickspringmean))
  rmaxhistsdspring <- sd(as.matrix(rmaxbrickspringmean))
  rmaxhistminspring <- min(as.vector(rmaxbrickspringmean))
  rmaxhistmaxspring <- max(as.vector(rmaxbrickspringmean))
  
  
  abline(v=rmaxhistspringmean,lwd=2)
  abline(v=rmaxhistmedianspring,col = "red",lwd = 2,lty = "dashed")
  #abline(v= rmax2007histsd,col = "blue", lwd = 2,lty = "dashed")
  points(rmaxhistminspring,100,pch = "*")
  points(rmaxhistmaxspring,100,pch = "*")
  
  
  plot(subset(rmaxbrickspringmean),  
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Max Rel Humidity - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  jpeg(paste("/agmesh-scenarios/", scen, "/sradimages", "/springplot", "srad", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(sradbrickspringmean,   
       col = "gray",
       xlab = "w/m2",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Solar Radiation - ", "Spring ", i, sep=""))
  
  #---Add summary statistics to plot
  sradhistspringmean <- mean(as.vector(sradbrickspringmean))
  sradhistmedianspring <- median(as.vector(sradbrickspringmean))
  sradhistsdspring <- sd(as.matrix(sradbrickspringmean))
  sradhistminspring <- min(as.vector(sradbrickspringmean))
  sradhistmaxspring <- max(as.vector(sradbrickspringmean))
  
  
  abline(v=sradhistspringmean,lwd=2)
  abline(v=sradhistmedianspring,col = "red",lwd = 2,lty = "dashed")
  #abline(v= srad2007histsd,col = "blue", lwd = 2,lty = "dashed")
  points(sradhistminspring,100,pch = "*")
  points(sradhistmaxspring,100,pch = "*")
  
  
  
  plot(subset(sradbrickspringmean),  
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Solar Radiation - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  jpeg(paste("/agmesh-scenarios/", scen, "/vsimages", "/springplot", "vs", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(vsbrickspringmean,   
       col = "gray",
       xlab = "m-s-1",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Wind Speed - ", "Spring ", i, sep=""))
  
  #---Add summary statistics to plot
  vshistspringmean <- mean(as.vector(vsbrickspringmean))
  vshistmedianspring <- median(as.vector(vsbrickspringmean))
  vshistsdspring <- sd(as.matrix(vsbrickspringmean))
  vshistminspring <- min(as.vector(vsbrickspringmean))
  vshistmaxspring <- max(as.vector(vsbrickspringmean))
  
  
  abline(v=vshistspringmean,lwd=2)
  abline(v=vshistmedianspring,col = "red",lwd = 2,lty = "dashed")
  abline(v= vshistsdspring,col = "blue", lwd = 2,lty = "dashed")
  points(vshistminspring,100,pch = "*")
  points(vshistmaxspring,100,pch = "*")
  
  plot(subset(vsbrickspringmean),  
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Wind Speed - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
}





#--winter plottingnew

for (i in yearspan) {
  
  jpeg(paste("/agmesh-scenarios/", scen, "/tmmximages", "/winterplot", "tmmx", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  
  tmmxbrickwintermean <- get(paste("tmmxET", i, "brickwintermean", sep=""))
  tmmnbrickwintermean <- get(paste("tmmnET", i, "brickwintermean", sep=""))
  rmaxbrickwintermean <- get(paste("rmaxET", i, "brickwintermean", sep=""))
  rminbrickwintermean <- get(paste("rminET", i, "brickwintermean", sep=""))
  sradbrickwintermean <- get(paste("sradET", i, "brickwintermean", sep=""))
  vsbrickwintermean <- get(paste("vsET", i, "brickwintermean", sep=""))
  
  
  #tmmxbrickmean2012 <- get(paste("tmmx", 2012, "brickmean", sep=""))
  #tmmnbrickmean2012 <- get(paste("tmmn", 2012, "brickmean", sep=""))
  #rminbrickmean2012 <- get(paste("rmin", 2012, "brickmean", sep=""))
  #rmaxbrickmean2012 <- get(paste("rmax", 2012, "brickmean", sep=""))
  #sradbrickmean2012 <- get(paste("srad", 2012, "brickmean", sep=""))
  #vsbrickmean2012 <- get(paste("vs", 2012, "brickmean", sep=""))      
  
  #tmmxbrickmean2013 <- get(paste("tmmx", 2013, "brickmean", sep=""))
  #tmmnbrickmean2013 <- get(paste("tmmn", 2013, "brickmean", sep=""))
  #rminbrickmean2013 <- get(paste("rmin", 2013, "brickmean", sep=""))
  #rmaxbrickmean2013 <- get(paste("rmax", 2013, "brickmean", sep=""))
  #sradbrickmean2013 <- get(paste("srad", 2013, "brickmean", sep=""))
  #vsbrickmean2013 <- get(paste("vs", 2013, "brickmean", sep="")) 
  
  
  hist(tmmxbrickwintermean,   
       col = "gray",
       xlab = "Celsius",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Max Air Temp - ", "Winter ", i, sep=""))
  
  #-----Add summary statistics to plot
  tmmxhistwintermean <- mean(as.vector(tmmxbrickwintermean))
  tmmxhistmedianwinter <- median(as.vector(tmmxbrickwintermean))
  tmmxhistsdwinter <- sd(as.vector(tmmxbrickwintermean))
  tmmxhistminwinter <- min(as.vector(tmmxbrickwintermean))
  tmmxhistmaxwinter <- max(as.vector(tmmxbrickwintermean))
  
  abline(v=tmmxhistwintermean,lwd=2)
  abline(v=tmmxhistmedianwinter,col = "red",lwd = 2,lty = "dashed")
  #abline(v= tmmx2007histsd,col = "blue", lwd = 2,lty = "dashed")
  points(tmmxhistminwinter,100,pch = "*")
  points(tmmxhistmaxwinter,100,pch = "*")
  
  #----Add Legend
  legend(-10,2000,
         #c("Mean","Median","St.Dev.","Min.","Max."),
         c("Mean","Median","Min.","Max."),
         #col = c("black","red","blue","black","black"),
         col = c("black","red","black","black"),
         pch = c(NA,NA,NA,"*","*"),
         lwd = c(2,2,2,NA,NA),
         lty = c(1,2,2,NA,NA),
         bty = "n",
         cex = 0.65)
  
  plot(subset(tmmxbrickwintermean),   
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,240),
       #xlim = c(-0.5,6),
       main = paste("Max Air Temp - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  
  
  jpeg(paste("/agmesh-scenarios/", scen, "/tmmnimages", "/winterplot", "tmmn", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(tmmnbrickwintermean,   
       col = "gray",
       xlab = "Celsius",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Min Air Temp - ", "Winter ", i, sep=""))
  
  #---Add summary statistics to plot
  
  tmmnhistwintermean <- mean(as.vector(tmmnbrickwintermean))
  tmmnhistmedianwinter <- median(as.vector(tmmnbrickwintermean))
  tmmnhistsdwinter <- sd(as.vector(tmmnbrickwintermean))
  tmmnhistminwinter <- min(as.vector(tmmnbrickwintermean))
  tmmnhistmaxwinter <- max(as.vector(tmmnbrickwintermean))
  
  
  abline(v=tmmnhistwintermean,lwd=2)
  abline(v=tmmnhistmedianwinter,col = "red",lwd = 2,lty = "dashed")
  #abline(v= tmmn2007histsd,col = "blue", lwd = 2,lty = "dashed")
  points(tmmnhistminwinter,100,pch = "*")
  points(tmmnhistmaxwinter,100,pch = "*")
  
  
  plot(subset(tmmnbrickwintermean),  
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Min Air Temp - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  
  
  jpeg(paste("/agmesh-scenarios/", scen, "/rminimages", "/winterplot", "rmin",  i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(rminbrickwintermean,   
       col = "gray",
       xlab = "%",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Min Rel Humidity - ", "Winter ", i, sep=""))
  
  #---Add summary statistics to plot
  rminhistwintermean <- mean(as.vector(rminbrickwintermean))
  rminhistmedianwinter <- median(as.vector(rminbrickwintermean))
  rminhistsdwinter <- sd(as.matrix(rminbrickwintermean))
  rminhistminwinter <- min(as.vector(rminbrickwintermean))
  rminhistmaxwinter <- max(as.vector(rminbrickwintermean))
  
  
  abline(v=rminhistwintermean,lwd=2)
  abline(v=rminhistmedianwinter,col = "red",lwd = 2,lty = "dashed")
  #abline(v= rmin2007histsd,col = "blue", lwd = 2,lty = "dashed")
  points(rminhistminwinter,100,pch = "*")
  points(rminhistmaxwinter,100,pch = "*")
  
  
  
  plot(subset(rminbrickwintermean),  
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Min Rel Humidity - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  jpeg(paste("/agmesh-scenarios/", scen, "/rmaximages", "/winterplot", "rmax", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(rmaxbrickwintermean,   
       col = "gray",
       xlab = "%",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Max Rel Humidity - ", "Winter ", i, sep=""))
  
  #---Add summary statistics to plot
  rmaxhistwintermean <- mean(as.vector(rmaxbrickwintermean))
  rmaxhistmedianwinter <- median(as.vector(rmaxbrickwintermean))
  rmaxhistsdwinter <- sd(as.matrix(rmaxbrickwintermean))
  rmaxhistminwinter <- min(as.vector(rmaxbrickwintermean))
  rmaxhistmaxwinter <- max(as.vector(rmaxbrickwintermean))
  
  
  abline(v=rmaxhistwintermean,lwd=2)
  abline(v=rmaxhistmedianwinter,col = "red",lwd = 2,lty = "dashed")
  #abline(v= rmax2007histsd,col = "blue", lwd = 2,lty = "dashed")
  points(rmaxhistminwinter,100,pch = "*")
  points(rmaxhistmaxwinter,100,pch = "*")
  
  
  plot(subset(rmaxbrickwintermean),  
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Max Rel Humidity - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  jpeg(paste("/agmesh-scenarios/", scen, "/sradimages", "/winterplot", "srad", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(sradbrickwintermean,   
       col = "gray",
       xlab = "w/m2",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Solar Radiation - ", "Winter ", i, sep=""))
  
  #---Add summary statistics to plot
  sradhistwintermean <- mean(as.vector(sradbrickwintermean))
  sradhistmedianwinter <- median(as.vector(sradbrickwintermean))
  sradhistsdwinter <- sd(as.matrix(sradbrickwintermean))
  sradhistminwinter <- min(as.vector(sradbrickwintermean))
  sradhistmaxwinter <- max(as.vector(sradbrickwintermean))
  
  
  abline(v=sradhistwintermean,lwd=2)
  abline(v=sradhistmedianwinter,col = "red",lwd = 2,lty = "dashed")
  #abline(v= srad2007histsd,col = "blue", lwd = 2,lty = "dashed")
  points(sradhistminwinter,100,pch = "*")
  points(sradhistmaxwinter,100,pch = "*")
  
  
  
  plot(subset(sradbrickwintermean),  
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Solar Radiation - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  jpeg(paste("/agmesh-scenarios/", scen, "/vsimages", "/winterplot", "vs", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(vsbrickwintermean,   
       col = "gray",
       xlab = "m-s-1",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Wind Speed - ", "Winter ", i, sep=""))
  
  #---Add summary statistics to plot
  vshistwintermean <- mean(as.vector(vsbrickwintermean))
  vshistmedianwinter <- median(as.vector(vsbrickwintermean))
  vshistsdwinter <- sd(as.matrix(vsbrickwintermean))
  vshistminwinter <- min(as.vector(vsbrickwintermean))
  vshistmaxwinter <- max(as.vector(vsbrickwintermean))
  
  
  abline(v=vshistwintermean,lwd=2)
  abline(v=vshistmedianwinter,col = "red",lwd = 2,lty = "dashed")
  abline(v= vshistsdwinter,col = "blue", lwd = 2,lty = "dashed")
  points(vshistminwinter,100,pch = "*")
  points(vshistmaxwinter,100,pch = "*")
  
  plot(subset(vsbrickwintermean),  
       #col = "gray",
       xlab = "Longitude",
       ylab = "Latitude",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Wind Speed - ", "Spring ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
}
