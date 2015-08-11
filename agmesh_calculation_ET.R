#------------------------------------------------------------------------#
# TITLE:        agmesh_calculation_ET.R
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

#-------Raster calculations for evapotranspiration ---------#

library("maptools")
library("sirad")
library("reshape")
library("reshape2")
#detach("package:hydroTSM")


season <- c("winter", "spring")


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
#--crop mlra by extent of the scenario area of interest

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

setwd(paste("/agmesh-scenarios/", scen, sep="")) 


for (i in yearspan) {
  
  print(paste("Generating ET for year:", i))
  
  
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
  
  assign(paste("erichet", i, "brick", sep=""), erichet)
  savedstackname <- paste("erichet", i, "brick_", scen[1], sep="")
  savedstack <- assign(paste("erichet", i, "brick_", scen[1], sep=""), erichet)
  fname <- paste("/agmesh-scenarios/", scen[1],  "/ET", i, scen[1], sep="")
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
  
  savedstacknamespring <- paste("erichet", i, "brick_", scen[1], sep="")
  savedstackspring <- assign(paste("erichet", i, "brick_", scen[1], sep=""), erichetspring)
  fnamespring <- paste("/agmesh-scenarios/", scen[1],  "/ETspring", i, scen[1], sep="")
  writeRaster(savedstackspring, filename=fnamespring, bandorder='BIL', overwrite=TRUE)
  
  savedstacknamewinter <- paste("erichet", i, "brick_", scen[1], sep="")
  savedstackwinter <- assign(paste("erichet", i, "brick_", scen[1], sep=""), erichetwinter)
  fnamewinter <- paste("/agmesh-scenarios/", scen[1],  "/ETwinter", i, scen[1], sep="")
  writeRaster(savedstackwinter, filename=fnamewinter, bandorder='BIL', overwrite=TRUE)
  
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
  
  
  #---saving core climate files for each year as a matrix
  #---for statistical analysis using factors of year and mlra
  
  mlrapoints <- rasterToPoints(croppedmlra)
  mlraframe <- as.data.frame(mlrapoints)
  mlrafactor <- as.factor(mlraframe$layer)
  mlracell <- nrow(mlraframe)
  
  combinerows <- ncol * nrow * nlayers
  #combinedallmatrix <- matrix(data=NA,nrow=combinerows,ncol=7) 
  varbrickspanrow <- nrow(tmmxETbrick)
  varbrickspancol <- ncol(tmmxETbrick)
  combinedallmatrixfinal <- c(1,1,1,1,1,1,1,1,1,1,1,1)
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
      combined10 <- as.matrix(c(1:nlayers(tmmnETbrick)))
      combined8 <- mlraframe[mlracell, -3]
      combined7a <- cbind(c(rep(combined7, nlayers(tmmnETbrick))))
      list2 <- cbind(c(rep(i, nlayers(tmmnETbrick))))
      list3 <-cbind(c(rep(mlraframe[j*k,1], nlayers(tmmnETbrick))))
      list4 <-cbind(c(rep(mlraframe[j*k,2], nlayers(tmmnETbrick))))
      combinedallmatrix <- cbind(combined1, combined2, combined3, combined4, combined5, combined6, combined9, list2, combined7a, list3, list4, combined10)
      combinedallmatrixfinal <- rbind2(combinedallmatrixfinal, combinedallmatrix)
    }
  }
  
  assign(paste("climatematrix", i, sep=""), combinedallmatrixfinal)

  
  for (j in 1:nlayers(erichet)) {
    plotz <- subset(erichet, j)
    #plotz <- get(paste("erichet$layer.", j, sep=""))
    jpeg(paste("/agmesh-scenarios/", scen, "/ETtimelapse/", "ET", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
    plot(plotz)
    dev.off()
  }  
  
  #----end 
}

fclimatematrix1 <- c(1,1,1,1,1,1,1,1,1,1,1,1)
for (i in yearspan){
  fclimatematrix2 <- get(paste("climatematrix", i, sep=""))
  fclimatematrix1 <- rbind2(fclimatematrix1, fclimatematrix2)
}

setwd(paste("/agmesh-scenarios/", scen, "/ETtimelapse", sep="")) 
system("ffmpeg -i '%*.jpg' -y -r 30 -q:v 2 timelapse.mp4")

#--copies files to standard scenario folder for display online

fclimatematrix1_original <- fclimatematrix1 

  