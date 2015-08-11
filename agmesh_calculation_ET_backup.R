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


season <- c("winter", "spring")


setwd("/agmesh-data/shapefiles/") 


tristate <- readShapePoly('tristate.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

cropyield <- readShapePoints('REACCHcropyield_nonulls.shp', 
                             proj4string=CRS
                             ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
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
  tmmnETbrick <- get(paste("tmmn", i, "brick", sep=""))
  rmaxETbrick <- get(paste("rmax", i, "brick", sep=""))
  rminETbrick <- get(paste("rmin", i, "brick", sep=""))
  sradETbrick <- get(paste("srad", i, "brick", sep=""))
  vsETbrick <- get(paste("vs", i, "brick", sep=""))
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
  
  erichetspring <- erichet[[1:100]]
  erichetwinter <- erichet[[210:310]]
  erichetspring_xyobs <- extract(erichetspring, cropyield_df_xyobs)
  erichetwinter_xyobs <- extract(erichetwinter, cropyield_df_xyobs)
  erichetspring_mean <- rowMeans(erichetspring_xyobs)
  erichetspring_mean <- data.frame(erichetspring_mean)
  erichetwinter_mean <- rowMeans(erichetwinter_xyobs)
  erichetwinter_mean <- data.frame(erichetwinter_mean)
  
  
  #---point of work break es02232015 - data above ready for seasonal ET comparison---#
  
  assign(paste("erichet", i, sep=""), erichet)
  assign(paste("erichet", i, "spring", sep=""), erichetspring)
  assign(paste("erichet", i, "winter", sep=""), erichetwinter)
  #assign(paste("erichet", i, "spring", "_xyobs", sep=""), erichetspring_xyobs)
  #assign(paste("erichet", i, "winter", "_xyobs", sep=""), erichetwinter_xyobs)
  assign(paste("erichet", i, "spring", "_mean", sep=""), erichetspring_mean)
  assign(paste("erichet", i, "winter", "_mean", sep=""), erichetwinter_mean)
  
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
}