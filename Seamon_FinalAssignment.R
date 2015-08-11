#------------------------------------------------------------------------#
# TITLE:        Seamon_FinalAssignment.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         December 1st. 2014
#
# STAGE:        Fully assembled script with all four stages.
#
# COMMENTS:     This is the agmesh assembled script stages 1-4.  Agmesh 
#               uses weather parameters for the inland pacific northwest  
#               to calculate climate parameter related outputs. 
#                
#               More on the agmesh design: reacchapp.nkn.uidaho.edu
#
#               NOTE:  this script runs against data loaded on 
#               reacchapp.nkn.uidaho.edu.  The best way to run this script
#               is natively using the rstudio server at th following url:
#               
#               reacchapp.nkn.uidaho.edu:8787.  
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

#memory.size(10000)

#--SECTION 1: Load dataset, remove missing values, and extract data---- #

#-----loading shapefiles for study region - tri-state shapefile
#-----and the crop yield point data for 2011-2013------#

setwd("/climatevariables") 

tristate <- readShapePoly('tristate.shp', 
                proj4string=CRS
                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

cropyield <- readShapePoints('REACCHcropyield_allyears.shp', 
                proj4string=CRS
                ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
cropyield_df <- data.frame(cropyield)
cropyield_df[cropyield_df==0] <- NA
cropyield_df_xyobs <- subset(cropyield_df, select = c(coords.x1, coords.x2))

#----Setting vectors for loops for years, variables, day, and rasters---#
#----associated with input datasets 2007-2011------#

#----input ranges of years to examine----------#
setwd("/reacchspace/obj1/netcdf/METREACCH/") 

rm(list = ls()) #--clears all lists------#

N1 <- readline("enter first year of data range: ")
N2 <- readline("enter last year of data range: ")
LAT1 <- readline("enter Lat min: ")
LAT2 <- readline("enter Lat max: ")
LON1 <- readline("enter Lon min: ")
LON2 <- readline("enter Lon max: ")

yearspan <- c(N1:N2)
#yearspan <- c(2007,2008,2009,2010,2011)
variablespan <- c("pdur", "vs", "srad", 
                  "rmin", "rmax", "tmmx", "tmmn")
rasterspan <- c("raster", "matrix", "raster_transposed", 
                "matrix_mean", "matrix_median", "matrix_sd", 
                "matrix_min", "matrix_max")
dayspan <- c(1:365)

ncolz <- c(1:109)
nrowz <- c(1:140)

#----writes subset variables to file for shell script operations--#

fileConn<-file("/tmp/agmesh-subset-vartmp.txt", "w")
writeLines(c(paste('yearstart=', N1, sep='')), fileConn)
writeLines(c(paste('yearend=', N2, sep='')), fileConn)
writeLines(c(paste('lat1=', LAT1, sep='')), fileConn)
writeLines(c(paste('lat2=', LAT2, sep='')), fileConn)
writeLines(c(paste('lon1=', LON1, sep='')), fileConn)
writeLines(c(paste('lon2=', LON2, sep='')), fileConn)
close(fileConn)

#write.table(data.frame(ID,N1,N2,LAT1,LAT2,LON1,LON2), 
#"/tmp/agmesh-subset-vartmp.txt", sep="\t")
system("/agmesh-code/agmesh-subset.sh")
#system("rm /tmp/agmesh-subset-vartmp.txt")

#---Loop below that loads netcdf data files for each year, for each 
#---of the six variables ( 1) tmmx - max temp, 2) tmmn - min temp, 
#---3) rmin - min rel humidity, 4) max rel humidity, 5) srad - solar  
#---radiation, 6) vs - wind speed) needed to calculate evapotranspitation.  
#---This loop creates a raster brick for each variable, per year.  
#---For example: tmmx2007brick - raster brick for all max temp daily data.  
#---365 layers (days) across the geographic region.

print("Generating raster arrays for analysis...")

#----SECTION 2: Load data and Derive Variables-------#
#yearspan=c(2000)
#variablespan=c("pdur")
for (i in yearspan) {
  for (j in variablespan) {
    
    #-----loads netcdfs one by one-------------------#
    
  dirname <- paste("/reacchspace/obj1/netcdf/METREACCH/", j, sep = "")
  aggmetnc <- file.path(paste("/reacchspace/obj1/netcdf/METREACCH/", 
                              j, sep = ""))
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
    assign(paste("aggmet", i, "_", j, "_", "raster", sep = ""),
           aggmet_raster)
    assign(paste("aggmet", i, "_", j, "_", "matrix",  sep = ""),
           aggmet_matrix)
    assign(paste("aggmet", i, "_", j, "_", "raster_transposed", 
                 sep = ""),aggmet_raster_transposed)
    assign(paste("aggmet", i, "_", j, "_", "matrix_mean", sep = ""),
           aggmet_matrix_mean)
    assign(paste("aggmet", i, "_", j, "_", "matrix_median", sep = ""),
           aggmet_matrix_median)
    assign(paste("aggmet", i, "_", j, "_", "matrix_sd", sep = ""),
           aggmet_matrix_sd)
    assign(paste("aggmet", i, "_", j, "_", "matrix_min", sep = ""),
           aggmet_matrix_min)
    assign(paste("aggmet", i, "_", j, "_", "matrix_max", sep = ""),
           aggmet_matrix_max)
  }
}

print("Converting temperature arrays from Kelvin to Celsius...")


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



#-----------------------------------------------------------------#
# TITLE:        agmesh_analysis_ET.R
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
# STAGE:        Raster Generation - Stage 2
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


#-------Prepping for Raster calculations for evapotranspiration---------#

print("Generating arrays for evapotranspiration calculation ...")

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
print(paste("Looping through yearly variable preparation for:", i))
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


#---------------------------------------------------------------------#
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
# STAGE:        Agmesh Calculation for ET - Stage 3
#
# COMMENTS:     This is the agmesh setup - stage 3.  Agmesh uses weather 
#               parameters for the inland pacific northwest to calculate  
#               climate parameter related outputs. Stage 3 uses 
#               data created in stages 1-2 to calculate ET.
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


setwd("/climatevariables") 

#tristate <- readShapePoly('tristate.shp', 
#             proj4string=CRS
#             ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

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


setwd("/reacchspace/obj1/netcdf/METREACCH/") 


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
  
  #----creating vector of growth days for spring wheat and winter wheat--#
  
  erichetspring <- erichet[[1:100]]
  erichetwinter <- erichet[[210:310]]
  erichetspring_xyobs <- extract(erichetspring, cropyield_df_xyobs)
  erichetwinter_xyobs <- extract(erichetwinter, cropyield_df_xyobs)
  erichetspring_mean <- rowMeans(erichetspring_xyobs)
  erichetspring_mean <- data.frame(erichetspring_mean)
  erichetwinter_mean <- rowMeans(erichetwinter_xyobs)
  erichetwinter_mean <- data.frame(erichetwinter_mean)
  
  for (j in season) {
    erichcrop_mean <- get(paste("cropyield_df_yieldobs", i, j, sep=""))
    erichcrop_mean <- as.numeric(unlist(erichcrop_mean))
    erichcrop_mean <- mean(erichcrop_mean)
    assign(paste("cropyield_df_yieldobs_mean", i, j, sep=""), erichcrop_mean)
    erichcrop_sd <- get(paste("cropyield_df_yieldobs", i, j, sep=""))
    erichcrop_sd <- as.numeric(unlist(erichcrop_sd))
    erichcrop_sd <- sd(erichcrop_sd)             
    assign(paste("cropyield_df_yieldobs_sd", i, j, sep=""), erichcrop_sd)
    
  }  
  assign(paste("erichet", i, sep=""), erichet)
  assign(paste("erichet", i, "spring", sep=""), erichetspring)
  assign(paste("erichet", i, "winter", sep=""), erichetwinter)
  assign(paste("erichet", i, "spring", "_xyobs", sep=""), erichetspring_xyobs)
  assign(paste("erichet", i, "winter", "_xyobs", sep=""), erichetwinter_xyobs)
  assign(paste("erichet", i, "spring", "_mean", sep=""), erichetspring_mean)
  assign(paste("erichet", i, "winter", "_mean", sep=""), erichetwinter_mean)
  
  ET_mean <- calc(erichet, mean)
  assign(paste("erichet", i, "brick", "mean", sep=""), ET_mean)
  ET_matrix <- as.matrix(ET_mean)
  assign(paste("ET", i, "brick", "matrix", sep=""), ET_matrix)
  
}

#---zscore calculation for et - for year and season datasets---#

for (i in yearspan) {
  for (l in season) {
    zscore <- data.frame(data=NaN, 1:22)
    assign(paste("erichet", i, l, "zscore", sep=""), zscore)
    zscore <- subset(zscore, select = c(data))
    zscoremean1 <- get(paste("erichet", i, l, "_mean", sep=""))
    zscoremean1 <- as.numeric(unlist(zscoremean1))
    zscoremean <- mean(zscoremean1)
    zscoremean1 <- data.frame(zscoremean1)
    zscoresd <- get(paste("erichet", i, l, "_mean", sep=""))
    zscoresd <- as.numeric(unlist(zscoresd))
    zscoresd <- sd(zscoresd)
    for (m in obs) {
      zscore[m,] <- (zscoremean1[m,] - zscoremean) / zscoresd
      assign(paste("erichet", i, l, "zscore", sep=""), zscore)  
    }
  }
}   

for (i in yearspan) {
  for (l in season) {
  cropzscore <- data.frame(data=NaN, 1:22)
  assign(paste("erichyield", i, l, "zscore", sep=""), cropzscore)
  cropzscore <- subset(cropzscore, select = c(data))
  cropzscoreall <- get(paste("cropyield_df_yieldobs", i, j, sep=""))
  cropzscoreall1 <- as.numeric(unlist(cropzscoreall))
  #cropzscoreall1 <- data.frame(cropzscoreall1)
  assign(paste("cropyield_df_yieldobs2", i, j, sep=""), cropzscoreall1)
  cropzscoremean1 <- get(paste("cropyield_df_yieldobs_mean", 
                               i, j, sep=""))
  cropzscoremean1 <- as.numeric(unlist(cropzscoremean1))
  #cropzscoremean <- mean(cropzscoremean1)
  cropzscoremean1 <- data.frame(cropzscoremean1)
  cropzscoresd <- get(paste("erichet", i, l, "_mean", sep=""))
    cropzscoresd <- as.numeric(unlist(cropzscoresd))
    cropzscoresd <- sd(cropzscoresd)
  for (m in obs) {
    cropzscore[m,] <- (cropzscoreall[m,] - cropzscoremean1) / cropzscoresd
    assign(paste("erichyield", i, l, "zscore", sep=""), cropzscore)
      
    }
  }
}

#---loop to combine et & crop yield into matrices for cor analysis--#

for (i in yearspan) {
  for (l in season) {
    cropcombo <- get(paste("cropyield_df_yieldobs", i, l, sep=""))
    etcombo <- get(paste("erichet", i, l, "_mean", sep=""))
    et_crop_combo <- cbind(cropcombo, etcombo)
    et_crop_combo <- data.matrix(et_crop_combo)
    assign(paste("etcrop", i, l, sep=""), et_crop_combo)
  }}

#---anova preparation------#

for (i in yearspan) {
  for (l in season) {
    aovcombo <- get(paste("erichet", i, l, "_xyobs", sep=""))
    yieldcombo <- get(paste("cropyield_df_yieldobs", i, l, sep=""))
    aovcombo2 <- cbind(aovcombo, yieldcombo)
    aovcombo2 <- data.matrix(aovcombo2)
    aovcombo3 <- rep(i,nrow(aovcombo2)) # make new column 
    aovcombo3 <- data.frame(aovcombo3)
    aovcombo2 <- cbind(aovcombo2, aovcombo3)
    assign(paste("aov", i, l, sep=""), aovcombo2)
    aovcombo4 <- rep(l,nrow(aovcombo2)) # make new column 
    aovcombo4 <- data.frame(aovcombo4)
    aovcombo2 <- cbind(aovcombo2, aovcombo4)
    aovcombo2 <- data.matrix(aovcombo2)
    #aovcombo2 <- t(aovcombo2)
    #assign(paste("aov", i, l, sep=""), aovcombo2)
    #aovcombo2stack <- stack(aovcombo2)
    #aovcombo2stack <- rep(i,nrow(aovcombo2stack))
    meltaov <- melt(aovcombo)
    assign(paste("aov", i, l, sep=""), meltaov)
    
  }}

#i<-1 
#while(i < 100) { 
#cropyield_df_yieldobs2012winter <- rbind(cropyield_df_yieldobs2012winter,
# cropyield_df_yieldobs2012winter) 
#  i <- i + 1
#} 


#for (i in 100) {
#  i <- i + 1
#sss <- rbind(cropyield_df_yieldobs2012winter, 
#cropyield_df_yieldobs2012winter)
#}


#----------------------------------------------------------------------#
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
# STAGE:        Plotting - Stage 4
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

#------SECTION 3 - Plot Univariate and BiVariate Data ------#

#------Plotting - ET with climate variables ------#

#win.graph(15,15)

# dev.off()

lyt = c(1, 2, 3, 4, 5, 6,
        7, 7, 7, 7, 7, 7, 
        7, 7, 7, 7, 7, 7)

lytmtx = matrix(lyt,nrow=3,ncol=6,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' function

tmmxbrickmean2012 <- get(paste("tmmx", 2012, "brickmean", sep=""))
tmmnbrickmean2012 <- get(paste("tmmn", 2012, "brickmean", sep=""))
rminbrickmean2012 <- get(paste("rmin", 2012, "brickmean", sep=""))
rmaxbrickmean2012 <- get(paste("rmax", 2012, "brickmean", sep=""))
sradbrickmean2012 <- get(paste("srad", 2012, "brickmean", sep=""))
vsbrickmean2012 <- get(paste("vs", 2012, "brickmean", sep=""))      

tmmxbrickmean2013 <- get(paste("tmmx", 2013, "brickmean", sep=""))
tmmnbrickmean2013 <- get(paste("tmmn", 2013, "brickmean", sep=""))
rminbrickmean2013 <- get(paste("rmin", 2013, "brickmean", sep=""))
rmaxbrickmean2013 <- get(paste("rmax", 2013, "brickmean", sep=""))
sradbrickmean2013 <- get(paste("srad", 2013, "brickmean", sep=""))
vsbrickmean2013 <- get(paste("vs", 2013, "brickmean", sep="")) 

cat ("Press [enter] to see 2012 climatic variables and mean evapotranspiration")
line <- readline()



hist(tmmxbrickmean2012,   
     col = "gray",
     xlab = "max air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2012 Max Temp")

#-----Add summary statistics to plot
tmmxhistmean <- mean(as.vector(tmmxbrickmean2012))
tmmxhistmedian <- median(as.vector(tmmxbrickmean2012))
tmmxhistsd <- sd(as.vector(tmmxbrickmean2012))
tmmxhistmin <- min(as.vector(tmmxbrickmean2012))
tmmxhistmax <- max(as.vector(tmmxbrickmean2012))

abline(v=tmmxhistmean,lwd=2)
abline(v=tmmxhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmx2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmxhistmin,100,pch = "*")
points(tmmxhistmax,100,pch = "*")

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

hist(tmmnbrickmean2012,   
     col = "gray",
     xlab = "min air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2012 Min Air Temp")

#---Add summary statistics to plot

tmmnhistmean <- mean(as.vector(tmmnbrickmean2012))
tmmnhistmedian <- median(as.vector(tmmnbrickmean2012))
tmmnhistsd <- sd(as.vector(tmmnbrickmean2012))
tmmnhistmin <- min(as.vector(tmmnbrickmean2012))
tmmnhistmax <- max(as.vector(tmmnbrickmean2012))


abline(v=tmmnhistmean,lwd=2)
abline(v=tmmnhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmn2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmnhistmin,100,pch = "*")
points(tmmnhistmax,100,pch = "*")

hist(rminbrickmean2012,   
     col = "gray",
     xlab = "min relative humidity (%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2012 Min Rel Humidity")

#---Add summary statistics to plot
rminhistmean <- mean(as.vector(rminbrickmean2012))
rminhistmedian <- median(as.vector(rminbrickmean2012))
rminhistsd <- sd(as.matrix(rminbrickmean2012))
rminhistmin <- min(as.vector(rminbrickmean2012))
rminhistmax <- max(as.vector(rminbrickmean2012))


abline(v=rminhistmean,lwd=2)
abline(v=rminhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmin2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rminhistmin,100,pch = "*")
points(rminhistmax,100,pch = "*")

hist(rmaxbrickmean2012,   
     col = "gray",
     xlab = "max relative humidity(%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2012 Max Rel Humidity")

#---Add summary statistics to plot
rmaxhistmean <- mean(as.vector(rmaxbrickmean2012))
rmaxhistmedian <- median(as.vector(rmaxbrickmean2012))
rmaxhistsd <- sd(as.matrix(rmaxbrickmean2012))
rmaxhistmin <- min(as.vector(rmaxbrickmean2012))
rmaxhistmax <- max(as.vector(rmaxbrickmean2012))


abline(v=rmaxhistmean,lwd=2)
abline(v=rmaxhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmax2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rmaxhistmin,100,pch = "*")
points(rmaxhistmax,100,pch = "*")

hist(sradbrickmean2012,   
     col = "gray",
     xlab = "solar radiation (w/m2)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2012 Solar Radiation")

#---Add summary statistics to plot
sradhistmean <- mean(as.vector(sradbrickmean2012))
sradhistmedian <- median(as.vector(sradbrickmean2012))
sradhistsd <- sd(as.matrix(sradbrickmean2012))
sradhistmin <- min(as.vector(sradbrickmean2012))
sradhistmax <- max(as.vector(sradbrickmean2012))


abline(v=sradhistmean,lwd=2)
abline(v=sradhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= srad2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(sradhistmin,100,pch = "*")
points(sradhistmax,100,pch = "*")

hist(vsbrickmean2012,   
     col = "gray",
     xlab = "Wind Speed Frequency (m-s-1)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2012 Wind Speed")

#---Add summary statistics to plot
vshistmean <- mean(as.vector(vsbrickmean2012))
vshistmedian <- median(as.vector(vsbrickmean2012))
vshistsd <- sd(as.matrix(vsbrickmean2012))
vshistmin <- min(as.vector(vsbrickmean2012))
vshistmax <- max(as.vector(vsbrickmean2012))


abline(v=vshistmean,lwd=2)
abline(v=vshistmedian,col = "red",lwd = 2,lty = "dashed")
abline(v= vshistsd,col = "blue", lwd = 2,lty = "dashed")
points(vshistmin,100,pch = "*")
points(vshistmax,100,pch = "*")

#--histograms of mean et for 2012 and 2013--#

#----ET plotting---#

erichet2012brickmean2 <- as.matrix(erichet2012brickmean)
erichet2012brickmean2 <- na.omit(erichet2012brickmean2)

mu = mean(erichet2012brickmean2, na.rm=TRUE)
sigma = sd(erichet2012brickmean2, na.rm=TRUE)

fit = fitdistr(erichet2012brickmean2, "normal", nan.rm=TRUE) 
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(erichet2012brickmean2),muHat,sigmaHat)


hist(erichet2012brickmean2, freq=FALSE, 
     ylab = "Frequency of Evapotranspiration (mm/d-1)",
     ylim = c(0,.25),
     xlim = c(10,26),
     xlab = "Evapotranspiration (mm/d-1)",
     notch = FALSE,
     main = "2012 Mean Daily Distributions of ET")

#lines(density(erichet2013brickmean, col="blue", lwd=2))
lines(sort(erichet2012brickmean2),nHat,lwd=2)
text(min(erichet2012brickmean2),max(nHat), pos = 1,
     paste("Fit: mu = ",round(muHat*100)/100))
text(min(erichet2012brickmean2),0.9*max(nHat), pos = 1,
     paste("sigma = ", round(sigmaHat*100)/100))

cat ("Press [enter] to see 2013 climatic variables and mean evapotranspiration")
line <- readline()


hist(tmmxbrickmean2013,   
     col = "gray",
     xlab = "max air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Max Temp")

#-----Add summary statistics to plot
tmmxhistmean <- mean(as.vector(tmmxbrickmean2013))
tmmxhistmedian <- median(as.vector(tmmxbrickmean2013))
tmmxhistsd <- sd(as.vector(tmmxbrickmean2013))
tmmxhistmin <- min(as.vector(tmmxbrickmean2013))
tmmxhistmax <- max(as.vector(tmmxbrickmean2013))

abline(v=tmmxhistmean,lwd=2)
abline(v=tmmxhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmx2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmxhistmin,100,pch = "*")
points(tmmxhistmax,100,pch = "*")

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

hist(tmmnbrickmean2013,   
     col = "gray",
     xlab = "min air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Min Air Temp")

#---Add summary statistics to plot

tmmnhistmean <- mean(as.vector(tmmnbrickmean2013))
tmmnhistmedian <- median(as.vector(tmmnbrickmean2013))
tmmnhistsd <- sd(as.vector(tmmnbrickmean2013))
tmmnhistmin <- min(as.vector(tmmnbrickmean2013))
tmmnhistmax <- max(as.vector(tmmnbrickmean2013))


abline(v=tmmnhistmean,lwd=2)
abline(v=tmmnhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmn2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmnhistmin,100,pch = "*")
points(tmmnhistmax,100,pch = "*")

hist(rminbrickmean2013,   
     col = "gray",
     xlab = "min relative humidity (%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Min Rel Humidity")

#---Add summary statistics to plot
rminhistmean <- mean(as.vector(rminbrickmean2013))
rminhistmedian <- median(as.vector(rminbrickmean2013))
rminhistsd <- sd(as.matrix(rminbrickmean2013))
rminhistmin <- min(as.vector(rminbrickmean2013))
rminhistmax <- max(as.vector(rminbrickmean2013))


abline(v=rminhistmean,lwd=2)
abline(v=rminhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmin2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rminhistmin,100,pch = "*")
points(rminhistmax,100,pch = "*")

hist(rmaxbrickmean2013,   
     col = "gray",
     xlab = "max relative humidity(%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Max Rel Humidity")

#---Add summary statistics to plot
rmaxhistmean <- mean(as.vector(rmaxbrickmean2013))
rmaxhistmedian <- median(as.vector(rmaxbrickmean2013))
rmaxhistsd <- sd(as.matrix(rmaxbrickmean2013))
rmaxhistmin <- min(as.vector(rmaxbrickmean2013))
rmaxhistmax <- max(as.vector(rmaxbrickmean2013))


abline(v=rmaxhistmean,lwd=2)
abline(v=rmaxhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmax2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rmaxhistmin,100,pch = "*")
points(rmaxhistmax,100,pch = "*")

hist(sradbrickmean2013,   
     col = "gray",
     xlab = "solar radiation (w/m2)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Solar Radiation")

#---Add summary statistics to plot
sradhistmean <- mean(as.vector(sradbrickmean2013))
sradhistmedian <- median(as.vector(sradbrickmean2013))
sradhistsd <- sd(as.matrix(sradbrickmean2013))
sradhistmin <- min(as.vector(sradbrickmean2013))
sradhistmax <- max(as.vector(sradbrickmean2013))


abline(v=sradhistmean,lwd=2)
abline(v=sradhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= srad2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(sradhistmin,100,pch = "*")
points(sradhistmax,100,pch = "*")

hist(vsbrickmean2013,   
     col = "gray",
     xlab = "Wind Speed Frequency (m-s-1)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Wind Speed")

#---Add summary statistics to plot
vshistmean <- mean(as.vector(vsbrickmean2013))
vshistmedian <- median(as.vector(vsbrickmean2013))
vshistsd <- sd(as.matrix(vsbrickmean2013))
vshistmin <- min(as.vector(vsbrickmean2013))
vshistmax <- max(as.vector(vsbrickmean2013))


abline(v=vshistmean,lwd=2)
abline(v=vshistmedian,col = "red",lwd = 2,lty = "dashed")
abline(v= vshistsd,col = "blue", lwd = 2,lty = "dashed")
points(vshistmin,100,pch = "*")
points(vshistmax,100,pch = "*")



#----ET plotting---#

erichet2013brickmean2 <- as.matrix(erichet2013brickmean)

mu = mean(erichet2013brickmean2)
sigma = sd(erichet2013brickmean2)

fit = fitdistr(erichet2013brickmean2,"normal") 
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(erichet2013brickmean2),muHat,sigmaHat)


hist(erichet2013brickmean, freq=FALSE, 
     ylab = "Frequency of Evapotranspiration (mm/d-1)",
     ylim = c(0,.25),
     xlim = c(10,26),
     xlab = "Evapotranspiration (mm/d-1)",
     notch = FALSE,
     main = "2013 Mean Daily Distributions of ET")

#lines(density(erichet2013brickmean, col="blue", lwd=2))
lines(sort(erichet2013brickmean2),nHat,lwd=2)
text(min(erichet2013brickmean2),max(nHat), pos = 4,
     paste("Fit: mu = ",round(muHat*100)/100))
text(min(erichet2013brickmean2),0.9*max(nHat), pos = 4,
     paste("sigma = ", round(sigmaHat*100)/100))
cat ("Press [enter] to see April 1, Aug 1 and Dec 1 compared in 2012")
line <- readline()

#---comparing daily ET distributions for April 1st, Aug 1st, 
#---and Dec 1st, 2012

#---Creates the window size--------------------#
#win.graph(3.5,8) 
#---par function sets number or rows and columns for plotting---#
par(mfrow=c(3,1))  


plot(subset(erichet2012, 90,   
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,240),
            #xlim = c(-0.5,6),
            main = "ET - April 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(erichet2012, 210,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - August 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(erichet2012, 315,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - Dec 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)


cat ("Press [enter] to see April 1, Aug 1 and Dec 1 compared in 2013")
line <- readline()

#---comparing daily ET distributions for April 1st, Aug 1st, 
#---and Dec 1st, 2013

#---Creates the window size--------------------#
#win.graph(3.5,8) 
#---par function sets number or rows and columns for plotting---#
par(mfrow=c(3,1)) 

setwd("/climatevariables") 
tristate <- readShapePoly('tristate.shp', 
  proj4string=CRS
  ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
setwd("/reacchspace/obj1/netcdf/METREACCH/") 



plot(subset(erichet2013, 90,   
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,240),
            #xlim = c(-0.5,6),
            main = "ET - April 1st 2013"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(erichet2013, 210,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - August 1st 2013"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(erichet2013, 315,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - Dec 1st 2013"))

plot(tristate, bg="transparent", add=TRUE)


cat ("Press [enter] to see April 1 examined by boxplot/map/histogram")
line <- readline()

#---------Comparing ET - April 1st for 2012 and 2013

#---Creates the window size---------------#
#win.graph(11,7) 
#---par function sets number or rows and columns for plotting--#
par(mfrow=c(1,3))  


boxplot(erichet2012$layer.90,
        ylim = c(2,25),
        col="aquamarine3",
        xlab="April1 ET 2007",
        ylab="ET Distribution (mm/d-1)")

plot(erichet2012$layer.90,
     xlab="April1 ET 2007",
     ylab="ET Distribution (mm/d-1)")

hist(erichet2012$layer.90,
     col="aquamarine3",
     xlab="April1 ET 2007",
     ylab="ET Distribution (mm/d-1)")


cat ("Press [enter] to see April 1 examined by boxplot/map/histogram")
line <- readline()

par(mfrow=c(1,3))

boxplot(erichet2013$layer.90,
        ylim = c(15,22),
        col="aquamarine3",
        xlab="April1 ET 2008",
        ylab="ET Distribution (mm/d-1)")

plot(erichet2013$layer.90,
     xlab="April1 ET 2007",
     ylab="ET Distribution (mm/d-1)")

hist(erichet2013$layer.90,
     col="aquamarine3",
     xlab="April1 ET 2007",
     ylab="ET Distribution (mm/d-1)")




#boxplot(erichet2009$layer.90,
#        ylim = c(2,25),
#        col="aquamarine3",
#        xlab="April1 ET 2009",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2010$layer.90,
#        ylim = c(2,25),
#        col="aquamarine3",
#        xlab="April1 ET 2010",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2011$layer.90,
#        ylim = c(2,25),
#        col="aquamarine3",
#        xlab="April1 ET 2011",
#        ylab="ET Distribution (mm/d-1)")

cat ("Press [enter] to see August 1st compared for 2012 and 2013")
line <- readline()

#---------Comparing ET - August 1st for 2012 and 2013

#---Creates the window size-------------------#
#win.graph(11,7) 
#---par function sets number or rows and columns for plotting--#
par(mfrow=c(1,5))  


boxplot(erichet2012$layer.210,
        ylim = c(10,60),
        col="aquamarine3",
        xlab="Aug1 ET 2007",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2013$layer.210,
        ylim = c(10,60),
        col="aquamarine3",
        xlab="Aug1 ET 2008",
        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2009$layer.210,
#        ylim = c(10,60),
#        col="aquamarine3",
#        xlab="Aug1 ET 2009",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2010$layer.210,
#        ylim = c(10,60),
#        col="aquamarine3",
#        xlab="Aug1 ET 2010",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2011$layer.210,
#        ylim = c(10,60),
#        col="aquamarine3",
#        xlab="Aug1 ET 2011",
#        ylab="ET Distribution (mm/d-1)")

cat ("Press [enter] to see December 1st compared for 2012 and 2013")
line <- readline()

#---------Comparing ET - December 1st for 2012 and 2013

#---Creates the window size-----#
#win.graph(11,7) 
#---par function sets number or rows and columns for plotting----#
par(mfrow=c(1,5))  

boxplot(erichet2012$layer.335,
        ylim = c(0,15),
        col="aquamarine3",
        xlab="Dec1 ET 2007",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2013$layer.335,
        ylim = c(0,15),
        col="aquamarine3",
        xlab="Dec1 ET 2008",
        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2009$layer.335,
#        ylim = c(0,15),
#        col="aquamarine3",
#        xlab="Dec1 ET 2009",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2010$layer.335,
#        ylim = c(0,15),
#        col="aquamarine3",
#        xlab="Dec1 ET 2010",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2011$layer.335,
#        ylim = c(0,15),
#        col="aquamarine3",
#        xlab="Dec1 ET 2011",
#        ylab="ET Distribution (mm/d-1)")


cat ("Press [enter] see histograms of crop yield data for 2012 and 2013")
line <- readline()

#------crop yield plots-----#
#win.graph(11,7) 
#---par function sets number or rows and columns for plotting----#
par(mfrow=c(2,2))  

hist(cropyield_df[,8],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Winter Wheat 2012 Yield")

hist(cropyield_df[,9],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Winter Wheat 2013 Yield")



hist(cropyield_df[,11],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Spring Wheat 2012 Yield")

hist(cropyield_df[,12],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Spring Wheat 2013 Yield")
#------ET Correlations

cat ("Press [enter] for gof analysis of crop yield 2012 winter")
line <- readline()

#--plots et vs crop yield, 2012 winter-#


etcrop2012winter_varcrop = etcrop2012winter[,1]
etcrop2012winter_varet = etcrop2012winter[,2]
etcrop2012spring_varcrop = etcrop2012spring[,1]
etcrop2012spring_varet = etcrop2012spring[,2]

etcrop2013winter_varcrop = etcrop2013winter[,1]
etcrop2013winter_varet = etcrop2013winter[,2]
etcrop2013spring_varcrop = etcrop2013spring[,1]
etcrop2013spring_varet = etcrop2013spring[,2]


#---setup for goodnesss of fit analysis of crop yield 2012 winter--#

mu = mean(etcrop2012winter_varcrop)
sigma = sd(etcrop2012winter_varcrop)
#mu = 100  # Set the mean.
#sigma = 2 # Set the standard deviation.

# Use rnorm function to generate random numbers.
#randNorm = rnorm(n, mu, sigma) 

muExp = mean(etcrop2012winter_varcrop)
#muExp = 10  # Set the mean.
#randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
#randWbl = sort(rweibull(n = n, scale = b,shape = c))  

lambda = mean(etcrop2012winter_varcrop)
#lambda = 10 # Set the mean. 
#randPois = sort(rpois(n = n, lambda = lambda)) 

#----normal distribution gof---#

fit = fitdistr(etcrop2012winter_varcrop,"normal") 
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(etcrop2012winter_varcrop),muHat,sigmaHat)
#win.graph()
hist(etcrop2012winter_varcrop, breaks=50, freq = FALSE, 
     col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "2012 winter crop yield",
     main = paste("Normal Distribution fit to 2012 winter crop yield\n", 
                  "mu = ", mu, ";\n sigma = ", sigma))
lines(sort(etcrop2012winter_varcrop),nHat,lwd=2)
#text(min(etcrop2012winter_varcrop),max(nHat), pos = 4,
#     paste("Fit: mu = ",round(muHat*100)/100))
#text(min(etcrop2012winter_varcrop),0.9*max(nHat), pos = 4,
#     paste("sigma = ", round(sigmaHat*100)/100))

cat ("Press [enter] for goodnesss of fit analysis of crop yield 2013 winter")
line <- readline()

#---setup for goodnesss of fit analysis of crop yield 2013 winter--#

mu = mean(etcrop2013winter_varcrop)
sigma = sd(etcrop2013winter_varcrop)
#mu = 100  # Set the mean.
#sigma = 2 # Set the standard deviation.

# Use rnorm function to generate random numbers.
#randNorm = rnorm(n, mu, sigma) 

muExp = mean(etcrop2013winter_varcrop)
#muExp = 10  # Set the mean.
#randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
#randWbl = sort(rweibull(n = n, scale = b,shape = c))  

lambda = mean(etcrop2013winter_varcrop)
#lambda = 10 # Set the mean. 
#randPois = sort(rpois(n = n, lambda = lambda)) 

#----normal distribution gof---#

fit = fitdistr(etcrop2013winter_varcrop,"normal") 
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(etcrop2013winter_varcrop),muHat,sigmaHat)
#win.graph()
hist(etcrop2013winter_varcrop, breaks=50, freq = FALSE, 
     col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "2013 winter crop yield",
     main = paste("Normal Distribution fit to 2013 winter crop yield\n", 
                  "mu = ", mu, ";\n sigma = ", sigma))
lines(sort(etcrop2013winter_varcrop),nHat,lwd=2)
#text(min(etcrop2013winter_varcrop),max(nHat), pos = 4,
#     paste("Fit: mu = ",round(muHat*100)/100))
#text(min(etcrop2013winter_varcrop),0.9*max(nHat), pos = 4,
#     paste("sigma = ", round(sigmaHat*100)/100))

cat ("Press [enter] for gof analysis of crop yield 2012 spring")
line <- readline()

#---setup for goodnesss of fit analysis of crop yield 2012 spring--#

mu = mean(etcrop2012spring_varcrop)
sigma = sd(etcrop2012spring_varcrop)
#mu = 100  # Set the mean.
#sigma = 2 # Set the standard deviation.

# Use rnorm function to generate random numbers.
#randNorm = rnorm(n, mu, sigma) 

muExp = mean(etcrop2012spring_varcrop)
#muExp = 10  # Set the mean.
#randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
#randWbl = sort(rweibull(n = n, scale = b,shape = c))  

lambda = mean(etcrop2012spring_varcrop)
#lambda = 10 # Set the mean. 
#randPois = sort(rpois(n = n, lambda = lambda)) 

#----normal distribution gof---#

fit = fitdistr(etcrop2012spring_varcrop,"normal") 
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(etcrop2012spring_varcrop),muHat,sigmaHat)
#win.graph()
hist(etcrop2012spring_varcrop, breaks=50, freq = FALSE, 
     col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "2012 spring crop yield",
     main = paste("Normal Distribution fit to 2012 spring crop yield\n", 
                  "mu = ", mu, ";\n sigma = ", sigma))
lines(sort(etcrop2012spring_varcrop),nHat,lwd=2)
#text(min(etcrop2012spring_varcrop),max(nHat), pos = 4,
#     paste("Fit: mu = ",round(muHat*100)/100))
#text(min(etcrop2012spring_varcrop),0.9*max(nHat), pos = 4,
#     paste("sigma = ", round(sigmaHat*100)/100))

cat ("Press [enter] for gof analysis of crop yield 2013 spring")
line <- readline()

#---setup for goodnesss of fit analysis of crop yield 2013 spring--#

mu = mean(etcrop2013spring_varcrop)
sigma = sd(etcrop2013spring_varcrop)
#mu = 100  # Set the mean.
#sigma = 2 # Set the standard deviation.

# Use rnorm function to generate random numbers.
#randNorm = rnorm(n, mu, sigma) 

muExp = mean(etcrop2013spring_varcrop)
#muExp = 10  # Set the mean.
#randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
#randWbl = sort(rweibull(n = n, scale = b,shape = c))  

lambda = mean(etcrop2013spring_varcrop)
#lambda = 10 # Set the mean. 
#randPois = sort(rpois(n = n, lambda = lambda)) 

#----normal distribution gof---#

fit = fitdistr(etcrop2013spring_varcrop,"normal") 
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(etcrop2013spring_varcrop),muHat,sigmaHat)
#win.graph()
hist(etcrop2013spring_varcrop, breaks=50, freq = FALSE, 
     col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "2013 spring crop yield",
     main = paste("Normal Distribution fit to 2013 spring crop yield\n", 
                  "mu = ", mu, ";\n sigma = ", sigma))
lines(sort(etcrop2013spring_varcrop),nHat,lwd=2)
#text(min(etcrop2013spring_varcrop),max(nHat),
#     paste("Fit: mu = ",round(muHat*100)/100))
#text(min(etcrop2013spring_varcrop),0.9*max(nHat),
#     paste("sigma = ", round(sigmaHat*100)/100))


cat ("Press [enter] see bivariate data of ET vs. crop yield - raw data")
line <- readline()


newplot <- lm(etcrop2012winter[,2] ~ etcrop2012winter[,1])
plot(etcrop2012winter[,1], etcrop2012winter[,2])
abline(newplot)

#--plots et vs crop yield, 2013 winter-#

newplot <- lm(etcrop2013winter[,2] ~ etcrop2013winter[,1])
plot(etcrop2013winter[,1], etcrop2013winter[,2])
abline(newplot)

#--plots et vs crop yield, 2012 winter-#

newplot <- lm(etcrop2012spring[,2] ~ etcrop2012spring[,1])
plot(etcrop2012spring[,1], etcrop2012spring[,2])
abline(newplot)

#--plots et vs crop yield, 2013 winter-#

newplot <- lm(etcrop2013spring[,2] ~ etcrop2013spring[,1])
plot(etcrop2013spring[,1], etcrop2013spring[,2])
abline(newplot)

cat ("Press [enter] to see zscores of ET vs crop yield for 2012 and 2013")
line <- readline()

#---zscore plots

newplot <- lm(erichet2012springzscore$data ~ 
                erichyield2012springzscore$data)
plot(erichet2012springzscore$data, erichyield2012springzscore$data,
     
     xlab ="2012 Spring ET Zscore",
     ylab ="2012 Spring Crop Yield Zscore",
     main = paste("2012 Spring ET vs Crop Yield - Zscore comparison\n 
                  p-value: 0.09434"))
abline(0,1)

newplot <- lm(erichet2013springzscore$data ~ 
                erichyield2013springzscore$data)
plot(erichet2013springzscore$data, erichyield2013springzscore$data,
     xlab ="2013 Spring ET Zscore",
     ylab ="2013 Spring Crop Yield Zscore",
     main = paste("2013 Spring ET vs Crop Yield - Zscore comparison\n 
                  p-value: 0.00317"))
abline(0,2)

newplot <- lm(erichet2012winterzscore$data ~ 
                erichyield2012winterzscore$data)
plot(erichet2012springzscore$data, erichyield2012springzscore$data,
     xlab ="2012 Winter ET Zscore",
     ylab ="2012 Winter Crop Yield Zscore",
     main = paste("2012 Winter ET vs Crop Yield - Zscore comparison\n 
                  p-value: 0.5495"))
abline(0,1)

newplot <- lm(erichet2013winterzscore$data ~ 
                erichyield2013winterzscore$data)
plot(erichet2013springzscore$data, erichyield2013springzscore$data,
     xlab ="2013 Winter ET Zscore",
     ylab ="2013 Winter Crop Yield Zscore",
     main = paste("2013 Winter ET vs Crop Yield - Zscore comparison\n 
                  p-value: 0.1854"))
abline(0,2)




