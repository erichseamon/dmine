#------------------------------------------------------------------------#
# TITLE:        dmine-wordcloud.R
#
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         December 2015
#
# STAGE:        dmine-soilmoisture.R
#
# COMMENTS:     Soil moisture machine learning model
#                
#
#--Setting the working directory an d clearing the workspace-----------
rm(list=ls())

library(plyr)
library(maptools)
library(sp) 
library(rgdal)
library(data.table)
library(raster)
library(caret)

#--working directory for file outputs and shapefile locations.
#setwd("/git/dmine/circ/dataoutput/")

#TAMU_pnw<-readShapeSpatial("TAMU.pnw.shp", proj4string=CRS("+proj=longlat"))
#plot(TAMU_pnw,col="red", pch=20)

#--working directory for soil moisture readings
#setwd("/nethome/erichs/TAMU_NASDM_Full-2013-12-10/readings/")

#localDir <- "/nethome/erichs/TAMU_NASDM_Full-2013-12-10/readings/"

#-----------------------

setwd("/git/dmine/circ/dataoutput/")
localDir <- "/git/dmine/circ/dataoutput/"

# Show the unzipped files 
list.files(localDir)

# layerName is the name of the unzipped shapefile without file type extensions 
layerName <- "TAMU_pnw"  
layerName2 <- "states"
# Read in the data
data_projected <- readOGR(dsn=localDir, layer=layerName) 
states_projected <- readOGR(dsn=localDir, layer=layerName2)

# What is this thing and what's in it?
class(data_projected)
slotNames(data_projected)

#--plot the data
plot(states_projected)
points(data_projected, col="red", pch=20)


#---organize the readings and associate spatially - and in a format ready for analysis
#---the data itself is structured in *readings.txt files - one each for each observation 
#---location.  The other challenging aspect is that all the files do not have consistent
#---columns.  Some have varying depths of soil moisture measurements - and that results
#---in the need to bind all the data together - and to induce NAN/missing values for cells
#---that have no values.  

#--make sure right number of zeros are in front of stationID
data_projected$StationID <- sprintf("%010d", data_projected$StationID)

#--create a list of unique stationIDs
file_list <- data.frame(unique(data_projected$StationID))
pnwlist <- file_list

#--cross reference between observations and readings files

pnwlist$unique.data_projected.StationID. <- paste(pnwlist$unique.data_projected.StationID., "_readings.txt",  sep = "")
pnwlist <- data.frame(pnwlist)

readinglist <- list.files("/git/dmine/circ/TAMU/readings")
newlist <- setdiff(pnwlist, readinglist)

newlist = newlist[-9,]
newlist = newlist[-11]

#DF[!is.na(DF$y),]




setwd("/git/dmine/circ/TAMU/readings")



#for (i in 1:100){

for (file in newlist){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file[1], header=TRUE, sep="\t")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.table(file[1], header=TRUE, sep="\t", as.is=TRUE)
    dataset<-rbind.fill(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}
#}
#--capitialize stationID column for dataset - which has 238K readings from just pnw observations
colnames(dataset)[1] <- "StationID"

#--do an outer join between spatial observations and table of 238K readings
p <- data.frame(shapefile("/git/dmine/circ/dataoutput/TAMU_pnw.shp"))
m <- merge(p, dataset, by='StationID')

#--subset full readings data to only list depth_10
m2 <- subset(m, select=c(StationID, depth_10))
#-----------------------------------------------------
m2[complete.cases(m2), ]

#--setting up test and train

#--10fold CV for depth_10

require(caTools)
set.seed(101) 
sample = sample.split(m$depth_10, SplitRatio = .90)
train = subset(m, sample == TRUE)
test = subset(m, sample == FALSE)

#--machine learning modeling process

library(caret)



x <- cbind(x_train,y_train)
# Fitting model
fitControl <- trainControl( method = "repeatedcv", number = 4, repeats = 4)
fit <- train(y ~ ., data = x, method = "gbm", trControl = fitControl,verbose = FALSE)
predicted= predict(fit,x_test,type= "prob")[,2] 
