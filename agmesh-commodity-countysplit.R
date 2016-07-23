#--This code splits all the usda data by county and commodity for machine learning

library(rasterVis)
library(data.table)
library(stringr)
setwd("/agmesh-scenarios/scenario_52177/summaries")
wheat <- read.csv("2001_2015_usda_gridmet_WA", strip.white=TRUE)
setwd("/agmesh-scenarios/scenario_52177/commodity_county/")
uniquecounty <- unique(wheat$county)
commodityunique <- unique(wheat$commodity)
#lapply(uniquecounty, function(funct){write.csv(wheat$county[[funct]], file = paste("wheat_", funct, ".csv", sep=""))})


for (i in uniquecounty) {
  for (j in commodityunique) {
  x <- subset(wheat, county == i & commodity == j)
  write.csv(x, file = paste(i, "_", j, ".csv", sep=""))
  
  }
}
  

