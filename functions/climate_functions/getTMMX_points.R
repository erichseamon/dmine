#--gets TMMX data from gridmet repo hosted @ USGS
#--the function allows for entering a start date and end date (e.g. 2009-06-0-01)
#--It is currently hard coded to extract data for a particular shapefile,
#--but can be altered to allow for selection of data using differing polygon or point files.

getTMMX_points <- function(startDate, endDate){
  
  library(dplyr)
  library(tidyr)
  library(geoknife) #order matters because 'query' is masked by a function in dplyr
  library(RColorBrewer)
  library(maps) 

  #scen_state = c("Idaho", "Washington")
  scen_state = paste(states,sep="", collapse="|")
  
  setwd("/dmine/data/norwest/")
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  streamtemp <- readShapePoints('NorWeST_ObservedTempPoints_WACoast_wgs84/NorWeST_ObservedTempPoints_WACoast_wgs84.shp', proj4string = projection)
  counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
  
  xystream <- data.frame(t(data.frame(streamtemp@coords)))
  rownames(xystream) <- c("longitude", "latitude")
  stencil <- simplegeom(xystream)
             
  fabric <- webdata(list(url = 'https://cida.usgs.gov/thredds/dodsC/UofIMETDATA', variables = "max_air_temperature", 
            times = c('startDate','endDate')))
  
  job <- geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
  check(job)
  tmmxData_result <- result(job, with.units=TRUE)
  tmmxData_result_frame <- data.frame(colMeans(tmmxData_result[sapply(tmmxData_result, is.numeric)]))
  colnames(tmmxData_result_frame) <- c("max_air_temperature")
  streamtemp_tmmx_combined <- cbind(streamtemp, tmmxData_result_frame$max_air_temperature)
  colnames(streamtemp_combined[,8]) <- "tmmx_gridmet"
  streamtemp_combined_frame <- data.frame(streamtemp_tmmx_combined)
  
  return(streamtemp_tmmx_combined)
  
}