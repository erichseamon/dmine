library(maps)

#ADD variable span
for (jj in varspan)

getPrecip <- function(states, startDate, endDate){
  
  #scen_state = c("Idaho", "Washington")
  scen_state = paste(states,sep="", collapse="|")
  
  setwd("/dmine/data/counties/")
  
  counties <- readShapePoly('UScounties.shp', 
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  #counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
  
  counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
  
  
  # use fips data from maps package
#  counties_fips <- maps::county.fips %>% 
#    mutate(statecounty=as.character(polyname)) %>% # character to split into state & county
#    tidyr::separate(polyname, c('statename', 'county'), ',') %>%
#    mutate(fips = sprintf('%05d', fips)) %>% # fips need 5 digits to join w/ geoknife result
#    filter(statename %in% states) 
  
  stencil <- webgeom(geom = 'derivative:US_Counties',
                     attribute = 'FIPS',
                     values = counties$FIPS)
  
  fabric <- webdata(url = 'dods://cida.usgs.gov/thredds/dodsC/UofIMETDATA', 
                    variables = "precipitation_amount", 
                    times = c(startDate, endDate))
  
  job <- geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
  check(job)
  precipData_result <- result(job, with.units=TRUE)
  precipData_result_frame <- data.frame(colMeans(precipData_result[sapply(precipData_result, is.numeric)]))
  colnames(precipData_result_frame) <- c("precipitation_amount")
  precipData_result_frame$FIPS <- rownames(precipData_result_frame)
 # precipData <- precipData_result_frame %>% 
  #  select(-precipitation_amount) %>% 
  #  gather(key = FIPS, value = precipitation_amount) %>%
  #  left_join(counties, by="FIPS") #join w/ counties data
  
  precipData <- merge(counties, precipData_result_frame, by="FIPS")
  
  return(precipData)
  
}



library(dplyr)
library(tidyr)
library(geoknife) #order matters because 'query' is masked by a function in dplyr
library(RColorBrewer)
library(maps)

statesTSColin <- c('Idaho', 'Washington', 'Oregon')
startTSColin <- "2016-06-01"
endTSColin <- "2017-03-01"

months <- seq(from=as.Date('2007-01-01'), to=as.Date("2015-12-01"),by='months' )
monthspan <- length(months) - 1

precipDatafinal <- data.frame()
for (i in monthspan) {
 ii <- i+1
 precipData <- getPrecip(states = statesTSColin, 
                        startDate = as.character(months[i]), 
                        endDate = as.character(months[ii]))
 
 precipData$date <- months[i]
 precipData$month <- months(as.Date(precipData$date))

 precipData$month <- substring(precipData$date,6,7)
 precipData$month <- month.abb[as.numeric(precipData@data[8,8])]
 precipData$month <- tolower(precipData$month)
 
 precipData$year <- substring(precipData$date,1,4)
 precipDatafinal <- rbind(data.frame(precipData), precipDatafinal)
                             
}





subset(precipData, )

spplot(precipData, "precipitation_amount")






