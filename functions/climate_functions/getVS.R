getVS <- function(states, startDate, endDate){
  
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
  
  fabric <- webdata(url = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_vs_1979_CurrentYear_CONUS.nc', 
                    variables = "daily_mean_wind_speed", 
                    times = c(startDate, endDate))
  
  job <- geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
  check(job)
  precipData_result <- result(job, with.units=TRUE)
  precipData_result_frame <- data.frame(colMeans(precipData_result[sapply(precipData_result, is.numeric)]))
  colnames(precipData_result_frame) <- c("daily_mean_wind_speed")
  precipData_result_frame$FIPS <- rownames(precipData_result_frame)
  # precipData <- precipData_result_frame %>% 
  #  select(-precipitation_amount) %>% 
  #  gather(key = FIPS, value = precipitation_amount) %>%
  #  left_join(counties, by="FIPS") #join w/ counties data
  
  VSData <- merge(counties, precipData_result_frame, by="FIPS")
  
  return(VSData)
  
}