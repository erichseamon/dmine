getPrecip <- function(states, startDate, endDate){
  
  # use fips data from maps package
  counties_fips <- maps::county.fips %>% 
    mutate(statecounty=as.character(polyname)) %>% # character to split into state & county
    tidyr::separate(polyname, c('statename', 'county'), ',') %>%
    mutate(fips = sprintf('%05d', fips)) %>% # fips need 5 digits to join w/ geoknife result
    filter(statename %in% states) 
  
  stencil <- webgeom(geom = 'derivative:US_Counties',
                     attribute = 'FIPS',
                     values = counties_fips$fips)
  
  fabric <- webdata(url = 'dods://cida.usgs.gov/thredds/dodsC/UofIMETDATA', 
                    variables = "precipitation_amount", 
                    times = c(startDate, endDate))
  
  job <- geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
  check(job)
  precipData_result <- result(job, with.units=TRUE)
  precipData <- precipData_result %>% 
    select(-variable, -statistic, -units) %>% 
    gather(key = fips, value = precipVal, -DateTime) %>%
    left_join(counties_fips, by="fips") #join w/ counties data
  
  return(precipData)
  
}