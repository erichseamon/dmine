
climatecalc <- function(getfunction, statesclim, startdatec, enddatec) {


  library(dplyr)
  library(tidyr)
  library(geoknife) #order matters because 'query' is masked by a function in dplyr
  library(RColorBrewer)
  library(maps)
  
  statesTSColin <- statesclim


  months <- seq(from=as.Date(startdatec), to=as.Date(enddatec),by='months' )
  monthspan <- length(months) - 1
  
  precipDatafinal <- data.frame()
  for (i in monthspan) {
    ii <- i+1
    precipData <- getfunction(states = statesTSColin, 
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

}