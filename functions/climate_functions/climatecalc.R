climatecalc <- function(gfunction, statesclim, startdatec, enddatec) {
  source("/home/git/dmine/functions/admin_functions/sourceDir.R")
  sourceDir("/home/git/dmine/functions/climate_functions")
  
  library(dplyr)
  library(tidyr)
  library(geoknife) #order matters because 'query' is masked by a function in dplyr
  library(RColorBrewer)
  library(maps)
  
  statevar <- as.character(substitute(gfunction))
  statevar <- substring(statevar, 4, 9)
  
  months <- seq(from=as.Date(startdatec), to=as.Date(enddatec),by='months' )
  monthspan <- length(months) - 1
  
  
  
  final <- data.frame()
  for (i in 1:monthspan) {
    ii <- i+1
    precipData <- gfunction(states = statesclim, 
                            startDate = as.character(months[i]), 
                            endDate = as.character(months[ii]))
    
    precipData$date <- months[i]
    precipData$month <- months(as.Date(precipData$date))
    
    precipData$month <- substring(precipData$date,6,7)
    precipData$month <- month.abb[as.numeric(precipData@data[8,8])]
    precipData$month <- tolower(precipData$month)
    
    precipData$year <- substring(precipData$date,1,4)
    final <- rbind(data.frame(precipData), final)
    assign(paste(statevar, "final", sep=""), final)
    
  }
  
  return(get(paste(statevar, "final", sep="")))
  
  
}