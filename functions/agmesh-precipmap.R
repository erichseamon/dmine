precipMap <- function(precipData, startDate, endDate){
  cols <- colorRampPalette(brewer.pal(9,'Blues'))(9)
  precip_breaks <- c(seq(0,80,by = 10), 200)
  
  precipData_cols <- precipData %>% 
    group_by(statename, statecounty) %>% 
    summarize(cumprecip = sum(precipVal)) %>% 
    mutate(cols = cut(cumprecip, breaks = precip_breaks, labels = cols, right=FALSE)) %>%
    mutate(cols = as.character(cols))
  
  par(mar = c(0,0,3,0))
  
  map('county', regions = precipData_cols$statecounty, 
      fill = TRUE, col = precipData_cols$cols, exact=TRUE)
  
  legend(x = "bottomright", fill = cols, cex = 0.7, bty = 'n', 
         title = "Cumulative\nPrecipitation (mm)",
         legend = c(paste('<', precip_breaks[-c(1,length(precip_breaks))]), 
                    paste('>', tail(precip_breaks,2)[1]))) # greater
  graphics::title("Precipitation UIDAHO Gridmet",
                  line = 2, cex.main=1.2)  #title was being masked by geoknife
  mtext(side = 3, line = 1, cex = 0.9, 
        text= paste("By county from", startDate, "to", endDate))
}