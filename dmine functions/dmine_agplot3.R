dmine_agplot3 <- function(state, yearz, month, commodity) {
  

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
counties <- subset(counties, STATE_NAME %in% state)
#counties_one <- subset(counties, NAME %in% input$county)
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", state, "/summaries/", sep="")
setwd(yeardir)
zz <- as.numeric(month)
plotmonth <- month.abb[zz]


monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", state, "/month_1989_2015_county_nosum",sep="")

setwd(monthdir2)
i <- paste(yearz, ".", month, ".", commodity, ".csv", sep="")

commodity_year <- read.csv(i, strip.white = TRUE)

setwd("/tmp/")
pdf(paste(state, "_", yearz, "_", month, "_", commodity, "_damage.pdf", sep=""), width=11,height=8.5, onefile = TRUE) 




#         commodity <- read.csv(paste(yearz, "_monthly_usda_gridmet_post2001_", state, sep=""), strip.white = TRUE)

#        commodity_year <- subset(commodity, commodity == commodity & year == yearz & monthcode == month)
par(mfrow=c(2,2))
layout(matrix(c(1,2),2, 1, byrow=TRUE))
par(mar=c(1,14,2,1), oma = c(0,0,2,0))
#plot(counties)
#tr <- plot(counties, main = paste("Crop Commodity Statewide Monthly Loss Report", "State: ", state, "   Month ", month, "  Year: ", yearz, "   Commodity: ", commodity, sep=""))
library(lattice)
graphh <- aggregate(loss ~ damagecause,  commodity_year, sum)

par(las=2)

bw1 <- boxplot(log(commodity_year$loss) ~ commodity_year$damagecause, col=topo.colors(length(unique(commodity_year$damagecause))), horizontal = TRUE)

stripchart(log(commodity_year$loss) ~ commodity_year$damagecause, 
           vertical = FALSE, method = "jitter", 
           pch = 21, col = "maroon", bg = "bisque", 
           add = TRUE) 

title(paste("Crop Commodity Damage Causes", "\nState: ", state, "   Month: ", month, "  Year: ", yearz, "   Commodity: ", commodity, sep=""), adj = 0.5, line = 1) 

#bw1 <- bwplot(damagecause ~ log(loss), data=commodity_year, scales=list(x=list(rot=90, cex = 1.5), y=list(cex=1.2)), main = paste(state, " damages cause for ", plotmonth, " ",  yearz, "\n", "Commodity: ",  commodity, sep=""), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1)
print(bw1, position=c(0, .5, 1, 1))
#print(bw1)
#print(tr2, position = c(0, 0, 0.5, 1), more = TRUE)
#print(tra, position = c(0.5, 0, 1, 1))
#plot(counties_one, col="blue", add=T)

plot.new()
xxx <- addtable2plot(0,0,graphh, 
              xpad=.5, ypad=1,
              bty='o',
              display.rownames = TRUE, 
              hlines = TRUE,
              vlines = TRUE, 
              title = "Monthly Crop Losses Summarized by Damage Cause")
print(xxx, position=c(0, 0, .5, 1))

dev.off()

print(paste("PDF printed to local /tmp directory: ", state, "_", yearz, "_", commodity, ".pdf", sep=""))

}