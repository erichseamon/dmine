dmine_agplot4 <- function(state, yearz, commodity) {
library(data.table)
library(plotrix)
monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", state, "/month_1989_2015_county_nosum",sep="")

setwd(monthdir2)
files <- list.files(pattern = paste(yearz, ".*\\.", commodity, ".csv$", sep=""))
myfiles = lapply(files, read.csv, strip.white = TRUE, header = TRUE)
x <- do.call(rbind, myfiles)
DT <- data.table(x)



graphh <- aggregate(loss ~ damagecause,  DT, sum)

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", state, "/summaries/", sep=""))


setwd("/tmp/")
pdf(paste(state, "_", yearz, "_", commodity, "_annual_damage.pdf", sep=""), width=11,height=8.5, onefile = TRUE) 




#         commodity <- read.csv(paste(yearz, "_monthly_usda_gridmet_post2001_", state, sep=""), strip.white = TRUE)

#        commodity_year <- subset(commodity, commodity == commodity & year == yearz & monthcode == month)
par(mfrow=c(2,1))
layout(matrix(c(1,2),2, 1, byrow=TRUE))
par(mar=c(1,14,2,1), oma = c(0,0,2,0))







library(lattice)
bw2 <- boxplot(log(loss) ~ damagecause, data=DT, col=topo.colors(length(unique(DT$damagecause))), scales=list(x=list(rot=90, cex = 1.5), y=list(cex=1)), main = paste(state, " annual damage cause for ", yearz, ": ", "Commodity: ",  commodity, sep=""), cex.names = 1, cex.main = 1, cex.axis = 1, las = 2, horizontal = TRUE)
#bwplot(DT$damagecause ~ log(DT$loss), col=topo.colors(length(unique(DT$damagecause))), horizontal = TRUE, par.settings = list(box.rectangle = list(topo.colors(length(unique(DT$damagecause))))), main = paste("Crop Commodity Damage Causes", "\nState: ", state, "   Month: ", month, "  Year: ", yearz, "   Commodity: ", commodity, sep=""))

stripchart(log(DT$loss) ~ DT$damagecause, 
           vertical = FALSE, method = "jitter", 
           pch = 21, col = "maroon", bg = "bisque", 
           add = TRUE) 

#title(paste("Crop Commodity Damage Causes", "\nState: ", state, "   Month: ", month, "  Year: ", yearz, "   Commodity: ", commodity, sep=""), adj = 0.5, line = 1) 


#print(bw2, position=c(0, .4, 1, 1))




print(bw2, position=c(0, .55, 1, 1))



plot.new()

xxx <- addtable2plot(0,0,graphh, 
                     xpad=.5, ypad=1,
                     bty='o', cex = .7,
                     display.rownames = TRUE, 
                     hlines = TRUE,
                     vlines = TRUE, 
                     title = "Annual Crop Losses Summarized by Damage Cause")
print(xxx, position=c(0, 0, .55, 1))




dev.off()

print(paste("PDF printed to local /tmp directory: ", state, "_", yearz, "_", commodity, "_annual_damage.pdf", sep=""))



}