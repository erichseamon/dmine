dmine_agplot5 <- function(state, startyear, endyear, commodity) {

library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep(state, counties@data$STATE_NAME),]
#counties <- counties[grep(input$county, counties@data$NAME),]

monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

#newmat <- matrix(,ncol = 15, nrow = 12 )
newmat <- matrix(,ncol = 1, nrow = 12 )

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", state, "/summaries/", sep="")

setwd(yeardir)

temp = list.files(pattern="*post2001*")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
x <- as.data.frame(ziggy.df)


x$commodity <- lapply(x$commodity, as.character)
x$commodity <- do.call(rbind , x$commodity)
x$commodity <- trimws(x$commodity)

x$damagecause <- lapply(x$damagecause, as.character)
x$damagecause <- do.call(rbind , x$damagecause)
x$damagecause <- trimws(x$damagecause)




#x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- x
#DT <- subset(DT, year == "2009")
#DT2 <- subset(DT, county == input$county)
DT2 <- subset(DT, commodity == commodity)
#DT2 <- subset(DT2, damagecause == "Drought")

#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
monthzz <- unique(DT2$monthcode)
newjj <- 1
newmat5a <- NULL

for (mm in startyear:endyear) {
  
  newmat2 <- matrix(, ncol = 1, nrow = 12)
  
  
  
  #--loss
  newii <- 1
  for (ii in monthzz) {
    DT3 <- subset(DT2, year == mm)
    
    nez <- subset(DT3, monthcode == ii)
    newmat2[ii, newjj] <- nrow(nez)
    newii <- newii + 1
  }
  
  
  newmat3 <- matrix(, ncol = 1, nrow = 12)
  
  #--acres
  
  newii <- 1
  for (ii in monthzz) {
    DT4 <- subset(DT, year == mm)
    nez <- subset(DT4, monthcode == ii)
    newmat3[ii, newjj] <- sum(as.numeric(nez[,31]))
    newii <- newii + 1
  }
  
  
  setwd("/tmp/")
  pdf(paste(state, "_", startyear, "_to_", endyear, "_", commodity, "_monthly_distribution.pdf", sep=""), width=11,height=8.5, onefile = TRUE) 
  
  
  
  
  
  
  newmat4 <- cbind(newmat2, newmat3)
  colnames(newmat4) <- c("claims", "acres")
  newmat <- as.data.frame(c(1:12))
  newmatyear <- as.data.frame(c(1:12))
  newmatyear$year <- mm
  #newmat5 <- as.matrix(newmat4)
  newmat5 <- cbind(newmatyear, newmat4)
  colnames(newmat5) <- c("month", "year", "claims", "acres")
  
  #rownames(newmat5) <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  
  newmat5a <- rbind(newmat5a, newmat5)
}

newmat5a$claims[newmat5a$claims == 0] <- NA
newmat5a <- subset(newmat5a, claims != "NA")
layout(matrix(c(1,1,2,2),4, 1, byrow=TRUE))
#-by month


boxplot(claims ~ month, data = newmat5a, names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ylab = "claim frequency", xlab = "Months", las = 2, main= paste(state, " Claim Frequency by Month, ", startyear, "-", endyear, " for ", commodity, sep=""), col=topo.colors(length(unique(newmat5a$month))))
stripchart(claims ~ month, data = newmat5a, 
           vertical = TRUE, method = "jitter", 
          pch = 21, col = "maroon", bg = "bisque", 
          add = TRUE )

newmat5aa <- data.table(newmat5a)

graphh <- aggregate(claims ~ month,  newmat5aa, sum)

graphh$month <- c(month.abb)

plot.new()
xxx <- addtable2plot(.3,0,graphh, 
                     xpad=.5, ypad=1,
                     bty='o', cex = 1.5,
                     display.rownames = TRUE, 
                     hlines = TRUE,
                     vlines = TRUE, 
                     title = "Monthly Crop Loss Claims")
print(xxx, position=c(0, 0, .5, 1))

dev.off()


print(paste("PDF printed to local /tmp directory: ", state, "_", startyear, "_to_", endyear, "_", commodity, "_monthly_distribution.pdf", sep=""))


}