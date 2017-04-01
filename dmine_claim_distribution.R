library(maptools)
library(data.table)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep("Washington", counties@data$STATE_NAME),]
#counties <- counties[grep(input$county, counties@data$NAME),]

monthz <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
climz <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100")

#newmat <- matrix(,ncol = 15, nrow = 12 )
newmat <- matrix(,ncol = 1, nrow = 12 )

yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", "Washington", "/summaries/", sep="")

setwd(yeardir)

temp = list.files(pattern="*post2001*")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
x <- as.data.frame(ziggy.df)


x$commodity <- lapply(x$commodity, as.character)
x$commodity <- do.call(rbind , x$commodity)
x$commodity <- trim(x$commodity)

x$damagecause <- lapply(x$damagecause, as.character)
x$damagecause <- do.call(rbind , x$damagecause)
x$damagecause <- trim(x$damagecause)




#x <- as.data.frame(read.csv(i, strip.white = TRUE))
DT <- x
#DT <- subset(DT, year == "2009")
#DT2 <- subset(DT, county == input$county)
DT2 <- subset(DT, commodity == "BARLEY")
#DT2 <- subset(DT2, damagecause == "Drought")

#colz <- c("loss", "acres", "majordamage") # for acres, loss, and major damage cause
monthzz <- unique(DT2$monthcode)
newjj <- 1
newmat5a <- NULL

for (mm in 2001:2015) {
  
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
layout(matrix(c(1,2,3,4),1, 1, byrow=TRUE))
#-by month
#boxplot(claims ~ month, data = newmat5a, names =unique(newmat5a$month), ylab = "claim frequency", xlab = "Months", las = 2, main= "Washington Claim Frequency by Month, 2010-2015")
#stripchart(claims ~ month, data = newmat5a, 
#           vertical = TRUE, method = "jitter", 
#           pch = 21, col = "maroon", bg = "bisque", 
#           add = TRUE)

#-all years
boxplot(claims ~ year, data = newmat5a, names =c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015"), ylab = "claim frequency", xlab = "Years", las = 2, main= "Idaho Claim Frequency by Year")
stripchart(claims ~ year, data = newmat5a, 
           vertical = TRUE, method = "jitter", 
           pch = 21, col = "maroon", bg = "bisque", 
           add = TRUE) 








names =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

