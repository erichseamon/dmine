#--bringing in county shapefile
setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state2, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% input$state2)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_positive/", sep=""))
#system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
#uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_positive/", sep=""))

setwd(monthdir2)
#setwd(paste("/dmine/data/USDA/agmesh_scenarios/", input$state2, "/summaries4/", sep=""))

#i <- paste(input$year2, ".", input$month, ".", input$commodity, ".csv", sep="")

#files <- list.files(pattern = ".*\\.WHEAT.csv$")
files <- list.files(pattern = paste(input$year2, ".*\\.", input$commodity, ".csv$", sep=""))
myfiles = lapply(files, read.csv, strip.white = TRUE, header = TRUE)
x <- do.call(rbind, myfiles)
x <- as.data.frame(x)



##cpi <- data.frame(read.csv("/dmine/data/FRED/cpi/CPIAUCSL1989_2015.csv", header = TRUE, strip.white = TRUE))
##sick <- subset(cpi, year == input$year2)
#input.monthz <- as.numeric(input$month)
#sick <- as.data.frame(subset(ick, month == input.monthz))
#sick <- as.data.frame(subset(ick, month == tolower(month.abb[input.monthz])))

##sickmean <- mean(sick$ratio)
##colnames(sick) <- c("ratio")

setwd("/dmine/data/counties/")
counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
counties <- subset(counties, STATE_NAME %in% input$state2)
#counties <- counties[grep(input$state2, counties@data$STATE_NAME),]

#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month", sep=""))
setwd(monthdir2)

#  x <- as.data.frame(read.csv(i, strip.white = TRUE))

##  sickk <- as.data.frame(rep(sickmean, each=nrow(x)))
##  colnames(sickk) <- c("ratio")
# x <- x[,-c(8)]  #--remove ID field



colnames(x) <- c("ID", "YEAR", "MONTHCODE", "COMMODITY", "NAME", "LOSS")

# colnames(x) <- c("UNIQUEID", "YEAR", "NAME", "COMMODITYCODE", "MONTHCODE", "COMMODITY", "DAMAGECAUSE", "ACRES", "LOSS")
## x$LOSS <- x$LOSS * sickk$ratio

#---

DT <- data.table(x)

#DTnew <- tolower(DT$commodity)

#simpleCap <- function(x) {
#  s <- strsplit(x, " ")[[1]]
#  paste(toupper(substring(s, 1,1)), substring(s, 2),
#        sep="", collapse=" ")
#}

#DTnew1a <- data.frame(sapply(DTnew,simpleCap))
#colnames(DTnew1a) <- c("commodity_new")
#DTnew3 <- cbind(DT, DTnew1a)

# DTnew3 <- DT
#  DTnew3$commodity <- DTnew3$commodity_new

#--change to lowercase DT2!!
DT2 <- subset(DT, COMMODITY == input$commodity)
DT2loss <- DT2[,list(LOSS=sum(LOSS)), by = NAME]
DT6 <- DT2loss
m <- merge(counties, DT6, by='NAME')
















#u <- data.frame(trimws(x$county))
#colnames(u) <- c("NAME")
#z <- cbind(x,u)
#m <- merge(counties, x, by='NAME')
m$LOSS[is.na(m$LOSS)] <- 0
#m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
#m$ACRES[is.na(m$ACRES)] <- 0

#shapefile(m)
#--begin polygon work
#length(na.omit(m$LOSS))
#tt <- colorRampPalette(brewer.pal(11, "Spectral")
tt <- colorRampPalette(c("light blue",  "dark blue"), space = "Lab")
mz <- subset(m, LOSS != 0)
#mzacres <- subset(m, ACRES > 0)
#lengacres <- length(m$ACRES)
leng <- length(m$LOSS)
len2 <- tt(len <- length(mz$LOSS))
#len2acres <- tt(len <- length(mzacres$ACRES))
len2a <- length(mz$LOSS)
#len2a <- length(mzacres$ACRES)
len3 <- tt(len <- length(m$LOSS))


orderedcolors2 <- tt(length(mz$LOSS))[order(order(mz$LOSS))]
#  orderedcolors3 <- tt(length(mzacres$ACRES))[order(order(mzacres$ACRES))]
newframe <- data.frame(m$LOSS)

xx <- 1
newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)


for (jj in 1:leng){
  
  if (m$LOSS[jj] == 0) {
    #print("yes this worked, added 0")
    newmatrix[jj,] <- 0
  } else {
    #print("yes, this worked, added color")
    #newmatrix[jj,] <- len3[jj]
    newmatrix[jj,] <- orderedcolors2[xx]
    xx <- xx + 1
  }
  
}



# xx <- 1
#  newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)

#  for (jj in 1:leng){

#    if (m$ACRES[jj] == 0) {
#print("yes this worked, added 0")
#      newmatrix_acres[jj,] <- 0
#    } else {
#print("yes, this worked, added color")
#newmatrix[jj,] <- len3[jj]
#      newmatrix_acres[jj,] <- orderedcolors3[xx]
#      xx <- xx + 1
#    }

#  }

msub <- subset(m, LOSS != 0)

xx <- 1
newmatrix_sub <- matrix(data = NA, nrow = leng, ncol = 1)
leng_sub <- length(msub$LOSS)
for (jj in 1:leng_sub){
  if (msub$LOSS[jj] == 0) {
    #print("yes this worked, added 0")
    
    newmatrix_sub[jj,] <- 0
  } else {
    #print("yes, this worked, added color")
    #newmatrix_sub[jj,] <- len3[jj]
    newmatrix_sub[jj,] <- orderedcolors2[xx]
    xx <- xx + 1
  }}






newmatrix[newmatrix==0] <- NA
newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
#newmatrix2 <- subset(newmatrix = TRUE)
newmatrix[newmatrix == NA] <- 0
newmatrix <- c(newmatrix)


#  newmatrix_acres[newmatrix_acres==0] <- NA
#  newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
#newmatrix2 <- subset(newmatrix = TRUE)
#  newmatrix_acres[newmatrix_acres == NA] <- 0
#  newmatrix_acres <- c(newmatrix_acres)


#orderedcolors2 <- colorRampPalette(c(44))
#m <- cbind(m$LOSS, newmatrix)
#midpoints <- barplot(mz$LOSS)
#png(paste("/dmine/data/USDA/agmesh-scenarios/", input$state2, "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))

par(mar=c(6,6,3,2)+1)
par(mfrow=c(2,3))
layout(matrix(c(1,2,3),1, 3, byrow=TRUE))
#--turn image horizontal
plotmonth <- month.abb[x$monthcode[1]]
plotyear <- x$year[1]
plotcommodity <- x$commodity[1]

midpoint_loss <- (max(m$loss) + min(m$loss)/2)
midpoint_acres <- (max(m$acres) + min(m$acres)/2)
#par(mar=c(6,6,4,2)+1)
b <- barplot(msub$LOSS, names.arg = msub$NAME, las=2, col = newmatrix_sub, cex.names=1.5, horiz=TRUE, main = paste(input$state2, " crop loss bar chart ($) \n", " ", plotyear, " ", input$commodity, sep=""), cex.axis=1.9, cex.main=2, width=4)
#text(bb, midpoint_loss, labels=mz$loss, srt=90)
plot(m, col = newmatrix, main = paste(input$state2, " crop loss map ($) \n", " ", plotyear, " ", input$commodity, sep=""), cex.main=2)
