library(compare)

#dmineplots <- function(scen_state, startyear, endyear, dcause, Kommodity) {

scen_state = "Idaho"
startyear = "2001"
endyear = "2015"
dcause = "Drought"
#Kommodity = "Wheat"



setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% scen_state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/summaries/", sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/netcdf/pdsi_apr_", startyear, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_positive/", sep=""))
#system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
#uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_positive/", sep=""))



years <- c(startyear:endyear)

for (j in years) {
setwd(yeardir)
i <- paste(j, "_monthly_usda_gridmet_post2001_", scen_state, sep="")
#i <- paste(input$year, ".", input$month, ".", input$commodity, ".csv", sep="")

#setwd("/dmine/data/counties/")
#counties <- readShapePoly('UScounties.shp', 
#                          proj4string=CRS
#                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#counties <- subset(counties, STATE_NAME %in% scen_state)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
setwd(yeardir)
x <- as.data.frame(read.csv(i, strip.white = TRUE))
uniquecomm <- unique(x$commodity)
months <- unique(x$month)
#--------------

for (kkk in uniquecomm) {
  for (jj in months) {

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
#DT2 <- DT
DT2 <- subset(DT, damagecause == dcause)

DT2 <- subset(DT2, month == jj)
DT2 <- subset(DT2, commodity == kkk)
if (nrow(DT2) != 0 ) {
  
#DT2 <- subset(DTnew3, commodity == input$commodity)
#DT3 <- data.frame(DT2$acres, DT2$loss)
#DT4 <- cbind(x, DT3)
DT2loss <- DT2[,list(loss=sum(loss)), by = county]
#DT2 <- DT2[, lapply(.SD, sum), by=list(county)]
DT2acres <- DT2[,list(acres=sum(acres)), by = county]
DTdamage_loss <- DT2[,list(loss=sum(loss)), by = damagecause]
DTdamage_acres <- DT2[,list(acres=sum(acres)), by = damagecause]
#lengthDT2 <- length(DT2)
#DT5 <- matrix(input@commodity, nrow = lengthDT2, ncol = 1) 
DT6 <- cbind(DT2loss, DT2acres$acres)
#--DT7 is for barplot of summarized damage causes for the state, annually, with loss and acres per damage type
DT7 <- cbind(DTdamage_loss, DTdamage_acres$acres) 
setnames(DT7, c("DAMAGECAUSE", "LOSS", "ACRES"))
#m <- subset(x, county = "ID")
names(counties)[1] <- "county"
#colnames(x) <- c("UNIQUEID", "YEAR", "COUNTY", "COMMODITYCODE", "MONTHCODE", "ACRES", "LOSS", "COMMODITY")

#colnames(u) <- c("NAME")
#z <- cbind(u,DT)
m <- merge(counties, DT2loss, by='county')
#names(m)[7] <- "acres" 


m$loss[is.na(m$loss)] <- 0
#m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
#m$acres[is.na(m$acres)] <- 0

#------------


#shapefile(m)
#--begin polygon work
#length(na.omit(m$LOSS))
#tt <- colorRampPalette(brewer.pal(11, "Spectral")
tt <- colorRampPalette(c("blue", "orange", "red"))
#mz <- subset(m, LOSS != 0)
#mzacres <- subset(m, acres > 0)
#lengacres <- length(m$acres)
leng <- length(m$loss)
#len2 <- tt(len <- length(mz$loss))
#len2acres <- tt(len <- length(mzacres$acres))
#len2a <- length(mz$loss)
#len2a <- length(mzacres$acres)
len3 <- tt(len <- length(m$loss))
#len4 <- tt(len <- length(m$acres))
len4 <- tt(nrow(as.data.frame(subset(m, loss > 0))))

orderedcolors2 <- tt(length(m$loss))[order(order(m$loss))]
#orderedcolors3 <- tt(length(m$acres))[order(order(m$acres))]
#newframe <- data.frame(m$LOSS)
m[["loss"]][is.na(m[["loss"]])] <- 0
#m[["acres"]][is.na(m[["acres"]])] <- 0 

xx <- unique(counties$county)
newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
vect <- as.vector(DT2loss$county)

for (ll in vect) {
  for (kk in xx){
    comp <- compare(kk,ll)
    if (isTRUE(comp)) {
    rownumber <- which(xx == kk)
    #print("yes this worked, added 0")
    newmatrix[rownumber,] <- len3[rownumber]
    #newmatrix[yy,] <- orderedcolors2[yy]
    }}}

newmatrix[is.na(newmatrix)] <- 0

  
#xx <- 1
#newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)

#for (jj in 1:leng){
  
  #if (DT7$ACRES[jj] == 0) {
  #print("yes this worked, added 0")
  # newmatrix_acres[jj,] <- 0
  #} else {
  #print("yes, this worked, added color")
  #newmatrix[jj,] <- len4[jj] 
  #newmatrix_acres[jj,] <- orderedcolors3[xx]
  #xx <- xx + 1
#}


#newmatrix[newmatrix==0] <- NA
#newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
#newmatrix2 <- subset(newmatrix = TRUE)
#newmatrix[newmatrix == NA] <- 0
#newmatrix <- c(newmatrix)

#newmatrix_acres[newmatrix_acres==0] <- NA
#newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
#newmatrix2acres <- subset(newmatrix = TRUE)
#newmatrix_acres[newmatrix_acres == NA] <- 0
#newmatrix_acres <- c(newmatrix_acres)

plotmonth <- month.abb[jj]
plotyear <- j
#plotcommodity <- x$commodity[1]

#orderedcolors2 <- colorRampPalette(c(44))
#m <- cbind(m$LOSS, newmatrix)
#midpoints <- barplot(mz$LOSS)
png(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png/drought/", j, "_", jj, "_", kkk,  "_plot.png", sep=""))
par(mar=c(6,3,3,2)+1)
par(mfrow=c(2,2))
layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
#--turn image horizontal



midpoint_loss <- (max(m$loss) + min(m$loss)/2)
#midpoint_acres <- (max(m$acres) + min(m$acres)/2)

#b <- barplot(DT7$LOSS, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix)
#text(bb, midpoint_loss, labels=mz$loss, srt=90)
#plot(m, col = newmatrix, main = paste(scen_state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))
plot(m, col = newmatrix, main = paste(scen_state, " crop loss $ \n", " ", jj, j, "\n", kkk, sep=""))
legend.col(col = orderedcolors2a, lev = m$loss)
dev.off()
#bb <- barplot(DT7$ACRES, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix_acres)
#text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
#plot(m, col = newmatrix_acres, main = paste(scen_state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))

} else {
  
                     m <- counties
                     m@data$loss <- runif(nrow(m@data))
                     m$loss <- 0
                     
                     
                     #------------
                     
                     
                     #shapefile(m)
                     #--begin polygon work
                     #length(na.omit(m$LOSS))
                     #tt <- colorRampPalette(brewer.pal(11, "Spectral")
                     tt <- colorRampPalette(c("blue", "orange", "red"))
                     #mz <- subset(m, LOSS != 0)
                     #mzacres <- subset(m, acres > 0)
                     #lengacres <- length(m$acres)
                     leng <- length(m$loss)
                     #len2 <- tt(len <- length(mz$loss))
                     #len2acres <- tt(len <- length(mzacres$acres))
                     #len2a <- length(mz$loss)
                     #len2a <- length(mzacres$acres)
                     len3 <- tt(len <- length(m$loss))
                     #len4 <- tt(len <- length(m$acres))
                     orderedcolors2a <- tt(length(m$loss))
                     
                     orderedcolors2 <- tt(length(m$loss))[order(order(m$loss))]
                     #orderedcolors3 <- tt(length(m$acres))[order(order(m$acres))]
                     #newframe <- data.frame(m$LOSS)
                     m[["loss"]][is.na(m[["loss"]])] <- 0
                     #m[["acres"]][is.na(m[["acres"]])] <- 0 
                     xx <- 1
                     newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
                     
                     for (k in 1:leng){
                       #if (DT7$LOSS[k] == 0) {
                       #print("yes this worked, added 0")
                       # newmatrix[k,] <- 0
                       #} else {
                       #print("yes, this worked, added color")
                       #newmatrix[k,] <- len3[k] 
                       newmatrix[k,] <- "#ffffff"
                       xx <- xx + 1
                     }
                     
                     #xx <- 1
                     #newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)
                     
                     #for (jj in 1:leng){
                       
                       #if (DT7$ACRES[jj] == 0) {
                       #print("yes this worked, added 0")
                       # newmatrix_acres[jj,] <- 0
                       #} else {
                       #print("yes, this worked, added color")
                       #newmatrix[jj,] <- len4[jj] 
                       #newmatrix_acres[jj,] <- orderedcolors3[xx]
                       #xx <- xx + 1
                     #}
                     
                     
                     #newmatrix[newmatrix==0] <- NA
                     #newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
                     #newmatrix2 <- subset(newmatrix = TRUE)
                     #newmatrix[newmatrix == NA] <- 0
                     #newmatrix <- c(newmatrix)
                     
                     #newmatrix_acres[newmatrix_acres==0] <- NA
                     #newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
                     #newmatrix2acres <- subset(newmatrix = TRUE)
                     #newmatrix_acres[newmatrix_acres == NA] <- 0
                     #newmatrix_acres <- c(newmatrix_acres)
                     
                     plotmonth <- month.abb[jj]
                     plotyear <- j
                     #plotcommodity <- x$commodity[1]
                     
                     #orderedcolors2 <- colorRampPalette(c(44))
                     #m <- cbind(m$LOSS, newmatrix)
                     #midpoints <- barplot(mz$LOSS)
                     png(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png/drought/", j, "_", jj, "_", kkk,  "_plot.png", sep=""))
                     par(mar=c(6,3,3,2)+1)
                     par(mfrow=c(2,2))
                     layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
                     #--turn image horizontal
                     
                     #plotmonth <- month.abb[x$monthcode[1]]
                     #plotyear <- x$year[1]
                     #plotcommodity <- x$commodity[1]
                     
                     midpoint_loss <- (max(m$loss) + min(m$loss)/2)
                     #midpoint_acres <- (max(m$acres) + min(m$acres)/2)
                     
                     #b <- barplot(m$loss, names.arg = m$loss, las=2, col = newmatrix)
                     #text(bb, midpoint_loss, labels=mz$loss, srt=90)
                     plot(m, col = newmatrix, main = paste(scen_state, " crop loss $ \n", " ", j, " ", jj, "\n", kkk, sep=""))
                     legend.col(col = orderedcolors2a, lev = m$loss)
                     dev.off()
                     #bb <- barplot(DT7$ACRES, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix_acres)
                     #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
                     #plot(m, col = newmatrix_acres, main = paste(scen_state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))                     
                     
} }}}