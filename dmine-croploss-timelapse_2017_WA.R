library(compare)
library(plyr)
library(reshape2)
library(data.table)
library(raster)
library(ncdf4)
library(maptools)
library(compare)
library(stringr)


rm(list = ls()) #--clears all lists------#
cat("\14")


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#---legend function


legend.col <- function(col, lev){
  
  opar <- par
  
  n <- length(col)
  
  bx <- par("usr")
  
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  
  xx <- rep(box.cx, each = 2)
  
  par(xpd = TRUE)
  for(i in 1:n){
    
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
    
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
}


#------





options(scipen=5)


#dmineplots <- function(scen_state, startyear, endyear, dcause, Kommodity) {

scen_state = "Washington"
startyear = "1989"
endyear = "2015"
#dcause = "Drought"
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
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_positive/", sep=""))
#system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
#uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_positive/", sep=""))

#--set tt outside of loop below
tt1 <- colorRampPalette(c("white", "blue", "red"))
#setwd(yeardir)
#it <- paste("2001_2015_usda_gridmet_", scen_state, sep="")

#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/summaries", sep=""))
setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/month_1989")
files  <- list.files(pattern = scen_state)
tables <- lapply(files, read.csv, header = TRUE, strip.white = TRUE)
combined.df_1989 <- do.call(rbind , tables)

combined.df_1989 <- combined.df_1989[,c(1,3,2,4,5,6,7,8,9,10,11,12,13,14,15,17,16)]
colnames(combined.df_1989)[17] <- "loss"
colnames(combined.df_1989)[16] <- "acres"

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/month_2001")
files  <- list.files(pattern = scen_state)
tables <- lapply(files, read.csv, header = TRUE, strip.white = TRUE)
combined.df_2001 <- do.call(rbind , tables)

zaa <- rbind(combined.df_1989, combined.df_2001)


  
#zaa <- as.data.frame(read.csv(it, strip.white = TRUE))
DTz1 <- data.table(zaa)
DTz1$damagecause <- trimws(DTz1$damagecause)

DTz1_damage_unique <- trimws(unique(DTz1$damagecause))
DTz1_damage_unique <- DTz1_damage_unique[DTz1_damage_unique != ""]

#DTz1_damage_unique <-DTz1_damage_unique[25:32]  #--remove first two causes since they have already been processed.



for (lll in DTz1_damage_unique) {
#DTz1 <- data.table(zaa)
DTz1$damagecause <- trimws(DTz1$damagecause)
DTz1_base <- DTz1

lll2 <- gsub(" ", "_", lll)
lll2 <- gsub("/", "_", lll2)
lll2 <- gsub("\\(", "_", lll2)
lll2 <- gsub("\\)", "_", lll2)
lll2 <- gsub(" ", "_", lll2)
 
DTz1 <- subset(DTz1, damagecause == lll) 
#DTz <- subset(DTz, commodity == kkk)
DTza1 <- as.data.frame(subset(DTz1, loss > 0))  #---- only by damagecause, all commodities, with no loss less than 0
#DTza1 <- subset(DTza1, monthcode == 0)
DTzmax1 <- max(DTza1$loss)
DTzmin1 <- min(DTza1$loss)
DTzlen1 <- (nrow(DTza1)/10)

DTza1nrow <- nrow(DTza1)

if(DTza1nrow == 1) {
  
  len4a_out = tt1(1 - 0 / nrow(DTza1))
  
} else {
  

len4a_out <- tt1((DTzmax1 - DTzmin1)/DTzlen1)
}

#---------------

years <- c(startyear:endyear)

for (j in years) {
setwd(yeardir)
#i <- paste("2001_2015_usda_gridmet_", scen_state, sep="")
#i <- paste(j, "_monthly_usda_gridmet_post2001_", scen_state, sep="")
#i <- paste(input$year, ".", input$month, ".", input$commodity, ".csv", sep="")

#setwd("/dmine/data/counties/")
#counties <- readShapePoly('UScounties.shp', 
#                          proj4string=CRS
#                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#counties <- subset(counties, STATE_NAME %in% scen_state)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
#setwd(yeardir)
#x <- as.data.frame(read.csv(i, strip.white = TRUE))
x3 <- subset(DTz1_base, monthcode != "NA")
x3 <- subset(x3, commoditycode != 88)
x3$commodity <- trimws(x3$commodity)
xright3 <- subset(x3, damagecause == lll)
uniquecomm4 <- unique(xright3$commodity)

x2 <- DTz1_base
x2 <- subset(x2, year == j)
x2 <- subset(x2, monthcode != "NA")
x2 <- subset(x2, commoditycode != 88)
x2$commodity <- trimws(x2$commodity)
xright2 <- subset(x2, damagecause == lll)

uniquecomm3 <- unique(xright2$commodity)
  
x <- subset(DTz1_base, year == j)
x <- subset(x, monthcode != "NA")
x <- subset(x, commoditycode != 88)
x$commodity <- trimws(x$commodity)


xright <- subset(x, damagecause == lll)
uniquecomm2 <- unique(xright$commodity)


uniquecomm <- unique(x$commodity)
months_noblanks <- subset(x, monthcode != 0)
months <- na.omit(unique(months_noblanks$month))
#-------------
 
for (kkk in uniquecomm) {

  tt1 <- colorRampPalette(c("white", "blue", "red"))
  setwd(yeardir)
  #it <- paste("2001_2015_usda_gridmet_", scen_state, sep="")
  #zaa <- as.data.frame(read.csv(it, strip.white = TRUE))
  #DTz1 <- data.table(zaa)
  DTz1_base$damagecause <- trimws(DTz1_base$damagecause)
  DTz1_base$commodity <- trimws(DTz1_base$commodity)
  DTz1 <- subset(DTz1_base, damagecause == lll) 
  DTz1 <- subset(DTz1, commodity == kkk)
  DTz1$county <- trimws(DTz1$county)
  DTz1 <- subset(DTz1, monthcode != 0)
  
  
  
  if (nrow(DTz1) == 0) {
    DTzmax1 <<- 1
    DTzmin1 <<- 0
  } 
  
  if (nrow(DTz1) != 0) {
    
    DTzsumz <- as.data.frame(aggregate(DTz1$loss~DTz1$month+DTz1$year+DTz1$county, DTz1, sum))
    colnames(DTzsumz) <- c("month", "year", "county", "loss")
    DTzsum_maxz <<- max(DTzsumz$loss)
    
    
  #DTza1 <- as.data.frame(subset(DTz1, loss > 0))
  #DTzmax1 <<- max(DTza1$loss)
  #DTzmin1 <<- min(DTza1$loss)
  #DTzlen1 <<- (nrow(DTza1)/10)
  
  #len4a_out1 <<- tt1((DTzmax1 - DTzmin1)/DTzlen1)
  
  DTza1a <- subset(DTz1, monthcode != 0)
  DTza1a$commodity <- trimws(DTza1a$commodity)
  DTza1aa <- subset(DTza1a, commodity == kkk)
  #--nEED IF STATEMENT - for aggregate
  
  months_all <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  DTza1aa$month  <- month.abb[DTza1aa$monthcode]

  if (nrow(DTza1aa) != 0) {
  rows.per.group  <- aggregate(rep(1, length(DTza1aa$loss)),
                               by=list(DTza1aa$month, DTza1aa$year), sum)
  
  rows.per.group22  <- aggregate(rep(1, length(DTza1aa$loss)),
                               by=list(DTza1aa$month, DTza1aa$year, DTza1aa$county), sum)
  
  rows.per.group$Group.1 <- tolower(rows.per.group$Group.1)
  rows.per.group$month <- sapply(rows.per.group$Group.1, simpleCap)
  
  rows.per.group$monthcode <- match(rows.per.group$month, month.abb)
  
  rows.per.group$monthcode2 <- sprintf("%02d", rows.per.group$monthcode)
  
  rows.per.group$yearmonth <- as.Date(paste(rows.per.group$Group.2, rows.per.group$monthcode2, "01", sep="-"))
  rows.per.group$yearmonth2 <- paste(rows.per.group$Group.2, rows.per.group$monthcode2, sep=".")
  
  rows.per.group <- rows.per.group[with(rows.per.group, order(yearmonth)), ]
  
  
  
  rows.years <- as.data.frame(rep(c(1989:2015), each = 12))
  
  rows.months <- as.data.frame(rep(c(1:12), times = 27))
  
  rows1 <- cbind(rows.years, rows.months)
  colnames(rows1) <- c("year", "monthcode")
  
  
  rows1$monthcode2 <- sprintf("%02d", rows1$monthcode)
  rows1$yearmonth <- paste(rows1$year, rows1$monthcode2, "01", sep="-")
  rows1$yearmonth2 <- paste(rows1$year, rows1$monthcode2, sep=".")
  
  
  rowsfinal <- join(rows1, rows.per.group, by = "yearmonth")
  
  rowsfinal$x[is.na(rowsfinal$x)] <- 0
  #rowsfinal <- subset(rowsfinal, month == NA)
  
  rowsfinal_max <- max(rowsfinal$x)
  rowsfinal_min <- min(rowsfinal$x)
  
  rowsfinal_length <- (nrow(rowsfinal))
  library(Hmisc)

  rowsfinal$month <- toupper(rowsfinal$month)
  rows.per.group22$Group.1 <- toupper(rows.per.group22$Group.1)
  
  
  tt <- colorRampPalette(c("light blue", "blue", "red"))
  len5a_out <- tt(rowsfinal_max)
  }}  
  
months_all <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  
  for (jj in months_all) {

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

  



DT2 <- subset(DT, damagecause == lll) #---set for drought!!!

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
DT2loss$county <- trimws(DT2loss$county)
m <- merge(counties, DT2loss, by='county')
#names(m)[7] <- "acres" 


m$loss[is.na(m$loss)] <- 0
#m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
#m$acres[is.na(m$acres)] <- 0

#------------


#shapefile(m)
#--begin polygon work
#length(na.omit(m$LOSS))
nrowcounties <- nrow(counties)
tt_county <- colorRampPalette(c("white", "blue", "red"))(nrowcounties) 
tt_county2 <- colorRampPalette(c("white", "blue", "red"))

tt <- colorRampPalette(c("light blue", "blue", "red"))
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




#len4 <- tt(nrow(as.data.frame(subset(m, loss > 0))))
#--create a color vector for ALL commodity drought values for all years.  used for the gradient legend and coloring
#za <- as.data.frame(read.csv(i, strip.white = TRUE))
#DTz <- data.table(za)
DTz_rev <- subset(DTz1_base, damagecause == lll) 
DTz <- subset(DTz_rev, commodity == kkk)

if (DTz != 0) {

DTzsum <- as.data.frame(aggregate(DTz$loss~DTz$month+DTz$year+DTz$county, DTz, sum))
colnames(DTzsum) <- c("month", "year", "county", "loss")
DTzsum_max <<- max(DTzsum$loss)


DTza <- as.data.frame(subset(DTz, loss > 0))
DTza <- subset(DTza, monthcode != 0)
DTzmax <- max(DTza$loss)
DTzmin <- min(DTza$loss)
DTzlen <- (nrow(DTza)/10)
DTzlen_county <- length(counties)

DTza_sorted <- sort(DTza$loss)
DTza_len <- length(DTza_sorted)

len4a <- tt((DTzmax - DTzmin))
len44a <- tt(DTzsum_max)

rowsfinal$x[is.na(rowsfinal$x)] <- 0

len5a <- tt((max(rowsfinal$x) - min(rowsfinal$x)))
len55a <- tt(max(rowsfinal$x))

#len4a_out <- tt((DTzmax - DTzmin)/DTzlen)

#----------
tt_DTza <- colorRampPalette(c("light blue", "blue", "red"))( DTza_len) 
DTza_s1 <- cbind (tt_DTza, DTza_sorted)

len4ab <- tt(DTzlen_county)
#len4abc <- tt(DTza_sorted)
#----

orderedcolors2 <- tt(length(m$loss))[order(order(m$loss))]
#orderedcolors3 <- tt(length(m$acres))[order(order(m$acres))]
#newframe <- data.frame(m$LOSS)
m[["loss"]][is.na(m[["loss"]])] <- 0
#m[["acres"]][is.na(m[["acres"]])] <- 0 

jjx <- tolower(jj)
jjx <- sapply(jjx, simpleCap)
jjx <- match(jjx, month.abb)

rowsfinal$month2 <- month.abb[ rowsfinal$month ]

rows.per.group222 <- subset(rows.per.group22, Group.2 == j)
rows.per.group222 <- subset(rows.per.group222, Group.1 == jj)
colnames(rows.per.group222) <- c("month", "year", "county", "x")
rows.per.group222$county <- trimws(rows.per.group222$county)

names(m)[1] <- "county"
mnew <- merge(m, rows.per.group222, by = "county")

mnew$x[is.na(mnew$x)] <- 0
mnew2 <- tt_county2(mnew$x)


xx <- unique(counties$county)
newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
vect <- as.vector(DT2loss$county)
#vect <- as.vector(mnew$county[mnew$x!=0])

for (ll in vect) {
  for (kk in xx){
    comp <- compare(kk,ll)
    if (isTRUE(comp)) {
    rownumber <- which(xx == kk)
    #print("yes this worked, added 0")
    verm <- subset(DTzsum, year == j)
    verm <- subset(DTzsum, month == jj)
    verm <- subset(DTzsum, county == kk)
    DT2zsum_loss <- sum(verm$loss)
    which(DT2loss ==DT2zsum_loss)
    tutu <- DT2loss$county == kk
    tutu2 <- DT2loss[tutu]
    if (tutu2$loss > 0) {
    newmatrix[rownumber,] <- len44a[tutu2$loss]
    #newmatrix[yy,] <- orderedcolors2[yy]
    } else {
      newmatrix[rownumber,] <- len44a[1]
    }
    }}}

newmatrix[is.na(newmatrix)] <- 0


newmatrix2 <- matrix(data = NA, nrow = leng, ncol = 1)
#vect <- mnew$county[mnew$x!=0]
vect <- as.vector(DT2loss$county)

for (ll in vect) {
  for (kk in xx){
    comp <- compare(kk,ll)
    if (isTRUE(comp)) {
      rownumber <- which(xx == kk)
      #print("yes this worked, added 0")
      if (mnew$x != 0) {
        newmatrix2[rownumber,] <- len55a[mnew$x]
        #newmatrix[yy,] <- orderedcolors2[yy]
      } else {
        newmatrix2[rownumber,] <- len55a[1]
      }
    }}}

  
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

monthlist <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
#monthlist2 <-match(monthlist,month.abb)

n <- match(jj, monthlist)
nn <- str_pad(n, 2, pad = "0")

#orderedcolors2 <- colorRampPalette(c(44))
#m <- cbind(m$LOSS, newmatrix)
#midpoints <- barplot(mz$LOSS)
#png(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png2/", j, "_", nn, "_", kkk,  "_plot.png", sep=""))


par(mar=c(3,3,3,2)+1)
#par(mfrow=c(1,1))
#layout(matrix(c(1,2,3,3),2, 2, byrow=TRUE))
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE),
       widths=c(2,1), heights=c(2,1))

#par(mar=c(6,3,3,2)+1)
#par(mfrow=c(2,2))
#layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
#--turn image horizontal


#------------------------begin barplot for animation

#yeardir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/summaries/", sep="")
#ix <- paste("2001_2015_usda_gridmet_", "Idaho", sep="")
#setwd(yeardir2)
#DT2x <- as.data.frame(read.csv(ix, strip.white = TRUE))
DT2x <- DTz1_base
#---creating vector for every year and month for full barchart with 0 months.

N1 = 1989
N2 = 2015
N3 = 27
newmatrixcomm <- matrix(NA, nrow=N3 * 12, ncol=1)
nmc <- c(1:(N3*12))
mon <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") 
yr <- c(1989:2015)

tt <- 1
for (iii in yr) {
  for (jjj in mon) {
    newmatrixcomm[tt,] <- paste(iii, ".", jjj, sep="")
    tt <- tt + 1
  }
  
}

#----------------







listzz <- newmatrixcomm
#---fix kkk spaces
#kkkk <- gsub(" ", "", kkk, fixed = TRUE)
kkkk <- gsub("\\s+","\\",kkk)

#--below - you need to include those months that have no data in the barplot.  unizz only has populated values.  How to get a column of all 
#listzz <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/month_png/", "drought/", kkkk, sep=""))
#lisss <- length(listzz)
#newframez <- t(data.frame(strsplit(listzz, "\\.")[1:lisss]))
#newframez2 <- t(data.frame(strsplit(newframez[,1], "\\_")[1:lisss]))
#allunizz <- data.frame(sort(unique(paste(newframez2[,1], ".", newframez2[,2], sep=""))))
colnames(listzz) <- c("yearmonth")
#need to use allunizz with unizz to create a vector of loss with 0 values as needed

#monthpad <- str_pad(newframez2[,2], 2, pad = "0")
#newframez4 <- cbind(newframez2, monthpad)

#newframez5 <- data.frame(sort(paste(newframez4[,1], ".", newframez4[,5], sep="")))
#colnames(newframez5) <- c("yearmonth")

DT2x$commodity <- gsub(" ", "", DT2x$commodity, fixed = TRUE)

#unizz <- sort(unique(paste(DT2x$year, ".", DT2x$monthcode, sep="")))

DT2x <- subset(DT2x, damagecause == lll )
DT2x <- subset(DT2x, commodity == kkkk)
DT2x <- data.table(DT2x)
DT2x <- subset(DT2x, monthcode != 0)

##--merge

#DT3x <- merge(newframez5, DT2x, by='yearmonth')

DT2x$yearmonth <- paste(DT2x$year, ".", str_pad(DT2x$monthcode, 2, pad = "0"), sep="")

DT2lossx <- DT2x[,list(loss=sum(loss)), by = yearmonth]

nxx <- merge(DT2lossx, listzz, by='yearmonth', all=TRUE)
nxx[["loss"]][is.na(nxx[["loss"]])] <- 0

#----

monthlist <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
#monthlist2 <-match(monthlist,month.abb)
n <- match(jj, monthlist)

yearmonth2 <- paste(j, ".", str_pad(n, 2, pad = "0"), sep="")

thenum <- which(nxx ==yearmonth2, arr.ind=TRUE)
dev.off()



#-------end data construction for bar plot for animation

midpoint_loss <- (max(m$loss) + min(m$loss)/2)
#midpoint_acres <- (max(m$acres) + min(m$acres)/2)

#b <- barplot(DT7$LOSS, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix)
#text(bb, midpoint_loss, labels=mz$loss, srt=90)
#plot(m, col = newmatrix, main = paste(scen_state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))

lll <- gsub(" ", "_", lll)
lll <- gsub("/", "_", lll)
lll <- gsub("\\(", "_", lll)
lll <- gsub("\\)", "_", lll)
lll <- gsub(" ", "_", lll)

png(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png2/", j, "_", nn, "_", kkkk, "_", lll,  "_plot.png", sep=""), width=1200, height=1000)
par(mar=c(3,3,3,2)+1)
layout(matrix(c(1,1,2,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5), 3, 6, byrow = TRUE))

plot(m, col = newmatrix, main = paste(scen_state,  " ", jj, " ", j, " ", kkk, " ", lll, sep=""), sub=paste("Total Monthly Loss: $", sum(m$loss), sep=""), col.sub="blue", cex.sub=2.5, cex.main = 2, line=1)

legend_image <- as.raster(matrix(rev(len4a_out), ncol=1))
plot(c(0,3),c(0,DTzsum_max),type = 'n', axes = F,xlab = '', ylab = '', main = 'Loss $ Range', cex.main = 1.5)
#text(x=1.5, y = c(0,5), labels = c(0,5))
text(x=2.2, y = seq(0,DTzsum_max,l=5), labels = seq(0,DTzsum_max,l=5), cex = 1.5)
rasterImage(legend_image, 0, 0, .45, DTzsum_max)

barplot(mnew$x, names = mnew$county, col=newmatrix, las=3, cex.axis=1.5, cex.main = 2, cex.names=1.5, ylim=c(0,max(rows.per.group22$x)), main=paste(scen_state,  " ", jj, " ", j, " ", kkk, " Claim Counts", sep=""))
mtext(paste("Total Claim Counts: ", sum(mnew$x), sep=""), col="blue", cex = 1.5, line=-1)
mtext(paste("**color coding is for loss, matches map**", sep=""), col="black", cex = 1, line=-2.2)


##legend_image <- as.raster(matrix(rev(len5a_out), ncol=1))
##plot(c(0,3),c(0,rowsfinal_max),type = 'n', axes = F,xlab = '', ylab = '', main = "Claim Count Range: \n 1989-2015", cex.main = 1.5)
#text(x=1.5, y = c(0,5), labels = c(0,5))
##text(x=2.2, y = seq(0,rowsfinal_max,l=5), labels = seq(0,rowsfinal_max,l=5), cex = 1.5)
##rasterImage(legend_image, 0, 0, .45, rowsfinal_max)



par(mar=c(14,12,5,5))
par(lty = 1) 

#--fix barplot xaxis names
baryears <- c("1989", "", "", "", "", "", "", "", "", "", "", "",
              "1990", "", "", "", "", "", "", "", "", "", "", "",
              "1991", "", "", "", "", "", "", "", "", "", "", "",
              "1992", "", "", "", "", "", "", "", "", "", "", "",
              "1993", "", "", "", "", "", "", "", "", "", "", "",
              "1994", "", "", "", "", "", "", "", "", "", "", "",
              "1995", "", "", "", "", "", "", "", "", "", "", "",
              "1996", "", "", "", "", "", "", "", "", "", "", "",
              "1997", "", "", "", "", "", "", "", "", "", "", "",
              "1998", "", "", "", "", "", "", "", "", "", "", "",
              "1999", "", "", "", "", "", "", "", "", "", "", "",
              "2000", "", "", "", "", "", "", "", "", "", "", "",
              "2001", "", "", "", "", "", "", "", "", "", "", "", 
              "2002", "", "", "", "", "", "", "", "", "", "", "", 
              "2003", "", "", "", "", "", "", "", "", "", "", "", 
              "2004", "", "", "", "", "", "", "", "", "", "", "", 
              "2005", "", "", "", "", "", "", "", "", "", "", "", 
              "2006", "", "", "", "", "", "", "", "", "", "", "", 
              "2007", "", "", "", "", "", "", "", "", "", "", "", 
              "2008", "", "", "", "", "", "", "", "", "", "", "", 
              "2009", "", "", "", "", "", "", "", "", "", "", "", 
              "2010", "", "", "", "", "", "", "", "", "", "", "", 
              "2011", "", "", "", "", "", "", "", "", "", "", "", 
              "2012", "", "", "", "", "", "", "", "", "", "", "", 
              "2013", "", "", "", "", "", "", "", "", "", "", "", 
              "2014", "", "", "", "", "", "", "", "", "", "", "", 
              "2015", "", "", "", "", "", "", "", "", "", "", "")

nxxsd <- sd(nxx$loss)
cols <- ifelse(nxx$loss > nxxsd, "red","blue")
#legend.col(col = len4a, lev = m$loss)

par(adj = 0)
bar <- barplot(nxx$loss, space = 0, col=cols, xlab="", ylab="", main = paste(scen_state,  " ", kkk, " for ", lll, ",", " Total loss ($) by month, 1989 - 2015", sep="")
               , names.arg = baryears, las = 2, cex.main = 2, cex.names = 2, cex.axis = 2)

par(adj = .5)
title(ylab="Commodity loss ($)", line=9, cex.lab=2)
title(xlab="Commodity loss totals ($) 1989 - 2015", line=8, cex.lab=2)

#legend("topleft",
#       lty = 5, bty = "n", cex = 2, col = c("red"),
#       legend = c("current month and year"))






#axis(1, xaxp=c(1, 15, 19), las=2)
      
 #-plot the bar plot for the animation beside the map
abline(v=(bar[thenum[1]]), col="red", lty=2)


countsd <- sd(rowsfinal$x)
cols2 <- ifelse(rowsfinal$x > countsd, "red","blue")
bar <- barplot(rowsfinal$x, space = 0, col=cols2, xlab="", ylab="", main = paste(scen_state,  " ", kkk, " for ", lll, ",", " Total claim counts by month, 1989 - 2015", sep="")
               , names.arg = baryears, las = 2, cex.main = 2, cex.names = 2, cex.axis = 2)

title(ylab="Commodity loss in dollars ($)", line=9, cex.lab=2)
title(xlab="Commodity loss totals ($) 1989 - 2015", line=8, cex.lab=2)

#axis(1, xaxp=c(1, 15, 19), las=2)

#-plot the bar plot for the animation beside the map
abline(v=(bar[thenum[1]]), col="red", lty=2)

#legend("topleft",
#       lty = 5, bty = "n", cex = 2, col = c("red"),
#       legend = c("current month and year"))

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
                     
                     #za <- as.data.frame(read.csv(i, strip.white = TRUE))
                     DTz <- data.table(DTz1_base)
                     DTz <- subset(DTz, damagecause == lll) 
                     DTz <- subset(DTz, commodity == kkk)
                     DTza <- as.data.frame(subset(DTz, loss > 0))
                     DTzmax <- max(1)
                     DTzmin <- min(0)
                     DTzlen <- (nrow(DTza)/5)
                     
                     #len4a_out <- tt((DTzmax - DTzmin)/DTzlen)
                     
                     #if (is.data.frame(DTz) && nrow(DTz)==0) {
                      # DTzsum_max <<- 0
                     #} else {

                     #DTzsum <- as.data.frame(aggregate(DTz$loss~DTz$month+DTz$year+DTz$county, DTz, sum))
                     #colnames(DTzsum) <- c("month", "year", "county", "loss")
                     #DTzsum_max <<- max(DTzsum$loss)
                     #}
                     
                     
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
        
                    
                     
                     
                     #------------------------begin barplot for animation
                     
                     #yeardir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/summaries/", sep="")
                     #ix <- paste("2001_2015_usda_gridmet_", "Idaho", sep="")
                     #setwd(yeardir2)
                     #DT2x <- as.data.frame(read.csv(ix, strip.white = TRUE))
                     #DT2x <- subset(DT2x, month = "JAN" & "FEB")
    
                     
                     DT2x <- DTz1_base
                     #---creating vector for every year and month for full barchart with 0 months.
                  
                     N1 = 1989
                     N2 = 2015
                     N3 = 27
                     
                     kkkk <- gsub(" ", "", kkk, fixed = TRUE)
                     
                     #---creating vector for every year and month for full barchart with 0 months.
                     
                     newmatrixcomm <- matrix(NA, nrow=N3 * 12, ncol=1)
                     nmc <- c(1:(N3*12))
                     mon <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") 
                     yr <- c(1989:2015)
                     
                     tt <- 1
                     for (iii in yr) {
                       for (jjj in mon) {
                          newmatrixcomm[tt,] <- paste(iii, ".", jjj, sep="")
                       tt <- tt + 1
                       }
                       
                     }
                     
                     #----------------
                     
                     
                     listzz <- newmatrixcomm
                     #--below - you need to include those months that have no data in the barplot.  unizz only has populated values.  How to get a column of all 
                     #listzztest <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/month_png/", "drought/", kkkk, sep=""))
                     #lisss <- length(listzz)
                     #newframez <- t(data.frame(strsplit(listzz, "\\.")[1:lisss]))
                     #newframez2 <- t(data.frame(strsplit(newframez[,1], "\\_")[1:lisss]))
                     #allunizz <- data.frame(sort(unique(paste(newframez2[,1], ".", newframez2[,2], sep=""))))
                     #colnames(allunizz) <- c("yearmonth")
                     #need to use allunizz with unizz to create a vector of loss with 0 values as needed
                     
                     #monthpad <- str_pad(newframez2[,2], 2, pad = "0")
                     #newframez4 <- cbind(newframez2, monthpad)
                     
                     #newframez5 <- data.frame(sort(paste(newframez4[,1], ".", newframez4[,5], sep="")))
                     colnames(listzz) <- c("yearmonth")
                     
                     DT2x$commodity <- gsub(" ", "", DT2x$commodity, fixed = TRUE)
                     
                     #unizz <- sort(unique(paste(DT2x$year, ".", DT2x$monthcode, sep="")))
                     kkkk <- gsub("\\s+","\\",kkk)
                     
                     
                     DT2x <- subset(DT2x, damagecause == lll )
                     DT2x <- subset(DT2x, commodity == kkkk)
                     DT2x <- data.table(DT2x)
                     DT2x <- subset(DT2x, monthcode != 0)
                     ##--merge
                     
                     #DT3x <- merge(newframez5, DT2x, by='yearmonth')
                     
                     DT2x$yearmonth <- paste(DT2x$year, ".", str_pad(DT2x$monthcode, 2, pad = "0"), sep="")
                     
                     DT2lossx <- DT2x[,list(loss=sum(loss)), by = yearmonth]
                     
                     nxx <- merge(DT2lossx, listzz, by='yearmonth', all=TRUE)
                     nxx[["loss"]][is.na(nxx[["loss"]])] <- 0
                     
                     #----
                     
                     monthlist <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
                     #monthlist2 <-match(monthlist,month.abb)
                     n <- match(jj, monthlist)
                     
                     yearmonth2 <- paste(j, ".", str_pad(n, 2, pad = "0"), sep="")
                     
                     thenum <- which(nxx ==yearmonth2, arr.ind=TRUE)
                     
                  
                     
                     #-------end data construction for bar plot for animation

                                  
                     plotmonth <- month.abb[jj]
                     plotyear <- j
                     #plotcommodity <- x$commodity[1]
                     
                     monthlist <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
                     #monthlist2 <-match(monthlist,month.abb)
                     n <- match(jj, monthlist)
                     nn <- str_pad(n, 2, pad = "0")
                     
                     #orderedcolors2 <- colorRampPalette(c(44))
                     #m <- cbind(m$LOSS, newmatrix)
                     #midpoints <- barplot(mz$LOSS)
                     
                     lll <- gsub(" ", "_", lll)
                     lll <- gsub("/", "_", lll)
                     lll <- gsub("\\(", "_", lll)
                     lll <- gsub("\\)", "_", lll)
                     lll <- gsub(" ", "_", lll)
                     
                     png(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png2/", j, "_", nn, "_", kkkk, "_", lll,  "_plot.png", sep=""), width=1200, height=1000)
                     #par(mar=c(1,1,1,1))
                     #par(mar=c(3,3,3,2)+1)
                     #par(mfrow=c(1,1))
                     #layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE)) #--changes from 1, 2, to 2, 1 for additional plot
                     
                     
                     #par(mfrow=c(1,1))
                     #layout(matrix(c(1,2,3,3),2, 2, byrow=TRUE))
                     
                     #par(mar=c(3,3,3,2)+1)
                     #layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE),
                     #      widths=c(2,1), heights=c(2,1))
                     
                     
                     #par(mar=c(3,3,3,2)+1)
                     #layout(matrix(c(1,2,3,3,3,3), 3, 2, byrow = TRUE),
                    #        widths=c(2,1), heights=c(2,1))
                     
                     par(mar=c(3,3,3,2)+1)
                     layout(matrix(c(1,1,2,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5), 3, 6, byrow = TRUE))
                     
                     #layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
                     #--turn image horizontal
                     
                     #plotmonth <- month.abb[x$monthcode[1]]
                     #plotyear <- x$year[1]
                     #plotcommodity <- x$commodity[1]
                     
                     midpoint_loss <- (max(m$loss) + min(m$loss)/2)
                     #midpoint_acres <- (max(m$acres) + min(m$acres)/2)
                     
                     #b <- barplot(m$loss, names.arg = m$loss, las=2, col = newmatrix)
                     #text(bb, midpoint_loss, labels=mz$loss, srt=90)
                     #plot(m, col = newmatrix, main = paste(scen_state,  " ", jj, " ", j, " ", kkk, "\n", "monthly total loss: $", "0", ":", " monthly drought claims:", "0", sep=""), cex.main = 2)
                     
                     plot(m, col = newmatrix, main = paste(scen_state,  " ", jj, " ", j, " ", kkk, " ", lll, sep=""), sub=paste("Total Monthly Loss: $", sum(m$loss), sep=""), col.sub="blue", cex.sub=2.5, cex.main = 2, line=1)
                     
                     
                     
                     
                     #legend_image <- as.raster(matrix(rev(len4a_out), ncol=1))
                     #plot(c(0,2),c(0,DTzmax),type = 'n', axes = F,xlab = '', ylab = '', main = 'legend title')
                     ##text(x=1.5, y = c(0,5), labels = c(0,5))
                     #text(x=1.5, y = seq(0,DTzmax,l=5), labels = seq(0,DTzmax,l=5))
                     #rasterImage(legend_image, 0, 0, 1, 1 )
                     
                     if (DTzsum_maxz == 0) {
                       
                       legend_image <- as.raster(matrix(rev(len4a_out), ncol=1))
                       plot(c(0,3),c(0,DTzmax1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Loss $ Range', cex.main=1.5)
                       #text(x=1.5, y = c(0,5), labels = c(0,5))
                       text(x=1.5, y = seq(0,DTzmax1,l=5), labels = seq(0,DTzmax1,l=5), cex = 1.5)
                       rasterImage(legend_image, 0, 0, .35, DTzmax1)
                    
                     } else {
                       
                       legend_image <- as.raster(matrix(rev(len4a_out), ncol=1))
                       plot(c(0,3),c(0,DTzsum_maxz),type = 'n', axes = F,xlab = '', ylab = '', main = 'Loss $ Range', cex.main = 1.5)
                       text(x=1.5, y = c(0,5), labels = c(0,5), cex = 2)
                       text(x=1.5, y = seq(0,DTzsum_maxz,l=5), labels = seq(0,DTzsum_maxz,l=5), cex = 1.5)
                       rasterImage(legend_image, 0, 0, .35, DTzsum_maxz)
                       
                     }
                    
                     #plot(m, col = newmatrix, main = paste(scen_state, " crop loss $ \n", " ", jj, " ", j, "\n", kkk, sep=""))
                     #legend.col(col = orderedcolors2a, lev = m$loss)

                     
                     barplot(m$loss, names = mnew$county, col=newmatrix, las=3, cex.main = 2, cex.names=1.5, ylim=c(0,max(rows.per.group22$x)), cex.axis = 2, main=paste(scen_state,  " ", jj, " ", j, " ", kkk, " Claim Counts", sep=""))
                     mtext(paste("Total Claim Counts: ", m$loss, sep=""), col="blue", cex = 1.5, line=-1)
                     mtext(paste("**color coding is for loss, matches map**", sep=""), col="black", cex = 1, line=-2.2)
                     
                       
                       par(mar=c(14,12,5,5))
                       par(lty = 1) 
                       
                       #--fix barplot xaxis names
                       baryears <- c("1989", "", "", "", "", "", "", "", "", "", "", "",
                                     "1990", "", "", "", "", "", "", "", "", "", "", "",
                                     "1991", "", "", "", "", "", "", "", "", "", "", "",
                                     "1992", "", "", "", "", "", "", "", "", "", "", "",
                                     "1993", "", "", "", "", "", "", "", "", "", "", "",
                                     "1994", "", "", "", "", "", "", "", "", "", "", "",
                                     "1995", "", "", "", "", "", "", "", "", "", "", "",
                                     "1996", "", "", "", "", "", "", "", "", "", "", "",
                                     "1997", "", "", "", "", "", "", "", "", "", "", "",
                                     "1998", "", "", "", "", "", "", "", "", "", "", "",
                                     "1999", "", "", "", "", "", "", "", "", "", "", "",
                                     "2000", "", "", "", "", "", "", "", "", "", "", "",
                                     "2001", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2002", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2003", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2004", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2005", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2006", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2007", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2008", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2009", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2010", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2011", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2012", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2013", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2014", "", "", "", "", "", "", "", "", "", "", "", 
                                     "2015", "", "", "", "", "", "", "", "", "", "", "")
                       
                       nxxsd <- sd(nxx$loss)
                       cols <- ifelse(nxx$loss > nxxsd, "red","blue")
                       #legend.col(col = len4a, lev = m$loss)
                       
                       
                       par(adj=0)
                       bar <- barplot(nxx$loss, space = 0, col=cols, xlab="", ylab="", main = paste(scen_state,  " ", kkk, " for ", lll, ", ", "Total loss by month, 1989 - 2015", sep="")
                                      , names.arg = baryears, las = 2, cex.main = 2, cex.names = 2, cex.axis = 2)
                       par(adj = .5)
                       title(ylab="Commodity loss ($)", line=9, cex.lab=2)
                       title(xlab="Commodity loss totals ($) 1989 - 2015", line=8, cex.lab=2)
                       
                       #legend("topleft",
                      #        lty = 5, bty = "n", cex = 2, col = c("red"),
                      #        legend = c("current month and year"))
                       
                       
                     #bar <- barplot(nxx$loss) #-plot the bar plot for the animation beside the map
                     abline(v=(bar[thenum[1]]), col="red", lty=2)
                     
                     
                     
                     countsd <- sd(rowsfinal$x)
                     cols2 <- ifelse(rowsfinal$x > countsd, "red","blue")
                     bar <- barplot(rowsfinal$x, space = 0, col=cols2, xlab="", ylab="", main = paste(scen_state,  " ", kkk, " for ", lll, ",", " Total claim counts by month, 1989 - 2015", sep="")
                                    , names.arg = baryears, las = 2, cex.main = 2, cex.names = 2, cex.axis = 2)
                     
                     title(ylab="Commodity claim counts", line=9, cex.lab=2)
                     title(xlab="Commodity claim counts, 1989 - 2015", line=8, cex.lab=2)
                     
                     #axis(1, xaxp=c(1, 15, 19), las=2)
                     
                     #-plot the bar plot for the animation beside the map
                     abline(v=(bar[thenum[1]]), col="red", lty=2)    
                     
                     #legend("topleft", inset=c(0,-.2),
                      #      lty = 2, bty = "n", cex = 2, col = c("red"),
                      #      legend = c("current month and year"))
                     
                     dev.off()
                     #bb <- barplot(DT7$ACRES, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix_acres)
                     #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
                     #plot(m, col = newmatrix_acres, main = paste(scen_state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))                     
                     
} }}}}}


listz <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png2/", lll, sep=""))
subset(listz, grepl("*.png",listz))
liss <- length(listz)
newframe <- t(data.frame(strsplit(listz, "\\_")[1:liss]))
uniz <- unique(newframe[,3])

       



for (i in uniz) {
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png/", lll, sep="")) 
  system(paste("mkdir ", i, sep=""))
  system(paste("mv *_", i, "_*", " /dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png/", lll, "/", i, sep=""))
}

