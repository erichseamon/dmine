#------------------------------------------------------------------------#
# TITLE:        dmine-gridmet-monthly_rev6_threestate.R
# AUTHOR:       Erich Seamon
# INSTITUITON:  College of Natural Resources
#               University of Idaho
# DATE:         May-June. 2016
# STAGE:        DMINE extraction and subsetting of UI GRIDMET data for machine
#               learning analysis
#
# COMMENTS:     The purpose of this script is to allow a user to input a range
#               of years, and a lat/long bounding box - and the code will extract all variables
#               for that time period and location.  The second portion of the script steps thru
#               all counties within the US (or a subset), and extracts daily data from each county
#               and averages it for the month.  Each monthly average, for all variables, 
#               for each county, by the month, is then put into a matrix for machine learning analysis.
#                
#               More on the dmine design: dmine.io
#------------------------------------------------------------------------#


#----SETUP SECTION----------------------------------------------------#

#--clear the variable list and set the working directory----#

rm(list = ls()) #--clears all lists------#
cat("\14")

#----supress warnings

options(warn=0)

#----set packages--------------------------------------------------------#

library("ncdf")
library("raster")
library("sp")
library("rgeos")
library("rgdal")
library("proj4")
library("RNetCDF")
library("ncdf4")
library("RColorBrewer")
library("raster")
library("rasterVis")
library("latticeExtra")
library("maptools")
library("parallel")
library("Evapotranspiration")
#library("plyr")
library("data.table")
library("sirad")
library("rgeos")
library("MASS")
library("stringr")
library("car")
library("sp")
detach(package:tidyr)

library(compare)
library(tidyr)
#detach(package:sirad)
#detach(package:rasterVis)
#detach(package:raster)
library(graphics)


#rm(list = ls()) #--clears all lists------#
#cat("\14")

#---legend function

print("Creating Legend function...")

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
N1 <- "2001"
N2 <- "2015"
state1 <- "Washington"
state2 <- "Oregon"
state3 <- "Idaho"


statelist <- c(state1, state2, state3)
RSCENARIO1 <- sample(50000:100000, 1, replace=F)
scen <- paste("multi_state_scenario_", RSCENARIO1, sep="")

#dmineplots <- function(scen_state, N1, N2, dcause, Kommodity) {

dirname <- paste("/dmine/data/USDA/agmesh-scenarios/", sep="")
setwd(dirname)
system(paste("mkdir ", scen))
dirname2 <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep="")
setwd(dirname2)
system(paste("mkdir ", "month_png"))
dirname3 <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_png", sep="")
setwd(dirname3)
system(paste("mkdir ", "Drought"))

#-------------

#scen_state = "Idaho"
#N1 = "2001"
#N2 = "2015"
#dcause = "Drought"
#Kommodity = "Wheat"

#--add summary USDA state data together

onedata <- paste("/dmine/data/USDA/agmesh-scenarios/", state1, "/summaries/", N1, "_", N2, "_usda_gridmet_", state1, sep="")
twodata <- paste("/dmine/data/USDA/agmesh-scenarios/", state2, "/summaries/", N1, "_", N2, "_usda_gridmet_", state2, sep="")
threedata <- paste("/dmine/data/USDA/agmesh-scenarios/", state3, "/summaries/", N1, "_", N2, "_usda_gridmet_", state3, sep="")

stater1 <- as.data.frame(read.csv(onedata, strip.white = TRUE))
stater2 <- as.data.frame(read.csv(twodata, strip.white = TRUE))
stater3 <- as.data.frame(read.csv(threedata, strip.white = TRUE))

threestate_summary <- rbind(stater1, stater2, stater3)





setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

counties <- counties[grep(paste(statelist[1], "|", statelist[2], "|", statelist[3], sep=""), counties@data$STATE_NAME),]
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
#counties <- subset(counties, STATE_NAME %in% scen_state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_png", sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month", sep=""))
#maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/netcdf/pdsi_apr_", N1, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_positive/", sep=""))
#system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
#uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_positive/", sep=""))

#--set tt outside of loop below
tt1 <- colorRampPalette(c("white", "blue", "red"))
#setwd(yeardir)
#it <- paste(N1, "_", N2, "_usda_gridmet_", scen_state, sep="")
#zaa <- as.data.frame(read.csv(it, strip.white = TRUE))
#DTz1 <- data.table(zaa)
#DTz1 <- subset(DTz1, damagecause == dcause) 
#DTz <- subset(DTz, commodity == kkk)
#DTza1 <- as.data.frame(subset(DTz1, loss > 0))
#DTzmax1 <- max(DTza1$loss)
#DTzmin1 <- min(DTza1$loss)
#DTzlen1 <- (nrow(DTza1)/10)

#len4a_out <- tt1((DTzmax1 - DTzmin1)/DTzlen1)

#---------------

years <- c(N1:N2)

for (j in years) {
  #setwd(yeardir)
  
  i <- threestate_summary
  x <- threestate_summary
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
  x <- subset(x, monthcode != "NA")
  x <- subset(x, commoditycode != 88)
  
  x <- subset(x, year == j)
  
  
  uniquecomm <- unique(x$commodity)
  months <- unique(x$month)
  #-------------
  #uniquecomm <- "All Other Crops"
  for (kkk in uniquecomm) {
    
    
    tt1 <- colorRampPalette(c("white", "blue", "red"))
    setwd(yeardir)
    #it <- paste(N1, "_", N2, "_usda_gridmet_", scen_state, sep="")
    #zaa <- as.data.frame(read.csv(it, strip.white = TRUE)) #commented out testing
    #DTz1 <- data.table(zaa) #commented out to test issues with kkk barley only working
    
    xgee <- threestate_summary
    xgee <- subset(xgee, monthcode != "NA")
    xgee <- subset(xgee, commoditycode != 88)
    DTz1 <- data.table(xgee) #added to confirm consistency with below code that uses x
    DTz1 <- subset(DTz1, year == j)
    
    DTz1 <- subset(DTz1, damagecause == dcause) 
    DTz1 <- subset(DTz1, commodity == kkk)
    
    if (nrow(DTz1) == 0) {
      DTzmax1 <<- 1
      DTzmin1 <<- 0
    } else {
      
      DTzsumz <- as.data.frame(aggregate(DTz1$loss~DTz1$month+DTz1$year+DTz1$county+DTz1$state, DTz1, sum))
      colnames(DTzsumz) <- c("month", "year", "county", "state", "loss")
      DTzsum_maxz <<- max(DTzsumz$loss)
      
      
      #DTza1 <- as.data.frame(subset(DTz1, loss > 0))
      #DTzmax1 <<- max(DTza1$loss)
      #DTzmin1 <<- min(DTza1$loss)
      #DTzlen1 <<- (nrow(DTza1)/10)
      
      #len4a_out1 <<- tt1((DTzmax1 - DTzmin1)/DTzlen1)
      
    }
    
    
    
    
    
    
    
    
    for (jj in months) {
      
      #DT <- data.table(x) #added DTz1 to be consistent with above?
      
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
      
      #DT2 <- subset(DT, damagecause == dcause) #---set for drought!!!
      DT2 <- DTz1
      DT2 <<- subset(DT2, month == jj)
      #DT2 <- subset(DT2, commodity == kkk)
     
      if (nrow(DT2) != 0 ) {
        DT2$acres <- as.numeric(as.character(DT2$acres))
        #DT2 <- subset(DTnew3, commodity == input$commodity)
        #DT3 <- data.frame(DT2$acres, DT2$loss)
        #DT4 <- cbind(x, DT3)
        DT2loss <- DT2[,list(loss=sum(loss)), by = c("county", "state")]
        #DT2 <- DT2[, lapply(.SD, sum), by=list(county)]
        DT2acres <- DT2[,list(acres=sum(acres)), by = c("county", "state")]
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
        
        xyyy <- unique(DT2loss$state)
        DT2lossnew <- transform(DT2loss, newcol = state.name[match(DT2loss$state,state.abb)])
        DT2lossnew2 <- transform(DT2lossnew, newcol=paste(DT2lossnew$county, DT2lossnew$newcol, sep="_"))
        names(DT2lossnew2)[4] <- "county_state"

        counties3 <- transform(counties, newcol=paste(counties$county, counties$STATE_NAME, sep="_"))
        counties4 <- spCbind(counties, counties3$newcol)
        names(counties4)[6] <- "county_state"
        
        m <- merge(counties4, DT2lossnew2, by="county_state")
        #names(m)[7] <- "acres" 
        
        #--stopped here!!!
        
        
        m$loss[is.na(m$loss)] <- 0
        #m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
        #m$acres[is.na(m$acres)] <- 0
        
        #------------
        
        
        #shapefile(m)
        #--begin polygon work
        #length(na.omit(m$LOSS))
        tt_county <- colorRampPalette(c("white", "blue", "red"))( 44) 
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
        
        
        
        #DTz <- data.table(za)
        #DTz <- data.table(x)
        #DTz <- subset(DTz, damagecause == dcause) 
        #DTz <- subset(DTz, commodity == kkk)
        #DTz <<- subset(DTz, month == jj)
        
        #it <- paste(N1, "_", N2, "_usda_gridmet_", scen_state, sep="")
        
        xgee <- threestate_summary
        #xgee <- as.data.frame(read.csv(it, strip.white = TRUE))
        xgee <- subset(xgee, monthcode != "NA")
        xgee <- subset(xgee, commoditycode != 88)
        DTz <- data.table(xgee) #added to confirm consistency with below code that uses x
        DTz <- subset(DTz, year == j)
        DTz <- subset(DTz, damagecause == dcause) 
        DTz <- subset(DTz, commodity == kkk)
        
        if (nrow(DTz) == 0) {
          DTzmax1 <<- 1
          DTzmin1 <<- 0
        } else {
        
        #changed below to DT2 from DTz on Nov 1, 2016
        DTzsum <- as.data.frame(aggregate(DTz$loss~DTz$month+DTz$year+DTz$county+DTz$state, DTz, sum))
        colnames(DTzsum) <- c("month", "year", "county", "state", "loss")
        DTzsum_max <<- max(DTzsum$loss)
        
        }
        
        #DTza <- as.data.frame(subset(DTz, loss > 0))
        #DTzmax <- max(DTza$loss)
        #DTzmin <- min(DTza$loss)
        #DTzlen <- (nrow(DTza)/10)
        #DTzlen_county <- length(counties)
        
        #DTza_sorted <- sort(DTza$loss)
        #DTza_len <- length(DTza_sorted)
        
        #len4a <- tt((DTzmax - DTzmin))
        len44a <- tt(DTzsum_max)
        
        #len4a_out <- tt((DTzmax - DTzmin)/DTzlen)
        
        #----------
        #tt_DTza <- colorRampPalette(c("light blue", "blue", "red"))( DTza_len) 
        #DTza_s1 <- cbind (tt_DTza, DTza_sorted)
        
        #len4ab <- tt(DTzlen_county)
        #len4abc <- tt(DTza_sorted)
        #----
        
        orderedcolors2 <- tt(length(m$loss))[order(order(m$loss))]
        #orderedcolors3 <- tt(length(m$acres))[order(order(m$acres))]
        #newframe <- data.frame(m$LOSS)
        m[["loss"]][is.na(m[["loss"]])] <- 0
        #m[["acres"]][is.na(m[["acres"]])] <- 0 
        
        #xyyy <- unique(DT2loss$state)
        #DT2lossnew <- transform(DT2loss, newcol = state.name[match(DT2loss$state,state.abb)])
        #DT2lossnew2 <- transform(DT2lossnew, newcol=paste(DT2lossnew$county, DT2lossnew$newcol, sep="_"))
        
        xxyy <- as.data.frame(counties[c("county", "STATE_NAME")])
        xxzz <- transform(xxyy, newcol=paste(counties$county, counties$STATE_NAME, sep="_"))
        xx <- unique(xxzz$newcol)
        colnames(DT2lossnew2) <- c("county", "state", "loss", "county_state")
        
        newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
        vect <- DT2lossnew2$county_state
        
        for (ll in vect) {
          for (kk in xx){
            comp <- compare(kk,ll)
            if (isTRUE(comp)) {
              rownumber <- which(xx == kk)
              #print("yes this worked, added 0")
              lltwo <- unlist(strsplit(ll, "_"))
              llcounty <- lltwo[1]
              llstate <- lltwo[2]
              llstate2 <- state.abb[match(llstate, state.name)]
              verm <- subset(DTzsum, year == j)
              verm <- subset(verm, month == jj)
              verm <- subset(verm, county == llcounty)
              verm <- subset(verm, state == llstate2)
              #DT2zsum_loss <- sum(verm$loss)
              #which(DT2loss == DT2zsum_loss)
              #tutu <- DT2loss$county == llcounty
              #tutu2 <- DT2loss[tutu]
              if (length(verm$loss) == 0) {
                newmatrix[rownumber,] <- len44a[1]
              } else {
              
              if (verm$loss > 0) {
                newmatrix[rownumber,] <- len44a[verm$loss]
                #newmatrix[yy,] <- orderedcolors2[yy]
              } else {
                newmatrix[rownumber,] <- len44a[1]
              }}
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
        
        monthlist <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
        #monthlist2 <-match(monthlist,month.abb)
        
        n <- match(jj, monthlist)
        nn <- str_pad(n, 2, pad = "0")
        
        #orderedcolors2 <- colorRampPalette(c(44))
        #m <- cbind(m$LOSS, newmatrix)
        #midpoints <- barplot(mz$LOSS)
        kkk <- gsub("\\s+","\\",kkk)
        #png(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_png/drought/", j, "_", nn, "_", kkk,  "_plot.png", sep=""))
        
        
        #par(mar=c(3,3,3,2)+1)
        #par(mfrow=c(1,1))
        #layout(matrix(c(1,2,3,3),2, 2, byrow=TRUE))
        #layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE),
        #       widths=c(2,1), heights=c(2,1))
        
        #par(mar=c(6,3,3,2)+1)
        #par(mfrow=c(2,2))
        #layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
        #--turn image horizontal
        
        
        #------------------------begin barplot for animation
        
        yeardir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep="")
        #ix <- paste(N1, "_", N2, "_usda_gridmet_", scen_state, sep="")
        #setwd(yeardir2)
        #DT2x <- as.data.frame(read.csv(ix, strip.white = TRUE))
        DT2x <- threestate_summary
        
        #---creating vector for every year and month for full barchart with 0 months.
        N1_num <- as.numeric(N1)
        N2_num <- as.numeric(N2)
        N3 = (N2_num - N1_num) + 1
        newmatrixcomm <- matrix(NA, nrow=N3 * 12, ncol=1)
        nmc <- c(1:(N3*12))
        mon <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") 
        yr <- c(N1:N2)
        
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
        kkkk <- gsub(" ", "", kkk, fixed = TRUE)
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
        
        DT2x <- subset(DT2x, damagecause == dcause )
        DT2x <- subset(DT2x, commodity == kkk)
        DT2x <- data.table(DT2x)
        
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
        
        #png(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_png/Drought/", j, "_", nn, "_", kkk,  "_plot.png", sep=""))
        #par(mar=c(3,3,3,2)+1)
        #layout(matrix(c(1,2,3,3,3,3), 3, 2, byrow = TRUE),
        #       widths=c(2,1), heights=c(2,1))
        
        
        #-------end data construction for bar plot for animation
        
        midpoint_loss <- (max(m$loss) + min(m$loss)/2)
        #midpoint_acres <- (max(m$acres) + min(m$acres)/2)
        
        #b <- barplot(DT7$LOSS, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix)
        #text(bb, midpoint_loss, labels=mz$loss, srt=90)
        #plot(m, col = newmatrix, main = paste(scen_state, "\n", "crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))
        
        #plot(m, col = newmatrix, main = paste(jj, " ", j, " ", kkk,  "\n", "monthly total loss: $", DT7$LOSS, "\n", " monthly drought claims:", nrow(DT2), sep=""))
        
        yeardir3 <- paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/shapefiles/", sep="")
        setwd(yeardir3)
        writeSpatialShape(m, paste(jj, "_", j, "_", kkk, "_", dcause, sep=""))
        
        #--changing out len4a_out for new tt
        #len5a_out <- as.raster(tt1(DTzsum_max))
        #legend_image <- as.raster(matrix(rev(len4a_out), ncol=1))
        #plot(c(0,3),c(0,DTzsum_max),type = 'n', axes = F,xlab = '', ylab = '', main = 'Loss $ Range: 2001-2015')
        #text(x=1.5, y = c(0,5), labels = c(0,5))
        #text(x=1.5, y = seq(0,DTzsum_max,l=5), labels = seq(0,DTzsum_max,l=5))
        #rasterImage(y, 0, 0, .35, DTzsum_max)
        
        #library(plotrix)
        #my.colors2 = colorRampPalette(c("white", "light blue", "blue", "red"))
        #pnts = cbind(x =c(0,0.35,0.35,0), y =c(DTzsum_max,DTzsum_max,0.8,0.8))
        #legend.gradient(pnts,my.colors2(100), title = "", limits = "")

        #par(mar=c(6,8,5,5))
        #par(lty = 1) 
        
        #yearspan <- c(N1:N2)
        #yearspanz2 <- yearspan[-1]
        
        #for (jjjj in N1) {
        #  bary <<- c(jjjj, "", "", "", "", "", "", "", "", "", "", "")
        #}
        
        #for (iiii in yearspanz2) {
        #  bary2 <<- c(iiii, "", "", "", "", "", "", "", "", "", "", "")
        #  bary <- append(bary, bary2)
        #}
        #baryears <- bary
        
        #--fix barplot xaxis names
        #baryears <- c("2001", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2002", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2003", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2004", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2005", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2006", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2007", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2008", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2009", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2010", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2011", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2012", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2013", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2014", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2015", "", "", "", "", "", "", "", "", "", "", "")
        
        #nxxsd <- sd(nxx$loss)
        #cols <- ifelse(nxx$loss > nxxsd, "red","blue")
        #legend.col(col = len4a, lev = m$loss)
        #bar <- barplot(nxx$loss, space = 0, col=cols, xlab="", ylab="", main = paste(scen_state,  " ", kkk,  "\n", "Total loss by month, 2001 - 2015", sep="")
        #               , names.arg = baryears, las = 2)
        
        #title(ylab="Commodity loss in dollars ($)", line=6, cex.lab=1.2)
        #title(xlab="Commodity loss totals ($) 2001- 2015", line=4, cex.lab=1.2)
        
        
        
        
        
        
        #axis(1, xaxp=c(1, 15, 19), las=2)
        
        #-plot the bar plot for the animation beside the map
        #abline(v=(bar[thenum[1]]), col="red", lty=2)
        #dev.off()
        #bb <- barplot(DT7$ACRES, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix_acres)
        #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
        #plot(m, col = newmatrix_acres, main = paste(scen_state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))
        
      } else {
          #---begin else loop to months and counties with NO cause values.  So zero rows of values.
        
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
        #DTz <- data.table(za)
        #DTz <- subset(DTz, damagecause == dcause) 
        #DTz <- subset(DTz, commodity == kkk)
        #DTza <- as.data.frame(subset(DTz, loss > 0))
        #DTzmax <- max(1)
        #DTzmin <- min(0)
        #DTzlen <- (nrow(DTza)/5)
        
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
        
        yeardir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep="")
        #ix <- paste(N1, "_", N2, "_usda_gridmet_", scen_state, sep="")
        setwd(yeardir2)
        #DT2x <- as.data.frame(read.csv(ix, strip.white = TRUE))
        #DT2x <- subset(DT2x, month = "JAN" & "FEB")
        #DT2x <- subset(DT2x, year == j)
        DT2x <- threestate_summary
        
        kkkk <- gsub(" ", "", kkk, fixed = TRUE)
        
        #---creating vector for every year and month for full barchart with 0 months.
        N1_num <- as.numeric(N1)
        N2_num <- as.numeric(N2)
        N3 = (N2_num - N1_num) + 1
        newmatrixcomm <- matrix(NA, nrow=N3 * 12, ncol=1)
        nmc <- c(1:(N3*12))
        mon <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") 
        yr <- c(N1:N2)
        
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
        
        DT2x <- subset(DT2x, damagecause == dcause )
        DT2x <- subset(DT2x, commodity == kkk)
        DT2x <- data.table(DT2x)
        
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
        kkk <- gsub("\\s+","\\",kkk)
        #par(mar=c(1,1,1,1))
        #par(mar=c(3,3,3,2)+1)
        #par(mfrow=c(1,1))
        #layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE)) #--changes from 1, 2, to 2, 1 for additional plot
        
        
        #par(mfrow=c(1,1))
        #layout(matrix(c(1,2,3,3),2, 2, byrow=TRUE))
        
        #par(mar=c(3,3,3,2)+1)
        #layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE),
        #      widths=c(2,1), heights=c(2,1))
        
        
        #png(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_png/Drought/", j, "_", nn, "_", kkk,  "_plot.png", sep=""))
        #par(mar=c(3,3,3,2)+1)
        
        #layout(matrix(c(1,2,3,3,3,3), 3, 2, byrow = TRUE),
        #       widths=c(2,1), heights=c(2,1))
        
        
        #layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
        #--turn image horizontal
        
        #plotmonth <- month.abb[x$monthcode[1]]
        #plotyear <- x$year[1]
        #plotcommodity <- x$commodity[1]
        
        midpoint_loss <- (max(m$loss) + min(m$loss)/2)
        #midpoint_acres <- (max(m$acres) + min(m$acres)/2)
        
        #b <- barplot(m$loss, names.arg = m$loss, las=2, col = newmatrix)
        #text(bb, midpoint_loss, labels=mz$loss, srt=90)
        
        #plot(m, col = m$loss, main = paste(scen_state,  " ", jj, " ", j, " ", kkk, "\n", "monthly total loss: $", "0", "\n", " monthly drought claims:", "0", sep=""))
        
        yeardir3 <- paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/shapefiles/", sep="")
        setwd(yeardir3)
        writeSpatialShape(m, paste(jj, "_", j, "_", kkk, "_", dcause, sep=""))
        
        #legend_image <- as.raster(matrix(rev(len4a_out), ncol=1))
        #plot(c(0,2),c(0,DTzmax),type = 'n', axes = F,xlab = '', ylab = '', main = 'legend title')
        ##text(x=1.5, y = c(0,5), labels = c(0,5))
        #text(x=1.5, y = seq(0,DTzmax,l=5), labels = seq(0,DTzmax,l=5))
        #rasterImage(legend_image, 0, 0, 1, 1 )
        
        #if (exists('DTzsum_maxz') == TRUE) {
        #if (sum(nxx$loss) > 0 ) {
          #legend_image <- as.raster(matrix(rev(len4a_out), ncol=1))
          #plot(c(0,3),c(0,DTzsum_maxz),type = 'n', axes = F,xlab = '', ylab = '', main = 'Loss $ Range')
          #text(x=1.5, y = c(0,5), labels = c(0,5))
          #text(x=1.5, y = seq(0,DTzsum_maxz,l=5), labels = seq(0,DTzsum_maxz,l=5))
          #rasterImage(legend_image, 0, 0, .35, DTzsum_maxz)
          
          #library(plotrix)
          #my.colors2 = colorRampPalette(c("white", "light blue", "blue", "red"))
          #pnts = cbind(x =c(0,0.35,0.35,0), y =c(DTzsum_maxz,DTzsum_maxz,0.8,0.8))
          #legend.gradient(pnts,my.colors2(100), title = "", limits = "")
          
        #} else {
          
          #legend_image <- as.matrix(rev(len4a_out), ncol=1)
          #plot(c(0,3),c(0,DTzmax1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Loss $ Range')
          #text(x=1.5, y = c(0,5), labels = c(0,5))
          #text(x=1.5, y = seq(0,DTzmax1,l=5), labels = seq(0,DTzmax1,l=5))
          #rasterImage(legend_image, 0, 0, .35, DTzmax1)
          #rasterImage(as.raster(y), 0, 0, .35, DTzmax1)
          
          #library(plotrix)
          #my.colors2 = colorRampPalette(c("white", "light blue", "blue", "red"))
          #pnts = cbind(x =c(0,0.35,0.35,0), y =c(DTzmax1,DTzmax1,0.8,0.8))
          #legend.gradient(pnts,my.colors2(100), title = "", limits = "")

          #par(new=TRUE)
          #legi <- length(legend_image)
          #my.colors = colorRampPalette(c("light blue", "blue", "red"))
          #x = 1
          #y = as.matrix(seq(0,DTzmax1,l=1000))
          #z = matrix(1:legi, nrow=1)
          #image(x,legend_image,z,col=my.colors(100),axes=FALSE,xlab="",ylab="")
        
        #}
        
        #plot(m, col = newmatrix, main = paste(scen_state, " crop loss $ \n", " ", jj, " ", j, "\n", kkk, sep=""))
        #legend.col(col = orderedcolors2a, lev = m$loss)
        #par(mar=c(1,1,1,1))
        #par(mar=c(6,8,5,5))
        #par(lty = 1)
        
        #yearspan <- c(N1:N2)
        #yearspanz2 <- yearspan[-1]
        
        
        #--fix barplot xaxis names
        
        #for (jjjj in N1) {
        #  bary <<- c(jjjj, "", "", "", "", "", "", "", "", "", "", "")
        #}
        
        #for (iiii in yearspanz2) {
        #  bary2 <<- c(iiii, "", "", "", "", "", "", "", "", "", "", "")
        #    bary <- append(bary, bary2)
        #}
        #baryears <- bary
          
        #baryears <- c("2001", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2002", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2003", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2004", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2005", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2006", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2007", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2008", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2009", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2010", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2011", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2012", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2013", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2014", "", "", "", "", "", "", "", "", "", "", "", 
        #              "2015", "", "", "", "", "", "", "", "", "", "", "")
        
        #nxxsd <- sd(nxx$loss)
        #cols <- ifelse(nxx$loss > nxxsd, "red","blue")
        #legend.col(col = len4a, lev = m$loss)
        #bar <- barplot(nxx$loss, space = 0, col=cols, xlab="", ylab="", main = paste("PNW",  " ", kkk,  "\n", "Total loss by month, 2001 - 2015", sep="")
        #               , names.arg = baryears, las = 2)
        
        #title(ylab="Commodity loss in dollars ($)", line=6, cex.lab=1.2)
        #title(xlab="Commodity loss totals ($) 2001- 2015", line=4, cex.lab=1.2)
        
        
        
        
        
        
        
        
        
        
        
        
        #bar <- barplot(nxx$loss) #-plot the bar plot for the animation beside the map
        #abline(v=(bar[thenum[1]]), col="red", lty=2)
        #dev.off()
        #bb <- barplot(DT7$ACRES, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix_acres)
        #text(b, midpoint_acres, labels=mzacres$acres, xpd=NA, col = "White")
        #plot(m, col = newmatrix_acres, main = paste(scen_state, " crop loss acres \n", " ", plotyear, "\n", plotcommodity, sep=""))                     
        
      } }}}


listz <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_png/", "Drought", sep=""))
subset(listz, grepl("*.png",listz))
liss <- length(listz)
newframe <- t(data.frame(strsplit(listz, "\\_")[1:liss]))
uniz <- unique(newframe[,3])




#--making a directory for the state - moving data from the scenario to the state
for (i in uniz) {
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_png/", "Drought", sep="")) 
  dirz <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_png/", "Drought/", sep="")
  system(paste("mkdir ", i, sep=""))
  system(paste("mv *_", i, "_* ", dirz, i, "/", sep=""))
}



