#------------------------------------------------------------------------#
# TITLE:        dmine-gridmet-monthly.R
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

#memory.size(10000)
#----Setting vectors for loops for years, variables, day, and rasters---#

#----input ranges of years to examine----------#

rm(list = ls()) #--clears all lists------#

N1 <- readline("enter first year of data range: ")
N2 <- readline("enter last year of data range: ")
LAT1 <- readline("enter Lat min: ")
LAT2 <- readline("enter Lat max: ")
LON1 <- readline("enter Lon min: ")
LON2 <- readline("enter Lon max: ")
STATE <- readline("enter state: ")
dcause <- readline("enter damage cause to focus on: ")
assign("N1", N1, envir = .GlobalEnv)
assign("N2", N2, envir = .GlobalEnv)

#----writes subset variables to file for shell script operations--#

RSCENARIO <- sample(50000:100000, 1, replace=F)
#scen <- paste("scenario_", RSCENARIO, sep="")

print(paste("Creating R Scenario ", RSCENARIO, sep=""))

#--writes data to a temp file for use later

fileConn<-file("/tmp/agmesh-subset-R-scenario.txt", "w")
writeLines(c(paste('yearstart=', N1, sep='')), fileConn)
writeLines(c(paste('yearend=', N2, sep='')), fileConn)
writeLines(c(paste('lat1=', LAT1, sep='')), fileConn)
writeLines(c(paste('lat2=', LAT2, sep='')), fileConn)
writeLines(c(paste('lon1=', LON1, sep='')), fileConn)
writeLines(c(paste('lon2=', LON2, sep='')), fileConn)
writeLines(c(paste('state=', STATE, sep='')), fileConn)
writeLines(c(paste('scenario=', "scenario_", RSCENARIO, sep='')), fileConn)
close(fileConn)

#--call bash script to run nc operator functions to extract
#--nc data for each file - using the input of file saved above

#write.table(data.frame(ID,N1,N2,LAT1,LAT2,LON1,LON2), "/tmp/agmesh-subset-vartmp.txt", sep="\t")
system("/agmesh-code/agmesh-subset.sh")
#system("rm /tmp/agmesh-subset-vartmp.txt")




#---Second portion of script that extracts data by county and
#---generates a matrix

scen <- read.table("/tmp/agmesh-subset-R-scenario.txt")
scen <- t(scen)

scen7 = unlist(strsplit(scen[8], split='=', fixed=TRUE))[2]
scen1 = unlist(strsplit(scen[1], split='=', fixed=TRUE))[2]
scen2 = unlist(strsplit(scen[2], split='=', fixed=TRUE))[2]
scen_state = unlist(strsplit(scen[7], split='=', fixed=TRUE))[2]

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen7, sep="")) 
yearspan <- c(scen1:scen2)

scen <- scen7
dirname <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep = "")

print("Generating raster arrays for analysis...")

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]

counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

#--loop list for county by fip
countyfiploop <- counties@data$FIPS

#--data frame of county fip list
countyfiplist <- data.frame(counties@data$FIPS)

#--data frame of county names
countynames <- data.frame(counties@data$NAME)

#combo of county names and fip for this list
countylist <- cbind(countynames, countyfiplist)

#--number of rows in county list
countylistrows <- 12 * nrow(countylist)

list <- list.files(path = dirname)
list2 = list
list2 = substr(list2,1,nchar(list2)-3)
list3 <- data.frame(list2)

listcols <- nrow(list)


#--creates an empty matrix that is the number of counties mulitiplied by the number of months (one year standards for the runs.  
#This will be the length of the full final matrix, which will be 14 variables/columns wide
#longlist <- matrix(NA, nrow=countylistrows * 12)

#--loop to generate raster brick from each nc file, subset by the county, extact the values 
#--for each variable, for each month and year combo.

setwd(dirname)
varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
yearspan = c(N1:N2)

for (i in yearspan) { 
  #--new matrix to contain variable, month, year, and county
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=18)
  colnames(newmatrix) <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100", "countyfips", "month", "year")
  varspannumber = 0
  for (j in varspan) { 
    varspannumber = varspannumber + 1
    jj=0
    for (k in monthspan) {
      ncfile <- paste(dirname, "/", j, "_", k, "_", i, ".nc", sep="")
      rasterout <- brick(ncfile)
      rasterout <- mean(rasterout)
      rasterout <- crop(rasterout, counties)
      png(paste(dirname, "/", j, "_", k, "_", i, ".png", sep=""))
      plot(rasterout, main = paste0("Monthly Plot for: ", j, ", ", k, ", ", i, sep=""))
      plot(counties, add=TRUE)
      dev.off() 
      #rasterout <- t(rasterout)
      proj4string(rasterout) <- projection 
      for (l in countyfiploop) {
        jj = jj + 1
        subset_county <- counties[counties@data$FIPS == l,]
        name_county <- subset_county$NAME
        e <- extract(rasterout, subset_county, fun=mean) 
        newmatrix[jj,varspannumber] <- mean(e)
        newmatrix[jj,16] <- l
        newmatrix[jj,17] <- k #--month
        newmatrix[jj,18] <- i #--yeari
        print(paste("county climate construction for:", scen_state, "-", i, "-", k, "-", name_county, "-", j,  sep=""))
      }  
    } 
  }
  setwd(dirname)
  name <- paste(scen_state, "_", i, "_summary", sep="") #--used for individual states
  #name <- paste(dirname, "/", variable, "_", month, "_", year, "_summary", sep="")
  write.matrix(newmatrix, file=name, sep=",")
}
#--put text notification here
#--test from here on

setwd(dirname)

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep=""))
system("mkdir netcdf")
system("mv *.nc ./netcdf")


system(paste("mkdir", "summaries"))
system(paste("mkdir", " month"))
system(paste("mkdir", " raster_commodity"))
system(paste("mkdir", " raster_commodity_plots"))
setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep=""))
system(paste("mkdir", " gridmet_monthly_plots"))
system(paste("mkdir", " commodity_csv"))
system(paste("mkdir", " month_positive"))
system(paste("mkdir", " commodity_csv_agr"))
system(paste("mkdir", " commodity_csv_agr_month"))
system(paste("mkdir", " month_png"))

system(paste("mv *summary", " ./summaries", sep=""))
system(paste("mv *.png", " ./gridmet_monthly_plots", sep=""))

#--alter yearspan below to minimize the number of years processed for merging.
#--done because years before 2001 have a different structure for USDA data,
#--and thus require an additional loop or alteration to the following loop
#--to factor in that alternative structure (so gridmet can merge correctly)

if (N1 > '2000') {
  
  yearspan <- c(N1:N2)
  #--merge usda data with gridmet data
  
  for (i in yearspan) {
    gridmetmonthly <- paste(dirname, "/summaries/", scen_state, "_",  i, "_summary", sep="")
    usda <- paste("/dmine/data/USDA/crop_indemnity_txt/", i, ".txt", sep="")
    usda <- read.csv(usda, sep="|")
    usda <- data.frame(usda)
    gridmetmonthly <- read.csv(gridmetmonthly, strip.white=TRUE)
    gridmetmonthly <- data.frame(gridmetmonthly)
    #usda <- as.matrix(usda)
    #gridmetmonthly <- as.matrix(gridmetmonthly)
    colnames(usda) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")
    usda$statecode <- str_pad(usda$statecode, 2, pad = "0") #--pad state with zeros in front so we can combine into one nationwide fips number
    usda$countycode <- str_pad(usda$countycode, 3, pad = "0") #--pad county with zeros in front so we can combine into one nationwide fips number
    usda["countyfips"] <- NA  #--creates a new countyfips column to hold the merged columns
    usda$countyfips <- paste(usda$statecode, usda$countycode, sep="") #--merges the two columns in to one
    gridmetmonthly$month <- sapply(gridmetmonthly$month, toupper)
    
    df3 = merge(gridmetmonthly, usda, by.x=c("year", "month", "countyfips"), by.y=c("year", "month", "countyfips"))
    name = paste(i, "_monthly_usda_gridmet_post2001_", scen_state, sep="")
    write.matrix(df3, file=name, sep=",")
    
    #--merge county shapefile with USDA data for mapping purposes
    #m <- merge(counties, usda, by.x="FIPS", by.y="countyfips")
    #mergename = paste(dirname, "annual_usda_croploss_geofile_WA", sep="")
    #shapefile(m, paste(dirname, "annual_usda_croploss_geofile_WA", sep=""))
    #write.matrix(m, file=mergename, sep=",")
  }
  
  setwd(dirname)
  files <- list.files(dirname, pattern = 'monthly_usda_gridmet_post2001')
  tables <- lapply(files, read.csv, header=TRUE)
  combined.df <- do.call(rbind, tables)
  name2 = paste(scen1, "_", scen2, "_", "usda_gridmet_", scen_state, sep="")
  write.matrix(combined.df, file=name2, sep=",")
  
} else {
  
  yearspan <- c(N1:N2)
  #--merge usda data with gridmet data
  
  for (i in yearspan) {
    gridmetmonthly <- paste(dirname, "/summaries/", scen_state, "_", i, "_summary", sep="")
    usda <- paste("/dmine/data/USDA/crop_indemnity_txt/", i, ".txt", sep="")
    usda <- read.csv(usda, sep="|")
    usda <- data.frame(usda)
    gridmetmonthly <- read.csv(gridmetmonthly, strip.white=TRUE)
    gridmetmonthly <- data.frame(gridmetmonthly)
    #usda <- as.matrix(usda)
    #gridmetmonthly <- as.matrix(gridmetmonthly)
    colnames(usda) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "loss")
    usda$statecode <- str_pad(usda$statecode, 2, pad = "0") #--pad state with zeros in front so we can combine into one nationwide fips number
    usda$countycode <- str_pad(usda$countycode, 3, pad = "0") #--pad county with zeros in front so we can combine into one nationwide fips number
    usda["countyfips"] <- NA  #--creates a new countyfips column to hold the merged columns
    usda$countyfips <- paste(usda$statecode, usda$countycode, sep="") #--merges the two columns in to one
    gridmetmonthly$month <- sapply(gridmetmonthly$month, toupper)
    
    df3 = merge(gridmetmonthly, usda, by.x=c("year", "month", "countyfips"), by.y=c("year", "month", "countyfips"))
    name = paste(i, "_monthly_usda_gridmet_pre2001_", scen_state, sep="")
    write.matrix(df3, file=name, sep=",")
    
    #--merge county shapefile with USDA data for mapping purposes
    #m <- merge(counties, usda, by.x="FIPS", by.y="countyfips")
    #mergename = paste(dirname, "annual_usda_croploss_geofile_WA", sep="")
    #shapefile(m, paste(dirname, "annual_usda_croploss_geofile_WA", sep=""))
    #write.matrix(m, file=mergename, sep=",")
  }
  
  setwd(dirname)
  files <- list.files(dirname, pattern = 'monthly_usda_gridmet_pre2001')
  tables <- lapply(files, read.csv, header=TRUE)
  combined.df <- do.call(rbind, tables)
  name2 = paste(scen1, "_", scen2, "_", "usda_gridmet_", scen_state, sep="")
  write.matrix(combined.df, file=name2, sep=",")
  
}

##-move files to appropriate locations



#system(paste("mkdir", scen_state))


#system(paste("mv *usda_monthly*", " ./summaries", sep=""))
system(paste("mv *usda_gridmet*", " ./summaries", sep=""))


#---test on - files created above for summary state

setwd(paste(dirname, "/month_png", sep=""))
system(paste("mkdir", dcause))

print("Finished climate variables, moving to USDA Ag data...")
#--break here put text notification


#-------------------  

library(RColorBrewer)
library(ggplot2)
library(rasterVis)
library(data.table)
library(rgdal)
library(maptools)
library(rasterVis)
library(maptools)
library(SDMTools)
library(fields)
library(dplyr)
library(tidyr)

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/summaries", sep=""))
combined.df <- data.frame(read.csv(paste(scen1, "_", scen2, "_", "usda_gridmet_", scen_state, sep=""), strip.white = TRUE))

#-remove all other variables to allow for datasets based on year, month, county, and commodity - loss and acres
combined.df2 <- subset(combined.df, select = -c(insuranceplancode,insurancename,stagecode,damagecausecode,damagecause,month,statecode,state,countyfips,countycode,bi,pr,th,pdsi,pet,erc,rmin,rmax,tmmn,tmmx,srad,sph,vs,fm1000,fm100) )

combined.df <- subset(combined.df, select = -c(insuranceplancode,insurancename,stagecode,damagecausecode,month,statecode,state,countyfips,countycode,bi,pr,th,pdsi,pet,erc,rmin,rmax,tmmn,tmmx,srad,sph,vs,fm1000,fm100) )

#-convert to a data table

combined.df <- data.table(combined.df)

ttt <- function (x) sub("\\s+$", "", x)

combined.df <- combined.df[with(combined.df, order(commoditycode,year,monthcode,county)), ]
setwd(paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/commodity_csv/", sep=""))
#function(commoditynames) sub("\\s+$", "", commoditynames)
#--strip white space off so file names are right
#lapply(combined.df$commodity, function(funct){sub("\\s+$", "", combined.df$commodity[[funct]])})
commoditynames <- unique(combined.df$commodity)
ttt <- function (x) sub("\\s+$", "", x)
#commoditytrim <- unique(ttt(combined.df$commodity))
commoditytrim <- unique(combined.df$commodity)

for (i in commoditytrim) {
  x <- subset(combined.df, combined.df$commodity == i)
  write.csv(x, file=paste(scen1, "_", scen2, "_", scen_state, i, ".csv", sep=""))
}


#-order the columns by commodity, then year, month, and county

combined.df <- combined.df[with(combined.df, order(commoditycode,year,monthcode,county)), ]

combined.df <- subset(combined.df, commoditycode != "NA")

#-sum the acres and loss columns for all common rows  This merges all rows that have the same values exept for acres and loss.  We sum those to create a geographic
#-representation for each commodity - for each county, year, and month.  This will be use to convert to a raster for comparison to meterological data.

combined.df <- combined.df[, lapply(.SD, sum), by=list(year,county,commoditycode,monthcode,commodity)]
#combined.df <- data.frame(lapply(combined.df, trimws))
#--replacing commoditycode with commodity name

#profession.code <- c(Apples=54, Wheat=11, Barley=91, SugarBeets=39, Cherries=57, Grapes=53, AdjustedGrossRevenue=63, 
#                     GreenPeas=64, AllOtherCrops=99, Pears=89, Canola=15, SweetCorn=42, Mint=74, Potatoes=84, 
#                     DryPeas=67, ProcessingBeans=46, DryBeans=47, Onions=13, Cranberries=58, Corn=41, 
#                     Oats=16, AlfalfaSeed=107, FreshApricots=218, FreshFreestonePeaches=223, Nursery=73, 
#                     Mustard=69, Bluberries=12, AdjustedGrossRevenuelite=61, Plums=92, Soybeans=81, 
#                     WholeFarmRevenueProtection=76, Buckwheat=114)

#combined.df$commodity <- names(profession.code)[match(combined.df$commoditycode, profession.code)]

#-----
combined.df$year <- as.numeric(as.character(combined.df$year))

vect <- c(N1:N2)
combined.df2 <- subset(combined.df, year > 2000) 
combined.df3 <- subset(combined.df2, year < 2017) 

combined.df <- combined.df3

combined.yearmonth <- split(combined.df,list(combined.df$year,combined.df$monthcode, combined.df$commodity))
setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month", sep=""))
lapply(names(combined.yearmonth), function(funct){write.csv(combined.yearmonth[[funct]], file = paste(funct, ".csv", sep = ""))})

system("mv *ADJUSTED* ../commodity_csv_agr/")
system("rm *NA*")
system("rm *..*")

monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep="")
setwd(monthdir)
system("find month -type f -size +100c -exec cp -nv {} month_positive/ \\;")


#----text notification

#--moving to final phase.  data construction for visualization
print("Finished createing Ag data - Moving to final phase - visualization")

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


#dmineplots <- function(scen_state, N1, N2, dcause, Kommodity) {

#scen_state = "Idaho"
#N1 = "2001"
#N2 = "2015"
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
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep="")
yeardir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/summaries/", sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/netcdf/pdsi_apr_", N1, ".nc", sep=""))
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
  setwd(yeardir)
  
  i <- paste(N1, "_", N2, "_usda_gridmet_", scen_state, sep="")
  #i <- paste(j, "_monthly_usda_gridmet_post2001_", scen_state, sep="")
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
    it <- paste(N1, "_", N2, "_usda_gridmet_", scen_state, sep="")
    #zaa <- as.data.frame(read.csv(it, strip.white = TRUE)) #commented out testing
    #DTz1 <- data.table(zaa) #commented out to test issues with kkk barley only working
    
    xgee <- as.data.frame(read.csv(it, strip.white = TRUE))
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
      
      DTzsumz <- as.data.frame(aggregate(DTz1$loss~DTz1$month+DTz1$year+DTz1$county, DTz1, sum))
      colnames(DTzsumz) <- c("month", "year", "county", "loss")
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
        
        it <- paste(N1, "_", N2, "_usda_gridmet_", scen_state, sep="")
        
        xgee <- as.data.frame(read.csv(it, strip.white = TRUE))
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
        DTzsum <- as.data.frame(aggregate(DTz$loss~DTz$month+DTz$year+DTz$county, DTz, sum))
        colnames(DTzsum) <- c("month", "year", "county", "loss")
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
        
        xx <- unique(counties$county)
        newmatrix <- matrix(data = NA, nrow = leng, ncol = 1)
        vect <- as.vector(DT2loss$county)
        
        
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
        
        yeardir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/summaries/", sep="")
        ix <- paste(N1, "_", N2, "_usda_gridmet_", scen_state, sep="")
        setwd(yeardir2)
        DT2x <- as.data.frame(read.csv(ix, strip.white = TRUE))
        
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
        
        png(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_png/Drought/", j, "_", nn, "_", kkk,  "_plot.png", sep=""))
        par(mar=c(3,3,3,2)+1)
        layout(matrix(c(1,2,3,3,3,3), 3, 2, byrow = TRUE),
               widths=c(2,1), heights=c(2,1))
        
        
        #-------end data construction for bar plot for animation
        
        midpoint_loss <- (max(m$loss) + min(m$loss)/2)
        #midpoint_acres <- (max(m$acres) + min(m$acres)/2)
        
        #b <- barplot(DT7$LOSS, names.arg = DT7$DAMAGECAUSE, las=2, col = newmatrix)
        #text(bb, midpoint_loss, labels=mz$loss, srt=90)
        #plot(m, col = newmatrix, main = paste(scen_state, " crop loss $ \n", " ", plotyear, "\n", plotcommodity, sep=""))
        
        plot(m, col = newmatrix, main = paste(scen_state,  " ", jj, " ", j, " ", kkk,  "\n", "monthly total loss: $", DT7$LOSS, "\n", " monthly drought claims:", nrow(DT2), sep=""))
        
        #--changing out len4a_out for new tt
        len5a_out <- as.raster(tt1(DTzsum_max))
        #legend_image <- as.raster(matrix(rev(len4a_out), ncol=1))
        plot(c(0,3),c(0,DTzsum_max),type = 'n', axes = F,xlab = '', ylab = '', main = 'Loss $ Range: 2001-2015')
        #text(x=1.5, y = c(0,5), labels = c(0,5))
        text(x=1.5, y = seq(0,DTzsum_max,l=5), labels = seq(0,DTzsum_max,l=5))
        #rasterImage(y, 0, 0, .35, DTzsum_max)
        
        library(plotrix)
        my.colors2 = colorRampPalette(c("white", "light blue", "blue", "red"))
        pnts = cbind(x =c(0,0.35,0.35,0), y =c(DTzsum_max,DTzsum_max,0.8,0.8))
        legend.gradient(pnts,my.colors2(100), title = "", limits = "")

        par(mar=c(6,8,5,5))
        par(lty = 1) 
        
        yearspan <- c(N1:N2)
        yearspanz2 <- yearspan[-1]
        
        for (jjjj in N1) {
          bary <<- c(jjjj, "", "", "", "", "", "", "", "", "", "", "")
        }
        
        for (iiii in yearspanz2) {
          bary2 <<- c(iiii, "", "", "", "", "", "", "", "", "", "", "")
          bary <- append(bary, bary2)
        }
        baryears <- bary
        
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
        
        nxxsd <- sd(nxx$loss)
        cols <- ifelse(nxx$loss > nxxsd, "red","blue")
        #legend.col(col = len4a, lev = m$loss)
        bar <- barplot(nxx$loss, space = 0, col=cols, xlab="", ylab="", main = paste(scen_state,  " ", kkk,  "\n", "Total loss by month, 2001 - 2015", sep="")
                       , names.arg = baryears, las = 2)
        
        title(ylab="Commodity loss in dollars ($)", line=6, cex.lab=1.2)
        title(xlab="Commodity loss totals ($) 2001- 2015", line=4, cex.lab=1.2)
        
        
        
        
        
        
        #axis(1, xaxp=c(1, 15, 19), las=2)
        
        #-plot the bar plot for the animation beside the map
        abline(v=(bar[thenum[1]]), col="red", lty=2)
        dev.off()
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
        
        yeardir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/summaries/", sep="")
        ix <- paste(N1, "_", N2, "_usda_gridmet_", scen_state, sep="")
        setwd(yeardir2)
        DT2x <- as.data.frame(read.csv(ix, strip.white = TRUE))
        DT2x <- subset(DT2x, month = "JAN" & "FEB")
        #DT2x <- subset(DT2x, year == j)
        
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
        
        
        png(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_png/Drought/", j, "_", nn, "_", kkk,  "_plot.png", sep=""))
        par(mar=c(3,3,3,2)+1)
        
        layout(matrix(c(1,2,3,3,3,3), 3, 2, byrow = TRUE),
               widths=c(2,1), heights=c(2,1))
        
        
        #layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
        #--turn image horizontal
        
        #plotmonth <- month.abb[x$monthcode[1]]
        #plotyear <- x$year[1]
        #plotcommodity <- x$commodity[1]
        
        midpoint_loss <- (max(m$loss) + min(m$loss)/2)
        #midpoint_acres <- (max(m$acres) + min(m$acres)/2)
        
        #b <- barplot(m$loss, names.arg = m$loss, las=2, col = newmatrix)
        #text(bb, midpoint_loss, labels=mz$loss, srt=90)
        
        plot(m, col = newmatrix, main = paste(scen_state,  " ", jj, " ", j, " ", kkk, "\n", "monthly total loss: $", "0", "\n", " monthly drought claims:", "0", sep=""))
        
        #legend_image <- as.raster(matrix(rev(len4a_out), ncol=1))
        #plot(c(0,2),c(0,DTzmax),type = 'n', axes = F,xlab = '', ylab = '', main = 'legend title')
        ##text(x=1.5, y = c(0,5), labels = c(0,5))
        #text(x=1.5, y = seq(0,DTzmax,l=5), labels = seq(0,DTzmax,l=5))
        #rasterImage(legend_image, 0, 0, 1, 1 )
        
        #if (exists('DTzsum_maxz') == TRUE) {
        if (sum(nxx$loss) > 0 ) {
          legend_image <- as.raster(matrix(rev(len4a_out), ncol=1))
          plot(c(0,3),c(0,DTzsum_maxz),type = 'n', axes = F,xlab = '', ylab = '', main = 'Loss $ Range')
          #text(x=1.5, y = c(0,5), labels = c(0,5))
          text(x=1.5, y = seq(0,DTzsum_maxz,l=5), labels = seq(0,DTzsum_maxz,l=5))
          #rasterImage(legend_image, 0, 0, .35, DTzsum_maxz)
          
          library(plotrix)
          my.colors2 = colorRampPalette(c("white", "light blue", "blue", "red"))
          pnts = cbind(x =c(0,0.35,0.35,0), y =c(DTzsum_maxz,DTzsum_maxz,0.8,0.8))
          legend.gradient(pnts,my.colors2(100), title = "", limits = "")
          
        } else {
          
          #legend_image <- as.matrix(rev(len4a_out), ncol=1)
          plot(c(0,3),c(0,DTzmax1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Loss $ Range')
          #text(x=1.5, y = c(0,5), labels = c(0,5))
          text(x=1.5, y = seq(0,DTzmax1,l=5), labels = seq(0,DTzmax1,l=5))
          #rasterImage(legend_image, 0, 0, .35, DTzmax1)
          #rasterImage(as.raster(y), 0, 0, .35, DTzmax1)
          
          library(plotrix)
          my.colors2 = colorRampPalette(c("white", "light blue", "blue", "red"))
          pnts = cbind(x =c(0,0.35,0.35,0), y =c(DTzmax1,DTzmax1,0.8,0.8))
          legend.gradient(pnts,my.colors2(100), title = "", limits = "")

          #par(new=TRUE)
          #legi <- length(legend_image)
          #my.colors = colorRampPalette(c("light blue", "blue", "red"))
          #x = 1
          #y = as.matrix(seq(0,DTzmax1,l=1000))
          #z = matrix(1:legi, nrow=1)
          #image(x,legend_image,z,col=my.colors(100),axes=FALSE,xlab="",ylab="")
        
        }
        
        #plot(m, col = newmatrix, main = paste(scen_state, " crop loss $ \n", " ", jj, " ", j, "\n", kkk, sep=""))
        #legend.col(col = orderedcolors2a, lev = m$loss)
        #par(mar=c(1,1,1,1))
        par(mar=c(6,8,5,5))
        par(lty = 1)
        
        yearspan <- c(N1:N2)
        yearspanz2 <- yearspan[-1]
        
        
        #--fix barplot xaxis names
        
        for (jjjj in N1) {
          bary <<- c(jjjj, "", "", "", "", "", "", "", "", "", "", "")
        }
        
        for (iiii in yearspanz2) {
          bary2 <<- c(iiii, "", "", "", "", "", "", "", "", "", "", "")
            bary <- append(bary, bary2)
        }
        baryears <- bary
          
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
        
        nxxsd <- sd(nxx$loss)
        cols <- ifelse(nxx$loss > nxxsd, "red","blue")
        #legend.col(col = len4a, lev = m$loss)
        bar <- barplot(nxx$loss, space = 0, col=cols, xlab="", ylab="", main = paste(scen_state,  " ", kkk,  "\n", "Total loss by month, 2001 - 2015", sep="")
                       , names.arg = baryears, las = 2)
        
        title(ylab="Commodity loss in dollars ($)", line=6, cex.lab=1.2)
        title(xlab="Commodity loss totals ($) 2001- 2015", line=4, cex.lab=1.2)
        
        
        
        
        
        
        
        
        
        
        
        
        #bar <- barplot(nxx$loss) #-plot the bar plot for the animation beside the map
        abline(v=(bar[thenum[1]]), col="red", lty=2)
        dev.off()
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



