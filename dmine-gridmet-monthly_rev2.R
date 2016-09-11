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
library("plyr")
library("data.table")
library("sirad")
library("rgeos")
library("MASS")
library("stringr")
library("car")
library("sp")

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
counties <- counties[grep("Idaho", counties@data$STATE_NAME),]

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
      dev.off() 
      #rasterout <- t(rasterout)
      proj4string(rasterout) <- projection 
      for (l in countyfiploop) {
        jj = jj + 1
        subset_county <- counties[counties@data$FIPS == l,]
        e <- extract(rasterout, subset_county, fun=mean) 
        newmatrix[jj,varspannumber] <- mean(e)
        newmatrix[jj,16] <- l
        newmatrix[jj,17] <- k #--month
        newmatrix[jj,18] <- i #--year
      }  
    } 
  }
  setwd(dirname)
  name <- paste(i, "_summary", sep="")
  #name <- paste(dirname, "/", variable, "_", month, "_", year, "_summary", sep="")
  write.matrix(newmatrix, file=name, sep=",")
}

#--test from here on

#--alter yearspan below to minimize the number of years processed for merging.
#--done because years before 2001 have a different structure for USDA data,
#--and thus require an additional loop or alteration to the following loop
#--to factor in that alternative structure (so gridmet can merge correctly)

if (N1 > '2000') {

yearspan <- c(N1:N2)
#--merge usda data with gridmet data

for (i in yearspan) {
  gridmetmonthly <- paste(dirname, "/", i, "_summary", sep="")
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
name2 = paste(N1, "_", N2, "_", "usda_gridmet_", scen_state, sep="")
write.matrix(combined.df, file=name2, sep=",")

} else {
  
yearspan <- c(N1:N2)
#--merge usda data with gridmet data

for (i in yearspan) {
  gridmetmonthly <- paste(dirname, "/", i, "_summary", sep="")
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
name2 = paste(N1, "_", N2, "_", "usda_gridmet_", scen_state, sep="")
write.matrix(combined.df, file=name2, sep=",")

}

##-move files to appropriate locations

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep=""))
system("mkdir netcdf")
system("mv *.nc ./netcdf")

system("mkdir summaries")
system("mkdir month")
system("mkdir raster_commodity")
system("mkdir raster_commodity_plots")
system("mkdir gridmet_monthly_plots")
system("mv *summary ./summaries")
system("mv *_monthly* ./summaries")
system("mv *_gridmet* ./summaries")
system("mv *.png ./gridmet_monthly_plots")




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
combined.df <- data.frame(read.csv(paste(N1, "_", N2, "_", "usda_gridmet_", scen_state, sep="")))

#-remove all other variables to allow for datasets based on year, month, county, and commodity - loss and acres
combined.df2 <- subset(combined.df, select = -c(insuranceplancode,insurancename,stagecode,damagecausecode,damagecause,month,statecode,state,countyfips,countycode,bi,pr,th,pdsi,pet,erc,rmin,rmax,tmmn,tmmx,srad,sph,vs,fm1000,fm100) )

combined.df <- subset(combined.df, select = -c(insuranceplancode,insurancename,stagecode,damagecausecode,damagecause,month,statecode,state,countyfips,countycode,bi,pr,th,pdsi,pet,erc,rmin,rmax,tmmn,tmmx,srad,sph,vs,fm1000,fm100) )

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
commoditytrim <- unique(ttt(combined.df$commodity))

for (i in commoditytrim) {
  x <- subset(combined.df, combined.df$commodity == i)
  write.csv(x, file=paste(i, ".csv", sep=""))
}






#-order the columns by commodity, then year, month, and county

combined.df <- combined.df[with(combined.df, order(commoditycode,year,monthcode,county)), ]

#-sum the acres and loss columns for all common rows  This merges all rows that have the same values exept for acres and loss.  We sum those to create a geographic
#-representation for each commodity - for each county, year, and month.  This will be use to convert to a raster for comparison to meterological data.

combined.df <- combined.df[, lapply(.SD, sum), by=list(year,county,commoditycode,monthcode)]

#--replacing commoditycode with commodity name

profession.code <- c(Apples=54, Wheat=11, Barley=91, SugarBeets=39, Cherries=57, Grapes=53, AdjustedGrossRevenue=63, 
                     GreenPeas=64, AllOtherCrops=99, Pears=89, Canola=15, SweetCorn=42, Mint=74, Potatoes=84, 
                     DryPeas=67, ProcessingBeans=46, DryBeans=47, Onions=13, Cranberries=58, Corn=41, 
                     Oats=16, AlfalfaSeed=107, FreshApricots=218, FresFreestonePeaches=223, Nursery=73, 
                     Mustard=69, Bluberries=12, AdjustedGrossRevenuelite=61, Plums=92, Soybeans=81, 
                     WholeFarmRevenueProtection=76, Buckwheat=114)

combined.df$commodity <- names(profession.code)[match(combined.df$commoditycode, profession.code)]

#-----

combined.yearmonth <- split(combined.df,list(combined.df$year,combined.df$monthcode, combined.df$commodity))
setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month", sep=""))
lapply(names(combined.yearmonth), function(funct){write.csv(combined.yearmonth[[funct]], file = paste(funct, ".csv", sep = ""))})

system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr/")

#--bringing in county shapefile
setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% scen_state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep="")
unique <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/netcdf/pdsi_apr_", N2, ".nc", sep=""))
setwd(monthdir)
system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")
unique <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month_positive", sep=""))

elems <- unlist( strsplit( unique, "\\." ) )
uf <- matrix( elems , ncol = 4 , byrow = TRUE )
as.data.frame( uf ) 

uf2 <- as.data.frame(uniqueframe) %>% separate(uniqueframe, into = paste("V", 1:4, sep = "."))
 
for (i in unique) {
  setwd("/dmine/data/counties/")
  counties <- readShapePoly('UScounties.shp', 
                            proj4string=CRS
                            ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  counties <- subset(counties, STATE_NAME %in% scen_state)
  #counties <- counties[grep(scen_state, counties@data$STATE_NAME),]
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/month", sep=""))
  x <- as.data.frame(read.csv(i, strip.white = TRUE))
  u <- data.frame(trimws(x$county))

    colnames(u) <- c("NAME")
    colnames(x) <- c("UNIQUEID", "YEAR", "COUNTY", "COMMODITYCODE", "MONTHCODE", "ACRES", "LOSS", "COMMODITY")
    z <- cbind(x,u)
    m <- merge(counties, z, by='NAME')
    m$LOSS[is.na(m$LOSS)] <- 0
    m$COMMODITYCODE[is.na(m$COMMODITYCODE)] <- 0
    m$ACRES[is.na(m$ACRES)] <- 0

    #shapefile(m)
    #--begin polygon work
    #length(na.omit(m$LOSS))
    #tt <- colorRampPalette(brewer.pal(11, "Spectral")
    tt <- colorRampPalette(c("blue", "orange", "red"), space = "Lab")
    mz <- subset(m, LOSS != 0)
    mzacres <- subset(m, ACRES > 0)
    lengacres <- length(m$ACRES)
    leng <- length(m$LOSS)
    len2 <- tt(len <- length(mz$LOSS))
    len2acres <- tt(len <- length(mzacres$ACRES))
    len2a <- length(mz$LOSS)
    len2a <- length(mzacres$ACRES)
    len3 <- tt(len <- length(m$LOSS))
  
    orderedcolors2 <- tt(length(mz$LOSS))[order(order(mz$LOSS))]
    orderedcolors3 <- tt(length(mzacres$ACRES))[order(order(mzacres$ACRES))]
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
    
    xx <- 1
    newmatrix_acres <- matrix(data = NA, nrow = leng, ncol = 1)
    
    for (jj in 1:leng){
      
      if (m$ACRES[jj] == 0) {
        #print("yes this worked, added 0")
        newmatrix_acres[jj,] <- 0
      } else {
        #print("yes, this worked, added color")
        #newmatrix[jj,] <- len3[jj] 
        newmatrix_acres[jj,] <- orderedcolors3[xx]
        xx <- xx + 1
      }
      
    }
  
  newmatrix[newmatrix==0] <- NA
  newmatrix2 <- newmatrix[complete.cases(newmatrix[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix[newmatrix == NA] <- 0
  newmatrix <- c(newmatrix)
  
  newmatrix_acres[newmatrix_acres==0] <- NA
  newmatrix2acres <- newmatrix_acres[complete.cases(newmatrix_acres[,1])]
  #newmatrix2 <- subset(newmatrix = TRUE)
  newmatrix_acres[newmatrix_acres == NA] <- 0
  newmatrix_acres <- c(newmatrix_acres)
  
  
  #orderedcolors2 <- colorRampPalette(c(44))
  #m <- cbind(m$LOSS, newmatrix)
  #midpoints <- barplot(mz$LOSS)
  png(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/", "/month_png/", x$YEAR[1], ".", x$MONTHCODE[1], ".", x$COMMODITY[1],  "_plot", sep=""))
  par(mar=c(6,3,3,2)+1)
  par(mfrow=c(2,2))
  layout(matrix(c(1,2,3,4),2, 2, byrow=TRUE))
  #--turn image horizontal
  
  plotmonth <- month.abb[x$MONTHCODE[1]]
  plotyear <- x$YEAR[1]
  plotcommodity <- x$COMMODITY[1]
  
  midpoint_loss <- (max(mz$LOSS) + min(mz$LOSS)/2)
  midpoint_acres <- (max(mzacres$ACRES) + min(mzacres$ACRES)/2)
  
  b <- barplot(mz$LOSS, names.arg = mz$NAME, las=2, col = newmatrix2)
  #text(bb, midpoint_loss, labels=mz$LOSS, srt=90)
  plot(m, col = newmatrix, main = paste(scen_state, " crop loss $ \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""))
  
  bb <- barplot(mzacres$ACRES, names.arg = mz$NAME, las=2, col = newmatrix2acres)
  #text(b, midpoint_acres, labels=mzacres$ACRES, xpd=NA, col = "White")
  plot(m, col = newmatrix_acres, main = paste(scen_state, " crop loss acres \n", plotmonth, " ", plotyear, "\n", plotcommodity, sep=""))
  
    
  dev.off()
}  

  