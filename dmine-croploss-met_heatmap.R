 
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
#library("rasterVis")
library("latticeExtra")
library("maptools")
library("parallel")
#library("Evapotranspiration")
#library("plyr")
library("data.table")
#library("sirad")
library("rgeos")
library("MASS")
library("stringr")
#library("car")
library("sp")
library("doParallel")  #Foreach Parallel Adaptor 
library("foreach") 
detach(package:tidyr)
library(Hmisc)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$state[match(st.x$state,st.codes$full)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

#Define how many cores you want to use
UseCores <- detectCores() -7

#Register CoreCluster
cl       <- makeCluster(UseCores)
registerDoParallel(cl)

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

statez = c("Idaho", "Washington", "Oregon")
Idaho_list1 <- paste("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
Washington_list1 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", sep="|")
Oregon_list1 <- paste("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")


combinedlist2 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", "Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
combinedlist <- c(Idaho_list1, Washington_list1, Oregon_list1)

#alllist <- c("Idaho", "Oregon", "Washington")


#--Oregon

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

or_counties <- counties[grep("Oregon", counties@data$STATE_NAME),]
palouse_Oregon_counties <- or_counties[grep(Oregon_list1, or_counties@data$NAME),]
kk="Oregon"

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Oregon_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

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
library(raster)

dirname <- paste("/dmine/data/VIC/month", sep = "")
dirname2 <- paste("/dmine/data/VIC/county", sep="")
dirname3 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/cdl", sep="")
setwd(dirname)
#varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
yearspan = c(1989:2015)


for (i in yearspan) { 
  ##cdl <- raster(paste("/dmine/data/CDL/", "CDL_", i, ".grd", sep=""))
  #sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #cdl <- projectRaster(cdl, crs = sr)
  ##wintercdl <- cdl == 24 #spring wheat
  ##wintercdl <- crop(wintercdl, extent(counties))
  ##wintercdl[wintercdl==0] <- NA
  #writeRaster(wintercdl, paste(dirname3, "/", "CDL_", kk, "-", i, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
  #print(paste("cdl yearly winter wheat construction for:", kk, "-", i, "-", "-", name_county, "-", j,  sep=""))
  #--new matrix to contain variable, month, year, and county
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=4)
  colnames(newmatrix) <- c("soil_moisture", "month", "year", "county")
  varspannumber = 0
  #for (j in varspan) { 
  #  varspannumber = varspannumber + 1
  jj=0
    for (k in monthspan) {
      ncfile <- paste(dirname, "/total_column_moisture_", k, "_", i, ".nc", sep="")
      rasterout <- raster(ncfile)
      #rasterout <- brick(ncfile) #create a brick
      #rasterout <- mean(rasterout) #get the mean of all 30 days for the month
      rasterout <- raster::mask(rasterout, counties) #- mask just the raster for the state in question
      rasterout4 <- crop(rasterout, extent(counties)) #now crop it for the state
      #r.new = resample(rasterout3, wintercdl, "bilinear")
      #rasterout4 <- mask(r.new, wintercdl)
      #png(paste(dirname, "/", j, "_", k, "_", i, ".png", sep=""))
      #plot(rasterout, main = paste0("Monthly Plot for: ", j, ", ", k, ", ", i, sep=""))
      #plot(counties, add=TRUE)
      #dev.off() 
      #rasterout <- t(rasterout)
      #proj4string(rasterout) <- projection 
      for (l in countyfiploop) {
        jj = jj + 1
        subset_county <- counties[counties@data$FIPS == l,]
        name_county <- subset_county$NAME
        e <- crop(rasterout4, subset_county) 
        
        r <- raster(ncol=90, nrow=45)
        extent(r) <- extent(subset_county)
        r.polys <- rasterize(subset_county, r, field = subset_county@data[,1], fun = "mean", 
                  update = TRUE, updateValue = "NA")
        #plot(r.polys)
        
        extent(r.polys) <- extent(subset_county)
        r.new2 = resample(r.polys, e, "bilinear")
        
        
        ee <- raster::mask(e, r.new2)
        sp <- SpatialPoints(ee)
        eee <- extract(ee, sp, method='bilinear')
        newmatrix[jj,1] <- mean(eee, na.rm=TRUE)
        #newmatrix[jj,16] <- l
        newmatrix[jj,2] <- k #--month
        newmatrix[jj,3] <- i #--year
        newmatrix[jj,4] <- l #-county
        #writeRaster(ee, paste(dirname3, "/", "CDL_", kk, "-", i, "-", k, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
        print(paste("writing raster, creating matrix of soil moisture for:", kk, "-", i, "-", k, "-", name_county,  sep=""))
      }  
    } 
  #} --for varspan
  setwd(dirname2)
  name <- paste(kk, "_", i, "_soil_moisture", sep="") #--used for individual states
  #name <- paste(dirname, "/", variable, "_", month, "_", year, "_summary", sep="")
  write.matrix(newmatrix, file=name, sep=",")
}


#-Washington



setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

wa_counties <- counties[grep("Washington", counties@data$STATE_NAME),]
palouse_Washington_counties <- wa_counties[grep(Washington_list1, wa_counties@data$NAME),]
kk="Washington"

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Washington_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

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
library(raster)

dirname <- paste("/dmine/data/VIC/month", sep = "")
dirname2 <- paste("/dmine/data/VIC/county", sep="")
#dirname3 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/cdl", sep="")
setwd(dirname)
#varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
yearspan = c(1989:2015)


for (i in yearspan) { 
  ##cdl <- raster(paste("/dmine/data/CDL/", "CDL_", i, ".grd", sep=""))
  #sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #cdl <- projectRaster(cdl, crs = sr)
  ##wintercdl <- cdl == 24 #spring wheat
  ##wintercdl <- crop(wintercdl, extent(counties))
  ##wintercdl[wintercdl==0] <- NA
  #writeRaster(wintercdl, paste(dirname3, "/", "CDL_", kk, "-", i, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
  #print(paste("cdl yearly winter wheat construction for:", kk, "-", i, "-", "-", name_county, "-", j,  sep=""))
  #--new matrix to contain variable, month, year, and county
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=4)
  colnames(newmatrix) <- c("soil_moisture", "month", "year", "county")
  varspannumber = 0
  #for (j in varspan) { 
  #  varspannumber = varspannumber + 1
  jj=0
  for (k in monthspan) {
    ncfile <- paste(dirname, "/total_column_moisture_", k, "_", i, ".nc", sep="")
    rasterout <- raster(ncfile)
    #rasterout <- brick(ncfile) #create a brick
    #rasterout <- mean(rasterout) #get the mean of all 30 days for the month
    rasterout <- raster::mask(rasterout, counties) #- mask just the raster for the state in question
    rasterout4 <- crop(rasterout, extent(counties)) #now crop it for the state
    #r.new = resample(rasterout3, wintercdl, "bilinear")
    #rasterout4 <- mask(r.new, wintercdl)
    #png(paste(dirname, "/", j, "_", k, "_", i, ".png", sep=""))
    #plot(rasterout, main = paste0("Monthly Plot for: ", j, ", ", k, ", ", i, sep=""))
    #plot(counties, add=TRUE)
    #dev.off() 
    #rasterout <- t(rasterout)
    #proj4string(rasterout) <- projection 
    for (l in countyfiploop) {
      jj = jj + 1
      subset_county <- counties[counties@data$FIPS == l,]
      name_county <- subset_county$NAME
      e <- crop(rasterout4, subset_county) 
      
      r <- raster(ncol=90, nrow=45)
      extent(r) <- extent(subset_county)
      r.polys <- rasterize(subset_county, r, field = subset_county@data[,1], fun = "mean", 
                           update = TRUE, updateValue = "NA")
      #plot(r.polys)
      
      extent(r.polys) <- extent(subset_county)
      r.new2 = resample(r.polys, e, "bilinear")
      
      
      ee <- raster::mask(e, r.new2)
      sp <- SpatialPoints(ee)
      eee <- extract(ee, sp, method='bilinear')
      newmatrix[jj,1] <- mean(eee, na.rm=TRUE)
      #newmatrix[jj,16] <- l
      newmatrix[jj,2] <- k #--month
      newmatrix[jj,3] <- i #--year
      newmatrix[jj,4] <- l #-county
      #writeRaster(ee, paste(dirname3, "/", "CDL_", kk, "-", i, "-", k, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
      print(paste("writing raster, creating matrix of soil moisture for:", kk, "-", i, "-", k, "-", name_county,  sep=""))
    }  
  } 
  #} --for varspan
  setwd(dirname2)
  name <- paste(kk, "_", i, "_soil_moisture", sep="") #--used for individual states
  #name <- paste(dirname, "/", variable, "_", month, "_", year, "_summary", sep="")
  write.matrix(newmatrix, file=name, sep=",")
}



#-Idaho

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

id_counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
palouse_Idaho_counties <- id_counties[grep(Idaho_list1, id_counties@data$NAME),]
kk="Idaho"
#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- assign(paste("palouse_", kk, "_counties", sep=""), palouse_Idaho_counties)
#counties <- counties[grep(scen_state, counties@data$STATE_NAME),]

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
library(raster)

dirname <- paste("/dmine/data/VIC/month", sep = "")
dirname2 <- paste("/dmine/data/VIC/county", sep="")
dirname3 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/cdl", sep="")
setwd(dirname)
#varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
yearspan = c(1989:2015)


for (i in yearspan) { 
  ##cdl <- raster(paste("/dmine/data/CDL/", "CDL_", i, ".grd", sep=""))
  #sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #cdl <- projectRaster(cdl, crs = sr)
  ##wintercdl <- cdl == 24 #spring wheat
  ##wintercdl <- crop(wintercdl, extent(counties))
  ##wintercdl[wintercdl==0] <- NA
  #writeRaster(wintercdl, paste(dirname3, "/", "CDL_", kk, "-", i, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
  #print(paste("cdl yearly winter wheat construction for:", kk, "-", i, "-", "-", name_county, "-", j,  sep=""))
  #--new matrix to contain variable, month, year, and county
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=4)
  colnames(newmatrix) <- c("soil_moisture", "month", "year", "county")
  varspannumber = 0
  #for (j in varspan) { 
  #  varspannumber = varspannumber + 1
  jj=0
  for (k in monthspan) {
    ncfile <- paste(dirname, "/total_column_moisture_", k, "_", i, ".nc", sep="")
    rasterout <- raster(ncfile)
    #rasterout <- brick(ncfile) #create a brick
    #rasterout <- mean(rasterout) #get the mean of all 30 days for the month
    rasterout <- raster::mask(rasterout, counties) #- mask just the raster for the state in question
    rasterout4 <- crop(rasterout, extent(counties)) #now crop it for the state
    #r.new = resample(rasterout3, wintercdl, "bilinear")
    #rasterout4 <- mask(r.new, wintercdl)
    #png(paste(dirname, "/", j, "_", k, "_", i, ".png", sep=""))
    #plot(rasterout, main = paste0("Monthly Plot for: ", j, ", ", k, ", ", i, sep=""))
    #plot(counties, add=TRUE)
    #dev.off() 
    #rasterout <- t(rasterout)
    #proj4string(rasterout) <- projection 
    for (l in countyfiploop) {
      jj = jj + 1
      subset_county <- counties[counties@data$FIPS == l,]
      name_county <- subset_county$NAME
      e <- crop(rasterout4, subset_county) 
      
      r <- raster(ncol=90, nrow=45)
      extent(r) <- extent(subset_county)
      r.polys <- rasterize(subset_county, r, field = subset_county@data[,1], fun = "mean", 
                           update = TRUE, updateValue = "NA")
      #plot(r.polys)
      
      extent(r.polys) <- extent(subset_county)
      r.new2 = resample(r.polys, e, "bilinear")
      
      
      ee <- raster::mask(e, r.new2)
      sp <- SpatialPoints(ee)
      eee <- extract(ee, sp, method='bilinear')
      newmatrix[jj,1] <- mean(eee, na.rm=TRUE)
      #newmatrix[jj,16] <- l
      newmatrix[jj,2] <- k #--month
      newmatrix[jj,3] <- i #--year
      newmatrix[jj,4] <- l #-county
      #writeRaster(ee, paste(dirname3, "/", "CDL_", kk, "-", i, "-", k, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
      print(paste("writing raster, creating matrix of soil moisture for:", kk, "-", i, "-", k, "-", name_county,  sep=""))
    }  
  } 
  #} --for varspan
  setwd(dirname2)
  name <- paste(kk, "_", i, "_soil_moisture", sep="") #--used for individual states
  #name <- paste(dirname, "/", variable, "_", month, "_", year, "_summary", sep="")
  write.matrix(newmatrix, file=name, sep=",")
}


#end cluster
stopCluster(cl)


palouse_counties <- rbind(palouse_Idaho_counties, palouse_Washington_counties, palouse_Oregon_counties)


#alllist <- c(palouse_Idaho_counties, palouse_Washington_counties, palouse_Oregon_counties)


#-------finished states, which creates summary files for each year, per state. 
#---newstart


#----Now we need to merge the state files into one large matrix, and assign a 
#-----continuous value column to allow us to sequentially select values across many years



usda000 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1988.txt", sep="")
usda000 <- read.csv(usda000, header=FALSE, sep="|")
usda000 <- data.frame(usda000)


usda0 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2000.txt", sep="")
usda0 <- read.csv(usda0, header=FALSE, sep="|")
usda0 <- data.frame(usda0)

usda89 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1989.txt", sep="")
usda89 <- read.csv(usda89, header=FALSE, sep="|")
usda89 <- data.frame(usda89)

usda90 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1990.txt", sep="")
usda90 <- read.csv(usda90, header=FALSE, sep="|")
usda90 <- data.frame(usda90)

usda91 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1991.txt", sep="")
usda91 <- read.csv(usda91, header=FALSE, sep="|")
usda91 <- data.frame(usda91)

usda92 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1992.txt", sep="")
usda92 <- read.csv(usda92, header=FALSE, sep="|")
usda92 <- data.frame(usda92)

usda93 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1993.txt", sep="")
usda93 <- read.csv(usda93, header=FALSE, sep="|")
usda93 <- data.frame(usda93)

usda94 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1994.txt", sep="")
usda94 <- read.csv(usda94, header=FALSE, sep="|")
usda94 <- data.frame(usda94)


usda95 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1995.txt", sep="")
usda95 <- read.csv(usda95, header=FALSE, sep="|")
usda95 <- data.frame(usda95)

usda96 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1996.txt", sep="")
usda96 <- read.csv(usda96, header=FALSE, sep="|")
usda96 <- data.frame(usda96)

usda97 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1997.txt", sep="")
usda97 <- read.csv(usda97, header=FALSE, sep="|")
usda97 <- data.frame(usda97)

usda98 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1998.txt", sep="")
usda98 <- read.csv(usda98, header=FALSE, sep="|")
usda98 <- data.frame(usda98)

usda99 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "1999.txt", sep="")
usda99 <- read.csv(usda99, header=FALSE, sep="|")
usda99 <- data.frame(usda99)

usda0 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2000.txt", sep="")
usda0 <- read.csv(usda0, header=FALSE, sep="|")
usda0 <- data.frame(usda0)







usda1 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2001.txt", sep="")
usda1 <- read.csv(usda1, header=FALSE, sep="|")
usda1 <- data.frame(usda1)
usda2 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2002.txt", sep="")
usda2 <- read.csv(usda2, header=FALSE, sep="|")
usda2 <- data.frame(usda2)
usda3 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2003.txt", sep="")
usda3 <- read.csv(usda3, header=FALSE, sep="|")
usda3 <- data.frame(usda3)
usda4 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2004.txt", sep="")
usda4 <- read.csv(usda4, header=FALSE, sep="|")
usda4 <- data.frame(usda4)
usda5 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2005.txt", sep="")
usda5 <- read.csv(usda5, header=FALSE, sep="|")
usda5 <- data.frame(usda5)
usda6 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2006.txt", sep="")
usda6 <- read.csv(usda6, header=FALSE, sep="|")
usda6 <- data.frame(usda6)
usda7 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2007.txt", sep="")
usda7 <- read.csv(usda7, header=FALSE, sep="|")
usda7 <- data.frame(usda7)
usda8 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2008.txt", sep="")
usda8 <- read.csv(usda8, header=FALSE, sep="|")
usda8 <- data.frame(usda8)
usda9 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2009.txt", sep="")
usda9 <- read.csv(usda9, header=FALSE, sep="|")
usda9 <- data.frame(usda9)
usda10 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2010.txt", sep="")
usda10 <- read.csv(usda10, header=FALSE, sep="|")
usda10 <- data.frame(usda10)
usda11 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2011.txt", sep="")
usda11 <- read.csv(usda11, header=FALSE, sep="|")
usda11 <- data.frame(usda11)
usda12 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2012.txt", sep="")
usda12 <- read.csv(usda12, header=FALSE, sep="|")
usda12 <- data.frame(usda12)
usda13 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2013.txt", sep="")
usda13 <- read.csv(usda13, header=FALSE, sep="|")
usda13 <- data.frame(usda13)
usda14 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2014.txt", sep="")
usda14 <- read.csv(usda14, header=FALSE, sep="|")
usda14 <- data.frame(usda14)
usda15 <- paste("/dmine/data/USDA/crop_indemnity_txt/", "2015.txt", sep="")
usda15 <- read.csv(usda15, header=FALSE, sep="|")
usda15 <- data.frame(usda15)

usdabound <- rbind(usda1,usda2,usda3,usda4,usda5,usda6,usda7,usda8,usda9,usda10,usda11,usda12,usda13,usda14,usda15)
usdabound2 <- rbind(usda89,usda90,usda91,usda92,usda93,usda94,usda95,usda96,usda97,usda98,usda99,usda0)

usdabound2a <- data.frame(append(usdabound2, list(acres=NA), after=match("V14", names(usdabound2)))) 
usdabound2b <- usdabound2a[,1:16]
colnames(usdabound2b) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

colnames(usdabound2) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")
colnames(usdabound) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")

usdabound3 <- rbind(usdabound2b,usdabound)


usdabound3$county <- trimws(usdabound3$county)
usdabound3$commodity <- trimws(usdabound3$commodity)
usdabound3$damagecause <- trimws(usdabound3$damagecause)


usdabound$county <- trimws(usdabound$county)
usdabound$commodity <- trimws(usdabound$commodity)
usdabound$damagecause <- trimws(usdabound$damagecause)

usdabound <- usdabound3


#--loss


u1 <- subset(usdabound, damagecause != "")
u1 <- subset(u1, acres != 0)
u1 <- subset(u1, month != "")

u1a<- subset(u1, state == "OR")
u2<- subset(u1, state == "ID")
u3<- subset(u1, state == "WA")
u4 <- rbind(u1a, u2, u3)

u4a <- subset(u4, monthcode != 0)
u4a <- subset(u4a, county != "All Other Counties")


#---all of ID, OR, and WA

claimagg <- aggregate(loss ~ month + year + damagecause + county + state + commodity, u4a, sum)
#toBeRemoved<-which(claimagg$month == "   ")
#claimagg<-droplevels(claimagg[-toBeRemoved,])

claimaggmean <- aggregate(loss ~ month + year + damagecause + county + state + commodity, u4a, mean)
#toBeRemoved<-which(claimaggmean$month == "   ")
#claimaggmean<-droplevels(claimaggmean[-toBeRemoved,])

claimaggcount <- aggregate(loss ~ month + year + damagecause + county + state + commodity, u4a, length)
#toBeRemoved<-which(claimaggcount$month == "   ")
#claimaggcount<-droplevels(claimaggcount[-toBeRemoved,])

claimaggacres <- aggregate(acres ~ month + year + damagecause + county + state + commodity, u4a, sum)
#toBeRemoved<-which(claimaggacres$month == "   ")
#claimaggacres<-droplevels(claimaggacres[-toBeRemoved,])


colnames(claimaggcount)[7] <- "count"
colnames(claimaggmean)[7] <- "meanloss"
#--testing

#test1 <- subset(claimaggpalouse, commodity == "WHEAT")
#test2 <- subset(test1, county == "Whitman")
#test3 <- subset(test2, damagecause == "Drought")

#---only palouse

Idaho_list1 <- paste("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", "Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")

claimaggpalouse <- subset(claimagg, county == "Idaho" | county == "Lewis" | county == "Nez Perce" | county == "Clearwater" | county == "Latah" | county == "Benewah" | county == "Kootenai" | county == "Okananogan" | county == "Douglas" | county == "Grant" | county == "Benton" | county == "Franklin" | county == "Walla Walla" | county == "Adams" | county == "Lincoln" | county == "Spokane" | county == "Whitman" | county == "Columbia" | county == "Garfield" | county == "Asotin" | county == "Wasco" | county == "Sherman" | county == "Gilliam" | county == "Morrow" | county == "Umatilla" | county == "Union" | county == "Wallowa")
claimaggmeanpalouse <- subset(claimaggmean, county == "Idaho" | county == "Lewis" | county == "Nez Perce" | county == "Clearwater" | county == "Latah" | county == "Benewah" | county == "Kootenai" | county == "Okananogan" | county == "Douglas" | county == "Grant" | county == "Benton" | county == "Franklin" | county == "Walla Walla" | county == "Adams" | county == "Lincoln" | county == "Spokane" | county == "Whitman" | county == "Columbia" | county == "Garfield" | county == "Asotin" | county == "Wasco" | county == "Sherman" | county == "Gilliam" | county == "Morrow" | county == "Umatilla" | county == "Union" | county == "Wallowa")
claimaggcountpalouse <- subset(claimaggcount, county == "Idaho" | county == "Lewis" | county == "Nez Perce" | county == "Clearwater" | county == "Latah" | county == "Benewah" | county == "Kootenai" | county == "Okananogan" | county == "Douglas" | county == "Grant" | county == "Benton" | county == "Franklin" | county == "Walla Walla" | county == "Adams" | county == "Lincoln" | county == "Spokane" | county == "Whitman" | county == "Columbia" | county == "Garfield" | county == "Asotin" | county == "Wasco" | county == "Sherman" | county == "Gilliam" | county == "Morrow" | county == "Umatilla" | county == "Union" | county == "Wallowa")
claimaggacrespalouse <- subset(claimaggacres, county == "Idaho" | county == "Lewis" | county == "Nez Perce" | county == "Clearwater" | county == "Latah" | county == "Benewah" | county == "Kootenai" | county == "Okananogan" | county == "Douglas" | county == "Grant" | county == "Benton" | county == "Franklin" | county == "Walla Walla" | county == "Adams" | county == "Lincoln" | county == "Spokane" | county == "Whitman" | county == "Columbia" | county == "Garfield" | county == "Asotin" | county == "Wasco" | county == "Sherman" | county == "Gilliam" | county == "Morrow" | county == "Umatilla" | county == "Union" | county == "Wallowa")


colnames(claimaggcountpalouse)[7] <- "count"
colnames(claimaggmeanpalouse)[7] <- "meanloss"

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/")
write.csv(claimaggcountpalouse, file = "Palouse_summary_counts.csv")
write.csv(claimaggmeanpalouse, file = "Palouse_summary_meanloss.csv")
write.csv(claimaggpalouse, file = "Palouse_summary_sumloss.csv")
write.csv(claimaggacrespalouse, file = "Palouse_summary_sumacres.csv")

write.csv(claimaggcount, file = "PNW_summary_counts.csv")
write.csv(claimaggmean, file = "PNW_summary_meanloss.csv")
write.csv(claimagg, file = "PNW_summary_sumloss.csv")
write.csv(claimaggacres, file = "PNW_summary_sumacres.csv")


claimaggallpalouse <- cbind(claimaggpalouse, claimaggcountpalouse$count, claimaggmeanpalouse$meanloss, claimaggacrespalouse$acres) 
yt <- as.data.frame(claimaggallpalouse$loss / claimaggallpalouse[10])
ytt <- round(yt, 4)
claimaggallpalouse <- cbind(claimaggallpalouse, ytt)

claimaggall <- cbind(claimagg, claimaggcount$count, claimaggmean$meanloss, claimaggacres$acres) 
xt <- as.data.frame(claimaggall$loss / claimaggall[10])
xtt <- round(xt, 4)
claimaggall <- cbind (claimaggall, xtt)


colnames(claimaggallpalouse)[8] <- "count"
colnames(claimaggallpalouse)[9] <- "meanloss"
colnames(claimaggallpalouse)[10] <- "acres"
colnames(claimaggallpalouse)[11] <- "lossperacre"


colnames(claimaggall)[8] <- "count"
colnames(claimaggall)[9] <- "meanloss"
colnames(claimaggall)[10] <- "acres"
colnames(claimaggall)[11] <- "lossperacres"

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/")
write.csv(claimaggall, file = "PNW_summary_all.csv")
write.csv(claimaggallpalouse, file = "palouse_summary_all.csv")






setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries5")
files  <- list.files(pattern = 'Idaho')
tables <- lapply(files, read.csv, header = TRUE)
combined_idaho.df <- do.call(rbind , tables)
combined_idaho.df$state <- "Idaho"

setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/summaries5")
files  <- list.files(pattern = 'Oregon')
tables <- lapply(files, read.csv, header = TRUE)
combined_oregon.df <- do.call(rbind , tables)
combined_oregon.df$state <- "Oregon"

setwd("/dmine/data/USDA/agmesh-scenarios/Washington/summaries5")
files  <- list.files(pattern = 'Washington')
tables <- lapply(files, read.csv, header = TRUE)
combined_washington.df <- do.call(rbind , tables)
combined_washington.df$state <- "Washington"
#----sums <- read.csv(paste(kk, "_", i, "_palouse_summary", sep=""))

combined.df <- rbind(combined_idaho.df, combined_washington.df, combined_oregon.df)

#---construct a county fips name file
library(maps)
data(county.fips)
colnames(county.fips)[1] <- "countyfips"
colnames(combined.df)[16] <- "countyfips"
library(stringr)
county.fips2 <- data.frame(str_split_fixed(county.fips$polyname, ",", 2))
colnames(county.fips2) <- c("state", "county")
county.fips3 <- cbind(county.fips, county.fips2)
combined1.df <- merge(combined.df,county.fips3, by = 'countyfips')

#--create unique values for each county grouping for palouse region
lister <- unique(combined1.df$county)
listerstate <- unique(combined1.df$state.x)
listeridaho<- paste(lister[1:7], "_", listerstate[1], sep="")
listeroregon <- paste(lister[8:14], "_", listerstate[2], sep="")
listerwashington <- paste(lister[15:26], "_", listerstate[3], sep="")
listercomb <- c(listeridaho, listeroregon, listerwashington)

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

#for (n in lister) {
#  newcounty <- subset(combined1.df, county == n)
#  newcounty$month <- capFirst(newcounty$month)
#  newcounty$month <- trimws(newcounty$month)
#  newcounty <-newcounty[with(newcounty, order(year, match(newcounty$month, month.abb))), ]
#  newcounty$ID<-seq.int(nrow(newcounty))
#  write.csv(newcounty, file = paste("2001_2015_palouse_", n, "_", newcounty$state[1], sep=""))
#}

#yearspan <- c(2001:2015)

#for (m in yearspan) {
#  setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
#  lister2 <- list.files(pattern = paste(m, "_palouse_summary", sep=""))
#  one <- read.csv(lister2[1], strip.white = TRUE)
#  two <- read.csv(lister2[2], strip.white = TRUE)
#  three <- read.csv(lister2[3], strip.white = TRUE)
#  #three$month <- trimws(three$month)
#  sumbound <- rbind(one, two, three)
#  setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-allstate-summaries/")
#  write.csv(sumbound, file = paste(m, "_allstate_palouse_summary", sep=""))
#}

#---algorithm selection

#-combine all three states climate data - all counties in all three states
#setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-county-summaries")
#files  <- list.files(pattern = '\\_palouse_summary')
#tables <- lapply(files, read.csv, header = TRUE)
#combined.df2 <- do.call(rbind , tables)




##-merge climate data into one file


#--put text notification here
#--test from here on




#---new start!





#setwd(dirname)

#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep=""))
#system("mkdir netcdf")
#system("mv *.nc ./netcdf")


#system(paste("mkdir", "summaries"))
#system(paste("mkdir", " month"))
#system(paste("mkdir", " raster_commodity"))
#system(paste("mkdir", " raster_commodity_plots"))
#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, sep=""))
#system(paste("mkdir", " gridmet_monthly_plots"))
#system(paste("mkdir", " commodity_csv"))
#system(paste("mkdir", " month_positive"))
#system(paste("mkdir", " commodity_csv_agr"))
#system(paste("mkdir", " commodity_csv_agr_month"))
#system(paste("mkdir", " month_png"))

#system(paste("mv *summary", " ./summaries", sep=""))
#system(paste("mv *.png", " ./gridmet_monthly_plots", sep=""))

#--alter yearspan below to minimize the number of years processed for merging.
#--done because years before 2001 have a different structure for USDA data,
#--and thus require an additional loop or alteration to the following loop
#--to factor in that alternative structure (so gridmet can merge correctly)


for (p in listercomb) {


statespan <- c("idaho", "oregon", "washington")

#if (N1 > '2000') {
  
  yearspan <- c(1989:2015)
  #--merge usda data with gridmet data
  #for (qq in statespan) {
  #for (i in yearspan) {
  
  
  

     library(Hmisc)
     
     
     simpleCap <- function(x) {
       s <- strsplit(x, " ")[[1]]
       paste(toupper(substring(s, 1,1)), substring(s, 2),
             sep="", collapse=" ")
     }
     
     #'x' is the column of a data.frame that holds 2 digit state codes
     stateFromLower <-function(x) {
       #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
       st.codes<-data.frame(
         state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                           "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                           "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                           "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                           "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
         full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                          "connecticut","district of columbia","delaware","florida","georgia",
                          "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                          "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                          "missouri","mississippi","montana","north carolina","north dakota",
                          "nebraska","new hampshire","new jersey","new mexico","nevada",
                          "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                          "rhode island","south carolina","south dakota","tennessee","texas",
                          "utah","virginia","vermont","washington","wisconsin",
                          "west virginia","wyoming"))
       )
       #create an nx1 data.frame of state codes from source column
       st.x<-data.frame(state=x)
       #match source codes with codes from 'st.codes' local variable and use to return the full state name
       refac.x<-st.codes$state[match(st.x$state,st.codes$full)]
       #return the full state names in the same order in which they appeared in the original source
       return(refac.x)
       
     }
     
     
     
     
     listersplitter <- unlist(strsplit(p, "[_]"))
     countyz <- listersplitter[1]
     statez <- listersplitter[2]
     pu <- capitalize(countyz)
     pu <- simpleCap(pu)
     p <- tolower(p) #--p is the county and state combined.  eg. whitman_washington
     statez <- tolower(statez)
     pp <- stateFromLower(statez)
     pp <- as.vector(pp) #--pp is state capitialized, abbrieviated.  eg. WA
     
     
     usdaboundsub <- subset(usdabound, state == pp)
     usdaboundsub <- subset(usdaboundsub, county == pu)
     commodityspan <- c(unique(usdaboundsub$commodity))
   
 
    #setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
    p <- tolower(p)
    countyz <- tolower(countyz)
    #statez <- toupper(statez)
    
    #files  <- list.files(pattern = '\\_palouse_summary$')
    #tables <- lapply(files, read.csv, header = TRUE, strip.white = TRUE)
    #gridmetmonthly <- do.call(rbind , tables)
    
    gridmetmonthly <- data.frame(combined1.df)
    colnames(gridmetmonthly)[19] <- "state"
    
    statez = simpleCap(statez)
    countyz = simpleCap(countyz)
    countyz = tolower(countyz)
    statez1a = state.abb[grep(simpleCap(statez), state.name)]
    gridmetmonthly <- subset(gridmetmonthly, state == statez)
    gridmetmonthly <- subset(gridmetmonthly, county == countyz)

    gridmetmonthly$monthchar <- as.character(gridmetmonthly$month)
    gridmetmonthly$monthchar <- trimws(gridmetmonthly$monthchar)
    library(Hmisc)
    gridmetmonthly$monthchar <- capitalize(gridmetmonthly$monthchar)
    gridmetmonthly$monthchar <- factor(gridmetmonthly$monthchar, levels=month.abb)
    #gridmetmonthly$monthchar <- as.numeric(gridmetmonthly$monthchar)
    gridmetmonthly <- gridmetmonthly[order(gridmetmonthly[,18], gridmetmonthly[,23]),]
    #gridmetmonthly$monthyear <- paste(as.numeric(gridmetmonthly$monthchar), ".", gridmetmonthly$year, sep="")
    gridmetmonthly$ID<-seq.int(nrow(gridmetmonthly))
    
    gridmetmonthly$month <- trimws(gridmetmonthly$month)
    #---05.18.17 need to create loop thru all claims and assign short term and long term drought variables.  mar 2009 is go back 3 and 6.  June 2009 is go back 6 and 9
   
    gridmetmonthly <- data.frame(gridmetmonthly)
    listersplit <- unlist(strsplit(p, "[_]"))
    countyz <- listersplit[1]
    countyz <- simpleCap(countyz)
    
    statez <- listersplit[2]
    #countyz <- capitalize(countyz)
    statez1 <- stateFromLower(statez)
    statez1 <- as.vector(statez1)
    
    
    
    
    
    
    for (q in commodityspan) {
      usdaboundsub2 <- subset(usdaboundsub, commodity == q)
      damagespan <- c(unique(usdaboundsub2$damagecause))
      
      for (m in damagespan) {
    
    library(zoo)
    
    
    #usda_county$commodity <- trimws(usda_county$commodity)
    
    wheatdroughtclaim_allID <- subset(usdabound, state == "ID")
    wheatdroughtclaim_allID <- subset(wheatdroughtclaim_allID, county == "Idaho" | county == "Nez Perce" | county == "Clearwater" | county == "Latah" | county == "Benewah" | county == "Kootenai" | county == "Lewis")
    wheatdroughtclaim_allWA <- subset(usdabound, state == "WA")
    wheatdroughtclaim_allWA <- subset(wheatdroughtclaim_allWA, county == "Okananogan" | county == "Douglas" | county == "Grant" | county == "Benton" | county == "Franklin" | county == "Walla Walla" | county == "Adams" | county == "Lincoln" | county == "Spokane" | county == "Whitman" | county == "Columbia" | county == "Garfield" | county == "Asotin")
    wheatdroughtclaim_allOR <- subset(usdabound, state == "OR")
    wheatdroughtclaim_allOR <- subset(wheatdroughtclaim_allOR, county == "Wasco" | county == "Sherman" | county == "Gilliam" | county == "Morrow" | county == "Umatilla" | county == "Union" | county == "Wallowa")
    wheatdroughtclaim_allall <- rbind(wheatdroughtclaim_allWA, wheatdroughtclaim_allOR, wheatdroughtclaim_allID)

    wheatdroughtclaim_allall_comm <- subset(wheatdroughtclaim_allall, commodity == q)
    #wheatdroughtclaim_allall_drought <- wheatdroughtclaim_allall_comm
    wheatdroughtclaim_allall_drought <- subset(wheatdroughtclaim_allall_comm, damagecause == m)
    #--only March thru Oct
    #wheatdroughtclaim_allall_final <- subset(wheatdroughtclaim_allall_drought, monthcode == 3 | monthcode == 4 | monthcode == 5 | monthcode == 6 | monthcode == 7 | monthcode == 8 | monthcode == 9 | monthcode == 10)
    #--all months below
    wheatdroughtclaim_allall_final <- subset(wheatdroughtclaim_allall_drought, monthcode == 1 | monthcode == 2 | monthcode == 3 | monthcode == 4 | monthcode == 5 | monthcode == 6 | monthcode == 7 | monthcode == 8 | monthcode == 9 | monthcode == 10 | monthcode == 11 | monthcode == 12)
    
    wheatdroughtclaim_allall_final$monthyear <- paste(wheatdroughtclaim_allall_final$year, ".", wheatdroughtclaim_allall_final$monthcode, sep="")
    
    
    #--claims summarized by month associated to climate short term and long term drought
    claimagg <- aggregate(loss ~ month + year, wheatdroughtclaim_allall_final, sum)
    claimaggmean <- aggregate(loss ~ month + year, wheatdroughtclaim_allall_final, mean)
    claimaggcount <- aggregate(loss ~ month + year, wheatdroughtclaim_allall_final, length)
    
    claimagg$month <- tolower(claimagg$month)
    claimaggmean$month <- tolower(claimaggmean$month)
    claimaggcount$month <- tolower(claimaggcount$month)
    
    claimagg$month <- capitalize(claimagg$month)
    claimaggmean$month <- capitalize(claimaggmean$month)
    claimaggcount$month <- capitalize(claimaggcount$month)
    
    claimagg$month <- factor(claimagg$month, levels=month.abb)
    claimaggmean$month <- factor(claimaggmean$month, levels=month.abb)
    claimaggcount$month <- factor(claimaggcount$month, levels=month.abb)
    
    claimaggloss_final <- claimagg[order(claimagg[,2], claimagg[,1]),]
    claimaggmean_final <- claimaggmean[order(claimaggmean[,2], claimaggmean[,1]),]
    claimaggcount_final <- claimaggcount[order(claimaggcount[,2], claimaggcount[,1]),]
    
    
    claimaggloss_final$monthcode <-  c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)[tolower(claimaggloss_final$month)]
    
    my1 <- as.data.frame(as.yearmon(seq(as.Date("1989-01-01"), as.Date("2015-12-01"), by = "month")))
    
    library(stringr)
    colnames(my1) <- c("monthyear")
    my2 <- as.data.frame(str_split_fixed(my1$monthyear, " ", 2))
    colnames(my2) <- c("month", "year")
    claimaggloss_final$year <- factor(claimaggloss_final$year)
    claimaggmean_final$year <- factor(claimaggloss_final$year)
    claimaggcount_final$year <- factor(claimaggloss_final$year)
    
    library(plyr)
    claimaggloss_final2 <- join(my2, claimaggloss_final, by = c("month", "year"))
    claimaggmean_final2 <- join(my2, claimaggmean_final, by = c("month", "year"))
    claimaggcount_final2 <- join(my2, claimaggcount_final, by = c("month", "year"))
    
    
    #------
    
    setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/")
    sumcount1 <-  read.csv("Palouse_summary_counts.csv")
    sumcount2 <- subset(sumcount1, state == statez1a)
    sumcount2 <- subset(sumcount2, county == countyz)
    sumcount2 <- subset(sumcount2, commodity == q)
    claimaggcount_final2 <- subset(sumcount2, damagecause == m)
    
    meanloss1 <- read.csv("Palouse_summary_meanloss.csv")
    meanloss2 <- subset(meanloss1, state == statez1a)
    meanloss2 <- subset(meanloss2, county == countyz)
    meanloss2 <- subset(meanloss2, commodity == q)
    claimaggmean_final2 <- subset(meanloss2, damagecause == m)
    
    sumloss1 <- read.csv("Palouse_summary_sumloss.csv")
    sumloss2 <- subset(sumloss1, state == statez1a)
    sumloss2 <- subset(sumloss2, county == countyz)
    sumloss2 <- subset(sumloss2, commodity == q)
    claimaggloss_final2 <- subset(sumloss2, damagecause == m)
    
    sumacres1 <- read.csv("Palouse_summary_sumacres.csv")
    sumacres2 <- subset(sumacres1, state == statez1a)
    sumacres2 <- subset(sumacres2, county == countyz)
    sumacres2 <- subset(sumacres2, commodity == q)
    claimaggacres_final2 <- subset(sumacres2, damagecause == m)
    
    claimaggloss_aggy <- aggregate(loss ~ month + damagecause + county + state + commodity, claimaggloss_final2, sum)
    
    yearr <- as.data.frame(toupper(month.abb))
    colnames(yearr) <- "month"
    claimaggloss_aggy2 <- join(yearr, claimaggloss_aggy)
    
    
    #--
    climvar <- c("pr", "th", "pdsi", "pet", "erc", "rmin", "rmax",  "tmmn",  "tmmx",  "srad",  "sph", "vs", "fm1000",  "fm100")
    startingmonth <- tolower(month.abb)
    monthrange <- rev(c(1:12))
    
    for (n in climvar) {}
    
      for (o in startingmonth) {}
    
    
         
         jan1 <- subset(gridmetmonthly, month == "jan"  | month == "dec")
         jan1 <- mean(getElement(jan1, n))
         
         jan2 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov")
         jan2 <- mean(getElement(jan2, n))
         
         jan3 <- subset(gridmetmonthly, month == "jan"  | month == "dec"| month == "nov" | month == "oct")
         jan3 <- mean(getElement(jan3, n))
         
         jan4 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         jan4 <- mean(getElement(jan4, n))
         
         jan5 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         jan5 <- mean(getElement(jan5, n))
         
         jan6 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         jan6 <- mean(getElement(jan6, n))
         
         jan7 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         jan7 <- mean(getElement(jan7, n))
         
         jan8 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         jan8 <- mean(getElement(jan8, n))
         
         jan9 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         jan9 <- mean(getElement(jan9, n))
         
         jan10 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         jan10 <- mean(getElement(jan10, n))
         
         jan11 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         jan11 <- mean(getElement(jan11, n))
         
         jan12 <- subset(gridmetmonthly, month == "jan"  | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" )
         jan12 <- mean(getElement(jan12, n))
         janfull <- c(jan1, jan2, jan3, jan4, jan5, jan6, jan7, jan8, jan9, jan10, jan11, jan12)
         
         
         
         
         
         feb1 <- subset(gridmetmonthly, month == "feb"  | month == "jan")
         feb1 <- mean(getElement(feb1, n))
         feb2 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec")
         feb2 <- mean(getElement(feb2, n))
         feb3 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov")
         feb3 <- mean(getElement(feb3, n))
         feb4 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         feb4 <- mean(getElement(feb4, n))
         feb5 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         feb5 <- mean(getElement(feb5, n))
         feb6 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         feb6 <- mean(getElement(feb6, n))
         feb7 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         feb7 <- mean(getElement(feb7, n))
         feb8 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         feb8 <- mean(getElement(feb8, n))
         feb9 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         feb9 <- mean(getElement(feb9, n))
         feb10 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         feb10 <- mean(getElement(feb10, n))
         feb11 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         feb11 <- mean(getElement(feb11, n))
         feb12 <- subset(gridmetmonthly, month == "feb"  | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" )
         feb12 <- mean(getElement(feb12, n))
         febfull <- c(feb1, feb2, feb3, feb4, feb5, feb6, feb7, feb8, feb9, feb10, feb11, feb12)
         
         mar1 <- subset(gridmetmonthly, month == "mar"  | month == "feb")
         mar1 <- mean(getElement(mar1, n))
         mar2 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan")
         mar2 <- mean(getElement(mar2, n))
         mar3 <- subset(gridmetmonthly, month == "mar"  | month == "feb"| month == "jan" | month == "dec")
         mar3 <- mean(getElement(mar3, n))
         mar4 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         mar4 <- mean(getElement(mar4, n))
         mar5 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         mar5 <- mean(getElement(mar5, n))
         mar6 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         mar6 <- mean(getElement(mar6, n))
         mar7 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         mar7 <- mean(getElement(mar7, n))
         mar8 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         mar8 <- mean(getElement(mar8, n))
         mar9 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         mar9 <- mean(getElement(mar9, n))
         mar10 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         mar10 <- mean(getElement(mar10, n))
         mar11 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         mar11 <- mean(getElement(mar11, n))
         mar12 <- subset(gridmetmonthly, month == "mar"  | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" )
         mar12 <- mean(getElement(mar12, n))
         marfull <- c(mar1, mar2, mar3, mar4, mar5, mar6, mar7, mar8, mar9, mar10, mar11, mar12)
         
         
         apr1 <- subset(gridmetmonthly, month == "apr"  | month == "mar")
         apr1 <- mean(getElement(apr1, n))
         apr2 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb")
         apr2 <- mean(getElement(apr2, n))
         apr3 <- subset(gridmetmonthly, month == "apr"  | month == "mar"| month == "feb" | month == "jan")
         apr3 <- mean(getElement(apr3, n))
         apr4 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         apr4 <- mean(getElement(apr4, n))
         apr5 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         apr5 <- mean(getElement(apr5, n))
         apr6 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         apr6 <- mean(getElement(apr6, n))
         apr7 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         apr7 <- mean(getElement(apr7, n))
         apr8 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         apr8 <- mean(getElement(apr8, n))
         apr9 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         apr9 <- mean(getElement(apr9, n))
         apr10 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         apr10 <- mean(getElement(apr10, n))
         apr11 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         apr11 <- mean(getElement(apr11, n))
         apr12 <- subset(gridmetmonthly, month == "apr"  | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" )
         apr12 <- mean(getElement(apr12, n))
         aprfull <- c(apr1, apr2, apr3, apr4, apr5, apr6, apr7, apr8, apr9, apr10, apr11, apr12)
         
         
         may1 <- subset(gridmetmonthly, month == "may"  | month == "apr")
         may1 <- mean(getElement(may1, n))
         may2 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar")
         may2 <- mean(getElement(may2, n))
         may3 <- subset(gridmetmonthly, month == "may"  | month == "apr"| month == "mar" | month == "feb")
         may3 <- mean(getElement(may3, n))
         may4 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         may4 <- mean(getElement(may4, n))
         may5 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         may5 <- mean(getElement(may5, n))
         may6 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         may6 <- mean(getElement(may6, n))
         may7 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         may7 <- mean(getElement(may7, n))
         may8 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         may8 <- mean(getElement(may8, n))
         may9 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         may9 <- mean(getElement(may9, n))
         may10 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         may10 <- mean(getElement(may10, n))
         may11 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         may11 <- mean(getElement(may11, n))
         may12 <- subset(gridmetmonthly, month == "may"  | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" )
         may12 <- mean(getElement(may12, n))
         mayfull <- c(may1, may2, may3, may4, may5, may6, may7, may8, may9, may10, may11, may12)
         
         
         jun1 <- subset(gridmetmonthly, month == "jun"  | month == "may")
         jun1 <- mean(getElement(jun1, n))
         jun2 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr")
         jun2 <- mean(getElement(jun2, n))
         jun3 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar")
         jun3 <- mean(getElement(jun3, n))
         jun4 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb")
         jun4 <- mean(getElement(jun4, n))
         jun5 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         jun5 <- mean(getElement(jun5, n))
         jun6 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         jun6 <- mean(getElement(jun6, n))
         jun7 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         jun7 <- mean(getElement(jun7, n))
         jun8 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         jun8 <- mean(getElement(jun8, n))
         jun9 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         jun9 <- mean(getElement(jun9, n))
         jun10 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         jun10 <- mean(getElement(jun10, n))
         jun11 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         jun11 <- mean(getElement(jun11, n))
         jun12 <- subset(gridmetmonthly, month == "jun"  | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" )
         jun12 <- mean(getElement(jun12, n))
         junfull <- c(jun1, jun2, jun3, jun4, jun5, jun6, jun7, jun8, jun9, jun10, jun11, jun12)
         
         
         jul1 <- subset(gridmetmonthly, month == "jul"  | month == "jun")
         jul1 <- mean(getElement(jul1, n))
         jul2 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may")
         jul2 <- mean(getElement(jul2, n))
         jul3 <- subset(gridmetmonthly, month == "jul"  | month == "jun"| month == "may" | month == "apr")
         jul3 <- mean(getElement(jul3, n))
         jul4 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar")
         jul4 <- mean(getElement(jul4, n))
         jul5 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         jul5 <- mean(getElement(jul5, n))
         jul6 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         jul6 <- mean(getElement(jul6, n))
         jul7 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         jul7 <- mean(getElement(jul7, n))
         jul8 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         jul8 <- mean(getElement(jul8, n))
         jul9 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         jul9 <- mean(getElement(jul9, n))
         jul10 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         jul10 <- mean(getElement(jul10, n))
         jul11 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         jul11 <- mean(getElement(jul11, n))
         jul12 <- subset(gridmetmonthly, month == "jul"  | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" )
         jul12 <- mean(getElement(jul12, n))
         julfull <- c(jul1, jul2, jul3, jul4, jul5, jul6, jul7, jul8, jul9, jul10, jul11, jul12)
         
         aug1 <- subset(gridmetmonthly, month == "aug"  | month == "jul")
         aug1 <- mean(getElement(aug1, n))
         aug2 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun")
         aug2 <- mean(getElement(aug2, n))
         aug3 <- subset(gridmetmonthly, month == "aug"  | month == "jul"| month == "jun" | month == "may")
         aug3 <- mean(getElement(aug3, n))
         aug4 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr")
         aug4 <- mean(getElement(aug4, n))
         aug5 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         aug5 <- mean(getElement(aug5, n))
         aug6 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         aug6 <- mean(getElement(aug6, n))
         aug7 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         aug7 <- mean(getElement(aug7, n))
         aug8 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         aug8 <- mean(getElement(aug8, n))
         aug9 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         aug9 <- mean(getElement(aug9, n))
         aug10 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         aug10 <- mean(getElement(aug10, n))
         aug11 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep")
         aug11 <- mean(getElement(aug11, n))
         aug12 <- subset(gridmetmonthly, month == "aug"  | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" | month == "aug" )
         aug12 <- mean(getElement(aug12, n))
         augfull <- c(aug1, aug2, aug3, aug4, aug5, aug6, aug7, aug8, aug9, aug10, aug11, aug12)
         
         
         sep1 <- subset(gridmetmonthly, month == "sep"  | month == "aug")
         sep1 <- mean(getElement(sep1, n))
         sep2 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul")
         sep2 <- mean(getElement(sep2, n))
         sep3 <- subset(gridmetmonthly, month == "sep"  | month == "aug"| month == "jul" | month == "jun")
         sep3 <- mean(getElement(sep3, n))
         sep4 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may")
         sep4 <- mean(getElement(sep4, n))
         sep5 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         sep5 <- mean(getElement(sep5, n))
         sep6 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         sep6 <- mean(getElement(sep6, n))
         sep7 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         sep7 <- mean(getElement(sep7, n))
         sep8 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         sep8 <- mean(getElement(sep8, n))
         sep9 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         sep9 <- mean(getElement(sep9, n))
         sep10 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         sep10 <- mean(getElement(sep10, n))
         sep11 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct")
         sep11 <- mean(getElement(sep11, n))
         sep12 <- subset(gridmetmonthly, month == "sep"  | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" | month == "sep" )
         sep12 <- mean(getElement(sep12, n))
         sepfull <- c(sep1, sep2, sep3, sep4, sep5, sep6, sep7, sep8, sep9, sep10, sep11, sep12)
         
         
         oct1 <- subset(gridmetmonthly, month == "oct"  | month == "sep")
         oct1 <- mean(getElement(oct1, n))
         oct2 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug")
         oct2 <- mean(getElement(oct2, n))
         oct3 <- subset(gridmetmonthly, month == "oct"  | month == "sep"| month == "aug" | month == "jul")
         oct3 <- mean(getElement(oct3, n))
         oct4 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         oct4 <- mean(getElement(oct4, n))
         oct5 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         oct5 <- mean(getElement(oct5, n))
         oct6 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         oct6 <- mean(getElement(oct6, n))
         oct7 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         oct7 <- mean(getElement(oct7, n))
         oct8 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         oct8 <- mean(getElement(oct8, n))
         oct9 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         oct9 <- mean(getElement(oct9, n))
         oct10 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         oct10 <- mean(getElement(oct10, n))
         oct11 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov")
         oct11 <- mean(getElement(oct11, n))
         oct12 <- subset(gridmetmonthly, month == "oct"  | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" | month == "oct" )
         oct12 <- mean(getElement(oct12, n))
         octfull <- c(oct1, oct2, oct3, oct4, oct5, oct6, oct7, oct8, oct9, oct10, oct11, oct12)
         
         
         nov1 <- subset(gridmetmonthly, month == "nov"  | month == "oct")
         nov1 <- mean(getElement(nov1, n))
         nov2 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep")
         nov2 <- mean(getElement(nov2, n))
         nov3 <- subset(gridmetmonthly, month == "nov"  | month == "oct"| month == "sep" | month == "aug")
         nov3 <- mean(getElement(nov3, n))
         nov4 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         nov4 <- mean(getElement(nov4, n))
         nov5 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         nov5 <- mean(getElement(nov5, n))
         nov6 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         nov6 <- mean(getElement(nov6, n))
         nov7 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         nov7 <- mean(getElement(nov7, n))
         nov8 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         nov8 <- mean(getElement(nov8, n))
         nov9 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         nov9 <- mean(getElement(nov9, n))
         nov10 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         nov10 <- mean(getElement(nov10, n))
         nov11 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec")
         nov11 <- mean(getElement(nov11, n))
         nov12 <- subset(gridmetmonthly, month == "nov"  | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" | month == "nov" )
         nov12 <- mean(getElement(nov12, n))
         novfull <- c(nov1, nov2, nov3, nov4, nov5, nov6, nov7, nov8, nov8, nov10, nov11, nov12)
         
         
         dec1 <- subset(gridmetmonthly, month == "dec"  | month == "nov")
         dec1 <- mean(getElement(dec1, n))
         dec2 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct")
         dec2 <- mean(getElement(dec2, n))
         dec3 <- subset(gridmetmonthly, month == "dec"  | month == "nov"| month == "oct" | month == "sep")
         dec3 <- mean(getElement(dec3, n))
         dec4 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug")
         dec4 <- mean(getElement(dec4, n))
         dec5 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul")
         dec5 <- mean(getElement(dec5, n))
         dec6 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun")
         dec6 <- mean(getElement(dec6, n))
         dec7 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may")
         dec7 <- mean(getElement(dec7, n))
         dec8 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr")
         dec8 <- mean(getElement(dec8, n))
         dec9 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar")
         dec9 <- mean(getElement(dec9, n))
         dec10 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb")
         dec10 <- mean(getElement(dec10, n))
         dec11 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan")
         dec11 <- mean(getElement(dec11, n))
         dec12 <- subset(gridmetmonthly, month == "dec"  | month == "nov" | month == "oct" | month == "sep" | month == "aug" | month == "jul" | month == "jun" | month == "may" | month == "apr" | month == "mar" | month == "feb" | month == "jan" | month == "dec" )
         dec12 <- mean(getElement(dec12, n))
         decfull <- c(dec1, dec2, dec3, dec4, dec5, dec6, dec7, dec8, dec9, dec10, dec11, dec12)
         
         monthfull <- rbind(janfull, febfull, marfull, aprfull, mayfull, junfull, julfull, augfull, sepfull, octfull, novfull, decfull)
         
         
         
         
         
    
    climmeanlongterm1989<- mean(longterm1989[,2])
    wheatdrought1989 <- subset(wheatdroughtclaim, year == 1989)
    wheatdroughtclaim_all1989 <- subset(wheatdroughtclaim_allall_final, year == 1989)
    wheatclaimlosssum1989 <- sum(wheatdrought1989$loss)
    wheatclaimacressum1989 <- sum(wheatdrought1989$acres)
    wheatclaimcounttotal1989 <- nrow(wheatdrought1989)/nrow(wheatdroughtclaim_all1989)
    wheatclaimcountsum1989 <- nrow(wheatdrought1989)
    
    
    longterm1990 <- gridmetmonthly[10:18,]
    shortterm1990 <- gridmetmonthly[15:18,]
    climmeanlongterm1990 <- mean(longterm1990[,2])
    climmeanshortterm1990 <- mean(shortterm1990[,2])
    wheatdrought1990 <- subset(wheatdroughtclaim, year == 1990)
    wheatdroughtclaim_all1990 <- subset(wheatdroughtclaim_allall_final, year == 1990)
    wheatclaimlosssum1990 <- sum(wheatdrought1990$loss)
    wheatclaimacressum1990 <- sum(wheatdrought1990$acres)
    wheatclaimcounttotal1990 <- nrow(wheatdrought1990)/nrow(wheatdroughtclaim_all1990)
    wheatclaimcountsum1990 <- nrow(wheatdrought1990)
    
    longterm1991<- gridmetmonthly[22:30,]
    shortterm1991 <- gridmetmonthly[27:30,]
    climmeanlongterm1991 <- mean(longterm1991[,2])
    climmeanshortterm1991 <- mean(shortterm1991[,2])
    wheatdrought1991 <- subset(wheatdroughtclaim, year == 1991)
    wheatdroughtclaim_all1991 <- subset(wheatdroughtclaim_allall_final, year == 1991)
    wheatclaimlosssum1991 <- sum(wheatdrought1991$loss)
    wheatclaimacressum1991 <- sum(wheatdrought1991$acres)
    wheatclaimcounttotal1991 <- nrow(wheatdrought1991)/nrow(wheatdroughtclaim_all1991)
    wheatclaimcountsum1991 <- nrow(wheatdrought1991)
    
    longterm1992 <- gridmetmonthly[34:42,]
    shortterm1992 <- gridmetmonthly[39:42,]
    climmeanlongterm1992 <- mean(longterm1992[,2])
    climmeanshortterm1992 <- mean(shortterm1992[,2])
    wheatdrought1992 <- subset(wheatdroughtclaim, year == 1992)
    wheatdroughtclaim_all1992 <- subset(wheatdroughtclaim_allall_final, year == 1992)
    wheatclaimlosssum1992 <- sum(wheatdrought1992$loss)
    wheatclaimacressum1992 <- sum(wheatdrought1992$acres)
    wheatclaimcounttotal1992 <- nrow(wheatdrought1992)/nrow(wheatdroughtclaim_all1992)
    wheatclaimcountsum1992 <- nrow(wheatdrought1992)
    
    longterm1993 <- gridmetmonthly[46:54,]
    shortterm1993 <- gridmetmonthly[51:54,]
    climmeanlongterm1993 <- mean(longterm1993[,2])
    climmeanshortterm1993 <- mean(shortterm1993[,2])
    wheatdrought1993 <- subset(wheatdroughtclaim, year == 1993)
    wheatdroughtclaim_all1993 <- subset(wheatdroughtclaim_allall_final, year == 1993)
    wheatclaimlosssum1993 <- sum(wheatdrought1993$loss)
    wheatclaimacressum1993 <- sum(wheatdrought1993$acres)
    wheatclaimcounttotal1993 <- nrow(wheatdrought1993)/nrow(wheatdroughtclaim_all1993)
    wheatclaimcountsum1993 <- nrow(wheatdrought1993)
    
    longterm1994 <- gridmetmonthly[58:66,]
    shortterm1994 <- gridmetmonthly[63:66,]
    climmeanlongterm1994 <- mean(longterm1994[,2])
    climmeanshortterm1994 <- mean(shortterm1994[,2])
    wheatdrought1994 <- subset(wheatdroughtclaim, year == 1994)
    wheatdroughtclaim_all1994 <- subset(wheatdroughtclaim_allall_final, year == 1994)
    wheatclaimlosssum1994 <- sum(wheatdrought1994$loss)
    wheatclaimacressum1994 <- sum(wheatdrought1994$acres)
    wheatclaimcounttotal1994 <- nrow(wheatdrought1994)/nrow(wheatdroughtclaim_all1994)
    wheatclaimcountsum1994 <- nrow(wheatdrought1994)
    
    longterm1995 <- gridmetmonthly[70:78,]
    shortterm1995 <- gridmetmonthly[75:78,]
    climmeanlongterm1995 <- mean(longterm1995[,2])
    climmeanshortterm1995 <- mean(shortterm1995[,2])
    wheatdrought1995 <- subset(wheatdroughtclaim, year == 1995)
    wheatdroughtclaim_all1995 <- subset(wheatdroughtclaim_allall_final, year == 1995)
    wheatclaimlosssum1995 <- sum(wheatdrought1995$loss)
    wheatclaimacressum1995 <- sum(wheatdrought1995$acres)
    wheatclaimcounttotal1995 <- nrow(wheatdrought1995)/nrow(wheatdroughtclaim_all1995)
    wheatclaimcountsum1995 <- nrow(wheatdrought1995)
    
    longterm1996 <- gridmetmonthly[82:90,]
    shortterm1996 <- gridmetmonthly[87:90,]
    climmeanlongterm1996 <- mean(longterm1996[,2])
    climmeanshortterm1996 <- mean(shortterm1996[,2])
    wheatdrought1996 <- subset(wheatdroughtclaim, year == 1996)
    wheatdroughtclaim_all1996 <- subset(wheatdroughtclaim_allall_final, year == 1996)
    wheatclaimlosssum1996 <- sum(wheatdrought1996$loss)
    wheatclaimacressum1996 <- sum(wheatdrought1996$acres)
    wheatclaimcounttotal1996 <- nrow(wheatdrought1996)/nrow(wheatdroughtclaim_all1996)
    wheatclaimcountsum1996 <- nrow(wheatdrought1996)
    
    longterm1997 <- gridmetmonthly[94:102,]
    shortterm1997  <- gridmetmonthly[99:102,]
    climmeanlongterm1997  <- mean(longterm1997[,2])
    climmeanshortterm1997  <- mean(shortterm1997[,2])
    wheatdrought1997  <- subset(wheatdroughtclaim, year == 1997)
    wheatdroughtclaim_all1997  <- subset(wheatdroughtclaim_allall_final, year == 1997)
    wheatclaimlosssum1997  <- sum(wheatdrought1997$loss)
    wheatclaimacressum1997 <- sum(wheatdrought1997$acres)
    wheatclaimcounttotal1997 <- nrow(wheatdrought1997)/nrow(wheatdroughtclaim_all1997)
    wheatclaimcountsum1997 <- nrow(wheatdrought1997)
    
    longterm1998 <- gridmetmonthly[106:114,]
    shortterm1998 <- gridmetmonthly[111:114,]
    climmeanlongterm1998 <- mean(longterm1998[,2])
    climmeanshortterm1998 <- mean(shortterm1998[,2])
    wheatdrought1998 <- subset(wheatdroughtclaim, year == 1998)
    wheatdroughtclaim_all1998 <- subset(wheatdroughtclaim_allall_final, year == 1998)
    wheatclaimlosssum1998 <- sum(wheatdrought1998$loss)
    wheatclaimacressum1998 <- sum(wheatdrought1998$acres)
    wheatclaimcounttotal1998 <- nrow(wheatdrought1998)/nrow(wheatdroughtclaim_all1998)
    wheatclaimcountsum1998 <- nrow(wheatdrought1998)
    
    longterm1999 <- gridmetmonthly[118:126,]
    shortterm1999 <- gridmetmonthly[123:126,]
    climmeanlongterm1999 <- mean(longterm1999[,2])
    climmeanshortterm1999 <- mean(shortterm1999[,2])
    wheatdrought1999 <- subset(wheatdroughtclaim, year == 1999)
    wheatdroughtclaim_all1999 <- subset(wheatdroughtclaim_allall_final, year == 1999)
    wheatclaimlosssum1999 <- sum(wheatdrought1999$loss)
    wheatclaimacressum1999 <- sum(wheatdrought1999$acres)
    wheatclaimcounttotal1999 <- nrow(wheatdrought1999)/nrow(wheatdroughtclaim_all1999)
    wheatclaimcountsum1999 <- nrow(wheatdrought1999)
    
    
    longterm2000 <- gridmetmonthly[130:138,]
    shortterm2000 <- gridmetmonthly[135:138,]
    climmeanlongterm2000 <- mean(longterm2000[,2])
    climmeanshortterm2000 <- mean(shortterm2000[,2])
    wheatdrought2000 <- subset(wheatdroughtclaim, year == 2000)
    wheatdroughtclaim_all2000 <- subset(wheatdroughtclaim_allall_final, year == 2000)
    wheatclaimlosssum2000 <- sum(wheatdrought2000$loss)
    wheatclaimacressum2000 <- sum(wheatdrought2000$acres)
    wheatclaimcounttotal2000 <- nrow(wheatdrought2000)/nrow(wheatdroughtclaim_all2000)
    wheatclaimcountsum2000 <- nrow(wheatdrought2000)
    
  
    
    #--
    
    longterm2001 <- gridmetmonthly[142:150,]
    shortterm2001 <- gridmetmonthly[147:150,]
    climmeanlongterm2001 <- mean(longterm2001[,2])
    climmeanshortterm2001 <- mean(shortterm2001[,2])
    wheatdrought2001 <- subset(wheatdroughtclaim, year == 2001)
    wheatdroughtclaim_all2001 <- subset(wheatdroughtclaim_allall_final, year == 2001)
    wheatclaimlosssum2001 <- sum(wheatdrought2001$loss)
    wheatclaimacressum2001 <- sum(wheatdrought2001$acres)
    wheatclaimcounttotal2001 <- nrow(wheatdrought2001)/nrow(wheatdroughtclaim_all2001)
    wheatclaimcountsum2001 <- nrow(wheatdrought2001)
    
    
    longterm2002 <- gridmetmonthly[154:162,]
    shortterm2002 <- gridmetmonthly[159:162,]
    climmeanlongterm2002 <- mean(longterm2002[,2])
    climmeanshortterm2002 <- mean(shortterm2002[,2])
    wheatdrought2002 <- subset(wheatdroughtclaim, year == 2002)
    wheatdroughtclaim_all2002 <- subset(wheatdroughtclaim_allall_final, year == 2002)
    wheatclaimlosssum2002 <- sum(wheatdrought2002$loss)
    wheatclaimacressum2002 <- sum(wheatdrought2002$acres)
    wheatclaimcounttotal2002 <- nrow(wheatdrought2002)/nrow(wheatdroughtclaim_all2002)
    wheatclaimcountsum2002 <- nrow(wheatdrought2002)
    
    longterm2003 <- gridmetmonthly[166:174,]
    shortterm2003 <- gridmetmonthly[171:174,]
    climmeanlongterm2003 <- mean(longterm2003[,2])
    climmeanshortterm2003 <- mean(shortterm2003[,2])
    wheatdrought2003 <- subset(wheatdroughtclaim, year == 2003)
    wheatdroughtclaim_all2003 <- subset(wheatdroughtclaim_allall_final, year == 2003)
    wheatclaimlosssum2003 <- sum(wheatdrought2003$loss)
    wheatclaimacressum2003 <- sum(wheatdrought2003$acres)
    wheatclaimcounttotal2003 <- nrow(wheatdrought2003)/nrow(wheatdroughtclaim_all2003)
    wheatclaimcountsum2003 <- nrow(wheatdrought2003)
    
    longterm2004 <- gridmetmonthly[178:186,]
    shortterm2004 <- gridmetmonthly[183:186,]
    climmeanlongterm2004 <- mean(longterm2004[,2])
    climmeanshortterm2004 <- mean(shortterm2004[,2])
    wheatdrought2004 <- subset(wheatdroughtclaim, year == 2004)
    wheatdroughtclaim_all2004 <- subset(wheatdroughtclaim_allall_final, year == 2004)
    wheatclaimlosssum2004 <- sum(wheatdrought2004$loss)
    wheatclaimacressum2004 <- sum(wheatdrought2004$acres)
    wheatclaimcounttotal2004 <- nrow(wheatdrought2004)/nrow(wheatdroughtclaim_all2004)
    wheatclaimcountsum2004 <- nrow(wheatdrought2004)
    
    longterm2005 <- gridmetmonthly[190:198,]
    shortterm2005 <- gridmetmonthly[195:198,]
    climmeanlongterm2005 <- mean(longterm2005[,2])
    climmeanshortterm2005 <- mean(shortterm2005[,2])
    wheatdrought2005 <- subset(wheatdroughtclaim, year == 2005)
    wheatdroughtclaim_all2005 <- subset(wheatdroughtclaim_allall_final, year == 2005)
    wheatclaimlosssum2005 <- sum(wheatdrought2005$loss)
    wheatclaimacressum2005 <- sum(wheatdrought2005$acres)
    wheatclaimcounttotal2005 <- nrow(wheatdrought2005)/nrow(wheatdroughtclaim_all2005)
    wheatclaimcountsum2005 <- nrow(wheatdrought2005)
    
    longterm2006 <- gridmetmonthly[202:210,]
    shortterm2006 <- gridmetmonthly[207:210,]
    climmeanlongterm2006 <- mean(longterm2006[,2])
    climmeanshortterm2006 <- mean(shortterm2006[,2])
    wheatdrought2006 <- subset(wheatdroughtclaim, year == 2006)
    wheatdroughtclaim_all2006 <- subset(wheatdroughtclaim_allall_final, year == 2006)
    wheatclaimlosssum2006 <- sum(wheatdrought2006$loss)
    wheatclaimacressum2006 <- sum(wheatdrought2006$acres)
    wheatclaimcounttotal2006 <- nrow(wheatdrought2006)/nrow(wheatdroughtclaim_all2006)
    wheatclaimcountsum2006 <- nrow(wheatdrought2006)
    
    longterm2007 <- gridmetmonthly[214:222,]
    shortterm2007 <- gridmetmonthly[219:222,]
    climmeanlongterm2007 <- mean(longterm2007[,2])
    climmeanshortterm2007 <- mean(shortterm2007[,2])
    wheatdrought2007 <- subset(wheatdroughtclaim, year == 2007)
    wheatdroughtclaim_all2007 <- subset(wheatdroughtclaim_allall_final, year == 2007)
    wheatclaimlosssum2007 <- sum(wheatdrought2007$loss)
    wheatclaimacressum2007 <- sum(wheatdrought2007$acres)
    wheatclaimcounttotal2007 <- nrow(wheatdrought2007)/nrow(wheatdroughtclaim_all2007)
    wheatclaimcountsum2007 <- nrow(wheatdrought2007)
    
    longterm2008 <- gridmetmonthly[226:234,]
    shortterm2008 <- gridmetmonthly[231:234,]
    climmeanlongterm2008 <- mean(longterm2008[,2])
    climmeanshortterm2008 <- mean(shortterm2008[,2])
    wheatdrought2008 <- subset(wheatdroughtclaim, year == 2008)
    wheatdroughtclaim_all2008 <- subset(wheatdroughtclaim_allall_final, year == 2008)
    wheatclaimlosssum2008 <- sum(wheatdrought2008$loss)
    wheatclaimacressum2008 <- sum(wheatdrought2008$acres)
    wheatclaimcounttotal2008 <- nrow(wheatdrought2008)/nrow(wheatdroughtclaim_all2008)
    wheatclaimcountsum2008 <- nrow(wheatdrought2008)
    
    longterm2009 <- gridmetmonthly[238:246,]
    shortterm2009 <- gridmetmonthly[243:246,]
    climmeanlongterm2009 <- mean(longterm2009[,2])
    climmeanshortterm2009 <- mean(shortterm2009[,2])
    wheatdrought2009 <- subset(wheatdroughtclaim, year == 2009)
    wheatdroughtclaim_all2009 <- subset(wheatdroughtclaim_allall_final, year == 2009)
    wheatclaimlosssum2009 <- sum(wheatdrought2009$loss)
    wheatclaimacressum2009 <- sum(wheatdrought2009$acres)
    wheatclaimcounttotal2009 <- nrow(wheatdrought2009)/nrow(wheatdroughtclaim_all2009)
    wheatclaimcountsum2009 <- nrow(wheatdrought2009)

    longterm2010 <- gridmetmonthly[250:258,]
    shortterm2010 <- gridmetmonthly[255:258,]
    climmeanlongterm2010 <- mean(longterm2010[,2])
    climmeanshortterm2010 <- mean(shortterm2010[,2])
    wheatdrought2010 <- subset(wheatdroughtclaim, year == 2010)
    wheatdroughtclaim_all2010 <- subset(wheatdroughtclaim_allall_final, year == 2010)
    wheatclaimlosssum2010 <- sum(wheatdrought2010$loss)
    wheatclaimacressum2010 <- sum(wheatdrought2010$acres)
    wheatclaimcounttotal2010 <- nrow(wheatdrought2010)/nrow(wheatdroughtclaim_all2010)
    wheatclaimcountsum2010 <- nrow(wheatdrought2010)
    
    longterm2011 <- gridmetmonthly[262:270,]
    shortterm2011 <- gridmetmonthly[267:270,]
    climmeanlongterm2011 <- mean(longterm2011[,2])
    climmeanshortterm2011 <- mean(shortterm2011[,2])
    wheatdrought2011 <- subset(wheatdroughtclaim, year == 2011)
    wheatdroughtclaim_all2011 <- subset(wheatdroughtclaim_allall_final, year == 2011)
    wheatclaimlosssum2011 <- sum(wheatdrought2011$loss)
    wheatclaimacressum2011 <- sum(wheatdrought2011$acres)
    wheatclaimcounttotal2011 <- nrow(wheatdrought2011)/nrow(wheatdroughtclaim_all2011)
    wheatclaimcountsum2011 <- nrow(wheatdrought2011)
    
    
    longterm2012 <- gridmetmonthly[274:282,]
    shortterm2012 <- gridmetmonthly[279:282,]
    climmeanlongterm2012 <- mean(longterm2012[,2])
    climmeanshortterm2012 <- mean(shortterm2012[,2])
    wheatdrought2012 <- subset(wheatdroughtclaim, year == 2012)
    wheatdroughtclaim_all2012 <- subset(wheatdroughtclaim_allall_final, year == 2012)
    wheatclaimlosssum2012 <- sum(wheatdrought2012$loss)
    wheatclaimacressum2012 <- sum(wheatdrought2012$acres)
    wheatclaimcounttotal2012 <- nrow(wheatdrought2012)/nrow(wheatdroughtclaim_all2012)
    wheatclaimcountsum2012 <- nrow(wheatdrought2012)
    
    longterm2013 <- gridmetmonthly[286:294,]
    shortterm2013 <- gridmetmonthly[291:294,]
    climmeanlongterm2013 <- mean(longterm2013[,2])
    climmeanshortterm2013 <- mean(shortterm2013[,2])
    wheatdrought2013 <- subset(wheatdroughtclaim, year == 2013)
    wheatdroughtclaim_all2013 <- subset(wheatdroughtclaim_allall_final, year == 2013)
    wheatclaimlosssum2013 <- sum(wheatdrought2013$loss)
    wheatclaimacressum2013 <- sum(wheatdrought2013$acres)
    wheatclaimcounttotal2013 <- nrow(wheatdrought2013)/nrow(wheatdroughtclaim_all2013)
    wheatclaimcountsum2013 <- nrow(wheatdrought2013)
    
    longterm2014 <- gridmetmonthly[298:306,]
    shortterm2014 <- gridmetmonthly[303:306,]
    climmeanlongterm2014 <- mean(longterm2014[,2])
    climmeanshortterm2014 <- mean(shortterm2014[,2])
    wheatdrought2014 <- subset(wheatdroughtclaim, year == 2014)
    wheatdroughtclaim_all2014 <- subset(wheatdroughtclaim_allall_final, year == 2014)
    wheatclaimlosssum2014 <- sum(wheatdrought2014$loss)
    wheatclaimacressum2014 <- sum(wheatdrought2014$acres)
    wheatclaimcounttotal2014 <- nrow(wheatdrought2014)/nrow(wheatdroughtclaim_all2014)
    wheatclaimcountsum2014 <- nrow(wheatdrought2014)
    
    longterm2015 <- gridmetmonthly[310:318,]
    shortterm2015 <- gridmetmonthly[315:318,]
    climmeanlongterm2015 <- mean(longterm2015[,2])
    climmeanshortterm2015 <- mean(shortterm2015[,2])
    wheatdrought2015 <- subset(wheatdroughtclaim, year == 2015)
    wheatdroughtclaim_all2015 <- subset(wheatdroughtclaim_allall_final, year == 2015)
    wheatclaimlosssum2015 <- sum(wheatdrought2015$loss)
    wheatclaimacressum2015 <- sum(wheatdrought2015$acres)
    wheatclaimcounttotal2015 <- nrow(wheatdrought2015)/nrow(wheatdroughtclaim_all2015)
    wheatclaimcountsum2015 <- nrow(wheatdrought2015)
    
    
    wls <- rbind(wheatclaimlosssum1989, wheatclaimlosssum1990, wheatclaimlosssum1991, wheatclaimlosssum1992, wheatclaimlosssum1993, wheatclaimlosssum1994, wheatclaimlosssum1995, wheatclaimlosssum1996, wheatclaimlosssum1997, wheatclaimlosssum1998, wheatclaimlosssum1999, wheatclaimlosssum2000, wheatclaimlosssum2001, wheatclaimlosssum2002, wheatclaimlosssum2003, wheatclaimlosssum2004, wheatclaimlosssum2005,wheatclaimlosssum2006, wheatclaimlosssum2007, wheatclaimlosssum2008, wheatclaimlosssum2009,wheatclaimlosssum2010,wheatclaimlosssum2011,wheatclaimlosssum2012,wheatclaimlosssum2013,wheatclaimlosssum2014,wheatclaimlosssum2015)
    wla <- rbind(wheatclaimacressum1989, wheatclaimacressum1990, wheatclaimacressum1991, wheatclaimacressum1992, wheatclaimacressum1993, wheatclaimacressum1994, wheatclaimacressum1995, wheatclaimacressum1996, wheatclaimacressum1997, wheatclaimacressum1998, wheatclaimacressum1999, wheatclaimacressum2000, wheatclaimacressum2001, wheatclaimacressum2002,wheatclaimacressum2003,wheatclaimacressum2004, wheatclaimacressum2005, wheatclaimacressum2006, wheatclaimacressum2007, wheatclaimacressum2008,wheatclaimacressum2009,wheatclaimacressum2010,wheatclaimacressum2011,wheatclaimacressum2012,wheatclaimacressum2013,wheatclaimacressum2014,wheatclaimacressum2015)
    wlc <- rbind(wheatclaimcountsum1989, wheatclaimcountsum1990, wheatclaimcountsum1991, wheatclaimcountsum1992, wheatclaimcountsum1993, wheatclaimcountsum1994, wheatclaimcountsum1995, wheatclaimcountsum1996, wheatclaimcountsum1997, wheatclaimcountsum1998, wheatclaimcountsum1999, wheatclaimcountsum2000, wheatclaimcountsum2001, wheatclaimcountsum2002, wheatclaimcountsum2003, wheatclaimcountsum2004, wheatclaimcountsum2005, wheatclaimcountsum2006, wheatclaimcountsum2007, wheatclaimcountsum2008, wheatclaimcountsum2009, wheatclaimcountsum2010, wheatclaimcountsum2011, wheatclaimcountsum2012, wheatclaimcountsum2013, wheatclaimcountsum2014, wheatclaimcountsum2015)
    climmeanlongterm1 <- rbind(climmeanlongterm1989, climmeanlongterm1990, climmeanlongterm1991, climmeanlongterm1992, climmeanlongterm1993, climmeanlongterm1994, climmeanlongterm1995, climmeanlongterm1996, climmeanlongterm1997, climmeanlongterm1998, climmeanlongterm1999, climmeanlongterm2000, climmeanlongterm2001, climmeanlongterm2002, climmeanlongterm2003, climmeanlongterm2004, climmeanlongterm2005, climmeanlongterm2006, climmeanlongterm2007,climmeanlongterm2008,climmeanlongterm2009,climmeanlongterm2010,climmeanlongterm2011,climmeanlongterm2012,climmeanlongterm2013,climmeanlongterm2014,climmeanlongterm2015)
    climmeanshortterm1 <- rbind(climmeanshortterm1989, climmeanshortterm1990, climmeanshortterm1991, climmeanshortterm1992, climmeanshortterm1993, climmeanshortterm1994, climmeanshortterm1995, climmeanshortterm1996, climmeanshortterm1997, climmeanshortterm1998, climmeanshortterm1999, climmeanshortterm2000, climmeanshortterm2001, climmeanshortterm2002, climmeanshortterm2003, climmeanshortterm2004, climmeanshortterm2005, climmeanshortterm2006, climmeanshortterm2007,climmeanshortterm2008,climmeanshortterm2009,climmeanshortterm2010,climmeanshortterm2011,climmeanshortterm2012,climmeanshortterm2013,climmeanshortterm2014,climmeanshortterm2015)
    wheatclaimcounttotal <- rbind(wheatclaimcounttotal1989, wheatclaimcounttotal1990, wheatclaimcounttotal1991, wheatclaimcounttotal1992, wheatclaimcounttotal1993, wheatclaimcounttotal1994, wheatclaimcounttotal1995, wheatclaimcounttotal1996, wheatclaimcounttotal1997, wheatclaimcounttotal1998, wheatclaimcounttotal1999, wheatclaimcounttotal2000, wheatclaimcounttotal2001, wheatclaimcounttotal2002, wheatclaimcounttotal2003, wheatclaimcounttotal2004, wheatclaimcounttotal2005,  wheatclaimcounttotal2006, wheatclaimcounttotal2007, wheatclaimcounttotal2008,wheatclaimcounttotal2009, wheatclaimcounttotal2010, wheatclaimcounttotal2011, wheatclaimcounttotal2012, wheatclaimcounttotal2013, wheatclaimcounttotal2014, wheatclaimcounttotal2015)
    wheatclaimcountsumtotal <- rbind(wheatclaimcountsum1989, wheatclaimcountsum1990, wheatclaimcountsum1991, wheatclaimcountsum1992, wheatclaimcountsum1993, wheatclaimcountsum1994, wheatclaimcountsum1995, wheatclaimcountsum1996, wheatclaimcountsum1997, wheatclaimcountsum1998, wheatclaimcountsum1999, wheatclaimcountsum2000, wheatclaimcountsum2001, wheatclaimcountsum2002, wheatclaimcountsum2003, wheatclaimcountsum2004, wheatclaimcountsum2005, wheatclaimcountsum2006, wheatclaimcountsum2007, wheatclaimcountsum2008, wheatclaimcountsum2009, wheatclaimcountsum2010, wheatclaimcountsum2011, wheatclaimcountsum2012, wheatclaimcountsum2013, wheatclaimcountsum2014, wheatclaimcountsum2015)
    
    finalz <- cbind(climmeanlongterm1,wls, wla, wlc, wheatclaimcounttotal, climmeanshortterm1)
    finalz <- data.frame(finalz)
    names(finalz)[1] <- c("soil_moisture_longterm")
    names(finalz)[2] <- c("loss")
    names(finalz)[3] <- c("acres")
    names(finalz)[4] <- c("count")
    names(finalz)[5] <- c("countratio")
    names(finalz)[6] <- c("soil_moisture_shorterm")
    
    rownames(finalz) <- c(1989:2015)
    finalz[7]<- c(1989:2015)
    finalz[8] <- c(pu)
    names(finalz)[7] <- c("year")
    names(finalz)[8] <- c("county")
    finalz[9] <- q
    finalz[10] <- m
    names(finalz)[10] <- c("damagecause")
    names(finalz)[9] <- c("state")
    
    finalz_soil <- finalz
    
    ###--------
    
    setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3")
    files  <- list.files(pattern = '\\_palouse_summary$')
    tables <- lapply(files, read.csv, header = TRUE)
    combined.df <- do.call(rbind , tables)
    #----sums <- read.csv(paste(kk, "_", i, "_palouse_summary", sep=""))
    
    
    
    #---construct a county fips name file
    library(maps)
    data(county.fips)
    colnames(combined.df)[16] <- "fips"
    library(stringr)
    county.fips2 <- data.frame(str_split_fixed(county.fips$polyname, ",", 2))
    colnames(county.fips2) <- c("state", "county")
    county.fips3 <- cbind(county.fips, county.fips2)
    combined1.df <- merge(combined.df,county.fips3, by = 'fips')
    
    #--create unique values for each county grouping for palouse region
    lister <- unique(combined1.df$county)
    listerstate <- unique(combined1.df$state)
    listeridaho<- paste(lister[1:7], "_", listerstate[1], sep="")
    listeroregon <- paste(lister[8:14], "_", listerstate[2], sep="")
    listerwashington <- paste(lister[15:26], "_", listerstate[3], sep="")
    listercomb <- c(listeridaho, listeroregon, listerwashington)
    
    capFirst <- function(s) {
      paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
    }
    
    
    setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
    p <- tolower(p)
    countyz <- tolower(countyz)
    #statez <- toupper(statez)
    
    files  <- list.files(pattern = '\\_palouse_summary$')
    tables <- lapply(files, read.csv, header = TRUE, strip.white = TRUE)
    gridmetmonthly <- do.call(rbind , tables)
    
    gridmetmonthly <- data.frame(gridmetmonthly)
    
    library(maps)
    data(county.fips)
    colnames(gridmetmonthly)[16] <- "fips"
    library(stringr)
    county.fips2 <- data.frame(str_split_fixed(county.fips$polyname, ",", 2))
    colnames(county.fips2) <- c("state", "county")
    county.fips3 <- cbind(county.fips, county.fips2)
    gridmetmonthly <- merge(gridmetmonthly,county.fips3, by = 'fips')
    
    
    #statez = simpleCap(statez)
    #countyz = simpleCap(countyz)
    #statez = state.abb[grep(statez, state.name)]
    gridmetmonthly <- subset(gridmetmonthly, state == statez)
    gridmetmonthly <- subset(gridmetmonthly, county == countyz)
    
    gridmetmonthly$monthchar <- as.character(gridmetmonthly$month)
    
    library(Hmisc)
    gridmetmonthly$monthchar <- capitalize(gridmetmonthly$monthchar)
    
    gridmetmonthly$monthchar <- factor(gridmetmonthly$monthchar, levels=month.abb)
    #gridmetmonthly$monthchar <- as.numeric(gridmetmonthly$monthchar)
    
    gridmetmonthly <- gridmetmonthly[order(gridmetmonthly[,18], gridmetmonthly[,22]),]
    
    #gridmetmonthly$monthyear <- paste(as.numeric(gridmetmonthly$monthchar), ".", gridmetmonthly$year, sep="")
    gridmetmonthly$ID<-seq.int(nrow(gridmetmonthly))
    
    #---05.18.17 need to create loop thru all claims and assign short term and long term drought variables.  mar 2009 is go back 3 and 6.  June 2009 is go back 6 and 9
    
    
    
    
    
    #usda <- paste("/dmine/data/USDA/crop_indemnity_txt/", i, ".txt", sep="")
    #usda <- read.csv(usda, header=FALSE, sep="|")
    #usda <- data.frame(usda)
    #gridmetmonthly <- read.csv(gridmetmonthly, strip.white=TRUE)
    gridmetmonthly <- data.frame(gridmetmonthly)
    #usda <- as.matrix(usda)
    #gridmetmonthly <- as.matrix(gridmetmonthly)
    #colnames(usda) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insuranceplancode", "insurancename", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "acres", "loss")
    #usda$county <- trimws(usda$county)
    listersplit <- unlist(strsplit(p, "[_]"))
    countyz <- listersplit[1]
    countyz <- simpleCap(countyz)
    
    statez <- listersplit[2]
    #countyz <- capitalize(countyz)
    statez1 <- stateFromLower(statez)
    statez1 <- as.vector(statez1)
    #usda_state <- subset(usda, state == statez)
    #usda_county <- subset(usda, county == countyz)
    #usda_county$ID<-seq.int(nrow(usda_county))
    
    #--
    
    longterm1989 <- gridmetmonthly[1:6,]
    shortterm1989 <- gridmetmonthly[3:6,]
    climmeanlongterm1989<- colMeans(longterm1989[,3:16])
    climmeanshortterm1989 <- colMeans(shortterm1989[,3:16])
    wheatdrought1989 <- subset(wheatdroughtclaim, year == 1989)
    wheatdroughtclaim_all1989 <- subset(wheatdroughtclaim_allall_final, year == 1989)
    wheatclaimlosssum1989 <- sum(wheatdrought1989$loss)
    wheatclaimacressum1989 <- sum(wheatdrought1989$acres)
    wheatclaimcounttotal1989 <- nrow(wheatdrought1989)/nrow(wheatdroughtclaim_all1989)
    wheatclaimcountsum1989 <- nrow(wheatdrought1989)
    
    
    longterm1990 <- gridmetmonthly[10:18,]
    shortterm1990 <- gridmetmonthly[15:18,]
    climmeanlongterm1990 <- colMeans(longterm1990[,3:16])
    climmeanshortterm1990 <- colMeans(shortterm1990[,3:16])
    wheatdrought1990 <- subset(wheatdroughtclaim, year == 1990)
    wheatdroughtclaim_all1990 <- subset(wheatdroughtclaim_allall_final, year == 1990)
    wheatclaimlosssum1990 <- sum(wheatdrought1990$loss)
    wheatclaimacressum1990 <- sum(wheatdrought1990$acres)
    wheatclaimcounttotal1990 <- nrow(wheatdrought1990)/nrow(wheatdroughtclaim_all1990)
    wheatclaimcountsum1990 <- nrow(wheatdrought1990)
    
    longterm1991<- gridmetmonthly[22:30,]
    shortterm1991 <- gridmetmonthly[27:30,]
    climmeanlongterm1991 <- colMeans(longterm1991[,3:16])
    climmeanshortterm1991 <- colMeans(shortterm1991[,3:16])
    wheatdrought1991 <- subset(wheatdroughtclaim, year == 1991)
    wheatdroughtclaim_all1991 <- subset(wheatdroughtclaim_allall_final, year == 1991)
    wheatclaimlosssum1991 <- sum(wheatdrought1991$loss)
    wheatclaimacressum1991 <- sum(wheatdrought1991$acres)
    wheatclaimcounttotal1991 <- nrow(wheatdrought1991)/nrow(wheatdroughtclaim_all1991)
    wheatclaimcountsum1991 <- nrow(wheatdrought1991)
    
    longterm1992 <- gridmetmonthly[34:42,]
    shortterm1992 <- gridmetmonthly[39:42,]
    climmeanlongterm1992 <- colMeans(longterm1992[,3:16])
    climmeanshortterm1992 <- colMeans(shortterm1992[,3:16])
    wheatdrought1992 <- subset(wheatdroughtclaim, year == 1992)
    wheatdroughtclaim_all1992 <- subset(wheatdroughtclaim_allall_final, year == 1992)
    wheatclaimlosssum1992 <- sum(wheatdrought1992$loss)
    wheatclaimacressum1992 <- sum(wheatdrought1992$acres)
    wheatclaimcounttotal1992 <- nrow(wheatdrought1992)/nrow(wheatdroughtclaim_all1992)
    wheatclaimcountsum1992 <- nrow(wheatdrought1992)
    
    longterm1993 <- gridmetmonthly[46:54,]
    shortterm1993 <- gridmetmonthly[51:54,]
    climmeanlongterm1993 <- colMeans(longterm1993[,3:16])
    climmeanshortterm1993 <- colMeans(shortterm1993[,3:16])
    wheatdrought1993 <- subset(wheatdroughtclaim, year == 1993)
    wheatdroughtclaim_all1993 <- subset(wheatdroughtclaim_allall_final, year == 1993)
    wheatclaimlosssum1993 <- sum(wheatdrought1993$loss)
    wheatclaimacressum1993 <- sum(wheatdrought1993$acres)
    wheatclaimcounttotal1993 <- nrow(wheatdrought1993)/nrow(wheatdroughtclaim_all1993)
    wheatclaimcountsum1993 <- nrow(wheatdrought1993)
    
    longterm1994 <- gridmetmonthly[58:66,]
    shortterm1994 <- gridmetmonthly[63:66,]
    climmeanlongterm1994 <- colMeans(longterm1994[,3:16])
    climmeanshortterm1994 <- colMeans(shortterm1994[,3:16])
    wheatdrought1994 <- subset(wheatdroughtclaim, year == 1994)
    wheatdroughtclaim_all1994 <- subset(wheatdroughtclaim_allall_final, year == 1994)
    wheatclaimlosssum1994 <- sum(wheatdrought1994$loss)
    wheatclaimacressum1994 <- sum(wheatdrought1994$acres)
    wheatclaimcounttotal1994 <- nrow(wheatdrought1994)/nrow(wheatdroughtclaim_all1994)
    wheatclaimcountsum1994 <- nrow(wheatdrought1994)
    
    longterm1995 <- gridmetmonthly[70:78,]
    shortterm1995 <- gridmetmonthly[75:78,]
    climmeanlongterm1995 <- colMeans(longterm1995[,3:16])
    climmeanshortterm1995 <- colMeans(shortterm1995[,3:16])
    wheatdrought1995 <- subset(wheatdroughtclaim, year == 1995)
    wheatdroughtclaim_all1995 <- subset(wheatdroughtclaim_allall_final, year == 1995)
    wheatclaimlosssum1995 <- sum(wheatdrought1995$loss)
    wheatclaimacressum1995 <- sum(wheatdrought1995$acres)
    wheatclaimcounttotal1995 <- nrow(wheatdrought1995)/nrow(wheatdroughtclaim_all1995)
    wheatclaimcountsum1995 <- nrow(wheatdrought1995)
    
    longterm1996 <- gridmetmonthly[82:90,]
    shortterm1996 <- gridmetmonthly[87:90,]
    climmeanlongterm1996 <- colMeans(longterm1996[,3:16])
    climmeanshortterm1996 <- colMeans(shortterm1996[,3:16])
    wheatdrought1996 <- subset(wheatdroughtclaim, year == 1996)
    wheatdroughtclaim_all1996 <- subset(wheatdroughtclaim_allall_final, year == 1996)
    wheatclaimlosssum1996 <- sum(wheatdrought1996$loss)
    wheatclaimacressum1996 <- sum(wheatdrought1996$acres)
    wheatclaimcounttotal1996 <- nrow(wheatdrought1996)/nrow(wheatdroughtclaim_all1996)
    wheatclaimcountsum1996 <- nrow(wheatdrought1996)
    
    longterm1997 <- gridmetmonthly[94:102,]
    shortterm1997  <- gridmetmonthly[99:102,]
    climmeanlongterm1997  <- colMeans(longterm1997[,3:16])
    climmeanshortterm1997  <- colMeans(shortterm1997[,3:16])
    wheatdrought1997  <- subset(wheatdroughtclaim, year == 1997)
    wheatdroughtclaim_all1997  <- subset(wheatdroughtclaim_allall_final, year == 1997)
    wheatclaimlosssum1997  <- sum(wheatdrought1997$loss)
    wheatclaimacressum1997 <- sum(wheatdrought1997$acres)
    wheatclaimcounttotal1997 <- nrow(wheatdrought1997)/nrow(wheatdroughtclaim_all1997)
    wheatclaimcountsum1997 <- nrow(wheatdrought1997)
    
    longterm1998 <- gridmetmonthly[106:114,]
    shortterm1998 <- gridmetmonthly[111:114,]
    climmeanlongterm1998 <- colMeans(longterm1998[,3:16])
    climmeanshortterm1998 <- colMeans(shortterm1998[,3:16])
    wheatdrought1998 <- subset(wheatdroughtclaim, year == 1998)
    wheatdroughtclaim_all1998 <- subset(wheatdroughtclaim_allall_final, year == 1998)
    wheatclaimlosssum1998 <- sum(wheatdrought1998$loss)
    wheatclaimacressum1998 <- sum(wheatdrought1998$acres)
    wheatclaimcounttotal1998 <- nrow(wheatdrought1998)/nrow(wheatdroughtclaim_all1998)
    wheatclaimcountsum1998 <- nrow(wheatdrought1998)
    
    longterm1999 <- gridmetmonthly[118:126,]
    shortterm1999 <- gridmetmonthly[123:126,]
    climmeanlongterm1999 <- colMeans(longterm1999[,3:16])
    climmeanshortterm1999 <- colMeans(shortterm1999[,3:16])
    wheatdrought1999 <- subset(wheatdroughtclaim, year == 1999)
    wheatdroughtclaim_all1999 <- subset(wheatdroughtclaim_allall_final, year == 1999)
    wheatclaimlosssum1999 <- sum(wheatdrought1999$loss)
    wheatclaimacressum1999 <- sum(wheatdrought1999$acres)
    wheatclaimcounttotal1999 <- nrow(wheatdrought1999)/nrow(wheatdroughtclaim_all1999)
    wheatclaimcountsum1999 <- nrow(wheatdrought1999)
    
    
    longterm2000 <- gridmetmonthly[130:138,]
    shortterm2000 <- gridmetmonthly[135:138,]
    climmeanlongterm2000 <- colMeans(longterm2000[,3:16])
    climmeanshortterm2000 <- colMeans(shortterm2000[,3:16])
    wheatdrought2000 <- subset(wheatdroughtclaim, year == 2000)
    wheatdroughtclaim_all2000 <- subset(wheatdroughtclaim_allall_final, year == 2000)
    wheatclaimlosssum2000 <- sum(wheatdrought2000$loss)
    wheatclaimacressum2000 <- sum(wheatdrought2000$acres)
    wheatclaimcounttotal2000 <- nrow(wheatdrought2000)/nrow(wheatdroughtclaim_all2000)
    wheatclaimcountsum2000 <- nrow(wheatdrought2000)
    
    
    
    #--
    
    longterm2001 <- gridmetmonthly[142:150,]
    shortterm2001 <- gridmetmonthly[147:150,]
    climmeanlongterm2001 <- colMeans(longterm2001[,3:16])
    climmeanshortterm2001 <- colMeans(shortterm2001[,3:16])
    wheatdrought2001 <- subset(wheatdroughtclaim, year == 2001)
    wheatdroughtclaim_all2001 <- subset(wheatdroughtclaim_allall_final, year == 2001)
    wheatclaimlosssum2001 <- sum(wheatdrought2001$loss)
    wheatclaimacressum2001 <- sum(wheatdrought2001$acres)
    wheatclaimcounttotal2001 <- nrow(wheatdrought2001)/nrow(wheatdroughtclaim_all2001)
    wheatclaimcountsum2001 <- nrow(wheatdrought2001)
    
    
    longterm2002 <- gridmetmonthly[154:162,]
    shortterm2002 <- gridmetmonthly[159:162,]
    climmeanlongterm2002 <- colMeans(longterm2002[,3:16])
    climmeanshortterm2002 <- colMeans(shortterm2002[,3:16])
    wheatdrought2002 <- subset(wheatdroughtclaim, year == 2002)
    wheatdroughtclaim_all2002 <- subset(wheatdroughtclaim_allall_final, year == 2002)
    wheatclaimlosssum2002 <- sum(wheatdrought2002$loss)
    wheatclaimacressum2002 <- sum(wheatdrought2002$acres)
    wheatclaimcounttotal2002 <- nrow(wheatdrought2002)/nrow(wheatdroughtclaim_all2002)
    wheatclaimcountsum2002 <- nrow(wheatdrought2002)
    
    longterm2003 <- gridmetmonthly[166:174,]
    shortterm2003 <- gridmetmonthly[171:174,]
    climmeanlongterm2003 <- colMeans(longterm2003[,3:16])
    climmeanshortterm2003 <- colMeans(shortterm2003[,3:16])
    wheatdrought2003 <- subset(wheatdroughtclaim, year == 2003)
    wheatdroughtclaim_all2003 <- subset(wheatdroughtclaim_allall_final, year == 2003)
    wheatclaimlosssum2003 <- sum(wheatdrought2003$loss)
    wheatclaimacressum2003 <- sum(wheatdrought2003$acres)
    wheatclaimcounttotal2003 <- nrow(wheatdrought2003)/nrow(wheatdroughtclaim_all2003)
    wheatclaimcountsum2003 <- nrow(wheatdrought2003)
    
    longterm2004 <- gridmetmonthly[178:186,]
    shortterm2004 <- gridmetmonthly[183:186,]
    climmeanlongterm2004 <- colMeans(longterm2004[,3:16])
    climmeanshortterm2004 <- colMeans(shortterm2004[,3:16])
    wheatdrought2004 <- subset(wheatdroughtclaim, year == 2004)
    wheatdroughtclaim_all2004 <- subset(wheatdroughtclaim_allall_final, year == 2004)
    wheatclaimlosssum2004 <- sum(wheatdrought2004$loss)
    wheatclaimacressum2004 <- sum(wheatdrought2004$acres)
    wheatclaimcounttotal2004 <- nrow(wheatdrought2004)/nrow(wheatdroughtclaim_all2004)
    wheatclaimcountsum2004 <- nrow(wheatdrought2004)
    
    longterm2005 <- gridmetmonthly[190:198,]
    shortterm2005 <- gridmetmonthly[195:198,]
    climmeanlongterm2005 <- colMeans(longterm2005[,3:16])
    climmeanshortterm2005 <- colMeans(shortterm2005[,3:16])
    wheatdrought2005 <- subset(wheatdroughtclaim, year == 2005)
    wheatdroughtclaim_all2005 <- subset(wheatdroughtclaim_allall_final, year == 2005)
    wheatclaimlosssum2005 <- sum(wheatdrought2005$loss)
    wheatclaimacressum2005 <- sum(wheatdrought2005$acres)
    wheatclaimcounttotal2005 <- nrow(wheatdrought2005)/nrow(wheatdroughtclaim_all2005)
    wheatclaimcountsum2005 <- nrow(wheatdrought2005)
    
    longterm2006 <- gridmetmonthly[202:210,]
    shortterm2006 <- gridmetmonthly[207:210,]
    climmeanlongterm2006 <- colMeans(longterm2006[,3:16])
    climmeanshortterm2006 <- colMeans(shortterm2006[,3:16])
    wheatdrought2006 <- subset(wheatdroughtclaim, year == 2006)
    wheatdroughtclaim_all2006 <- subset(wheatdroughtclaim_allall_final, year == 2006)
    wheatclaimlosssum2006 <- sum(wheatdrought2006$loss)
    wheatclaimacressum2006 <- sum(wheatdrought2006$acres)
    wheatclaimcounttotal2006 <- nrow(wheatdrought2006)/nrow(wheatdroughtclaim_all2006)
    wheatclaimcountsum2006 <- nrow(wheatdrought2006)
    
    longterm2007 <- gridmetmonthly[214:222,]
    shortterm2007 <- gridmetmonthly[219:222,]
    climmeanlongterm2007 <- colMeans(longterm2007[,3:16])
    climmeanshortterm2007 <- colMeans(shortterm2007[,3:16])
    wheatdrought2007 <- subset(wheatdroughtclaim, year == 2007)
    wheatdroughtclaim_all2007 <- subset(wheatdroughtclaim_allall_final, year == 2007)
    wheatclaimlosssum2007 <- sum(wheatdrought2007$loss)
    wheatclaimacressum2007 <- sum(wheatdrought2007$acres)
    wheatclaimcounttotal2007 <- nrow(wheatdrought2007)/nrow(wheatdroughtclaim_all2007)
    wheatclaimcountsum2007 <- nrow(wheatdrought2007)
    
    longterm2008 <- gridmetmonthly[226:234,]
    shortterm2008 <- gridmetmonthly[231:234,]
    climmeanlongterm2008 <- colMeans(longterm2008[,3:16])
    climmeanshortterm2008 <- colMeans(shortterm2008[,3:16])
    wheatdrought2008 <- subset(wheatdroughtclaim, year == 2008)
    wheatdroughtclaim_all2008 <- subset(wheatdroughtclaim_allall_final, year == 2008)
    wheatclaimlosssum2008 <- sum(wheatdrought2008$loss)
    wheatclaimacressum2008 <- sum(wheatdrought2008$acres)
    wheatclaimcounttotal2008 <- nrow(wheatdrought2008)/nrow(wheatdroughtclaim_all2008)
    wheatclaimcountsum2008 <- nrow(wheatdrought2008)
    
    longterm2009 <- gridmetmonthly[238:246,]
    shortterm2009 <- gridmetmonthly[243:246,]
    climmeanlongterm2009 <- colMeans(longterm2009[,3:16])
    climmeanshortterm2009 <- colMeans(shortterm2009[,3:16])
    wheatdrought2009 <- subset(wheatdroughtclaim, year == 2009)
    wheatdroughtclaim_all2009 <- subset(wheatdroughtclaim_allall_final, year == 2009)
    wheatclaimlosssum2009 <- sum(wheatdrought2009$loss)
    wheatclaimacressum2009 <- sum(wheatdrought2009$acres)
    wheatclaimcounttotal2009 <- nrow(wheatdrought2009)/nrow(wheatdroughtclaim_all2009)
    wheatclaimcountsum2009 <- nrow(wheatdrought2009)
    
    longterm2010 <- gridmetmonthly[250:258,]
    shortterm2010 <- gridmetmonthly[255:258,]
    climmeanlongterm2010 <- colMeans(longterm2010[,3:16])
    climmeanshortterm2010 <- colMeans(shortterm2010[,3:16])
    wheatdrought2010 <- subset(wheatdroughtclaim, year == 2010)
    wheatdroughtclaim_all2010 <- subset(wheatdroughtclaim_allall_final, year == 2010)
    wheatclaimlosssum2010 <- sum(wheatdrought2010$loss)
    wheatclaimacressum2010 <- sum(wheatdrought2010$acres)
    wheatclaimcounttotal2010 <- nrow(wheatdrought2010)/nrow(wheatdroughtclaim_all2010)
    wheatclaimcountsum2010 <- nrow(wheatdrought2010)
    
    longterm2011 <- gridmetmonthly[262:270,]
    shortterm2011 <- gridmetmonthly[267:270,]
    climmeanlongterm2011 <- colMeans(longterm2011[,3:16])
    climmeanshortterm2011 <- colMeans(shortterm2011[,3:16])
    wheatdrought2011 <- subset(wheatdroughtclaim, year == 2011)
    wheatdroughtclaim_all2011 <- subset(wheatdroughtclaim_allall_final, year == 2011)
    wheatclaimlosssum2011 <- sum(wheatdrought2011$loss)
    wheatclaimacressum2011 <- sum(wheatdrought2011$acres)
    wheatclaimcounttotal2011 <- nrow(wheatdrought2011)/nrow(wheatdroughtclaim_all2011)
    wheatclaimcountsum2011 <- nrow(wheatdrought2011)
    
    
    longterm2012 <- gridmetmonthly[274:282,]
    shortterm2012 <- gridmetmonthly[279:282,]
    climmeanlongterm2012 <- colMeans(longterm2012[,3:16])
    climmeanshortterm2012 <- colMeans(shortterm2012[,3:16])
    wheatdrought2012 <- subset(wheatdroughtclaim, year == 2012)
    wheatdroughtclaim_all2012 <- subset(wheatdroughtclaim_allall_final, year == 2012)
    wheatclaimlosssum2012 <- sum(wheatdrought2012$loss)
    wheatclaimacressum2012 <- sum(wheatdrought2012$acres)
    wheatclaimcounttotal2012 <- nrow(wheatdrought2012)/nrow(wheatdroughtclaim_all2012)
    wheatclaimcountsum2012 <- nrow(wheatdrought2012)
    
    longterm2013 <- gridmetmonthly[286:294,]
    shortterm2013 <- gridmetmonthly[291:294,]
    climmeanlongterm2013 <- colMeans(longterm2013[,3:16])
    climmeanshortterm2013 <- colMeans(shortterm2013[,3:16])
    wheatdrought2013 <- subset(wheatdroughtclaim, year == 2013)
    wheatdroughtclaim_all2013 <- subset(wheatdroughtclaim_allall_final, year == 2013)
    wheatclaimlosssum2013 <- sum(wheatdrought2013$loss)
    wheatclaimacressum2013 <- sum(wheatdrought2013$acres)
    wheatclaimcounttotal2013 <- nrow(wheatdrought2013)/nrow(wheatdroughtclaim_all2013)
    wheatclaimcountsum2013 <- nrow(wheatdrought2013)
    
    longterm2014 <- gridmetmonthly[298:306,]
    shortterm2014 <- gridmetmonthly[303:306,]
    climmeanlongterm2014 <- colMeans(longterm2014[,3:16])
    climmeanshortterm2014 <- colMeans(shortterm2014[,3:16])
    wheatdrought2014 <- subset(wheatdroughtclaim, year == 2014)
    wheatdroughtclaim_all2014 <- subset(wheatdroughtclaim_allall_final, year == 2014)
    wheatclaimlosssum2014 <- sum(wheatdrought2014$loss)
    wheatclaimacressum2014 <- sum(wheatdrought2014$acres)
    wheatclaimcounttotal2014 <- nrow(wheatdrought2014)/nrow(wheatdroughtclaim_all2014)
    wheatclaimcountsum2014 <- nrow(wheatdrought2014)
    
    longterm2015 <- gridmetmonthly[310:318,]
    shortterm2015 <- gridmetmonthly[315:318,]
    climmeanlongterm2015 <- colMeans(longterm2015[,3:16])
    climmeanshortterm2015 <- colMeans(shortterm2015[,3:16])
    wheatdrought2015 <- subset(wheatdroughtclaim, year == 2015)
    wheatdroughtclaim_all2015 <- subset(wheatdroughtclaim_allall_final, year == 2015)
    wheatclaimlosssum2015 <- sum(wheatdrought2015$loss)
    wheatclaimacressum2015 <- sum(wheatdrought2015$acres)
    wheatclaimcounttotal2015 <- nrow(wheatdrought2015)/nrow(wheatdroughtclaim_all2015)
    wheatclaimcountsum2015 <- nrow(wheatdrought2015)
    
    wls <- rbind(wheatclaimlosssum1989, wheatclaimlosssum1990, wheatclaimlosssum1991, wheatclaimlosssum1992, wheatclaimlosssum1993, wheatclaimlosssum1994, wheatclaimlosssum1995, wheatclaimlosssum1996, wheatclaimlosssum1997, wheatclaimlosssum1998, wheatclaimlosssum1999, wheatclaimlosssum2000, wheatclaimlosssum2001, wheatclaimlosssum2002, wheatclaimlosssum2003, wheatclaimlosssum2004, wheatclaimlosssum2005,wheatclaimlosssum2006, wheatclaimlosssum2007, wheatclaimlosssum2008, wheatclaimlosssum2009,wheatclaimlosssum2010,wheatclaimlosssum2011,wheatclaimlosssum2012,wheatclaimlosssum2013,wheatclaimlosssum2014,wheatclaimlosssum2015)
    wla <- rbind(wheatclaimacressum1989, wheatclaimacressum1990, wheatclaimacressum1991, wheatclaimacressum1992, wheatclaimacressum1993, wheatclaimacressum1994, wheatclaimacressum1995, wheatclaimacressum1996, wheatclaimacressum1997, wheatclaimacressum1998, wheatclaimacressum1999, wheatclaimacressum2000, wheatclaimacressum2001, wheatclaimacressum2002,wheatclaimacressum2003,wheatclaimacressum2004, wheatclaimacressum2005, wheatclaimacressum2006, wheatclaimacressum2007, wheatclaimacressum2008,wheatclaimacressum2009,wheatclaimacressum2010,wheatclaimacressum2011,wheatclaimacressum2012,wheatclaimacressum2013,wheatclaimacressum2014,wheatclaimacressum2015)
    wlc <- rbind(wheatclaimcountsum1989, wheatclaimcountsum1990, wheatclaimcountsum1991, wheatclaimcountsum1992, wheatclaimcountsum1993, wheatclaimcountsum1994, wheatclaimcountsum1995, wheatclaimcountsum1996, wheatclaimcountsum1997, wheatclaimcountsum1998, wheatclaimcountsum1999, wheatclaimcountsum2000, wheatclaimcountsum2001, wheatclaimcountsum2002, wheatclaimcountsum2003, wheatclaimcountsum2004, wheatclaimcountsum2005, wheatclaimcountsum2006, wheatclaimcountsum2007, wheatclaimcountsum2008, wheatclaimcountsum2009, wheatclaimcountsum2010, wheatclaimcountsum2011, wheatclaimcountsum2012, wheatclaimcountsum2013, wheatclaimcountsum2014, wheatclaimcountsum2015)
    climmeanlongterm1 <- rbind(climmeanlongterm1989, climmeanlongterm1990, climmeanlongterm1991, climmeanlongterm1992, climmeanlongterm1993, climmeanlongterm1994, climmeanlongterm1995, climmeanlongterm1996, climmeanlongterm1997, climmeanlongterm1998, climmeanlongterm1999, climmeanlongterm2000, climmeanlongterm2001, climmeanlongterm2002, climmeanlongterm2003, climmeanlongterm2004, climmeanlongterm2005, climmeanlongterm2006, climmeanlongterm2007,climmeanlongterm2008,climmeanlongterm2009,climmeanlongterm2010,climmeanlongterm2011,climmeanlongterm2012,climmeanlongterm2013,climmeanlongterm2014,climmeanlongterm2015)
    climmeanshortterm1 <- rbind(climmeanshortterm1989, climmeanshortterm1990, climmeanshortterm1991, climmeanshortterm1992, climmeanshortterm1993, climmeanshortterm1994, climmeanshortterm1995, climmeanshortterm1996, climmeanshortterm1997, climmeanshortterm1998, climmeanshortterm1999, climmeanshortterm2000, climmeanshortterm2001, climmeanshortterm2002, climmeanshortterm2003, climmeanshortterm2004, climmeanshortterm2005, climmeanshortterm2006, climmeanshortterm2007,climmeanshortterm2008,climmeanshortterm2009,climmeanshortterm2010,climmeanshortterm2011,climmeanshortterm2012,climmeanshortterm2013,climmeanshortterm2014,climmeanshortterm2015)
    wheatclaimcounttotal <- rbind(wheatclaimcounttotal1989, wheatclaimcounttotal1990, wheatclaimcounttotal1991, wheatclaimcounttotal1992, wheatclaimcounttotal1993, wheatclaimcounttotal1994, wheatclaimcounttotal1995, wheatclaimcounttotal1996, wheatclaimcounttotal1997, wheatclaimcounttotal1998, wheatclaimcounttotal1999, wheatclaimcounttotal2000, wheatclaimcounttotal2001, wheatclaimcounttotal2002, wheatclaimcounttotal2003, wheatclaimcounttotal2004, wheatclaimcounttotal2005,  wheatclaimcounttotal2006, wheatclaimcounttotal2007, wheatclaimcounttotal2008,wheatclaimcounttotal2009, wheatclaimcounttotal2010, wheatclaimcounttotal2011, wheatclaimcounttotal2012, wheatclaimcounttotal2013, wheatclaimcounttotal2014, wheatclaimcounttotal2015)
    wheatclaimcountsumtotal <- rbind(wheatclaimcountsum1989, wheatclaimcountsum1990, wheatclaimcountsum1991, wheatclaimcountsum1992, wheatclaimcountsum1993, wheatclaimcountsum1994, wheatclaimcountsum1995, wheatclaimcountsum1996, wheatclaimcountsum1997, wheatclaimcountsum1998, wheatclaimcountsum1999, wheatclaimcountsum2000, wheatclaimcountsum2001, wheatclaimcountsum2002, wheatclaimcountsum2003, wheatclaimcountsum2004, wheatclaimcountsum2005, wheatclaimcountsum2006, wheatclaimcountsum2007, wheatclaimcountsum2008, wheatclaimcountsum2009, wheatclaimcountsum2010, wheatclaimcountsum2011, wheatclaimcountsum2012, wheatclaimcountsum2013, wheatclaimcountsum2014, wheatclaimcountsum2015)
    
    finalz <- cbind(climmeanlongterm1,wls, wla, wlc, wheatclaimcounttotal, climmeanshortterm1)
    finalz <- data.frame(finalz)
    names(finalz)[16] <- c("loss")
    names(finalz)[17] <- c("acres")
    names(finalz)[18] <- c("count")
    names(finalz)[19] <- c("countratio")
    rownames(finalz) <- c(1989:2015)
    finalz[33]<- c(1989:2015)
    finalz[34] <- c(pu)
    names(finalz)[33] <- c("year")
    names(finalz)[34] <- c("county")
    finalz[35] <- q
    finalz[36] <- m
    names(finalz)[36] <- c("damagecause")
    finalz[37] <- statez
    
    colnames(finalz)[15] <- c("loss")
    colnames(finalz)[16] <- c("acres")
    colnames(finalz)[17] <- c("count")
    colnames(finalz)[18] <- c("countratio")
    colnames(finalz)[19] <- c("pr")
    
    names(finalz)[37] <- c(statez)
    
    
    names(finalz)[19:32] <- c("pr_short", "th_short", "pdsi_short", "pet_short", "erc_short", "rmin_short", "rmax_short", "tmmn_short", "tmmz_short", "srad_short", "sph_short", "vs_short", "fm_1000_short", "fm_100_short")
    
    finalzz <- merge(finalz, finalz_soil, by = "year")
     
    #names(finalz)[20:34] <- c("bi_short", "pr_short", "th_short", "pdsi_short", "pet_short", "erc_short", "rmin_short", "rmax_short", "tmmn_short", "tmmz_short", "srad_short", "sph_short", "vs_short", "fm_1000_short", "fm_100_short")
    
    #finalz <- cbind(finalz, wheatclaimcounttotal)
    #names(finalz)[37] <- c("countratio")
    
    #usda$statecode <- str_pad(usda$statecode, 2, pad = "0") #--pad state with zeros in front so we can combine into one nationwide fips number
    #usda$countycode <- str_pad(usda$countycode, 3, pad = "0") #--pad county with zeros in front so we can combine into one nationwide fips number
    #usda["countyfips"] <- NA  #--creates a new countyfips column to hold the merged columns
    #usda$countyfips <- paste(usda$statecode, usda$countycode, sep="") #--merges the two columns in to one
    #gridmetmonthly$month <- sapply(gridmetmonthly$month, toupper)
    
    #df3 = merge(gridmetmonthly, usda, by.x=c("year", "month", "countyfips"), by.y=c("year", "month", "countyfips"))

    setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries-soil-moisture/")
    
    m <- gsub("/", "-", m)
    m <- gsub(" ", "-", m)
    name = paste("Annual_climate_crop_", p, "_", q, "_", m, sep="")
    write.csv(finalzz, file=name)   
    
    #--merge county shapefile with USDA data for mapping purposes
    #m <- merge(counties, usda, by.x="FIPS", by.y="countyfips")
    #mergename = paste(dirname, "annual_usda_croploss_geofile_WA", sep="")
    #shapefile(m, paste(dirname, "annual_usda_croploss_geofile_WA", sep=""))
    #write.matrix(m, file=mergename, sep=",")
     }
   }
}

dirname4 <- "/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries-soil-moisture/"
files <- list.files(dirname4)

for (ll in files) {
setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries-soil-moisture/")
fileopen <- read.csv(ll, header=TRUE)
#colnames(fileopen) <- vectorr
setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
write.table(fileopen, file="1989-2015_combined.csv",
            append=TRUE,
            col.names = FALSE,
            sep = ',')
}

vectorr <- c("X", "ID", "year", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax",  "tmmn",  "tmmx",  "srad",  "sph", "vs", "fm1000",  "fm100",  "loss",  "acres",               
             "count", "countratio", "pr_short", "th_short", "pdsi_short", "pet_short",             
             "erc_short", "rmin_short", "rmax_short", "tmmn_short", "tmmz_short", "srad_short",            
             "sph_short", "vs_short", "fm_1000_short", "fm_100_short",  "county", "commodity",                   
             "damagecause", "state",  "soil_moisture_longterm", "loss.y", "acres.y", "count.y",               
             "countratio.y",  "soil_moisture_shorterm", "county.y",  "commodity.y",  "damagecause.y")


setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
combined_allall <- read.csv("1989-2015_combined.csv", header = FALSE)
colnames(combined_allall) <- vectorr
#combined2_allall <- combined_allall[2:58016,]

#combined_allall$countratio.x[combined_allall$countratio.x== "<NA>"] <- 0

combined_allall$countratio[is.na(combined_allall$countratio)] <- 0

combined2_allall <- combined_allall[3:16]
combined2a_allall <- combined_allall[22:35]

combined2c_allall <- combined_allall[40]
combined2d_allall <- combined_allall[45]
combined2e_allall <- combined_allall[18:21]
combined2f_allall <- combined_allall[36:39]

combined4_allall <- cbind(combined2_allall, combined2a_allall, combined2c_allall, combined2d_allall, combined2e_allall, combined2f_allall)

combined5_allall <- subset(combined4_allall, loss != 0)

colnames(combined4_allall)[37] <- "commodity"
colnames(combined4_allall)[38] <- "damagecause"
colnames(combined4_allall)[36] <- "county"
colnames(combined4_allall)[21] <- "countratio"
colnames(combined4_allall)[20] <- "count"
colnames(combined4_allall)[19] <- "acres"
colnames(combined4_allall)[18] <- "loss"

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
write.table(combined5_allall, file="1989-2015_combined_revised.csv",
            col.names = TRUE,
            sep = ',')

  #setwd(dirname)
  #files <- list.files(dirname, pattern = 'monthly_usda_gridmet_post2001')
  #tables <- lapply(files, read.csv, header=TRUE)
  #combined.df <- do.call(rbind, tables)
  #name2 = paste(scen1, "_", scen2, "_", "usda_gridmet_", scen_state, sep="")
  #write.matrix(combined.df, file=name2, sep=",")
  
  #} 
    
  #}


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

