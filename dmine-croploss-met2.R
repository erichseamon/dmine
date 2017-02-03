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

dirname <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, sep = "")
dirname2 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/summaries2", sep="")
dirname3 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/cdl", sep="")
setwd(dirname)
varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
yearspan = c(2012:2012)


for (i in yearspan) { 
  cdl <- raster(paste("/dmine/data/CDL/", "CDL_", i, ".grd", sep=""))
  #sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #cdl <- projectRaster(cdl, crs = sr)
  wintercdl <- cdl == 24 #spring wheat
  wintercdl <- crop(wintercdl, extent(counties))
  wintercdl[wintercdl==0] <- NA
  #writeRaster(wintercdl, paste(dirname3, "/", "CDL_", kk, "-", i, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
  #print(paste("cdl yearly winter wheat construction for:", kk, "-", i, "-", "-", name_county, "-", j,  sep=""))
  #--new matrix to contain variable, month, year, and county
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=18)
  colnames(newmatrix) <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100", "countyfips", "month", "year")
  varspannumber = 0
  for (j in varspan2) { 
    varspannumber = varspannumber + 1
    jj=0
    for (k in monthspan) {
      ncfile <- paste(dirname, "/netcdf/", j, "_", k, "_", i, ".nc", sep="")
      rasterout <- brick(ncfile) #create a brick
      rasterout <- mean(rasterout) #get the mean of all 30 days for the month
      rasterout <- mask(rasterout, counties) #- mask just the raster for the state in question
      rasterout3 <- crop(rasterout, extent(counties)) #now crop it for the state
      r.new = resample(rasterout3, wintercdl, "bilinear")
      rasterout4 <- mask(r.new, wintercdl)
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
        
        
        ee <- mask(e, r.new2)
        sp <- SpatialPoints(ee)
        eee <- extract(ee, sp, method='bilinear')
        newmatrix[jj,varspannumber] <- mean(eee, na.rm=TRUE)
        newmatrix[jj,16] <- l
        newmatrix[jj,17] <- k #--month
        newmatrix[jj,18] <- i #--yeari
        writeRaster(ee, paste(dirname3, "/", "CDL_", kk, "-", i, "-", k, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
        print(paste("writing raster, creating matrix of climate variables for:", kk, "-", i, "-", k, "-", name_county, "-", j,  sep=""))
      }  
    } 
  }
  setwd(dirname2)
  name <- paste(kk, "_", i, "_palouse_summary", sep="") #--used for individual states
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

dirname <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, sep = "")
dirname2 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/summaries2", sep="")
dirname3 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/cdl", sep="")
dirname4 <- paste("/nethome/erichs/dmine-temp/", kk, "/cdl", sep="")
dirname5 <- paste("/nethome/erichs/dmine-temp/", kk, "/summaries2", sep="")

setwd(dirname)
varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
varspan2 = c("vs", "fm1000", "fm100") 


yearspan = c(2014:2015)


for (i in yearspan) { 
  cdl <- raster(paste("/dmine/data/CDL/", "CDL_", i, ".grd", sep=""))
  #sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #cdl <- projectRaster(cdl, crs = sr)
  wintercdl <- cdl == 24 #winter wheat
  wintercdl <- crop(wintercdl, extent(counties))
  wintercdl[wintercdl==0] <- NA
  #--new matrix to contain variable, month, year, and county
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=18)
  colnames(newmatrix) <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100", "countyfips", "month", "year")
  varspannumber = 0
  for (j in varspan) { 
    varspannumber = varspannumber + 1
    jj=0
    for (k in monthspan) {
      ncfile <- paste(dirname, "/netcdf/", j, "_", k, "_", i, ".nc", sep="")
      rasterout <- brick(ncfile) #create a brick
      rasterout <- mean(rasterout) #get the mean of all 30 days for the month
      rasterout <- mask(rasterout, counties) #- mask just the raster for the state in question
      rasterout3 <- crop(rasterout, extent(counties)) #now crop it for the state
      r.new = resample(rasterout3, wintercdl, "bilinear")
      rasterout4 <- mask(r.new, wintercdl)
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
        
        
        ee <- mask(e, r.new2)
        sp <- SpatialPoints(ee)
        eee <- extract(ee, sp, method='bilinear')
        newmatrix[jj,varspannumber] <- mean(eee, na.rm=TRUE)
        newmatrix[jj,16] <- l
        newmatrix[jj,17] <- k #--month
        newmatrix[jj,18] <- i #--yeari
        writeRaster(ee, paste(dirname3, "/", "CDL_", kk, "-", i, "-", k, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
        print(paste("writing raster, creating matrix of climate variables for:", kk, "-", i, "-", k, "-", name_county, "-", j,  sep=""))
        #print(paste("county climate construction for:", kk, "-", i, "-", k, "-", name_county, "-", j,  sep=""))
      }  
    } 
  }
  setwd(dirname2)
  name <- paste(kk, "_", i, "_palouse_summary", sep="") #--used for individual states
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


dirname <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, sep = "")
dirname2 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/summaries2", sep="")
dirname3 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/cdl", sep="")
dirname4 <- paste("/nethome/erichs/dmine-temp/", kk, "/cdl", sep="")
dirname5 <- paste("/nethome/erichs/dmine-temp/", kk, "/summaries2", sep="")

setwd(dirname)
varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
yearspan = c(2010:2010)


for (i in yearspan) { 
  cdl <- raster(paste("/dmine/data/CDL/", "CDL_", i, ".grd", sep=""))
  #sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #cdl <- projectRaster(cdl, crs = sr)
  cl <- makeCluster(3)
  registerDoParallel(cl)
  wintercdl <- cdl == 24 #winter wheat
  wintercdl <- crop(wintercdl, extent(counties))
  wintercdl[wintercdl==0] <- NA
  stopCluster(cl)
  #--new matrix to contain variable, month, year, and county
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=18)
  colnames(newmatrix) <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100", "countyfips", "month", "year")
  varspannumber = 0
  print(paste("annual winter cdl for:", kk, "-", i,  sep=""))
  for (j in varspan) { 
    varspannumber = varspannumber + 1
    jj=0
    for (k in monthspan) {
      ncfile <- paste(dirname, "/netcdf/", j, "_", k, "_", i, ".nc", sep="")
      rasterout <- brick(ncfile) #create a brick
      rasterout <- mean(rasterout) #get the mean of all 30 days for the month
      rasterout <- mask(rasterout, counties) #- mask just the raster for the state in question
      rasterout3 <- crop(rasterout, extent(counties)) #now crop it for the state
      r.new = resample(rasterout3, wintercdl, "bilinear")
      rasterout4 <- mask(r.new, wintercdl)
      #png(paste(dirname, "/", j, "_", k, "_", i, ".png", sep=""))
      #plot(rasterout, main = paste0("Monthly Plot for: ", j, ", ", k, ", ", i, sep=""))
      #plot(counties, add=TRUE)
      #dev.off() 
      #rasterout <- t(rasterout)
      #proj4string(rasterout) <- projection 
      print(paste("monthly raster and mask for:", kk, "-", i, "-", k, "-",  j,  sep=""))
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
        
        
        ee <- mask(e, r.new2)
        sp <- SpatialPoints(ee)
        eee <- extract(ee, sp, method='bilinear')
        newmatrix[jj,varspannumber] <- mean(eee, na.rm=TRUE)
        newmatrix[jj,16] <- l
        newmatrix[jj,17] <- k #--month
        newmatrix[jj,18] <- i #--yeari
        writeRaster(ee, paste(dirname3, "/", "CDL_", kk, "-", i, "-", k, "-", name_county, "-", j, ".grd", sep=""), overwrite=TRUE)
        print(paste("county loop for:", kk, "-", i, "-", k, "-", name_county, "-", j,  sep=""))
        #print(paste("county climate construction for:", kk, "-", i, "-", k, "-", name_county, "-", j,  sep=""))
      }  
    } 
  }
  setwd(dirname2)
  name <- paste(kk, "_", i, "_palouse_summary", sep="") #--used for individual states
  #name <- paste(dirname, "/", variable, "_", month, "_", year, "_summary", sep="")
  write.matrix(newmatrix, file=name, sep=",")
}

#end cluster
stopCluster(cl)


palouse_counties <- rbind(palouse_Idaho_counties, palouse_Washington_counties, palouse_Oregon_counties)


#alllist <- c(palouse_Idaho_counties, palouse_Washington_counties, palouse_Oregon_counties)


#-------finished states, which creates summary files for each year, per state. 


#----Now we need to merge the state files into one large matrix, and assign a 
#-----continuous value column to allow us to sequentially select values across many years

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries")
files  <- list.files(pattern = '\\_summary')
tables <- lapply(files, read.csv, header = TRUE)
combined.df <- do.call(rbind , tables)
#sums <- read.csv(paste(kk, "_", i, "_palouse_summary", sep=""))


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

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

for (n in lister) {
  newcounty <- subset(combined1.df, county == n)
  newcounty$month <- capFirst(newcounty$month)
  newcounty$month <- trimws(newcounty$month)
  newcounty$ID<-seq.int(nrow(newcounty))
  newcounty <-newcounty[with(newcounty, order(year, match(newcounty$month, month.abb))), ]
  write.csv(newcounty, file = paste("2007_2015_palouse_", n, "_", newcounty$state[1], sep=""))
}


#---algorithm selection



combined2.df <- subset(combined.df, countyfips == 41065)
combined3.df <- subset(combined.df, countyfips == 41063)
points(combined2.df$pdsi, col = "red")
lines(combined2.df$pdsi, col = "red")
plot(combined3.df$pdsi, col = "blue")
lines(combined3.df$pdsi, col = "blue")

  
for (kk in alllist ) {
  
setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
paste("palouse_", kk, "_counties", collapse="")
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

dirname <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, sep = "")
dirname2 <- paste("/dmine/data/USDA/agmesh-scenarios/", kk, "/summaries2", sep="")
setwd(dirname)
varspan = c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100") 
monthspan = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
yearspan = c(N1:N2)


for (i in yearspan) { 
  cdl <- raster(paste("/dmine/data/CDL/", "CDL_", i, ".grd", sep=""))
  #sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #cdl <- projectRaster(cdl, crs = sr)
  wintercdl <- cdl == 24 #spring wheat
  wintercdl <- crop(wintercdl, extent(counties))
  wintercdl[wintercdl==0] <- NA
  #--new matrix to contain variable, month, year, and county
  newmatrix <- matrix(NA, nrow=countylistrows, ncol=18)
  colnames(newmatrix) <- c("bi", "pr", "th", "pdsi", "pet", "erc", "rmin", "rmax", "tmmn", "tmmx", "srad", "sph", "vs", "fm1000", "fm100", "countyfips", "month", "year")
  varspannumber = 0
  for (j in varspan) { 
    varspannumber = varspannumber + 1
    jj=0
    for (k in monthspan) {
      ncfile <- paste(dirname, "/netcdf/", j, "_", k, "_", i, ".nc", sep="")
      rasterout <- brick(ncfile) #create a brick
      rasterout <- mean(rasterout) #get the mean of all 30 days for the month
      rasterout <- mask(rasterout, counties) #- mask just the raster for the state in question
      rasterout3 <- crop(rasterout, extent(counties)) #now crop it for the state
      r.new = resample(rasterout3, wintercdl, "bilinear")
      rasterout4 <- mask(r.new, wintercdl)
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
        ee <- mask(e, subset_county)
        sp <- SpatialPoints(ee)
        eee <- extract(ee, sp, method='bilinear')
        newmatrix[jj,varspannumber] <- mean(eee, na.rm=TRUE)
        newmatrix[jj,16] <- l
        newmatrix[jj,17] <- k #--month
        newmatrix[jj,18] <- i #--yeari
        print(paste("county climate construction for:", kk, "-", i, "-", k, "-", name_county, "-", j,  sep=""))
      }  
    } 
  }
  setwd(dirname2)
  name <- paste(kk, "_", i, "_palouse_summary", sep="") #--used for individual states
  #name <- paste(dirname, "/", variable, "_", month, "_", year, "_summary", sep="")
  write.matrix(newmatrix, file=name, sep=",")
}
}

##-merge climate data into one file


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

