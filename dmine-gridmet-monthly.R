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

scen7 = unlist(strsplit(scen[7], split='=', fixed=TRUE))[2]
scen2 = unlist(strsplit(scen[2], split='=', fixed=TRUE))[2]
scen3 = unlist(strsplit(scen[3], split='=', fixed=TRUE))[2]

setwd(paste("/agmesh-scenarios/", scen7, sep="")) 
yearspan <- c(scen2:scen3)

scen <- scen7
dirname <- paste("/agmesh-scenarios/", scen, sep = "")

print("Generating raster arrays for analysis...")

setwd("/nethome/erichs/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
counties <- counties[grep("Washington", counties@data$STATE_NAME),]

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
yearspan <- c(2001:2015)
#--merge usda data with gridmet data

for (i in yearspan) {
  gridmetmonthly <- paste(dirname, "/", i, "_summary", sep="")
  usda <- paste("/home/git/data/USDA/zip/", i, ".txt", sep="")
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
  name = paste(i, "_monthly_usda_gridmet_WA", sep="")
  write.matrix(df3, file=name, sep=",")
  
  
  
  #--merge county shapefile with USDA data for mapping purposes
  #m <- merge(counties, usda, by.x="FIPS", by.y="countyfips")
  #mergename = paste(dirname, "annual_usda_croploss_geofile_WA", sep="")
  #shapefile(m, paste(dirname, "annual_usda_croploss_geofile_WA", sep=""))
  #write.matrix(m, file=mergename, sep=",")
}
setwd(dirname)
files <- list.files(dirname, pattern = '\\_WA')
tables <- lapply(files, read.csv, header=TRUE)
combined.df <- do.call(rbind, tables)
name2 = paste("2001_2015_usda_gridmet_WA", sep="")
write.matrix(combined.df, file=name2, sep=",")

#--third phase - load newly created file and perform EDA

#--box plot by commodity--

setwd("/reacchspace/dmine/agmesh-scenarios/scenario_52177/commodity_csv/")
commodity <- read.csv("wheat.csv")
library(lattice)
bwplot(damagecause ~ log(loss), data=commodity, groups=year, scales=list(x=list(rot=90)))

#--

setwd("/agmesh-scenarios/scenario_52177")
combined.df <- read.csv("2001_2015_usda_gridmet_WA")
combined.df <- data.frame(combined.df)

combined.df$bi <- scale(combined.df$bi)
combined.df$pr <- scale(combined.df$pr)
combined.df$th <- scale(combined.df$th)
combined.df$pdsi <- scale(combined.df$pdsi)
combined.df$pet <- scale(combined.df$pet)
combined.df$erc <- scale(combined.df$erc)
combined.df$rmin <- scale(combined.df$rmin)
combined.df$rmax <- scale(combined.df$rmax)
combined.df$tmmn <- scale(combined.df$tmmn)
combined.df$tmmx <- scale(combined.df$tmmx)
combined.df$srad <- scale(combined.df$srad)
combined.df$sph <- scale(combined.df$sph)
combined.df$vs <- scale(combined.df$vs)
combined.df$fm1000 <- scale(combined.df$fm1000)
combined.df$fm100 <- scale(combined.df$fm100)


lm(loss ~ pr + th + pdsi + pet + erc + rmin + rmax + tmmn + tmmx + srad + sph + vs + fm1000 + fm100 + bi, data=combined.df)
summary(fit)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # confidence intervals for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) #  regression diagnostics

# diagnostics plot of fitted model 
layout(matrix(c(1,2,3,4),2,2))
plot(fit)

#--assess outliers plots
outlierTest(fit) # Bonferonni p-value for most extreme obs
layout(matrix(c(1,1)))
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(combined.df)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


layout(matrix(c(1,1)))
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)


# Evaluate Collinearity
vif(fit) # Evaluate Collinearity - variance inflation factors 
sqrt(vif(fit)) > 2 # problem?


# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)

# Test for Autocorrelated Errors
durbinWatsonTest(fit)
