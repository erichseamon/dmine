# - 

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
library("rpart")
library("rgdal")
library("tiff")
#--third phase - load newly created file and perform EDA


setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


yearspanz = c(2007:2015)
statez = c("Idaho", "Washington", "Oregon")
Idaho_list1 <- paste("Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
Washington_list1 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", sep="|")
Oregon_list1 <- paste("Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", sep="|")


combinedlist2 <- paste("Okananogan", "Douglas", "Grant", "Benton", "Franklin", "Walla Walla", "Adams", "Lincoln", "Spokane", "Whitman", "Columbia", "Garfield", "Asotin", "Wasco", "Sherman", "Gilliam", "Morrow", "Umatilla", "Union", "Wallowa", "Idaho", "Lewis", "Nez Perce", "Clearwater", "Latah", "Benewah", "Kootenai", sep="|")
combinedlist <- c(Idaho_list1, Washington_list2, Oregon_list2)

or_counties <- counties[grep("Oregon", counties@data$STATE_NAME),]
palouse_or_counties <- or_counties[grep(Oregon_list1, or_counties@data$NAME),]

wa_counties <- counties[grep("Washington", counties@data$STATE_NAME),]
palouse_wa_counties <- wa_counties[grep(Washington_list1, wa_counties@data$NAME),]

id_counties <- counties[grep("Idaho", counties@data$STATE_NAME),]
palouse_id_counties <- id_counties[grep(Idaho_list1, id_counties@data$NAME),]

palouse_counties <- rbind(palouse_id_counties, palouse_wa_counties, palouse_or_counties)

#Idaho Counties
county_vect <- as.vector(palouse_id_counties$NAME)


for (i in yearspanz) {
    for (m in county_vect) {
    county_county <- palouse_id_counties[grep(m, palouse_id_counties@data$NAME),]
    cdl <- paste("/dmine/data/CDL/", "CDL_", i, ".grd", sep="")
    cdl <- raster(cdl)
    
    cdl <- crop(cdl, extent(county_county))
    cdl <- mask(cdl, county_county)
    
    setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/cdl/"))
    writeRaster(cdl, paste(i, "_", "Idaho_", m, sep=""))
    #wintercdl <- cdl == 24 #winter wheat
    #springcdl <- cdl == 23 #spring wheat
    #layout(matrix(c(1,2,3,4),1,2))
    #plot(wintercdl)
    #plot(springcdl)
    }
  }


palouse_wa_counties <- counties[grep(Washington_list1, counties@data$NAME),]

#Washington Counties

for (i in yearspanz) {
  for (m in palouse_wa_counties) {
    county_county <- palouse_wa_counties[grep(m, palouse_wa_counties@data$NAME),]
    cdl <- paste("/dmine/data/CDL/", "CDL_", i, "_005.tif", sep="")
    cdl <- raster(cdl)
    
    sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    cdl <- projectRaster(cdl, crs = sr, method = "bilinear")
    
    cdl <- crop(cdl, extent(county_county))
    cdl <- mask(cdl, county_county)
    
    setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/cdl/"))
    writeRaster(cdl, paste(i, "_", "Washington_", m, sep=""))
    #wintercdl <- cdl == 24 #winter wheat
    #springcdl <- cdl == 23 #spring wheat
    #layout(matrix(c(1,2,3,4),1,2))
    #plot(wintercdl)
    #plot(springcdl)
  }
}



palouse_or_counties <- counties[grep(Oregon_list1, counties@data$NAME),]

#Oregon Counties

for (i in yearspanz) {
  for (m in palouse_or_counties) {
    county_county <- palouse_or_counties[grep(m, palouse_or_counties@data$NAME),]
    cdl <- paste("/dmine/data/CDL/", "CDL_", i, "_005.tif", sep="")
    cdl <- raster(cdl)
    
    sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    cdl <- projectRaster(cdl, crs = sr, method = "bilinear")
    
    cdl <- crop(cdl, extent(county_county))
    cdl <- mask(cdl, county_county)
    
    setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/cdl/"))
    writeRaster(cdl, paste(i, "_", "Oregon_", m, sep=""))
    #wintercdl <- cdl == 24 #winter wheat
    #springcdl <- cdl == 23 #spring wheat
    #layout(matrix(c(1,2,3,4),1,2))
    #plot(wintercdl)
    #plot(springcdl)
  }
}










for (i in yearspanz) {
  for (j in fullcounty) {
    cdl <- paste("/dmine/data/CDL/", "CDL_", i, "_005.tif", sep="")
    cdl <- raster(cdl)
    sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    cdl <- projectRaster(cdl, crs = sr)
    wintercdl <- cdl == 24 #winter wheat
    springcdl <- cdl == 23 #spring wheat
    layout(matrix(c(1,2,3,4),1,2))
    plot(wintercdl)
    plot(springcdl)
    
  }
}



for (i in yearspanz) {
  for (j in fullcounty) {
  cdl <- paste("/dmine/data/CDL/", "CDL_", i, "_005.tif", sep="")
  cdl <- raster(cdl)
  sr = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  cdl <- projectRaster(cdl, crs = sr)
  wintercdl <- cdl == 24 #winter wheat
  springcdl <- cdl == 23 #spring wheat
  layout(matrix(c(1,2,3,4),1,2))
  plot(wintercdl)
  plot(springcdl)

  }
}


setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
combined.df <- read.csv("2001_2015_Annual_climate_crop_palouse_summary")
combined.df$loss <- scale(combined.df$loss, center = TRUE, scale = FALSE)

fit <- lm(countratio ~ pr + th + pdsi + pet + erc + rmin + rmax + tmmn + tmmx + srad + sph + vs + fm1000 + fm100 + bi, data=combined.df)
fit <- fit
summary(fit)


combined.df$loss <- scale(combined.df$loss, center = TRUE, scale = FALSE)
combined.df[1:15] <- scale(combined.df[1:15], center = TRUE, scale = TRUE)


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
leveragePlots(fit) # leverage plots - partial regression plot for each variable

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
