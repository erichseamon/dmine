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





#--third phase - load newly created file and perform EDA

setwd("/reacchspace/dmine/crop_indemnity_commodity/")
combined.df <- read.csv("Wheat.csv")
combined.df <- data.frame(combined.df)


#wheat <- subset(combined.df, combined.df$commoditycode == "11")


setwd("/reacchspace/dmine/agmesh-scenarios/scenario_52177/commodity_csv")

commodity <- read.table("wheat.csv")
commodity <- as.matrix(commodity)
commodity_by_damage <- ddply(commodity,~damagecause)
boxplot(commodity_by_damage$damagecausecode)

monthcode#--scaling

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

#--linear regression modeling


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
