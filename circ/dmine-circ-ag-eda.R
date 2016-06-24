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

setwd("/agmesh-scenarios/scenario_52177")
combined.df <- read.csv("2001_2015_usda_gridmet_WA")


lm(loss ~ pr + th + pdsi + pet + erc + rmin + rmax + tmmn + tmmx + srad + sph + vs + fm1000 + fm100 + bi, data=combined.df)
fit <- fit
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
