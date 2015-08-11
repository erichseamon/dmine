#------------------------------------------------------------------------#
# TITLE:        agmesh_plotting_ET.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         November 14, 2014
#
# STAGE:        Plotting - Stage 5
#
# COMMENTS:     This is the agmesh plotting script for ET.  Agmesh  
#               uses weather parameters for the inland pacific northwest   
#               to calculateclimate parameter related outputs. This 
#               agmesh plotting script uses the data generated in 
#               stages 1 and 2 to create plots.
#
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------#

library("MASS")

#------SECTION 3 - Plot Univariate and BiVariate Data ------#

#------Plotting - ET with climate variables ------#

#win.graph(15,15)

#dev.off()

#--spring plotting

for (i in yearspan) {
  
lyt = c(1, 2, 3, 4, 5, 6,
        7, 8, 9, 10, 11, 12)

lytmtx = matrix(lyt,nrow=2,ncol=6,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' function


  tmmxbrickspringmean <- get(paste("tmmxET", i, "brickspringmean", sep=""))
  tmmxbrickwintermean <- get(paste("tmmxET", i, "brickwintermean", sep=""))
  tmmnbrickspringmean <- get(paste("tmmnET", i, "brickspringmean", sep=""))
  tmmnbrickwintermean <- get(paste("tmmnET", i, "brickwintermean", sep=""))
  rmaxbrickspringmean <- get(paste("rmaxET", i, "brickspringmean", sep=""))
  rmaxbrickwintermean <- get(paste("rmaxET", i, "brickwintermean", sep=""))
  rminbrickspringmean <- get(paste("rminET", i, "brickspringmean", sep=""))
  rminbrickwintermean <- get(paste("rminET", i, "brickwintermean", sep=""))
  sradbrickspringmean <- get(paste("sradET", i, "brickspringmean", sep=""))
  sradbrickwintermean <- get(paste("sradET", i, "brickwintermean", sep=""))
  vsbrickspringmean <- get(paste("vsET", i, "brickspringmean", sep=""))
  vsbrickwintermean <- get(paste("vsET", i, "brickwintermean", sep=""))

  
#tmmxbrickmean2012 <- get(paste("tmmx", 2012, "brickmean", sep=""))
#tmmnbrickmean2012 <- get(paste("tmmn", 2012, "brickmean", sep=""))
#rminbrickmean2012 <- get(paste("rmin", 2012, "brickmean", sep=""))
#rmaxbrickmean2012 <- get(paste("rmax", 2012, "brickmean", sep=""))
#sradbrickmean2012 <- get(paste("srad", 2012, "brickmean", sep=""))
#vsbrickmean2012 <- get(paste("vs", 2012, "brickmean", sep=""))      

#tmmxbrickmean2013 <- get(paste("tmmx", 2013, "brickmean", sep=""))
#tmmnbrickmean2013 <- get(paste("tmmn", 2013, "brickmean", sep=""))
#rminbrickmean2013 <- get(paste("rmin", 2013, "brickmean", sep=""))
#rmaxbrickmean2013 <- get(paste("rmax", 2013, "brickmean", sep=""))
#sradbrickmean2013 <- get(paste("srad", 2013, "brickmean", sep=""))
#vsbrickmean2013 <- get(paste("vs", 2013, "brickmean", sep="")) 

jpeg(paste("/agmesh-scenarios/", scen, "/springplot", i, ".jpg", sep=""))

hist(tmmxbrickspringmean,   
     col = "gray",
     xlab = "Celsius",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Max Temp")

#-----Add summary statistics to plot
tmmxhistspringmean <- mean(as.vector(tmmxbrickspringmean))
tmmxhistmedianspring <- median(as.vector(tmmxbrickspringmean))
tmmxhistsdspring <- sd(as.vector(tmmxbrickspringmean))
tmmxhistminspring <- min(as.vector(tmmxbrickspringmean))
tmmxhistmaxspring <- max(as.vector(tmmxbrickspringmean))

abline(v=tmmxhistspringmean,lwd=2)
abline(v=tmmxhistmedianspring,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmx2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmxhistminspring,100,pch = "*")
points(tmmxhistmaxspring,100,pch = "*")

#----Add Legend
legend(-10,2000,
       #c("Mean","Median","St.Dev.","Min.","Max."),
       c("Mean","Median","Min.","Max."),
       #col = c("black","red","blue","black","black"),
       col = c("black","red","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.65)

hist(tmmnbrickspringmean,   
     col = "gray",
     xlab = "Celsius",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Min Air Temp")

#---Add summary statistics to plot

tmmnhistspringmean <- mean(as.vector(tmmnbrickspringmean))
tmmnhistmedianspring <- median(as.vector(tmmnbrickspringmean))
tmmnhistsdspring <- sd(as.vector(tmmnbrickspringmean))
tmmnhistminspring <- min(as.vector(tmmnbrickspringmean))
tmmnhistmaxspring <- max(as.vector(tmmnbrickspringmean))


abline(v=tmmnhistspringmean,lwd=2)
abline(v=tmmnhistmedianspring,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmn2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmnhistminspring,100,pch = "*")
points(tmmnhistmaxspring,100,pch = "*")

hist(rminbrickspringmean,   
     col = "gray",
     xlab = "%",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Min Rel Humidity")

#---Add summary statistics to plot
rminhistspringmean <- mean(as.vector(rminbrickspringmean))
rminhistmedianspring <- median(as.vector(rminbrickspringmean))
rminhistsdspring <- sd(as.matrix(rminbrickspringmean))
rminhistminspring <- min(as.vector(rminbrickspringmean))
rminhistmaxspring <- max(as.vector(rminbrickspringmean))


abline(v=rminhistspringmean,lwd=2)
abline(v=rminhistmedianspring,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmin2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rminhistminspring,100,pch = "*")
points(rminhistmaxspring,100,pch = "*")

hist(rmaxbrickspringmean,   
     col = "gray",
     xlab = "%",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Max Rel Humidity")

#---Add summary statistics to plot
rmaxhistspringmean <- mean(as.vector(rmaxbrickspringmean))
rmaxhistmedianspring <- median(as.vector(rmaxbrickspringmean))
rmaxhistsdspring <- sd(as.matrix(rmaxbrickspringmean))
rmaxhistminspring <- min(as.vector(rmaxbrickspringmean))
rmaxhistmaxspring <- max(as.vector(rmaxbrickspringmean))


abline(v=rmaxhistspringmean,lwd=2)
abline(v=rmaxhistmedianspring,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmax2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rmaxhistminspring,100,pch = "*")
points(rmaxhistmaxspring,100,pch = "*")

hist(sradbrickspringmean,   
     col = "gray",
     xlab = "w/m2",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Solar Radiation")

#---Add summary statistics to plot
sradhistspringmean <- mean(as.vector(sradbrickspringmean))
sradhistmedianspring <- median(as.vector(sradbrickspringmean))
sradhistsdspring <- sd(as.matrix(sradbrickspringmean))
sradhistminspring <- min(as.vector(sradbrickspringmean))
sradhistmaxspring <- max(as.vector(sradbrickspringmean))


abline(v=sradhistspringmean,lwd=2)
abline(v=sradhistmedianspring,col = "red",lwd = 2,lty = "dashed")
#abline(v= srad2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(sradhistminspring,100,pch = "*")
points(sradhistmaxspring,100,pch = "*")

hist(vsbrickspringmean,   
     col = "gray",
     xlab = "m-s-1",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Wind Speed")

#---Add summary statistics to plot
vshistspringmean <- mean(as.vector(vsbrickspringmean))
vshistmedianspring <- median(as.vector(vsbrickspringmean))
vshistsdspring <- sd(as.matrix(vsbrickspringmean))
vshistminspring <- min(as.vector(vsbrickspringmean))
vshistmaxspring <- max(as.vector(vsbrickspringmean))


abline(v=vshistspringmean,lwd=2)
abline(v=vshistmedianspring,col = "red",lwd = 2,lty = "dashed")
abline(v= vshistsdspring,col = "blue", lwd = 2,lty = "dashed")
points(vshistminspring,100,pch = "*")
points(vshistmaxspring,100,pch = "*")

plot(subset(tmmxbrickspringmean,   
            col = "gray",
            xlab = "Celsius",
            ylab = "Count",
            #ylim = c(0,240),
            #xlim = c(-0.5,6),
            main = "Max Air Temp"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(tmmnbrickspringmean,  
            col = "gray",
            xlab = "Celsius",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "Min Air Temp"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(rmaxbrickspringmean,  
            col = "gray",
            xlab = "%",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "Max Rel Hum"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(rminbrickspringmean,  
            col = "gray",
            xlab = "%",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "Min Rel Hum"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(sradbrickspringmean,  
            col = "gray",
            xlab = "w/m2",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "Solar Radiation"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(vsbrickspringmean,  
            col = "gray",
            xlab = "m-s-1",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "Wind Speed"))

plot(tristate, bg="transparent", add=TRUE)
#dev.off()

}

#--winter plotting

for (i in yearspan) {
  
  lyt = c(1, 2, 3, 4, 5, 6,
          7, 8, 9, 10, 11, 12)
  
  lytmtx = matrix(lyt,nrow=2,ncol=6,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  jpeg(paste("/agmesh-scenarios/", scen, "/winterplot", i, ".jpg", sep=""))
  

hist(tmmxbrickwintermean,   
     col = "gray",
     xlab = "Celsius",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Max Temp")

#-----Add summary statistics to plot
tmmxhistwintermean <- mean(as.vector(tmmxbrickwintermean))
tmmxhistmedianwinter <- median(as.vector(tmmxbrickwintermean))
tmmxhistsdwinter <- sd(as.vector(tmmxbrickwintermean))
tmmxhistminwinter <- min(as.vector(tmmxbrickwintermean))
tmmxhistmaxwinter <- max(as.vector(tmmxbrickwintermean))

abline(v=tmmxhistwintermean,lwd=2)
abline(v=tmmxhistmedianwinter,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmx2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmxhistminwinter,100,pch = "*")
points(tmmxhistmaxwinter,100,pch = "*")

#----Add Legend
legend(-10,2000,
       #c("Mean","Median","St.Dev.","Min.","Max."),
       c("Mean","Median","Min.","Max."),
       #col = c("black","red","blue","black","black"),
       col = c("black","red","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.65)

hist(tmmnbrickwintermean,   
     col = "gray",
     xlab = "Celsius",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Min Air Temp")

#---Add summary statistics to plot

tmmnhistwintermean <- mean(as.vector(tmmnbrickwintermean))
tmmnhistmedianwinter <- median(as.vector(tmmnbrickwintermean))
tmmnhistsdwinter <- sd(as.vector(tmmnbrickwintermean))
tmmnhistminwinter <- min(as.vector(tmmnbrickwintermean))
tmmnhistmaxwinter <- max(as.vector(tmmnbrickwintermean))


abline(v=tmmnhistwintermean,lwd=2)
abline(v=tmmnhistmedianwinter,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmn2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmnhistminwinter,100,pch = "*")
points(tmmnhistmaxwinter,100,pch = "*")

hist(rminbrickwintermean,   
     col = "gray",
     xlab = "%",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Min Rel Hum")

#---Add summary statistics to plot
rminhistwintermean <- mean(as.vector(rminbrickwintermean))
rminhistmedianwinter <- median(as.vector(rminbrickwintermean))
rminhistsdwinter <- sd(as.matrix(rminbrickwintermean))
rminhistminwinter <- min(as.vector(rminbrickwintermean))
rminhistmaxwinter <- max(as.vector(rminbrickwintermean))


abline(v=rminhistwintermean,lwd=2)
abline(v=rminhistmedianwinter,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmin2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rminhistminwinter,100,pch = "*")
points(rminhistmaxwinter,100,pch = "*")

hist(rmaxbrickwintermean,   
     col = "gray",
     xlab = "%",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Max Rel Hum")

#---Add summary statistics to plot
rmaxhistwintermean <- mean(as.vector(rmaxbrickwintermean))
rmaxhistmedianwinter <- median(as.vector(rmaxbrickwintermean))
rmaxhistsdwinter <- sd(as.matrix(rmaxbrickwintermean))
rmaxhistminwinter <- min(as.vector(rmaxbrickwintermean))
rmaxhistmaxwinter <- max(as.vector(rmaxbrickwintermean))


abline(v=rmaxhistwintermean,lwd=2)
abline(v=rmaxhistmedianwinter,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmax2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rmaxhistminwinter,100,pch = "*")
points(rmaxhistmaxwinter,100,pch = "*")

hist(sradbrickwintermean,   
     col = "gray",
     xlab = "w/m2",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Solar Radiation")

#---Add summary statistics to plot
sradhistwintermean <- mean(as.vector(sradbrickwintermean))
sradhistmedianwinter <- median(as.vector(sradbrickwintermean))
sradhistsdwinter <- sd(as.matrix(sradbrickwintermean))
sradhistminwinter <- min(as.vector(sradbrickwintermean))
sradhistmaxwinter <- max(as.vector(sradbrickwintermean))


abline(v=sradhistwintermean,lwd=2)
abline(v=sradhistmedianwinter,col = "red",lwd = 2,lty = "dashed")
#abline(v= srad2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(sradhistminwinter,100,pch = "*")
points(sradhistmaxwinter,100,pch = "*")

hist(vsbrickwintermean,   
     col = "gray",
     xlab = "m-s-1",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Wind Speed")

#---Add summary statistics to plot
vshistwintermean <- mean(as.vector(vsbrickwintermean))
vshistmedianwinter <- median(as.vector(vsbrickwintermean))
vshistsdwinter <- sd(as.matrix(vsbrickwintermean))
vshistminwinter <- min(as.vector(vsbrickwintermean))
vshistmaxwinter <- max(as.vector(vsbrickwintermean))


abline(v=vshistwintermean,lwd=2)
abline(v=vshistmedianwinter,col = "red",lwd = 2,lty = "dashed")
abline(v= vshistsdwinter,col = "blue", lwd = 2,lty = "dashed")
points(vshistminwinter,100,pch = "*")
points(vshistmaxwinter,100,pch = "*")

plot(subset(tmmxbrickwintermean,   
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,240),
            #xlim = c(-0.5,6),
            main = "ET - April 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(tmmnbrickwintermean,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - August 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(rmaxbrickwintermean,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - Dec 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(rminbrickwintermean,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - Dec 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(sradbrickwintermean,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - Dec 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(vsbrickwintermean,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - Dec 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)

dev.off()

}



#--histograms of mean et for 2012 and 2013---#

#----ET plotting---#

erichet2012brickmean2 <- as.matrix(erichet2012brickmean)
erichet2012brickmean2 <- na.omit(erichet2012brickmean2)

mu = mean(erichet2012brickmean2, na.rm=TRUE)
sigma = sd(erichet2012brickmean2, na.rm=TRUE)

fit = fitdistr(erichet2012brickmean2, "normal", nan.rm=TRUE) # Fit normal distribution to data.
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(erichet2012brickmean2),muHat,sigmaHat)


hist(erichet2012brickmean2, freq=FALSE, 
     ylab = "Frequency of Evapotranspiration (mm/d-1)",
     ylim = c(0,.25),
     xlim = c(10,26),
     xlab = "Evapotranspiration (mm/d-1)",
     notch = FALSE,
     main = "2012 Mean Daily Distributions of ET")

#lines(density(erichet2013brickmean, col="blue", lwd=2))
lines(sort(erichet2012brickmean2),nHat,lwd=2)
text(min(erichet2012brickmean2),max(nHat), pos = 1,
     paste("Fit: mu = ",round(muHat*100)/100))
text(min(erichet2012brickmean2),0.9*max(nHat), pos = 1,
     paste("sigma = ", round(sigmaHat*100)/100))

cat ("Press [enter] to see April 1st, Aug 1st and Dec 1st compared in 2012")
line <- readline()

cat ("Press [enter] to see zscore plots of ET vs crop yield for 2012 and 2013")
line <- readline()


hist(tmmxbrickmean2013,   
     col = "gray",
     xlab = "max air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Max Temp")

#-----Add summary statistics to plot
tmmxhistmean <- mean(as.vector(tmmxbrickmean2013))
tmmxhistmedian <- median(as.vector(tmmxbrickmean2013))
tmmxhistsd <- sd(as.vector(tmmxbrickmean2013))
tmmxhistmin <- min(as.vector(tmmxbrickmean2013))
tmmxhistmax <- max(as.vector(tmmxbrickmean2013))

abline(v=tmmxhistmean,lwd=2)
abline(v=tmmxhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmx2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmxhistmin,100,pch = "*")
points(tmmxhistmax,100,pch = "*")

#----Add Legend
legend(-10,2000,
       #c("Mean","Median","St.Dev.","Min.","Max."),
       c("Mean","Median","Min.","Max."),
       #col = c("black","red","blue","black","black"),
       col = c("black","red","black","black"),
       pch = c(NA,NA,NA,"*","*"),
       lwd = c(2,2,2,NA,NA),
       lty = c(1,2,2,NA,NA),
       bty = "n",
       cex = 0.65)

hist(tmmnbrickmean2013,   
     col = "gray",
     xlab = "min air temperature (Celsius)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Min Air Temp")

#---Add summary statistics to plot

tmmnhistmean <- mean(as.vector(tmmnbrickmean2013))
tmmnhistmedian <- median(as.vector(tmmnbrickmean2013))
tmmnhistsd <- sd(as.vector(tmmnbrickmean2013))
tmmnhistmin <- min(as.vector(tmmnbrickmean2013))
tmmnhistmax <- max(as.vector(tmmnbrickmean2013))


abline(v=tmmnhistmean,lwd=2)
abline(v=tmmnhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= tmmn2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(tmmnhistmin,100,pch = "*")
points(tmmnhistmax,100,pch = "*")

hist(rminbrickmean2013,   
     col = "gray",
     xlab = "min relative humidity (%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Min Rel Humidity")

#---Add summary statistics to plot
rminhistmean <- mean(as.vector(rminbrickmean2013))
rminhistmedian <- median(as.vector(rminbrickmean2013))
rminhistsd <- sd(as.matrix(rminbrickmean2013))
rminhistmin <- min(as.vector(rminbrickmean2013))
rminhistmax <- max(as.vector(rminbrickmean2013))


abline(v=rminhistmean,lwd=2)
abline(v=rminhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmin2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rminhistmin,100,pch = "*")
points(rminhistmax,100,pch = "*")

hist(rmaxbrickmean2013,   
     col = "gray",
     xlab = "max relative humidity(%)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Max Rel Humidity")

#---Add summary statistics to plot
rmaxhistmean <- mean(as.vector(rmaxbrickmean2013))
rmaxhistmedian <- median(as.vector(rmaxbrickmean2013))
rmaxhistsd <- sd(as.matrix(rmaxbrickmean2013))
rmaxhistmin <- min(as.vector(rmaxbrickmean2013))
rmaxhistmax <- max(as.vector(rmaxbrickmean2013))


abline(v=rmaxhistmean,lwd=2)
abline(v=rmaxhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= rmax2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(rmaxhistmin,100,pch = "*")
points(rmaxhistmax,100,pch = "*")

hist(sradbrickmean2013,   
     col = "gray",
     xlab = "solar radiation (w/m2)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Solar Radiation")

#---Add summary statistics to plot
sradhistmean <- mean(as.vector(sradbrickmean2013))
sradhistmedian <- median(as.vector(sradbrickmean2013))
sradhistsd <- sd(as.matrix(sradbrickmean2013))
sradhistmin <- min(as.vector(sradbrickmean2013))
sradhistmax <- max(as.vector(sradbrickmean2013))


abline(v=sradhistmean,lwd=2)
abline(v=sradhistmedian,col = "red",lwd = 2,lty = "dashed")
#abline(v= srad2007histsd,col = "blue", lwd = 2,lty = "dashed")
points(sradhistmin,100,pch = "*")
points(sradhistmax,100,pch = "*")

hist(vsbrickmean2013,   
     col = "gray",
     xlab = "Wind Speed Frequency (m-s-1)",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "2013 Wind Speed")

#---Add summary statistics to plot
vshistmean <- mean(as.vector(vsbrickmean2013))
vshistmedian <- median(as.vector(vsbrickmean2013))
vshistsd <- sd(as.matrix(vsbrickmean2013))
vshistmin <- min(as.vector(vsbrickmean2013))
vshistmax <- max(as.vector(vsbrickmean2013))


abline(v=vshistmean,lwd=2)
abline(v=vshistmedian,col = "red",lwd = 2,lty = "dashed")
abline(v= vshistsd,col = "blue", lwd = 2,lty = "dashed")
points(vshistmin,100,pch = "*")
points(vshistmax,100,pch = "*")



#----ET plotting---#

erichet2013brickmean2 <- as.matrix(erichet2013brickmean)

mu = mean(erichet2013brickmean2)
sigma = sd(erichet2013brickmean2)

fit = fitdistr(erichet2013brickmean2,"normal") # Fit normal distribution to data.
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(erichet2013brickmean2),muHat,sigmaHat)


hist(erichet2013brickmean, freq=FALSE, 
     ylab = "Frequency of Evapotranspiration (mm/d-1)",
     ylim = c(0,.25),
     xlim = c(10,26),
     xlab = "Evapotranspiration (mm/d-1)",
     notch = FALSE,
     main = "2013 Mean Daily Distributions of ET")

#lines(density(erichet2013brickmean, col="blue", lwd=2))
lines(sort(erichet2013brickmean2),nHat,lwd=2)
text(min(erichet2013brickmean2),max(nHat), pos = 4,
     paste("Fit: mu = ",round(muHat*100)/100))
text(min(erichet2013brickmean2),0.9*max(nHat), pos = 4,
     paste("sigma = ", round(sigmaHat*100)/100))
cat ("Press [enter] to see April 1st, Aug 1st and Dec 1st compared in 2012")
line <- readline()

#---comparing daily ET distributions for April 1st, Aug 1st, 
#---and Dec 1st, 2012

#---Creates the window size--------------------#
#win.graph(3.5,8) 
#---par function sets number or rows and columns for plotting---#
par(mfrow=c(3,1))  


plot(subset(erichet2012, 90,   
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,240),
            #xlim = c(-0.5,6),
            main = "ET - April 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(erichet2012, 210,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - August 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(erichet2012, 315,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - Dec 1st 2012"))

plot(tristate, bg="transparent", add=TRUE)


cat ("Press [enter] to see April 1st, Aug 1st and Dec 1st compared in 2013")
line <- readline()

#---comparing daily ET distributions for April 1st, Aug 1st, 
#---and Dec 1st, 2013

#---Creates the window size--------------------#
#win.graph(3.5,8) 
#---par function sets number or rows and columns for plotting---#
par(mfrow=c(3,1))  


plot(subset(erichet2013, 90,   
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,240),
            #xlim = c(-0.5,6),
            main = "ET - April 1st 2013"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(erichet2013, 210,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - August 1st 2013"))

plot(tristate, bg="transparent", add=TRUE)

plot(subset(erichet2013, 315,  
            col = "gray",
            xlab = "min air temperature (Celsius)",
            ylab = "Count",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = "ET - Dec 1st 2013"))

plot(tristate, bg="transparent", add=TRUE)


cat ("Press [enter] to see April 1st examined by boxplot, map, and histogram")
line <- readline()

#---------Comparing ET - April 1st for 2012 and 2013

#---Creates the window size---------------#
#win.graph(11,7) 
#---par function sets number or rows and columns for plotting--#
par(mfrow=c(1,3))  


boxplot(erichet2012$layer.90,
        ylim = c(2,25),
        col="aquamarine3",
        xlab="April1 ET 2007",
        ylab="ET Distribution (mm/d-1)")

plot(erichet2012$layer.90,
        xlab="April1 ET 2007",
        ylab="ET Distribution (mm/d-1)")

hist(erichet2012$layer.90,
        col="aquamarine3",
        xlab="April1 ET 2007",
        ylab="ET Distribution (mm/d-1)")


cat ("Press [enter] to see April 1st examined by boxplot, map, and histogram")
line <- readline()

par(mfrow=c(1,3))

boxplot(erichet2013$layer.90,
        ylim = c(15,22),
        col="aquamarine3",
        xlab="April1 ET 2008",
        ylab="ET Distribution (mm/d-1)")

plot(erichet2013$layer.90,
     xlab="April1 ET 2007",
     ylab="ET Distribution (mm/d-1)")

hist(erichet2013$layer.90,
     col="aquamarine3",
     xlab="April1 ET 2007",
     ylab="ET Distribution (mm/d-1)")




#boxplot(erichet2009$layer.90,
#        ylim = c(2,25),
#        col="aquamarine3",
#        xlab="April1 ET 2009",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2010$layer.90,
#        ylim = c(2,25),
#        col="aquamarine3",
#        xlab="April1 ET 2010",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2011$layer.90,
#        ylim = c(2,25),
#        col="aquamarine3",
#        xlab="April1 ET 2011",
#        ylab="ET Distribution (mm/d-1)")

cat ("Press [enter] to see August 1st compared for 2012 and 2013")
line <- readline()

#---------Comparing ET - August 1st for 2012 and 2013

#---Creates the window size-------------------#
#win.graph(11,7) 
#---par function sets number or rows and columns for plotting--#
par(mfrow=c(1,5))  


boxplot(erichet2012$layer.210,
        ylim = c(10,60),
        col="aquamarine3",
        xlab="Aug1 ET 2007",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2013$layer.210,
        ylim = c(10,60),
        col="aquamarine3",
        xlab="Aug1 ET 2008",
        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2009$layer.210,
#        ylim = c(10,60),
#        col="aquamarine3",
#        xlab="Aug1 ET 2009",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2010$layer.210,
#        ylim = c(10,60),
#        col="aquamarine3",
#        xlab="Aug1 ET 2010",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2011$layer.210,
#        ylim = c(10,60),
#        col="aquamarine3",
#        xlab="Aug1 ET 2011",
#        ylab="ET Distribution (mm/d-1)")

cat ("Press [enter] to see December 1st compared for 2012 and 2013")
line <- readline()

#---------Comparing ET - December 1st for 2012 and 2013

#---Creates the window size-----#
#win.graph(11,7) 
#---par function sets number or rows and columns for plotting----#
par(mfrow=c(1,5))  

boxplot(erichet2012$layer.335,
        ylim = c(0,15),
        col="aquamarine3",
        xlab="Dec1 ET 2007",
        ylab="ET Distribution (mm/d-1)")

boxplot(erichet2013$layer.335,
        ylim = c(0,15),
        col="aquamarine3",
        xlab="Dec1 ET 2008",
        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2009$layer.335,
#        ylim = c(0,15),
#        col="aquamarine3",
#        xlab="Dec1 ET 2009",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2010$layer.335,
#        ylim = c(0,15),
#        col="aquamarine3",
#        xlab="Dec1 ET 2010",
#        ylab="ET Distribution (mm/d-1)")

#boxplot(erichet2011$layer.335,
#        ylim = c(0,15),
#        col="aquamarine3",
#        xlab="Dec1 ET 2011",
#        ylab="ET Distribution (mm/d-1)")


cat ("Press [enter] see histograms of crop yield data for 2012 and 2013")
line <- readline()

#------crop yield plots-----#
#win.graph(11,7) 
#---par function sets number or rows and columns for plotting----#
par(mfrow=c(2,2))  

hist(cropyield_df[,8],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Winter Wheat 2012 Yield")

hist(cropyield_df[,9],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Winter Wheat 2013 Yield")



hist(cropyield_df[,11],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Spring Wheat 2012 Yield")

hist(cropyield_df[,12],   
     col = "gray",
     xlab = "crop yield",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = "Spring Wheat 2013 Yield")
#------ET Correlations

cat ("Press [enter] for goodnesss of fit analysis of crop yield 2012 winter")
line <- readline()

#--plots et vs crop yield, 2012 winter-#


etcrop2012winter_varcrop = etcrop2012winter[,1]
etcrop2012winter_varet = etcrop2012winter[,2]
etcrop2012spring_varcrop = etcrop2012spring[,1]
etcrop2012spring_varet = etcrop2012spring[,2]

etcrop2013winter_varcrop = etcrop2013winter[,1]
etcrop2013winter_varet = etcrop2013winter[,2]
etcrop2013spring_varcrop = etcrop2013spring[,1]
etcrop2013spring_varet = etcrop2013spring[,2]


#---setup for goodnesss of fit analysis of crop yield 2012 winter--#

mu = mean(etcrop2012winter_varcrop)
sigma = sd(etcrop2012winter_varcrop)
#mu = 100  # Set the mean.
#sigma = 2 # Set the standard deviation.

# Use rnorm function to generate random numbers.
#randNorm = rnorm(n, mu, sigma) 

muExp = mean(etcrop2012winter_varcrop)
#muExp = 10  # Set the mean.
#randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
#randWbl = sort(rweibull(n = n, scale = b,shape = c))  

lambda = mean(etcrop2012winter_varcrop)
#lambda = 10 # Set the mean. 
#randPois = sort(rpois(n = n, lambda = lambda)) 

#----normal distribution gof---#

fit = fitdistr(etcrop2012winter_varcrop,"normal") # Fit normal distribution to data.
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(etcrop2012winter_varcrop),muHat,sigmaHat)
#win.graph()
hist(etcrop2012winter_varcrop, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "2012 winter crop yield",
     main = paste("Normal Distribution fit to 2012 winter crop yield\n", "mu = ", mu, ";\n sigma = ", sigma))
lines(sort(etcrop2012winter_varcrop),nHat,lwd=2)
#text(min(etcrop2012winter_varcrop),max(nHat), pos = 4,
#     paste("Fit: mu = ",round(muHat*100)/100))
#text(min(etcrop2012winter_varcrop),0.9*max(nHat), pos = 4,
#     paste("sigma = ", round(sigmaHat*100)/100))

cat ("Press [enter] for goodnesss of fit analysis of crop yield 2013 winter")
line <- readline()

#---setup for goodnesss of fit analysis of crop yield 2013 winter--#

mu = mean(etcrop2013winter_varcrop)
sigma = sd(etcrop2013winter_varcrop)
#mu = 100  # Set the mean.
#sigma = 2 # Set the standard deviation.

# Use rnorm function to generate random numbers.
#randNorm = rnorm(n, mu, sigma) 

muExp = mean(etcrop2013winter_varcrop)
#muExp = 10  # Set the mean.
#randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
#randWbl = sort(rweibull(n = n, scale = b,shape = c))  

lambda = mean(etcrop2013winter_varcrop)
#lambda = 10 # Set the mean. 
#randPois = sort(rpois(n = n, lambda = lambda)) 

#----normal distribution gof---#

fit = fitdistr(etcrop2013winter_varcrop,"normal") # Fit normal distribution to data.
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(etcrop2013winter_varcrop),muHat,sigmaHat)
#win.graph()
hist(etcrop2013winter_varcrop, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "2013 winter crop yield",
     main = paste("Normal Distribution fit to 2013 winter crop yield\n", "mu = ", mu, ";\n sigma = ", sigma))
lines(sort(etcrop2013winter_varcrop),nHat,lwd=2)
#text(min(etcrop2013winter_varcrop),max(nHat), pos = 4,
#     paste("Fit: mu = ",round(muHat*100)/100))
#text(min(etcrop2013winter_varcrop),0.9*max(nHat), pos = 4,
#     paste("sigma = ", round(sigmaHat*100)/100))

cat ("Press [enter] for goodnesss of fit analysis of crop yield 2012 spring")
line <- readline()

#---setup for goodnesss of fit analysis of crop yield 2012 spring--#

mu = mean(etcrop2012spring_varcrop)
sigma = sd(etcrop2012spring_varcrop)
#mu = 100  # Set the mean.
#sigma = 2 # Set the standard deviation.

# Use rnorm function to generate random numbers.
#randNorm = rnorm(n, mu, sigma) 

muExp = mean(etcrop2012spring_varcrop)
#muExp = 10  # Set the mean.
#randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
#randWbl = sort(rweibull(n = n, scale = b,shape = c))  

lambda = mean(etcrop2012spring_varcrop)
#lambda = 10 # Set the mean. 
#randPois = sort(rpois(n = n, lambda = lambda)) 

#----normal distribution gof---#

fit = fitdistr(etcrop2012spring_varcrop,"normal") # Fit normal distribution to data.
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(etcrop2012spring_varcrop),muHat,sigmaHat)
#win.graph()
hist(etcrop2012spring_varcrop, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "2012 spring crop yield",
     main = paste("Normal Distribution fit to 2012 spring crop yield\n", "mu = ", mu, ";\n sigma = ", sigma))
lines(sort(etcrop2012spring_varcrop),nHat,lwd=2)
#text(min(etcrop2012spring_varcrop),max(nHat), pos = 4,
#     paste("Fit: mu = ",round(muHat*100)/100))
#text(min(etcrop2012spring_varcrop),0.9*max(nHat), pos = 4,
#     paste("sigma = ", round(sigmaHat*100)/100))

cat ("Press [enter] for goodnesss of fit analysis of crop yield 2013 spring")
line <- readline()

#---setup for goodnesss of fit analysis of crop yield 2013 spring--#

mu = mean(etcrop2013spring_varcrop)
sigma = sd(etcrop2013spring_varcrop)
#mu = 100  # Set the mean.
#sigma = 2 # Set the standard deviation.

# Use rnorm function to generate random numbers.
#randNorm = rnorm(n, mu, sigma) 

muExp = mean(etcrop2013spring_varcrop)
#muExp = 10  # Set the mean.
#randExp = sort(rexp(n = n,rate = 1/muExp))

b = 150  # Set the scale parameter, b.
c = 1.5  # Set the shape parameter, c.
#randWbl = sort(rweibull(n = n, scale = b,shape = c))  

lambda = mean(etcrop2013spring_varcrop)
#lambda = 10 # Set the mean. 
#randPois = sort(rpois(n = n, lambda = lambda)) 

#----normal distribution gof---#

fit = fitdistr(etcrop2013spring_varcrop,"normal") # Fit normal distribution to data.
muHat = fit$estimate[1]  # MLE mean
sigmaHat = fit$estimate[2]  # MLE standard deviation
nHat = dnorm(sort(etcrop2013spring_varcrop),muHat,sigmaHat)
#win.graph()
hist(etcrop2013spring_varcrop, breaks=50, freq = FALSE, col=rgb(0.75,0.75,0.75),
     ylab = "Probability density",
     xlab = "2013 spring crop yield",
     main = paste("Normal Distribution fit to 2013 spring crop yield\n", "mu = ", mu, ";\n sigma = ", sigma))
lines(sort(etcrop2013spring_varcrop),nHat,lwd=2)
#text(min(etcrop2013spring_varcrop),max(nHat),
#     paste("Fit: mu = ",round(muHat*100)/100))
#text(min(etcrop2013spring_varcrop),0.9*max(nHat),
#     paste("sigma = ", round(sigmaHat*100)/100))


cat ("Press [enter] see bivariate data of ET vs. crop yield - raw data")
line <- readline()


newplot <- lm(etcrop2012winter[,2] ~ etcrop2012winter[,1])
plot(etcrop2012winter[,1], etcrop2012winter[,2])
abline(newplot)

#--plots et vs crop yield, 2013 winter-#

newplot <- lm(etcrop2013winter[,2] ~ etcrop2013winter[,1])
plot(etcrop2013winter[,1], etcrop2013winter[,2])
abline(newplot)

#--plots et vs crop yield, 2012 winter-#

newplot <- lm(etcrop2012spring[,2] ~ etcrop2012spring[,1])
plot(etcrop2012spring[,1], etcrop2012spring[,2])
abline(newplot)

#--plots et vs crop yield, 2013 winter-#

newplot <- lm(etcrop2013spring[,2] ~ etcrop2013spring[,1])
plot(etcrop2013spring[,1], etcrop2013spring[,2])
abline(newplot)

cat ("Press [enter] to see zscore plots of ET vs crop yield for 2012 and 2013")
line <- readline()

#---zscore plots

newplot <- lm(erichet2012springzscore$data ~ erichyield2012springzscore$data)
plot(erichet2012springzscore$data, erichyield2012springzscore$data,
     
     xlab ="2012 Spring ET Zscore",
     ylab ="2012 Spring Crop Yield Zscore",
     main = paste("2012 Spring ET vs Crop Yield - Zscore comparison\n p-value: 0.09434"))
abline(0,1)

newplot <- lm(erichet2013springzscore$data ~ erichyield2013springzscore$data)
plot(erichet2013springzscore$data, erichyield2013springzscore$data,
     xlab ="2013 Spring ET Zscore",
     ylab ="2013 Spring Crop Yield Zscore",
     main = paste("2013 Spring ET vs Crop Yield - Zscore comparison\n p-value: 0.00317"))
abline(0,2)

newplot <- lm(erichet2012winterzscore$data ~ erichyield2012winterzscore$data)
plot(erichet2012springzscore$data, erichyield2012springzscore$data,
     xlab ="2012 Winter ET Zscore",
     ylab ="2012 Winter Crop Yield Zscore",
     main = paste("2012 Winter ET vs Crop Yield - Zscore comparison\n p-value: 0.5495"))
abline(0,1)

newplot <- lm(erichet2013winterzscore$data ~ erichyield2013winterzscore$data)
plot(erichet2013springzscore$data, erichyield2013springzscore$data,
     xlab ="2013 Winter ET Zscore",
     ylab ="2013 Winter Crop Yield Zscore",
     main = paste("2013 Winter ET vs Crop Yield - Zscore comparison\n p-value: 0.1854"))
abline(0,2)
