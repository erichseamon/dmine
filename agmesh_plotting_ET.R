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
now <- Sys.time()
#------SECTION 3 - Plot Univariate and BiVariate Data ------#

#------Plotting - ET with climate variables ------#

#win.graph(15,15)

#dev.off()

#--spring plotting


for (i in yearspan) {

jpeg(paste("/agmesh-scenarios/", scen, "/tmmximages", "/springplot", "tmmx", i, ".jpg", sep=""))
  
lyt = c(1, 2)

lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' function


  tmmxbrickspringmean <- get(paste("tmmxET", i, "brickspringmean", sep=""))
  tmmnbrickspringmean <- get(paste("tmmnET", i, "brickspringmean", sep=""))
  rmaxbrickspringmean <- get(paste("rmaxET", i, "brickspringmean", sep=""))
  rminbrickspringmean <- get(paste("rminET", i, "brickspringmean", sep=""))
  sradbrickspringmean <- get(paste("sradET", i, "brickspringmean", sep=""))
  vsbrickspringmean <- get(paste("vsET", i, "brickspringmean", sep=""))

  
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


hist(tmmxbrickspringmean,   
     col = "gray",
     xlab = "Celsius",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = paste("Max Air Temp - ", "Spring ", i, sep=""))

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

plot(subset(tmmxbrickspringmean), 
            #col = "gray",
            xlab = "Longitude", 
            ylab = "Latitude",
            #ylim = c(0,240),
            #xlim = c(-0.5,6),
            main = paste("Max Air Temp - ", "Spring ", i, sep=""))

plot(tristate, bg="transparent", add=TRUE)
mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
dev.off()



jpeg(paste("/agmesh-scenarios/", scen, "/tmmnimages", "/springplot", "tmmn", i, ".jpg", sep=""))

lyt = c(1, 2)

lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' function

hist(tmmnbrickspringmean,   
     col = "gray",
     xlab = "Celsius",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = paste("Min Air Temp - ", "Spring ", i, sep=""))

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


plot(subset(tmmnbrickspringmean),  
            #col = "gray",
            xlab = "Longitude",
            ylab = "Latitude",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = paste("Min Air Temp - ", "Spring ", i, sep=""))

plot(tristate, bg="transparent", add=TRUE)
mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  

dev.off()



jpeg(paste("/agmesh-scenarios/", scen, "/rminimages", "/springplot", "rmin",  i, ".jpg", sep=""))

lyt = c(1, 2)

lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' function

hist(rminbrickspringmean,   
     col = "gray",
     xlab = "%",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = paste("Min Rel Humidity - ", "Spring ", i, sep=""))

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



plot(subset(rminbrickspringmean),  
            #col = "gray",
            xlab = "Longitude",
            ylab = "Latitude",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = paste("Min Rel Humidity - ", "Spring ", i, sep=""))

plot(tristate, bg="transparent", add=TRUE)
mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  

dev.off()

jpeg(paste("/agmesh-scenarios/", scen, "/rmaximages", "/springplot", "rmax", i, ".jpg", sep=""))

lyt = c(1, 2)

lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' function

hist(rmaxbrickspringmean,   
     col = "gray",
     xlab = "%",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = paste("Max Rel Humidity - ", "Spring ", i, sep=""))

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


plot(subset(rmaxbrickspringmean),  
            #col = "gray",
            xlab = "Longitude",
            ylab = "Latitude",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = paste("Max Rel Humidity - ", "Spring ", i, sep=""))

plot(tristate, bg="transparent", add=TRUE)
mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  

dev.off()

jpeg(paste("/agmesh-scenarios/", scen, "/sradimages", "/springplot", "srad", i, ".jpg", sep=""))

lyt = c(1, 2)

lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' function

hist(sradbrickspringmean,   
     col = "gray",
     xlab = "w/m2",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = paste("Solar Radiation - ", "Spring ", i, sep=""))

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



plot(subset(sradbrickspringmean),  
            #col = "gray",
            xlab = "Longitude",
            ylab = "Latitude",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = paste("Solar Radiation - ", "Spring ", i, sep=""))

plot(tristate, bg="transparent", add=TRUE)
mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  

dev.off()

jpeg(paste("/agmesh-scenarios/", scen, "/vsimages", "/springplot", "vs", i, ".jpg", sep=""))

lyt = c(1, 2)

lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
layout(lytmtx) # Set the layout of the figure using the 'layout' function

hist(vsbrickspringmean,   
     col = "gray",
     xlab = "m-s-1",
     ylab = "Count",
     #ylim = c(0,230),
     #xlim = c(-0.5,7),
     main = paste("Wind Speed - ", "Spring ", i, sep=""))

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

plot(subset(vsbrickspringmean),  
            #col = "gray",
            xlab = "Longitude",
            ylab = "Latitude",
            #ylim = c(0,230),
            #xlim = c(-0.5,7),
            main = paste("Wind Speed - ", "Spring ", i, sep=""))

plot(tristate, bg="transparent", add=TRUE)
mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  

dev.off()

}





#--winter plottingnew

for (i in yearspan) {
  
  jpeg(paste("/agmesh-scenarios/", scen, "/tmmximages", "/winterplot", "tmmx", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  
  tmmxbrickwintermean <- get(paste("tmmxET", i, "brickwintermean", sep=""))
  tmmnbrickwintermean <- get(paste("tmmnET", i, "brickwintermean", sep=""))
  rmaxbrickwintermean <- get(paste("rmaxET", i, "brickwintermean", sep=""))
  rminbrickwintermean <- get(paste("rminET", i, "brickwintermean", sep=""))
  sradbrickwintermean <- get(paste("sradET", i, "brickwintermean", sep=""))
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
  
  
  hist(tmmxbrickwintermean,   
       col = "gray",
       xlab = "Celsius",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Max Air Temp - ", "Winter ", i, sep=""))
  
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
  
  plot(subset(tmmxbrickwintermean),   
              #col = "gray",
              xlab = "Longitude",
              ylab = "Latitude",
              #ylim = c(0,240),
              #xlim = c(-0.5,6),
              main = paste("Max Air Temp - ", "Winter ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  
  
  jpeg(paste("/agmesh-scenarios/", scen, "/tmmnimages", "/winterplot", "tmmn", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(tmmnbrickwintermean,   
       col = "gray",
       xlab = "Celsius",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Min Air Temp - ", "Winter ", i, sep=""))
  
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
  
  
  plot(subset(tmmnbrickwintermean),  
              #col = "gray",
              xlab = "Longitude",
              ylab = "Latitude",
              #ylim = c(0,230),
              #xlim = c(-0.5,7),
              main = paste("Min Air Temp - ", "Winter ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  
  
  jpeg(paste("/agmesh-scenarios/", scen, "/rminimages", "/winterplot", "rmin",  i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(rminbrickwintermean,   
       col = "gray",
       xlab = "%",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Min Rel Humidity - ", "Winter ", i, sep=""))
  
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
  
  
  
  plot(subset(rminbrickwintermean),  
              #col = "gray",
              xlab = "Longitude",
              ylab = "Latitude",
              #ylim = c(0,230),
              #xlim = c(-0.5,7),
              main = paste("Min Rel Humidity - ", "Winter ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  jpeg(paste("/agmesh-scenarios/", scen, "/rmaximages", "/winterplot", "rmax", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(rmaxbrickwintermean,   
       col = "gray",
       xlab = "%",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Max Rel Humidity - ", "Winter ", i, sep=""))
  
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
  
  
  plot(subset(rmaxbrickwintermean),  
              #col = "gray",
              xlab = "Longitude",
              ylab = "Latitude",
              #ylim = c(0,230),
              #xlim = c(-0.5,7),
              main = paste("Max Rel Humidity - ", "Winter ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  jpeg(paste("/agmesh-scenarios/", scen, "/sradimages", "/winterplot", "srad", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(sradbrickwintermean,   
       col = "gray",
       xlab = "w/m2",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Solar Radiation - ", "Winter ", i, sep=""))
  
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
  
  
  
  plot(subset(sradbrickwintermean),  
              #col = "gray",
              xlab = "Longitude",
              ylab = "Latitude",
              #ylim = c(0,230),
              #xlim = c(-0.5,7),
              main = paste("Solar Radiation - ", "Winter ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
  jpeg(paste("/agmesh-scenarios/", scen, "/vsimages", "/winterplot", "vs", i, ".jpg", sep=""))
  
  lyt = c(1, 2)
  
  lytmtx = matrix(lyt,nrow=2,ncol=1,byrow=T)
  layout(lytmtx) # Set the layout of the figure using the 'layout' function
  
  hist(vsbrickwintermean,   
       col = "gray",
       xlab = "m-s-1",
       ylab = "Count",
       #ylim = c(0,230),
       #xlim = c(-0.5,7),
       main = paste("Wind Speed - ", "Winter ", i, sep=""))
  
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
  
  plot(subset(vsbrickwintermean),  
              #col = "gray",
              xlab = "Longitude",
              ylab = "Latitude",
              #ylim = c(0,230),
              #xlim = c(-0.5,7),
              main = paste("Wind Speed - ", "Winter ", i, sep=""))
  
  plot(tristate, bg="transparent", add=TRUE)
  mtext(paste("Scenario:   ", scen, "  Run Date:  ", now, sep=""), side=1, line=4, cex=1, col="black")  
  
  dev.off()
  
}

system("rm -r /agmesh-scenarios/loaded_scenario")
system("mkdir -p /agmesh-scenarios/loaded_scenario")
system("chmod -R 777 /agmesh-scenarios/loaded_scenario")
cmd <- paste("cp -r /agmesh-scenarios/", scen, "/* ", "/agmesh-scenarios/loaded_scenario/", sep="")
system(cmd)
