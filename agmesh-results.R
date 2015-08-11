#------------------------------------------------------------------------#
# TITLE:        agmesh_results.R
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         May 2015
#
# 
#
# COMMENTS:     This script generates statistical outputs for a  
#               dataset of climate variables -for a portion of  
#               Westrern WA and Eastern ID.  The script generates
#               a series of plots.
#               
#                
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------#


#--remove initial values that created matrix.  all ones at top
library(gridExtra)
#install.packages('dendextend')
library(dendextend)
                 
fclimatematrix1 <- fclimatematrix1_original

fclimatematrix1 = fclimatematrix1[-1,]
fclimatematrix1 = fclimatematrix1[-1,]

#--subset data for data reduction purposes

colnames(fclimatematrix1) <- c("TMMX", "TMMN", "RMIN", "RMAX", "SRAD", "VS", "ET", "YEAR", "MLRA", "LAT", "LON", "DAY")
setwd(paste("/agmesh-scenarios/", scen, sep="")) 
#write.matrix(fclimatematrix1, file="")

nrowz <- nrow(fclimatematrix1)
idx=sample(1:nrowz,nrowz,rep=T)

#day1 reduction
fclimatematrix1 <- subset(fclimatematrix1, fclimatematrix1[,12]=="1")
#fclimatematrix1b <- subset(fclimatematrix1, fclimatematrix1[,12]=="15")
#fclimatematrix1c <- subset(fclimatematrix1, fclimatematrix1[,12]=="30")
#fclimatematrix1d <- subset(fclimatematrix1, fclimatematrix1[,12]=="45")
#fclimatematrix1e <- subset(fclimatematrix1, fclimatematrix1[,12]=="60")
#fclimatematrix1f <- subset(fclimatematrix1, fclimatematrix1[,12]=="75")
#fclimatematrix1g <- subset(fclimatematrix1, fclimatematrix1[,12]=="90")
#fclimatematrix1h <- subset(fclimatematrix1, fclimatematrix1[,12]=="105")

#fclimatematrix1  <- rbind(fclimatematrix1a, fclimatematrix1b)


#fclimatematrix1  <- rbind(fclimatematrix1a, fclimatematrix1b, fclimatematrix1c, fclimatematrix1d, fclimatematrix1e, fclimatematrix1f, fclimatematrix1g, fclimatematrix1h)

#fclimatematrix1 = fclimatematrix1[337,]
#fclimatematrix1[, 9][fclimatematrix1[, 9] == 1] <- 59



#------start with reduced file read from server

#fclimatematrix1 <- read.csv("http://reacchapp.nkn.uidaho.edu/agmesh-scenarios/scenario_72698/analysis/datatable.csv")
#fclimatematrix1 <- fclimatematrix1[-1]

nrowz <- nrow(fclimatematrix1)

#-For multvariate normality test
#Ca <- t(fclimatematrix1[1:nrowz,1:6])
#mshapiro.test(Ca)


gp=as.factor(fclimatematrix1[,9])

fclimatemlrafactor <- factor(fclimatematrix1[,9])
fclimateyearfactor <- factor(fclimatematrix1[,8])
fclimatematrix2 <- cbind(fclimatematrix1[,1:6])
fclimatematrix2 <- as.matrix(fclimatematrix2)
#method <- factor(gl(3,18,36,labels=c("M1","M2","M3")))
#--Run manovas
manova_fclimatematrix2 <- summary(manova(fclimatematrix2 ~ fclimatemlrafactor))
manova_fclimatematrix3 <- summary(manova(fclimatematrix2 ~ fclimateyearfactor))
manova_fclimatematrix4 <- summary(manova(fclimatematrix2 ~ fclimatematrix1[,7]))

xx <- round(fclimatematrix1, digits=2)

#fcdf <- data.frame(fclimatematrix1)
#datafile <- paste("/agmesh-scenarios/", scen, "/analysis/", "reduced-datafile", "_", scen, "_", i, "_", j, "_", i, j, ".matrix", sep="")
#write.table(fcdf, file="/agmesh-scenarios/scenario_72698/analysis/datafile.table", sep="")
#save(fclimatematrix1,file="/agmesh-scenarios/scenario_72698/analysis/datafile.table")


#---Nearest Neighbor 10 fold CV
misrateknn10_20 <- matrix(NA, nrow=20, ncol=1)

for (m in 1:20) {
  
gp=as.factor(dat[,1])



#resu=lda(training,gp[idx!=1])

post.probknn1=matrix(rep(NA,3*nrowz),ncol=3) #-empty matrix for loop population

for (i in 1:10){#-loop for 10 fold CV for knn
  training=dat[idx!=1,1:6]#-training
  test <- dat[idx==i,1:6]#-test
  S <- cov(dat[idx!=i,1:6]) 
  jto <- nrowz-length(test[,1])
  jto2 <- length(test[,1])
  resu=matrix(NA,ncol=jto2, nrow=jto)
  resugroup=matrix(NA,ncol=1, nrow=jto)
  assign(paste("resu", i, sep = ""), resu)
  for (j in 1:jto){   
    resucolumns <- mahalanobis(dat[idx==i,1:6], as.numeric(dat[idx!=i,1:6][j,]), S)
    colnames(resu) <- names(resucolumns)
    resu[j,]=mahalanobis(dat[idx==i,1:6], as.numeric(dat[idx!=i,1:6][j,]), S)
    resugroup= as.matrix(dat[idx!=i,9])
  }
  resu <- cbind(resu, resugroup)
  assign(paste("resu", i, sep=""), resu)
}
#loop to restructure distances and calculate probabilities
knnprob10=t(matrix(NA,ncol=nrowz, nrow=3))
for (i in 1:10) {
  jto3 <- nrow(dat[idx==i,1:6])
  for (j in 1:jto3) {
    resuax <- data.frame(get(paste("resu", i, sep = "")))
    resua <- cbind(resuax[j], resuax[jto3+1])
    resua$rows <- rownames(resua)
    resua <- resua[order(resua[,1], decreasing=F),]
    
    #resua <- resuax[order(resuax[j]) , ]
    #vectornames <- as.matrix(names(resua))
    #vectornames2 <- rm1stchar(vectornames)
    #vectornames3 <- as.matrix(rm1stchar(vectornames[1,1]))
    #vectornames4 <- as.numeric(c(vectornames3[1,1], vectornames2[-1]))
    #vectornames5 <- as.matrix(vectornames4[1:jto3])
    #resua1 <- subset(resua, select = j)
    #resua2 <- subset(resua, select = jto3+1)
    #resua <- cbind(resua1, resua2)
    resua[, 2][resua[, 2] == 10] <- 1
    resua[, 2][resua[, 2] == 11] <- 2
    resua[, 2][resua[, 2] == 59] <- 3
    resuaall <- resua
    resua <- resua[1:9,]
    
    for (k in 1:3){ #sub-loop that takes lowest 9 distances, and creates probabilities
      xx <- as.numeric(resuaall[j,3])
      knnprob10[xx,k] <- (length(which(resua[,2] == k)))/9
    }
    assign(paste("resuaa", j, sep=""), resua)
  }
}

knnprob10[is.na(knnprob10)] <- 0

predknn10.gp=apply(knnprob10,1,which.max) #-predicted groups
predknn10.gp[predknn10.gp == 1] <- 10
predknn10.gp[predknn10.gp == 2] <- 11
predknn10.gp[predknn10.gp == 3] <- 59

predknn10.gp <- unlist(predknn10.gp, recursive=TRUE)
#predkde1.gp <- rbind (predkde1.gp, predkde1.gp[2304])
#predknn10.gp <- c(predknn.gp, 11)

gpknn=as.factor(dat[,9])
xx <- table(gpknn,predknn10.gp)
xx_sorted <- apply(xx,1,which.max)
xx <- xx[,xx_sorted]

gpknn=as.factor(dat[idx==i,9])
table(gpknn,predknn10.gp) #-original vs predicted groups

misrateknn10_20[m] = (nrowz-sum(diag(table(gpknn,predknn10.gp))))/nrowz #-misclassification rate
}


#--Tree classifcation


for (m in 1:20) {
  
  nrowz <- nrow(fclimatematrix2)
  gplevels <- nlevels(gpx3)
    
post.probtree10=matrix(rep(NA,gplevels*nrowz),ncol=gplevels)

df.fc <- data.frame(fclimatematrix1)

fc.1 <- cbind(df.fc[,1:6], df.fc[9])
fc.2 <- cbind(df.fc[,1:6])

attach(fc.1)


library(ggdendro)
#for(i in 1:10) {

fc.cart <- rpart(MLRA~., fc.2, 
      method="class")

plot(fc.cart, margin=.2)
text(fc.cart, use.n=FALSE, pretty=0)
  
  treetrain <- data.frame(fclimatematrix1[idx!=i,])
  treetest <- data.frame(fclimatematrix1[idx==i,])
  post.probtree10[idx==i,]=predict(rpart(MLRA~., treetrain, 
                                                      method="class"), treetest, type="prob")
}

predtree10.gp=apply(post.probtree10,1,which.max)
table(gpx3,predtree10.gp)

misratetree10_20[m] =(nrowz-sum(diag(table(gpx3,predtree10.gp))))/nrowz

#-HClustering

misratehc10_20=c()
m=10

for(j in 1:20){
  post.prob.hc=matrix(rep(NA,3*nrowz),ncol=3)
  idx=sample(1:m,nrowz,rep=TRUE)
  #dat.new=cbind(fc.1,idx)
  for(i in 1:m){
    training=fc.1[idx!=i,-7]
    test=fc.1[idx==i,-7]
    dat_hc_w=cutree(hclust(dist(scale(training)),method="ward.D"),3)
    for(g in 1:3){
      centroid=colMeans(fc.1[dat_hc_w==g,-7])
      post.prob.hc[idx==i,g]=apply(sweep(fc.1[idx==i,-7],2,centroid)^2,1,sum)
    }}
  
  pred_gp_hc=apply(post.prob.hc,1,which.min)
  misratehc10_20[j]=round(((nrowz-(sum(apply(table(gp,pred_gp_hc),2,max))))/nrowz),2)
}

dat_hc_w=hclust(dist(scale(fc.1)),method="ward.D")


#-dendrogram creation for Hclustering

dend <- as.dendrogram(dat_hc_w)

#plot(dend)


#labels_colors(dend) <- fc.1[,7] 
#labels_colors(dend)

#jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "dend1", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
#plot(dend)
#dev.off(); 

#dend2 <- cut(dendi, h = 70)

#plot(dend2$upper)

#jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "dend2", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
#plot(cut(dend, h = 70)$lower[[2]], cex=10)
#dev.off(); 

#ggdendrogram(dat_hc_w, rotate = FALSE, size = 2)

#dendi <- as.dendrogram(dat_hc_w)






#plot(X2~X1,pch=unclass(C.class),col=unclass(C.pred$class),data=C)

#prob=round(post.prob[rbind(1,31,61),], digits=2) #--1st, 31st, and 61st probabilities

#---PCA

fclim.pc <- prcomp(fclimatematrix1[,1:6], scale=T)  
U <- fclim.pc$rotation #---U
X <- fclimatematrix1[,1:6]
X2 <- fclimatematrix1[,9]
X3<- cbind(X,X2)
Z <- fclimatematrix1[,1:6] %*% fclim.pc$rotation
#Xsample <- X3[sample(nrow(X3),size=1000,replace=TRUE),]
#Zsample <- Z[sample(nrow(Z),size=1000,replace=TRUE),]

#-removing outliers - es050315
fclim.pc2 <- fclim.pc
fclim.pc2$x[337,] <-  0
fclim.pc2$x[154,] <-  0

#--remove any previously created plots in this scenario
#--before creating new versions below

system(paste("rm -r ", "/agmesh-scenarios/", scen, "/analysis/", "*.jpg", sep=""))


#--Plotting of all results

#-EDA plots

jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "datatable-example", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
grid.table(xx[1:10,1:11])
dev.off(); 

jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "boxplot-allvar", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
boxplot(fclimatematrix1[,1:7], main="Boxplot of all variables - Day 1 2000")
dev.off(); 

#--Dendrogram plotting

labels_colors(dend) <- fc.1[,7] 
labels_colors(dend)

jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "dend1", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
plot(dend)
dev.off(); 

jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "dend2", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
plot(cut(dend, h = 70)$lower[[2]], cex=10)
dev.off(); 


#-pairwise plot of initial data

jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "pairwise", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
gpx3=as.factor(X3[,7])
pairs(X3,pch=20,col=unclass(gpx3))
dev.off();

#-correlation
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "correlation", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
grid.table(round(cor(fclimatematrix1[,1:6]), digits=3))
(cor(fclimatematrix1[,1:6]))
dev.off();

#-box plot of Z
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "z-pca-boxplot", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
boxplot(Z)
dev.off(); 

#-pairwise plot of Z
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "z-pca-pairs", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
pairs(Z)
dev.off(); 

#--correlation of each variable to PC1
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "pca1-correlation", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
cor(as.matrix(scale(fclimatematrix1[,1:6])), fclim.pc2$x[,1])
dev.off(); 

#-loadings
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "pca-loadings", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
fclim.pc.loadings <- cor(as.matrix((fclimatematrix1[,1:6])), fclim.pc2$x)
dev.off(); 

#-sum of loadings
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "pca-sum-loadings", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
fclim.pc.loadings.sum <- apply(fclim.pc.loadings[,1:3]^2,2,sum)
dev.off(); 

#-variances of PC
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "pca-variances", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
fclim.pc.variances <- (fclim.pc2$sdev^2)
dev.off(); 

#pairwise plot of first 6 PCs
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "pca-pairwise", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
pairs(fclim.pc2$x[,1:6], pch=20,col=unclass(gpx3))
dev.off(); 

#-biplot of PC results with 337 removed
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "pca-biplot", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
biplot(fclim.pc2, xlabs=fclimatematrix1[,9], cex=c(.7,1))
dev.off(); 

#-zoomed in biplot of PC results with 337 removed
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "pca-biplot-zoom", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
biplot(fclim.pc2, xlabs=fclimatematrix1[,9], cex=c(.7,1), expand=20, xlim=c(-0.05, 0.05), ylim=c(-0.05, 0.05))
dev.off(); 

#plotting PC1 vs PC2
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "pc1-vs-pc2", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
plot(fclim.pc2$x[,1:2], type='p', col=c("blue", "red", "green")[unclass(gpx3)])
dev.off(); 

#-screeplot 
jpeg(paste("/agmesh-scenarios/", scen, "/analysis/", "screeplot", "_", scen, "_", i, "_", j, "_", i, j, ".jpg", sep=""))
screeplot(fclim.pc2)
dev.off(); 


system("rm -r /agmesh-scenarios/loaded_scenario")
system("mkdir -p /agmesh-scenarios/loaded_scenario")
system("chmod -R 777 /agmesh-scenarios/loaded_scenario")
cmd <- paste("cp -r /agmesh-scenarios/", scen, "/* ", "/agmesh-scenarios/loaded_scenario/", sep="")
system(cmd)