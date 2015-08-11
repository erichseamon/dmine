#------------------------------------------------------------------------#
# TITLE:        agmesh_combine_cropyield.R
#
# COURSE:       Computational Data Analysis (FOR504)
#               Final Project
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         October 17. 2014
#
# STAGE:        Agmesh Setup - Stage 
#
# COMMENTS:     This is the agmesh setup - stage .  Agmesh uses weather 
#               parameters for the inland pacific northwest to calculate  
#               climate parameter related outputs. Stage 2 aggregates and 
#               subsets climatic grid data, based on user input.  The files
#               that are created are used for agmesh analytics.
#                
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------


#----SETUP SECTION----------------------------------------------------#

#----creating vector of growth days for spring wheat and winter wheat--#

#---zscore calculation for et - for year and season datasets---#


for (i in yearspan) {
  for (l in season) {
    zscore <- data.frame(data=NaN, 1:22)
    assign(paste("erichet", i, l, "zscore", sep=""), zscore)
    zscore <- subset(zscore, select = c(data))
    zscoremean1 <- get(paste("erichet", i, l, "_mean", sep=""))
    zscoremean1 <- as.numeric(unlist(zscoremean1))
    zscoremean <- mean(zscoremean1)
    zscoremean1 <- data.frame(zscoremean1)
    zscoresd <- get(paste("erichet", i, l, "_mean", sep=""))
    zscoresd <- as.numeric(unlist(zscoresd))
    zscoresd <- sd(zscoresd)
    for (m in obs) {
      zscore[m,] <- (zscoremean1[m,] - zscoremean) / zscoresd
      assign(paste("erichet", i, l, "zscore", sep=""), zscore)  
    }
  }
}


for (i in yearspan) {
  for (l in season) {
    cropzscore <- data.frame(data=NaN, 1:22)
    assign(paste("erichyield", i, l, "zscore", sep=""), cropzscore)
    cropzscore <- subset(cropzscore, select = c(data))
    cropzscoreall <- get(paste("cropyield_df_yieldobs", i, j, sep=""))
    cropzscoreall1 <- as.numeric(unlist(cropzscoreall))
    #cropzscoreall1 <- data.frame(cropzscoreall1)
    assign(paste("cropyield_df_yieldobs2", i, j, sep=""), cropzscoreall1)
    cropzscoremean1 <- get(paste("cropyield_df_yieldobs_mean", i, j, sep=""))
    cropzscoremean1 <- as.numeric(unlist(cropzscoremean1))
    #cropzscoremean <- mean(cropzscoremean1)
    cropzscoremean1 <- data.frame(cropzscoremean1)
    cropzscoresd <- get(paste("erichet", i, l, "_mean", sep=""))
    cropzscoresd <- as.numeric(unlist(cropzscoresd))
    cropzscoresd <- sd(cropzscoresd)
    for (m in obs) {
      cropzscore[m,] <- (cropzscoreall[m,] - cropzscoremean1) / cropzscoresd
      assign(paste("erichyield", i, l, "zscore", sep=""), cropzscore)
      
    }
  }
}

#---loop to combine et & crop yield into matrices for cor analysis--#

for (i in yearspan) {
  for (l in season) {
    cropcombo <- get(paste("cropyield_df_yieldobs", i, l, sep=""))
    etcombo <- get(paste("erichet", i, l, "_mean", sep=""))
    et_crop_combo <- cbind(cropcombo, etcombo)
    et_crop_combo <- data.matrix(et_crop_combo)
    assign(paste("etcrop", i, l, sep=""), et_crop_combo)
  }}


for (i in yearspan) {
  for (l in season) {
    aovcombo <- get(paste("erichet", i, l, "_xyobs", sep=""))
    yieldcombo <- get(paste("cropyield_df_yieldobs", i, l, sep=""))
    aovcombo2 <- cbind(aovcombo, yieldcombo)
    aovcombo2 <- data.matrix(aovcombo2)
    aovcombo3 <- rep(i,nrow(aovcombo2)) # make new column 
    aovcombo3 <- data.frame(aovcombo3)
    aovcombo2 <- cbind(aovcombo2, aovcombo3)
    assign(paste("aov", i, l, sep=""), aovcombo2)
    aovcombo4 <- rep(l,nrow(aovcombo2)) # make new column 
    aovcombo4 <- data.frame(aovcombo4)
    aovcombo2 <- cbind(aovcombo2, aovcombo4)
    aovcombo2 <- data.matrix(aovcombo2)
    #aovcombo2 <- t(aovcombo2)
    #assign(paste("aov", i, l, sep=""), aovcombo2)
    #aovcombo2stack <- stack(aovcombo2)
    #aovcombo2stack <- rep(i,nrow(aovcombo2stack))
    meltaov <- melt(aovcombo)
    assign(paste("aov", i, l, sep=""), meltaov)
    
  }}

#i<-1 
#while(i < 100) { 
#  cropyield_df_yieldobs2012winter <- rbind(cropyield_df_yieldobs2012winter, cropyield_df_yieldobs2012winter) 
#  i <- i + 1
#} 


#for (i in 100) {
#  i <- i + 1
#sss <- rbind(cropyield_df_yieldobs2012winter, cropyield_df_yieldobs2012winter)
#}