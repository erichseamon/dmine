
  setdir <- paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/month_png/", "drought", sep="")
  listz <- list.dirs(setdir)
  liss <- length(listz)
  newframe <- t(data.frame(strsplit(listz, "\\/")[2:liss]))
  uni <- unique(newframe[,9])
  
  for (i in uni) {
    
  
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/month_png/", "drought/", i, sep="")) 
  #system(paste("rm ", i, "_timelapse.mp4", sep=""))
  #setpts=2.0*PTS
  #setwd(paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/month_png/", "timelapse", sep="")) 
  system(paste("ffmpeg -i ", "'%*", i, "_plot.png'", " -r 30 -q:v 1 ", i, "_timelapse.mp4", sep=""))
  #system("ffmpeg -i wheat_timelapse.mp4 -vf "setpts=2.0*PTS" wheat_slowtimelapse.mp4")
  system("mv *.mp4 /dmine/data/USDA/agmesh-scenarios/Idaho/month_png/timelapse/")
}

  
  
  
  
  
  