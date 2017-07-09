
  setdir <- paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png2/", sep="")
  
  
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png2/", sep=""))
  system(paste("mkdir ", "timelapse", sep=""))
  
  #----
  listallnames <- list.files(setdir)
  lan <- as.data.frame(listallnames)
  colnames(lan) <- "names"
  lan$names <- as.character(lan$names)
  
  lan$names2 = substr(lan$names,1,nchar(lan$names)-9)

  
  lan$names2 <- substring(lan$names2, 9)
  
  DTz1list <- unique(lan$names2)
 
  #----
  
  for (i in DTz1list) {
    
  setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png2/timelapse/, i, sep="")) 
  system(paste("ffmpeg -framerate 5 -i ", "'%*", i, "_plot.png' ", "-q:v 20 ", i, "_timelapse.mp4", sep="")) #--plays in chrome
  system(paste("mv *.mp4 /dmine/data/USDA/agmesh-scenarios/", scen_state, "/month_png/timelapse/", sep=""))
                
    }
  
 
    
  #system(paste("rm ", i, "_timelapse.mp4", sep=""))
  #setpts=2.0*PTS
  #setwd(paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/month_png/", "timelapse", sep="")) 

 
  #system(paste("/bin/ffmpeg-2.6.2/ffmpeg -framerate 10 -i ", "'%*", i, "_plot.png' ", i, "_timelapse.mp4", sep="")) #--plays in safari
  #system(paste("/bin/ffmpeg-2.6.2/ffmpeg -i ", i, "_timelapse.mp4", " ", "-codec libx264 ",  i,  "_timelapse_h264.mp4", sep=""))
  #system("ffmpeg -i wheat_timelapse.mp4 -vf "setpts=0.5*PTS" wheat_slowtimelapse.mp4")
  #system("mv *.mp4 /dmine/data/USDA/agmesh-scenarios/Idaho/month_png/timelapse/")
  
  
  
  

  #--joint timelapse
  
setwd(paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/month_png/", "timelapse/", sep="")) 

system("ffmpeg -i WHEAT_timelapse.mp4 -i BARLEY_timelapse.mp4 -i CANOLA_timelapse.mp4 -i OATS_timelapse.mp4 -filter_complex 'nullsrc=size=960x720 [base]; [0:v] setpts=PTS-STARTPTS, scale=480x360 [upperleft]; [1:v] setpts=PTS-STARTPTS, scale=480x360 [upperright]; [2:v] setpts=PTS-STARTPTS, scale=480x360 [lowerleft]; [3:v] setpts=PTS-STARTPTS, scale=480x360 [lowerright]; [base][upperleft] overlay=shortest=1 [tmp1]; [tmp1][upperright] overlay=shortest=1:x=480 [tmp2]; [tmp2][lowerleft] overlay=shortest=1:y=360 [tmp3]; [tmp3][lowerright] overlay=shortest=1:x=480:y=360' -q:v 5 WHEAT-BARLEY-CANOLA-OATS.mp4")

  
  
  
  
  
  
