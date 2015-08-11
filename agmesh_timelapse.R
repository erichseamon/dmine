setwd(paste("/agmesh-scenarios/", scen, "/ETtimelapse", sep="")) 
system("ffmpeg -i '%*.jpg' -r 30 -q:v 2 timelapse.mp4")