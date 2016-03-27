setwd("/git/data/USDA/jpgs/") 
system("rm timelapse.mp4")
setpts=2.0*PTS
system("ffmpeg -i '%*.jpg' -r 30 -q:v 1 timelapse.mp4")
system("ffmpeg -i timelapse.mp4 -vf "setpts=2.0*PTS" slowtimelapse.mp4")
