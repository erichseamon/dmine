#------------------------------------------------------------------------#
# TITLE:        dmine-wordcloud.R
#
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         December 2015
#
# STAGE:        dmine-soilmoisture.R
#
# COMMENTS:     Soil moisture machine learning model
#                
#
#--Setting the working directory an d clearing the workspace-----------


library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)




setwd("target_dir/")

file_list <- list.files()

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=TRUE, sep="\t")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.table(file, header=TRUE, sep="\t")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}



