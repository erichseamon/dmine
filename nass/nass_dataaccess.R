library(RCurl)
library(jsonlite)

file <- fromJSON("http://quickstats.nass.usda.gov/api/api_GET/?key=F9BAFEBF-FCA8-384A-8204-5DB45FBCDBDD&commodity_desc=WHEAT&year__GE=1989&state_alpha=WA&format=JSON")
file_frame <- as.data.frame(file)
file_name2 <- subset(file_frame, file_frame$data.Value != "NA")
#file_name2 <- subset(file_name2, file_name2$data.source_desc == "CENSUS")
file_name2 <- subset(file_name2, file_name2$data.statisticcat_desc == "SALES")

file_name2$data.Value <- (gsub(",","", file_name2[,'data.Value']))
file_name5 <- subset(file_name2, data.Value > 0)
file_name5 <- as.data.frame(file_name5)
x <- as.numeric(file_name5$data.Value)
y <- file_name5$data.year
boxplot(x~y, xlab="Race", ylab="Age", main="Boxplots of Age by Race")
