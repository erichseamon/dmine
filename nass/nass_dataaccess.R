library(RCurl)
library(jsonlite) 

file_ID <- fromJSON("http://quickstats.nass.usda.gov/api/api_GET/?key=F9BAFEBF-FCA8-384A-8204-5DB45FBCDBDD&commodity_desc=WHEAT&year__GE=1994&state_alpha=ID&format=JSON")

file_WA <- fromJSON("http://quickstats.nass.usda.gov/api/api_GET/?key=F9BAFEBF-FCA8-384A-8204-5DB45FBCDBDD&commodity_desc=WHEAT&year__GE=1989&state_alpha=WA&format=JSON")

file_OR <- fromJSON("http://quickstats.nass.usda.gov/api/api_GET/?key=F9BAFEBF-FCA8-384A-8204-5DB45FBCDBDD&commodity_desc=WHEAT&year__GE=1990&state_alpha=OR&format=JSON")

setwd("/dmine/data/NASS/")

file_frame_WA <- as.data.frame(file_WA)
file_frame_ID <- as.data.frame(file_ID)
file_frame_OR <- as.data.frame(file_OR)

write.table(file_WA, file = "/dmine/data/NASS/NASS_WA_1989_2015")
write.table(file_ID, file = "/dmine/data/NASS/NASS_ID_1989_2015")
write.table(file_OR, file = "/dmine/data/NASS/NASS_OR_1989_2015")


#-------------------




file <- read.table("/dmine/data/NASS/NASS_Washington_1989_2015")

file_frame <- as.data.frame(file)
file_name2 <- subset(file_frame, file_frame$data.Value != "NA")
file_name2 <- subset(file_name2, file_frame$data.county_name != "")
file_name2 <- subset(file_name2, file_frame$data.county_name != "OTHER (COMBINED) COUNTIES")
file_name2 <- subset(file_name2, file_name2$data.source_desc == "SURVEY")
file_name2 <- subset(file_name2, file_name2$data.statisticcat_desc == "YIELD")
file_name2 <- subset(file_name2, file_name2$data.county_name == "WHITMAN")


file_name2$data.Value <- (gsub(",","", file_name2[,'data.Value']))
file_name5 <- subset(file_name2, data.Value > 0)
file_name5 <- as.data.frame(file_name5)


x <- as.numeric(file_name5$data.Value)
y <- file_name5$data.year
boxplot(x~y, xlab="Year", ylab="Age", main="Boxplots of Age by Race", las=3)
