library(RCurl)
library(jsonlite) 

file_ID <- fromJSON("http://quickstats.nass.usda.gov/api/api_GET/?key=F9BAFEBF-FCA8-384A-8204-5DB45FBCDBDD&commodity_desc=WHEAT&year__GE=1994&state_alpha=ID&format=JSON")

file_WA <- fromJSON("http://quickstats.nass.usda.gov/api/api_GET/?key=F9BAFEBF-FCA8-384A-8204-5DB45FBCDBDD&commodity_desc=WHEAT&year__GE=1989&state_alpha=WA&format=JSON")

file_OR <- fromJSON("http://quickstats.nass.usda.gov/api/api_GET/?key=F9BAFEBF-FCA8-384A-8204-5DB45FBCDBDD&commodity_desc=WHEAT&year__GE=1990&state_alpha=OR&format=JSON")

setwd("/dmine/data/NASS/")

file_frame_WA <- as.data.frame(file_WA)
file_frame_ID <- as.data.frame(file_ID)
file_frame_OR <- as.data.frame(file_OR)

xx <- subset(file_frame_OR, data.year == 2000)
xxx <- subset(xx, data.county_name == "GRANT")

write.table(file_WA, file = "/dmine/data/NASS/NASS_WA_1989_2015")
write.table(file_ID, file = "/dmine/data/NASS/NASS_ID_1989_2015")
write.table(file_OR, file = "/dmine/data/NASS/NASS_OR_1989_2015")


#-------------------

fileWA <- read.table("/dmine/data/NASS/NASS_Washington_1989_2015")
fileOR <- read.table("/dmine/data/NASS/NASS_Oregon_1989_2015")
fileID <- read.table("/dmine/data/NASS/NASS_Idaho_1989_2015")

file_frameWA <- as.data.frame(fileWA)
file_nameWA <- subset(file_frameWA, file_frameWA$data.Value != "NA")
file_nameWA <- subset(file_nameWA, file_frameWA$data.county_name != "")
file_nameWA <- subset(file_nameWA, file_frameWA$data.county_name != "OTHER (COMBINED) COUNTIES")
file_nameWA <- subset(file_nameWA, file_nameWA$data.source_desc == "SURVEY")
file_nameWA <- subset(file_nameWA, file_nameWA$data.statisticcat_desc == "AREA HARVESTED")
#file_name2 <- subset(file_name2, file_name2$data.commodity_desc == "WHEAT")
#file_nameWA <- subset(file_nameWA, file_nameWA$data.county_name == "WHITMAN")

file_nameWA$data.Value <- as.numeric(file_nameWA$data.Value)
file_nameWAagg <-  aggregate(data.Value ~ data.year + data.commodity_desc + data.county_name, file_nameWA, mean)


file_frameID <- as.data.frame(fileID)
file_nameID <- subset(file_frameID, file_frameID$data.Value != "NA")
file_nameID <- subset(file_nameID, file_frameID$data.county_name != "")
file_nameID <- subset(file_nameID, file_frameID$data.county_name != "OTHER (COMBINED) COUNTIES")
file_nameID <- subset(file_nameID, file_nameID$data.source_desc == "SURVEY")
file_nameID <- subset(file_nameID, file_nameID$data.statisticcat_desc == "AREA HARVESTED")

file_nameID$data.Value <- as.numeric(file_nameID$data.Value)
file_nameIDagg <-  aggregate(data.Value ~ data.year + data.commodity_desc + data.county_name, file_nameID, mean)


file_frameOR <- as.data.frame(fileOR)
file_nameOR <- subset(file_frameOR, file_frameOR$data.Value != "NA")
file_nameOR <- subset(file_nameOR, file_frameOR$data.county_name != "")
file_nameOR <- subset(file_nameOR, file_frameOR$data.county_name != "OTHER (COMBINED) COUNTIES")
file_nameOR <- subset(file_nameOR, file_nameOR$data.source_desc == "SURVEY")
file_nameOR <- subset(file_nameOR, file_nameOR$data.statisticcat_desc == "AREA HARVESTED")

file_nameOR$data.Value <- as.numeric(file_nameOR$data.Value)
file_nameORagg <-  aggregate(data.Value ~ data.year + data.commodity_desc + data.county_name, file_nameOR, mean)


#-------

file_name2 <- rbind(file_nameOR, file_nameID, file_nameWA)
file_name2$data.Value <- as.numeric(file_name2$data.Value)

file_name3 <-  aggregate(data.Value ~ data.year + data.commodity_desc + data.county_name + data.state_name, file_name2, sum)
file_name4 <-  aggregate(data.Value ~ data.year + data.commodity_desc + data.county_name + data.state_name, file_name2, length)
file_name5 <-  aggregate(data.Value ~ data.year + data.commodity_desc + data.county_name + data.state_name, file_name2, mean)

file_name5 <- cbind(file_name3, file_name4$data.Value, file_name5$data.Value)
colnames(file_name5) <- c("year", "commodity", "county", "state", "totalharvested", "harvestedcount", "meanharvested")


file_name6 <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/")



file_name2$data.Value <- (gsub(",","", file_name2[,'data.Value']))

file_name5 <- subset(file_name5, totalharvested > 0)
file_name5 <- as.data.frame(file_name5)


x <- as.numeric(file_name5$data.Value)
y <- file_name5$data.year
boxplot(x~y, xlab="Year", ylab="Age", main="Boxplots of Age by Race", las=3)

