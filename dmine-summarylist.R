setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries4/")
files  <- as.data.frame(list.files(pattern = "Idaho"))
colnames(files) <- "summary"
files$url <- "http://dmine.io/waf/agmesh-scenarios/Idaho/summaries4/"

setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries3/")
files2  <- as.data.frame(list.files(pattern = "Idaho"))
colnames(files2) <- "summary"
files2$url <- "http://dmine.io/waf/agmesh-scenarios/Idaho/summaries3/"

setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/summaries4/")
files3  <- as.data.frame(list.files(pattern = "Oregon"))
colnames(files3) <- "summary"
files3$url <- "http://dmine.io/waf/agmesh-scenarios/Oregon/summaries4/"

setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/summaries3/")
files4  <- as.data.frame(list.files(pattern = "Oregon"))
colnames(files4) <- "summary"
files4$url <- "http://dmine.io/waf/agmesh-scenarios/Oregon/summaries3/"

setwd("/dmine/data/USDA/agmesh-scenarios/Washington/summaries4/")
files5  <- as.data.frame(list.files(pattern = "Washington"))
colnames(files5) <- "summary"
files5$url <- "http://dmine.io/waf/agmesh-scenarios/Washington/summaries4/"

setwd("/dmine/data/USDA/agmesh-scenarios/Washington/summaries3/")
files6  <- as.data.frame(list.files(pattern = "Washington"))
colnames(files6) <- "summary"
files6$url <- paste("http://dmine.io/waf/agmesh-scenarios/Washington/summaries3/", files6$summary, sep="")


files7 <- rbind(files, files2, files3, files4, files5, files6)

#--

setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/month_1989_2015_county_nosum/")
files8  <- list.files(pattern = "csv")
files8 <- files8[sapply(files8, file.size) > 600]
files9 <- as.data.frame(do.call(rbind, strsplit(files8, '\\.')))
files10 <- files9[-4]
files10$url <- paste("http://dmine.io/waf/agmesh-scenarios/Idaho/month_1989_2015_county_nosum/", files8, sep="")
files10$state <- "Idaho"
files10$month <- month.abb[files10$V2]

files10$filename <- paste("Idaho Agricultural Monthly Commodity Loss, ", files10$V3, ", ", files10$V1, ", ", files10$month, sep="")


setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/month_1989_2015_county_nosum/")
files11  <- list.files(pattern = "csv")
files11 <- files11[sapply(files11, file.size) > 600]
files12 <- as.data.frame(do.call(rbind, strsplit(files11, '\\.')))
files13 <- files12[-4]
files13$url <- paste("http://dmine.io/waf/agmesh-scenarios/Oregon/month_1989_2015_county_nosum/", files11, sep="")
files13$state <- "Oregon"
files13$month <- month.abb[files13$V2]

files13$filename <- paste("Oregon Agricultural Monthly Commodity Loss, ", files13$V3, ", ", files13$V1, ", ", files13$month, sep="")


setwd("/dmine/data/USDA/agmesh-scenarios/Washington/month_1989_2015_county_nosum/")
files14  <- list.files(pattern = "csv")
files14 <- files14[sapply(files14, file.size) > 600]
files15 <- as.data.frame(do.call(rbind, strsplit(files14, '\\.')))
files16 <- files15[-4]
files16$url <- paste("http://dmine.io/waf/agmesh-scenarios/Washington/month_1989_2015_county_nosum/", files14, sep="")
files16$state <- "Washington"
files16$month <- month.abb[files16$V2]

files16$filename <- paste("Washington Agricultural Monthly Commodity Loss, ", files16$V3, ", ", files16$V1, ", ", files16$month, sep="")

files17 <- rbind(files10, files13, files16)

colnames(files17)[1] <- "year"
colnames(files17)[2] <- "monthcode"
colnames(files17)[3] <- "commodity"

setwd("/dmine/data/USDA/agmesh-scenarios/")
write.csv(files17, file = "summarylist.csv")

#----
 



read.table(text = text, sep = ".", colClasses = "character")

