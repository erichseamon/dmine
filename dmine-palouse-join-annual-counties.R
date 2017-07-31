#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", scen, "/summaries", sep=""))
scen_state <- "Idaho"
setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries")
files_ID  <- list.files(pattern = "Idaho|_WHEAT_Drought")

tables <- lapply(files_ID, read.csv, header = TRUE, strip.white = TRUE)
tables_ID <- do.call(rbind , tables)
colnames(tables_ID)[38] <- "commodity"
tables_ID$state <- "Idaho"

files_ID <- as.data.frame(files_ID)
colnames(files_ID) <- c("list")







scen_state <- "Oregon"
setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries")
files_OR  <- list.files(pattern = "Oregon|_WHEAT_Drought")

tables <- lapply(files_OR, read.csv, header = TRUE, strip.white = TRUE)
tables_OR <- do.call(rbind , tables)
colnames(tables_OR)[38] <- "commodity"
tables_OR$state <- "Oregon"

files_OR <- as.data.frame(files_OR)
colnames(files_OR) <- c("list")








colnames(files_OR) <- c("list")

scen_state <- "Washington"
setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries")
files_WA  <- list.files(pattern = "Washington|_WHEAT_Drought")

tables <- lapply(files_WA, read.csv, header = TRUE, strip.white = TRUE)
tables_WA <- do.call(rbind , tables)
colnames(tables_WA)[38] <- "commodity"
tables_WA$state <- "Oregon"

files_WA <- as.data.frame(files_WA)
colnames(files_WA) <- c("list")

tables_all <- rbind(tables_WA, tables_OR, tables_ID)

