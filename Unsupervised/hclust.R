
library(plyr)

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
dirname <- "/dmine/data/USDA/agmesh-scenarios/Allstates/"
files <- list.files(dirname, pattern = 'summary')
tables <- lapply(files, read.csv, header=TRUE)
combined2.df <- do.call(rbind, tables)
names(combined2.df) <- c("X","year","statecode","state","countycode","county","commoditycode","commodity","insuranceplancode","insurancename","stagecode","damagecausecode","damagecause","monthcode","month","loss","acres")

write.csv(combined.df, file = "Allstates_claims_loss_summary.csv")


setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/summaries4")
dirname <- "/dmine/data/USDA/agmesh-scenarios/Oregon/summaries4"
files <- list.files(dirname, pattern = 'summary')
tables <- lapply(files, read.csv, header=TRUE)
combined_OR1.df <- do.call(rbind, tables)

setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/summaries3")
dirname <- "/dmine/data/USDA/agmesh-scenarios/Oregon/summaries3"
files <- list.files(dirname, pattern = 'summary')
tables <- lapply(files, read.csv, header=TRUE)
combined_OR2.df <- do.call(rbind, tables)





setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries4")
dirname <- "/dmine/data/USDA/agmesh-scenarios/Idaho/summaries4"
files <- list.files(dirname, pattern = 'summary')
tables <- lapply(files, read.csv, header=TRUE)
combined_ID1.df <- do.call(rbind, tables)

setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries3")
dirname <- "/dmine/data/USDA/agmesh-scenarios/Idaho/summaries3"
files <- list.files(dirname, pattern = '_summary')
tables <- lapply(files, read.csv, header=TRUE)
combined_ID2.df <- do.call(rbind, tables)

setwd("/dmine/data/USDA/agmesh-scenarios/Washington/summaries4")
dirname <- "/dmine/data/USDA/agmesh-scenarios/Washington/summaries4"
files <- list.files(dirname, pattern = 'summary')
tables <- lapply(files, read.csv, header=TRUE)
combined_WA1.df <- do.call(rbind, tables)


setwd("/dmine/data/USDA/agmesh-scenarios/Washington/summaries3")
dirname <- "/dmine/data/USDA/agmesh-scenarios/Washington/summaries3"
files <- list.files(dirname, pattern = 'summary')
tables <- lapply(files, read.csv, header=TRUE)
combined_WA2.df <- do.call(rbind, tables)

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
x <- rbind(combined_WA1.df, combined_WA2.df, combined_ID1.df, combined_ID2.df, combined_OR1.df, combined_OR2.df)

unique(x$countyfips)

xx <- subset(x, year == 1990)

xx$month <- trimws(xx$month)
xxx <- subset(xx, month == "jan")


write.csv(x, file = "Allstates_climate_summary.csv")

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/monthly-summaries")
dirname <- "/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/monthly-summaries/"
files <- list.files(dirname, pattern = 'Monthly_loss', full.names=TRUE)
tables <- lapply(files, read.csv, header=TRUE)
combined_Allstates_monthlyloss.df <- do.call(rbind, tables)

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/monthly-summaries")
dirname <- "/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/monthly-summaries/"
files <- list.files(dirname, pattern = 'Monthly_count', full.names=TRUE)
tables <- lapply(files, read.csv, header=TRUE)
combined_Allstates_monthlycount.df <- do.call(rbind, tables)

setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/monthly-summaries")
dirname <- "/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/monthly-summaries/"
files <- list.files(dirname, pattern = 'Monthly_mean', full.names=TRUE)
tables <- lapply(files, read.csv, header=TRUE)
combined_Allstates_monthlymean.df <- do.call(rbind, tables)

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/")
write.csv(combined_Allstates_monthlycount.df, file = "Palouse_summary_counts.csv")
write.csv(combined_Allstates_monthlymean.df, file = "Palouse_summary_meanloss.csv")

write.csv(combined_Allstates_monthlyloss.df, file = "Palouse_summary_sumloss.csv")



setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/")
new <- read.csv("1989-2015_combined_revised.csv")

new2 <- subset(new, damagecause == "Drought" || damagecause == "Heat")

clusters <- hclust(dist(new[, 2:14]), method = 'average')
plot(clusters)
clusterCut <- cutree(clusters, 5)
table(clusterCut, new$commodity)


ggplot(new, aes(pr, tmmx, pdsi, color = new$commodity)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut)


# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(new[,2:14], method.hclust="ward.D",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)


# Model Based Clustering
library(mclust)
fit <- Mclust(new[,2:14])
plot(fit) # plot results 
summary(fit) # display the best model



# K-Means Clustering with 5 clusters
fit <- kmeans(new[,2:14], 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(new[,2:14], fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(new[,2:14], fit$cluster)
