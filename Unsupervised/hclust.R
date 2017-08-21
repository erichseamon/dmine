


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
