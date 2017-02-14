library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
# but probably one of the best R packages ever. 
data(segmentationData)				# Get some data
data <- segmentationData[,-c(1,2)]
library(maptools)
#------

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp', 
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state, counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% "Idaho")
#monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", input$state, sep="")
#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
#maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/netcdf/pdsi_apr_", input$year, ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

#setwd(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month_positive/", sep=""))
#system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- read.csv(paste("/dmine/data/USDA/agmesh-scenarios/", "palouse", "/summary/", "2001_2015_palouse_summary", sep=""), strip.white=TRUE)
uniquez <- data.frame(uniquez)

train <- subset(uniquez, commodity == "WHEAT")

#--feature extraction

spring <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN")
winter <- c("JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

springdata <- subset(train, month %in% spring)
winterdata <- subset(train, month %in% winter)
#data <- subset(data, county %in% "Latah")




setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries/annual_county_summaries/")
myfiles_allstates <- read.csv("WHEAT_drought_summary")
attach(myfiles_allstates)



# Make big tree
form <- as.formula(loss ~ tmmx + sph + vs + pr + pdsi + pet + erc)
tree.1 <- rpart(form,data=myfiles_allstates,control=rpart.control(minsplit=20,cp=0))
# 
plot(tree.1)					# Will make a mess of the plot
text(tree.1, cex = .5)
# 
prp(tree.1)					# Will plot the tree
prp(tree.1,varlen=3)				# Shorten variable names

# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1) # display the new tree
#
#-------------------------------------------------------------------
tree.2 <- rpart(form,myfiles_allstates)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.1, )	
rpart.plot(tree.2)

rpart.plot(tree.1, # middle graph
           extra="auto", box.palette="GnBu",
           branch.lty=5, shadow.col="gray", nn=TRUE)




# A fancy plot from rattle
#
#-------------------------------------------------------------------
#sends to json for use with d3
jsontree <- json_prsr(tree.1)
setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/d3_tree_1/", sep=""))
jsontree2 <- gsub("'", '"', jsontree)
write(jsontree2, file="palousetree2.JSON")



#------additional tree
# Regression Tree Example
library(rpart)

# grow tree 
fit <- rpart(loss ~ acres + tmmx + tmmn + srad + sph + vs + fm100 + erc + fm1000 + pdsi + pet + rmin + rmax, 
             method="anova", data=springdata)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree palouse spring wheat ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
     title = "Regression Tree palouse spring wheat ")


#---random forest

library(randomForest)
rfit <- randomForest(loss ~ acres + tmmx + tmmn + srad + sph + vs + fm100 + erc + fm1000 + pdsi + pet + rmin + rmax,   data=springdata)
print(rfit) # view results 
importance(rfit) # importance of each predictor

