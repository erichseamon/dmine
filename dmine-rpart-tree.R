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

data <- subset(train, month %in% spring)

#data <- subset(data, county %in% "Latah")









# Make big tree
form <- as.formula(loss ~ tmmx + tmmn + srad + sph + vs + fm100)
tree.1 <- rpart(form,data=data,control=rpart.control(minsplit=20,cp=0))
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
tree.2 <- rpart(form,data)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.1, )				# A fancy plot from rattle
#
#-------------------------------------------------------------------
jsontree <- json_prsr(tree.1)
setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/d3_tree_1/", sep=""))
jsontree2 <- gsub("'", '"', jsontree)
write(jsontree2, file="palousetree.JSON")

