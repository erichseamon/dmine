







library(rpart)	       # Popular decision tree algorithm
library(rattle)	# Fancy tree plot
library(rpart.plot)	# Enhanced tree plots
library(RColorBrewer)	# Color selection for fancy tree plot
library(party)	# Alternative decision tree algorithm
library(partykit)	# Convert rpart object to BinaryTree
library(caret)	# Just a data source for this script
# but probably one of the best R packages ever. 
data(segmentationData)	# Get some data
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
uniquez <<- read.csv(paste("/dmine/data/USDA/agmesh-scenarios/", "palouse", "/summaries3/", "1989-2015_combined_revised.csv", sep=""), strip.white=TRUE)
uniquez <- data.frame(uniquez)

#dc <- unique(uniquez$damagecause)
#com <- unique(uniquez$commodity)

for (i in dc) {
  for (k in com) {
    uniquez <- subset(uniquez, county == "Whitman")
    train <- subset(uniquez, commodity == "WHEAT")
    train <- subset(train, damagecause == "Heat")
    
  }
  
}


cols <- train[]
data <- cbind(train[2:30])

x1 <- train[31]
x2 <-  train[33]
x3 <-  train[34]
#--feature extraction



#---cor panel example 1

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

#p1 <- na.omit(log(data))
#p2 <- subset(p1, pr != "-Inf")

data1 <- cbind(data, x1)
data2 <- cbind(data, x2)
data3 <- cbind(data, x3)

refined1 <- data[2:30,]

data1log <- log(data1)

cor2 <- cor(data1) 
pairs(countratio ~ pr + pet + pdsi + tmmx + erc + soil_moisture_shorterm + soil_moisture_longterm, data = data3,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "1989-2015 WHEAT Heat, Whitman County, WA - Countratio")

#--end

#--aov test
fit <- aov(loss ~  + pr + pet + pdsi + tmmx + erc + soil_moisture_shorterm + soil_moisture_longterm, data=data1log)

layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots

summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests

summary(fit_acres)
drop1(fit_acres,~.,test="F") # type III SS and F Tests

#--Two-way Interaction Plot 

attach(mtcars)
gears <- factor(gears)
cyl <- factor(cyl)
interaction.plot(cyl, gear, mpg, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Number of Cylinders", 
                 ylab="Mean Miles Per Gallon", 
                 main="Interaction Plot")

# Make big tree
form <- as.formula(loss ~ pr + pet + pdsi + tmmx + erc + soil_moisture_shorterm + soil_moisture_longterm)
tree.1 <- rpart(form,data=data1,control=rpart.control(minsplit=30,cp=0))
# 
plot(tree.1)	# Will make a mess of the plot
text(tree.1, cex = .5)
# 
prp(tree.1)	# Will plot the tree
prp(tree.1,varlen=3)	# Shorten variable names

# Interatively prune the tree
new.tree.1 <- prp(tree.1,snip=TRUE)$obj # interactively trim the tree
prp(new.tree.1) # display the new tree
#
#-------------------------------------------------------------------
tree.2 <- rpart(form,data1)	# A more reasonable tree
prp(tree.2)                                     # A fast plot
fancyRpartPlot(tree.1, )
rpart.plot(tree.2)






# A fancy plot from rattle
#
#-------------------------------------------------------------------
#sends to json for use with d3
jsontree <- json_prsr(tree.1)
setwd(paste("/dmine/data/USDA/agmesh-scenarios/palouse/d3_tree_1/", sep=""))
jsontree2 <- gsub("'", '"', jsontree)
write(jsontree2, file="palousetree.JSON")



#------additional tree
# Regression Tree Example
library(rpart)

# grow tree 
fit <- rpart(loss ~ + tmmx + tmmn + srad + sph + erc + fm1000 + pdsi + pet + rmin + rmax, 
             method="anova", data=data1)

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
rfit <- randomForest(count ~ + tmmx + tmmn + srad + sph + erc + fm1000 + pdsi + pet + rmin + rmax + soil_moisture_shorterm,   data=data2)
print(rfit) # view results 
importance(rfit) # importance of each predictor

#--count data

count(train, 'damagecause')