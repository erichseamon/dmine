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




palouse_sumloss <- read.csv("/waf/agmesh-scenarios/Allstates/summaries/Palouse_summary_sumloss.csv")
palouse_counts <- read.csv("/waf/agmesh-scenarios/Allstates/summaries/Palouse_summary_counts.csv")
palouse_sumloss <- aggregate(loss ~ year + damagecause + county,  palouse_sumloss, sum)
palouse_counts <- aggregate(count ~ year + damagecause + county,  palouse_counts, sum)


Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

palouse_sumloss$cube_loss <- Math.cbrt(palouse_sumloss$loss)
palouse_counts$cube_counts <- Math.cbrt(palouse_counts$count)


palouse_sumloss$county = factor(palouse_sumloss$county,
                      levels=unique(palouse_sumloss$county))

interaction.plot(x.factor     = palouse_sumloss$year,
                 trace.factor = palouse_sumloss$damagecause, 
                 response     = palouse_sumloss$cube_loss, 
                 fun = mean,
                 las = 2,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

interaction.plot(x.factor     = palouse_counts$year,
                 trace.factor = palouse_counts$county, 
                 response     = palouse_counts$count, 
                 fun = mean,
                 las = 2,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")



fit <- aov(cube_counts~damagecause, data=palouse_counts)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

plot(fit)





var1 <- read.csv("/waf/tmp/pr_jun2_cube_root_acres_climatecorrelation.csv")
colnames(var1)[9] <- paste(colnames(var1)[2], "_zscore", sep="")


var2 <- read.csv("/waf/tmp/pet_jun2_cube_root_acres_climatecorrelation.csv")
colnames(var2)[9] <- paste(colnames(var2)[2], "_zscore", sep="")
var3 <- read.csv("/waf/tmp/tmmx_jun1_cube_root_acres_climatecorrelation.csv")
colnames(var3)[9] <- paste(colnames(var3)[2], "_zscore", sep="")



data1 <- cbind(var1, var2[9], var3[9])



cor2 <- cor(data1) 
pairs(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore, data = data1,
      lower.panel=panel.smooth, upper.panel=panel.cor, main = "initial pairs plot")

#--end

#--aov test
fit <- aov(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore, data = data1)

layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots

summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests



#--Two-way Interaction Plot 

#attach(mtcars)
county <- factor(data1$county)
year <- factor(data1$year)
interaction.plot(year, county, data1$loss, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(1:25),	
                 xlab=" ", 
                 ylab="Mean Miles Per Gallon", 
                 main="Interaction Plot", las = 2)

# Make big tree
form <- as.formula(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore + fm100_zscore)
tree.1 <- rpart(form,data=data1,control=rpart.control(minsplit=30,cp=0))
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
tree.2 <- rpart(form,data1)			# A more reasonable tree
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
fit <- rpart(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore + fm100_zscore, 
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
rfit <- randomForest(loss_zscore ~ pr_zscore + tmmx_zscore + pet_zscore + fm100_zscore,  data=data1)
print(rfit) # view results 
importance(rfit) # importance of each predictor

#--count data

count(train, 'damagecause')

regre <- mgcv::gam(pet_zscore ~ pr_zscore + tmmx_zscore, data=data1)
VIF1 <- (1/(1-.89))

