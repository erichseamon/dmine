library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)					# Just a data source for this script
library(mvnormtest)
# but probably one of the best R packages ever. 
data(segmentationData)				# Get some data
data <- segmentationData[,-c(1,2)]
library(maptools)
#------


#-Loading all commodities for the palouse 1989 - 2015

palouse_sumloss_allcomm <- read.csv("/waf/agmesh-scenarios/Allstates/summaries/palouse_summary_all.csv")
palouse_sumloss_allcomm2  <- aggregate(loss ~ year + damagecause + county + commodity,  palouse_sumloss_allcomm, sum)
palouse_count_allcomm2  <- aggregate(count ~ year + damagecause + county + commodity,  palouse_sumloss_allcomm, sum)

#-Loading all WHEAT claims for the palouse from 1989-2015

palouse_sumloss <- read.csv("/waf/agmesh-scenarios/Allstates/summaries/Palouse_summary_sumloss.csv")
palouse_counts <- read.csv("/waf/agmesh-scenarios/Allstates/summaries/Palouse_summary_counts.csv")
palouse_sumloss <- aggregate(loss ~ year + damagecause + county,  palouse_sumloss, sum)
palouse_counts <- aggregate(count ~ year + damagecause + county,  palouse_counts, sum)


#-is there a normal signal using just wheat, drought claims across all of the pacific northwest

palouse_sumloss_drought <- subset(palouse_sumloss, damagecause == "Drought")
qqnorm(palouse_sumloss_drought$cube_loss)


#use a cube transformation on loss for WHEAT claims

Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

#palouse_sumloss2 <- subset(palouse_sumloss, loss > 0)

palouse_sumloss$cube_loss <- Math.cbrt(palouse_sumloss$loss)
palouse_counts$cube_counts <- Math.cbrt(palouse_counts$count)

#use a cube transformation on loss for all commodity claims

palouse_sumloss_allcomm2$cube_loss <- Math.cbrt(palouse_sumloss_allcomm2$loss)

#-use a log transform on the same WHEAT claims data

palouse_sumloss$log_loss <- log(which(!is.na(palouse_sumloss$loss)))

# - plot some qqplots to see how normal the data is

qqnorm(palouse_sumloss$loss)
qqnorm(palouse_sumloss$cube_loss)
qqnorm(palouse_sumloss$log_loss)
qqnorm(palouse_sumloss_allcomm2$cube_loss)
qqnorm(palouse_counts$count)

#-factor counties
palouse_sumloss$county = factor(palouse_sumloss$county,
                                levels=unique(palouse_sumloss$county))

#-factor years
palouse_sumloss$year = factor(palouse_sumloss$year,
                                levels=unique(palouse_sumloss$year))


#-plot basic interaction plots for WHEAT cube root loss using year as x and damagecause as the line

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

#-interaction plot with WHEAT Counts vs year as x and county as line

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

#---Bartlett Test - homogeneity of variances

bartlett.test(palouse_sumloss_allcomm2$loss, palouse_sumloss_allcomm2$county)

bartlett.test(palouse_sumloss$loss, palouse_sumloss$county)

bartlett.test(palouse_sumloss$loss, palouse_sumloss$year)

qchisq(0.95, 25)

#--All Bartlett tests show that the variances are not homogeneous

#--Fligner-Killeen test for homoskedasticity

fligner.test(palouse_sumloss_allcomm2$loss, palouse_sumloss_allcomm2$year)





#------

#AOV

#-is there any significant difference in loss betwen different damage causes
#-are the variations between the damage cause means due to true differences about 
#-the populations means or just due to sampling variability?
#-Ftest compares variation of sample means among damage causes to the variation between damage causes (within).


#--performing an single interaction aov for loss transformed by a cube root function
#--by damagecuase.  This is for ALL commodities


fit <- aov(cube_loss~damagecause, data=palouse_sumloss_allcomm2)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

#plot(fit)

library(broom)
library(magrittr)

#--ad hoc test

tuk <- TukeyHSD(fit)

psig=as.numeric(apply(tuk$`damagecause`[,2:3],1,prod)>=0)+1
plot(tuk,col=psig,yaxt="n")

tuk2 <- as.data.frame(tidy(tuk))
subset(tuk2, adj.p.value < .05)

#-listing combos where the mean differences in damage causes are significant
#Damage causes coming to forefront include: Drought and Decline in Price 

tuk3 <- subset(tuk2, adj.p.value < .05)

tuk3


#---------



#--performing an single interaction aov for loss transformed by a cube root function
#--by damagecuase.  This is for ONLY WHEAT loss

fit <- aov(cube_loss~damagecause, data=palouse_sumloss)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

#plot(fit)

library(broom)
library(magrittr)
#--ad hoc test

tukk <- TukeyHSD(fit)

psig=as.numeric(apply(tukk$`damagecause`[,2:3],1,prod)>=0)+1
plot(tukk,col=psig,yaxt="n")

tukk2 <- as.data.frame(tidy(tukk))
subset(tukk2, adj.p.value < .05)

#-listing combos where the mean differences in damage causes are significant
#Damage causes coming to forefront include: Drought and Decline in Price 
tukk3 <- subset(tukk2, adj.p.value < .05)

tukk3

#---------


#--performing an single interaction aov for loss transformed by a cube root function
#--by damagecuase.  This is for ONLY WHEAT counts

fit <- aov(count~damagecause, data=palouse_counts)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

#plot(fit)

library(broom)
library(magrittr)
#--ad hoc test

tukk <- TukeyHSD(fit)

psig=as.numeric(apply(tukk$`damagecause`[,2:3],1,prod)>=0)+1
plot(tukk,col=psig,yaxt="n")

tukk2 <- as.data.frame(tidy(tukk))
subset(tukk2, adj.p.value < .05)

#-listing combos where the mean differences in damage causes are significant
#Damage causes coming to forefront include: Drought and Decline in Price 
tukk3 <- subset(tukk2, adj.p.value < .05)

tukk3


#----

#--performing an single interaction aov for loss transformed by a cube root function
#--by commodity  This is for ALL commodities


fit <- aov(cube_loss~commodity, data=palouse_sumloss_allcomm2)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

#plot(fit)

library(broom)
library(magrittr)
#--ad hoc test

tukk <- TukeyHSD(fit)

psig=as.numeric(apply(tukk$`commodity`[,2:3],1,prod)>=0)+1
plot(tukk,col=psig,yaxt="n")

tukk2 <- as.data.frame(tidy(tukk))
subset(tukk2, adj.p.value < .05)

#-listing combos where the mean differences in damage causes are significant
#Damage causes coming to forefront include: Drought and Decline in Price 
tukk3 <- subset(tukk2, adj.p.value < .05)

tukk3


#----

#--performing an single interaction aov for loss transformed by a cube root function
#--by year  This is for ALL commodities


fit <- aov(loss~county, data=palouse_sumloss_allcomm2)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

#plot(fit)

library(broom)
library(magrittr)
#--ad hoc test

tukk <- TukeyHSD(fit)

psig=as.numeric(apply(tukk$`county`[,2:3],1,prod)>=0)+1
plot(tukk,col=psig,yaxt="n")

tukk2 <- as.data.frame(tidy(tukk))
subset(tukk2, adj.p.value < .05)

#-listing combos where the mean differences in damage causes are significant
#Damage causes coming to forefront include: Drought and Decline in Price 
tukk3 <- subset(tukk2, adj.p.value < .05)

tukk3



#---------

fit_c = lm(formula = palouse_sumloss_allcomm2$loss ~ palouse_sumloss_allcomm2$county)
anova(fit_c)
fit_d = lm(formula = palouse_sumloss_allcomm2$loss ~ palouse_sumloss_allcomm2$damagecause)
anova(fit_d)
fit_co = lm(formula = palouse_sumloss_allcomm2$loss ~ palouse_sumloss_allcomm2$commodity)
anova(fit_co)
fit_y = lm(formula = palouse_sumloss_allcomm2$loss ~ factor(palouse_sumloss_allcomm2$year))
anova(fit_y)

#---If F for test is above tabulated F - reject hypothesis.  Group means are not statistically equal

qf(0.950, 6692, 30)


all1 <- anova(lm(loss ~ county + damagecause + year, data = palouse_sumloss))

all_lm1 <- lm(loss ~ county + damagecause + year, data = palouse_sumloss)

palouse_sumloss_allcomm2$year <- factor(palouse_sumloss_allcomm2$year)

all2 <- anova(lm(loss ~ commodity + county + damagecause + year + commodity:damagecause, data = palouse_sumloss_allcomm2))

all_lm2 <- lm(loss ~ commodity + county + damagecause + year + commodity:damagecause, data = palouse_sumloss_allcomm2)

#--ad hoc tukey for multiple 
#-Post-hoc testing with lsmeans
#-Because the main effects were significant, we will want to perform post-hoc mean separation tests 
#-for each main effect factor variable.

library(lsmeans)

lsmeans(all_lm1,
        pairwise ~ county, 
        adjust="tukey")  


#---
#--all commodities in Palouse
plot(cube_loss ~ county + commodity + damagecause + year, data=palouse_sumloss_allcomm2)

plot(cube_loss ~ county + damagecause + year, data=palouse_sumloss)


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
