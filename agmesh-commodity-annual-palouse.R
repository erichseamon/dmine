library(DAAG)
library(ridge)


setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries/annual_county_summaries/")
files <- list.files(pattern = "\\_WHEAT_drought$")
myfiles = do.call(rbind, lapply(files, function(x) 
  read.csv(x, stringsAsFactors = FALSE)))

#names(myfiles)[19] <- c("year") 
myfiles$prpet <- (myfiles$pr - myfiles$pet)
write.csv(myfiles, file = "WHEAT_drought_summary")
myfiles_allyears <- subset(myfiles, , c(pr, pdsi, pet, tmmx, prpet, erc, countratio, loss, acres, count, county, year))

myfiles_allyears$county <- factor(myfiles_allyears$county)
myfiles_allyears$year <- factor(myfiles_allyears$year)
myfiles_allyears$loss <- scale(myfiles_allyears$loss, center = TRUE, scale = FALSE)
myfiles_allyears[1:7] <- scale(myfiles_allyears[1:7], center = TRUE, scale = TRUE)

#--allyears pairwise plot

#--countratio

pairs(myfiles_allyears[c(1,2,3,4,5,6,8)], lower.panel=panel.smooth, upper.panel=panel.cor)
dev.off()

#-loss

dev.off()

pairs(myfiles_allyears[c(1,2,3,4,5,6,7)], lower.panel=panel.smooth, upper.panel=panel.cor)
dev.off()

#---only 2008
myfiles_2008 <- subset(data.frame(myfiles_allyears), year == "2008")
#---only 2009
myfiles_2009 <- subset(data.frame(myfiles_allyears), year == "2009")
#--only 2015
myfiles_2015 <- subset(data.frame(myfiles_allyears), year == "2015")

#--some linear models
dev.off()
plot(lm(loss ~ pr+pdsi+prpet+tmmx*county, data=myfiles_allyears))

dev.off()
layout(matrix(c(1,2,3,4,5,6),3,2)) # optional 4 graphs/page 

lmcount <- lm(countratio ~ pr+pdsi+prpet+tmmx*year, data=myfiles_allyears)


plot(lmcount, which = 1:6, panel = panel.smooth) 
mtext("2007-2015 Palouse Regression pr+pdsi+prpet+tmmx*year", side = 3, line = -2, outer = TRUE)

dev.off()

#--loss and acre ranges as variables change over the length of the dataset

layout(matrix(c(1,2,3,4,5,6),3,2)) # optional 4 graphs/page 

library(effects)
model.lm <- lm(formula=countratio ~ pr+pet+erc+tmmx*year,data=myfiles_allyears)
plot(effect(term="year",mod=model.lm,default.levels=20),multiline=TRUE)

dev.off()

library(effects)
model.lm <- lm(formula=loss ~ pr+pet+erc+tmmx*year,data=myfiles_allyears)
plot(effect(term="year",mod=model.lm,default.levels=20),multiline=TRUE)

dev.off()


#--multiple regression 2009

fit <- lm(loss ~ pr + pet + prpet + tmmx, data=myfiles_2009)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
summary(fit)

#--multiple regression 2008

fit <- lm(loss ~ pr + pet + prpet + tmmx, data=myfiles_2008)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)


#--multiple regression 2015
fit <- lm(loss ~ pr + pet + prpet + tmmx, data=myfiles_2015)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)


#-3fold cross validation with 


dev.off()
#--cv.lm for all three years compared

layout(matrix(c(1,2,3,4,5,6),3,2))
       
#---Multicollinearity test

fit08_VIF <- VIF(lm(loss ~ pr + pet + prpet + pdsi + tmmx, data=myfiles_2008))

fit08 <- lm(loss ~ pr + pet + prpet + pdsi + tmmx, data=myfiles_2008)
cv.lm(data=myfiles_2008, fit08, m=3, main = "Wheat loss regression 2008") # 3 fold cross-validation

#---Multicollinearity test

fit09_VIF <- VIF(lm(loss ~ pr + pet + prpet + pdsi + tmmx, data=myfiles_2009))

fit09 <- lm(loss ~ pr + pet + prpet + pdsi + tmmx, data=myfiles_2009)
cv.lm(data=myfiles_2009, fit09, m=3, main = "Wheat loss regression 2009")

#---Multicollinearity test

fit15_VIF <- VIF(lm(loss ~ pr + pet + prpet + pdsi + tmmx, data=myfiles_2015))

fit15 <- lm(loss ~ pr + pet + prpet + pdsi + tmmx, data=myfiles_2015)
cv.lm(data=myfiles_2015, fit15, m=3, main = "Wheat loss regression 2015")

text <- capture.output(summary(fit08))
textplot(text, cex=.8, halign="right", valign="center")
text <- capture.output(summary(fit09))
textplot(text, cex=.8, halign="right", valign="center")
text <- capture.output(summary(fit15))
textplot(text, cex=.8, halign="right", valign="center")

dev.off()

layout(matrix(c(1,2,3,4),2,2))

#---Multicollinearity test all files

fit_VIF_loss <- VIF(lm(loss ~ pr + pet + prpet + pdsi + tmmx, data=myfiles_allyears))

fitallyears_loss <- lm(loss ~ pr + pet + prpet + pdsi + tmmx, data=myfiles_allyears)
cv.lm(data=myfiles_allyears, fitallyears_loss, m=3, main = "Wheat loss regression 2007-2015")

fit_VIF_countratio <- VIF(lm(countratio ~ pr + pet + prpet + pdsi + tmmx, data=myfiles_allyears))

fitallyears_count <- lm(countratio ~ pr + pet + prpet + pdsi + tmmx, data=myfiles_allyears)
cv.lm(data=myfiles_allyears, fitallyears_count, m=3, main = "Wheat count ratio regression 2007-2015")


text <- capture.output(summary(fitallyears_loss))
textplot(text, cex=.8, halign="right", valign="center")
text <- capture.output(summary(fitallyears_count))
textplot(text, cex=.8, halign="right", valign="center")


#--manova

dev.off()

manova_allyears <- manova(cbind(pr, pdsi, erc, tmmx) ~ county+year, data = myfiles_allyears)
summary(manova_allyears)

#--are counties and years significantly different between climate variables?

summary.aov(manova_allyears)

#--interaction between

dev.off()

layout(matrix(c(1,2,3,4),2,1)) # optional 4 graphs/page 

attach(myfiles_allyears)
year <- factor(year)
county <- factor(county)
interaction.plot(year, county, loss, type="b", las=2, col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Years", 
                 ylab="Loss ($)", 
                 main="Interaction of Loss across counties by year")

attach(myfiles_allyears)
year <- factor(year)
county <- factor(county)
interaction.plot(year, county, acres, type="b", las=2, col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Years", 
                 ylab="Loss unscaled", 
                 main="Interaction of Loss unscaled across counties by year")


dev.off()

layout(matrix(c(1,2,3,4),2,1)) # optional 4 graphs/page 

attach(myfiles_allyears)
year <- factor(year)
county <- factor(county)
interaction.plot(county, year, count, type="b", las=2, col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Years", 
                 ylab="Count", 
                 main="Interaction of frequency of WHEAT.DROUGHT claims across counties by year")

attach(myfiles_allyears)
year <- factor(year)
county <- factor(county)
interaction.plot(county, year, countratio, type="b", las=2, col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Years", 
                 ylab="Count ratio", 
                 main="Interaction of frequency RATIO of WHEAT.DROUGHT claims across counties by year")

dev.off()

layout(matrix(c(1,2,3,4),2,1)) # optional 4 graphs/page 


attach(myfiles_allyears)
year <- factor(year)
county <- factor(county)
interaction.plot(year, county, pet, type="b", las=2, col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Years", 
                 ylab="Count ratio", 
                 main="Interaction of frequency RATIO of WHEAT.DROUGHT claims across counties by year")

attach(myfiles_allyears)
year <- factor(year)
county <- factor(county)
interaction.plot(year, county, pdsi, type="b", las=2, col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 xlab="Years", 
                 ylab="Count ratio", 
                 main="Interaction of frequency RATIO of WHEAT.DROUGHT claims across counties by year")


dev.off()



# Plot Means with Error Bars
library(gplots)
attach(myfiles_allyears)
plotmeans(count~year,xlab="years",
          ylab="loss ($) ", main="Mean Claim Count Plot\nwith 95% CI")

dev.off()

library(gplots)
attach(myfiles_allyears)
plotmeans(pr~year,xlab="years",
          ylab="loss ($) ", main="Mean Claim Count Plot\nwith 95% CI")



