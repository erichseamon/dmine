#--USDA-cropexamine 
#- takes crop outputs from cropmerge and examines each commodity 

USDA_cropexamine <- function() {

library(DAAG)
library(ridge)

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
  
  text(0.5, 0.5, txt, cex = cex) 
  text(.8, .8, Signif, cex=cex, col=2) 
}


setwd("/dmine/data/USDA/agmesh-scenarios/palouse/summaries3/annual-summaries-drought/")
files <- list.files(pattern = "")
myfiles = do.call(rbind, lapply(files, function(x) 
  read.csv(x, stringsAsFactors = FALSE)))

#names(myfiles)[19] <- c("year") 
myfiles$prpet <- (myfiles$pr - myfiles$pet)
#write.csv(myfiles, file = "WHEAT_drought_summary")
myfiles_allyears <- subset(myfiles, , c(pr, pdsi, pet, tmmx, prpet, srad, sph, erc, bi, erc, countratio, loss, acres, count, county, year, commodity))
myfiles_allyears$county <- factor(myfiles_allyears$county)
myfiles_allyears$year <- factor(myfiles_allyears$year)
myfiles_allyears$loss <- scale(myfiles_allyears$loss, center = TRUE, scale = FALSE)
myfiles_loss[1:10] <- scale(myfiles_loss[1:10], center = TRUE, scale = TRUE)

myfiles_loss <- myfiles_allyears
myfiles_count <- myfiles_allyears

#-removes zero values
myfiles_loss <- myfiles_loss[apply(myfiles_loss[c(12)],1,function(z) any(z!=0)),]





#--allyears pairwise plot

myfiles_whitman<- subset(data.frame(myfiles_loss), county == "Whitman")
myfiles_whitman_wheat<- subset(data.frame(myfiles_whitman), commodity == "WHEAT")


#--countratio

pairs(myfiles_whitman[c(1,2,3,4,5,6,7,8,9,10,11)], lower.panel=panel.smooth, upper.panel=panel.cor)
dev.off()

#-loss

dev.off()

pairs(myfiles_whitman[c(1,2,3,4,5,6,7,8,9,10,12)], lower.panel=panel.smooth, upper.panel=panel.cor)
dev.off()

#---only 2008
myfiles_2008 <- subset(data.frame(myfiles_loss), year == "2008")
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


}
