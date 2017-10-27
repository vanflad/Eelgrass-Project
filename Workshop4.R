#Biol 501 Workshop 4 - Linear models

setwd("~/Desktop")
lion <- read.csv(file="lions.csv", stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
#read file

library(tidyverse)
#load library

head(lion)
#see data

plot(lion$black, lion$age)
#create initial plot (not with ggplot, too complicated)

z <- lm(age ~ black, data=lion)
#store linear model function in an object

abline(z)
#fit the linear model to the scatterplot data
#looking at it, there's more range with age but still mostly linear

summary(z)
#slope=10.647, intercept=0.879, SEs=0.569 and 1.510, respectively.
#and the R^2=0.611 (adjusted r squared in summary table)

confint(z)
#CI for intercept = -0.28 to 2.04, CI for slope = 7.56 to 13.73

anova(z)
#summarize the fir of the model to the data with an anova table

plot(z)
#apply plot to lm object to diagnose violations of assumptions
#it gets a bit messy with high age (as said above too), but it's ok

#optional: remove oldest lion outlier and see how it all changes
#max(lion$age)
#lion$age[13.1] <- "0"
#lion$black[which(lion$age=="0")] <- "0"
#lion <- filter(lion, age !="0")
#it just fucks up the data, i might just skip this dumb step

plot(age~black, data=lion)
abline(z)
xpoints <- seq(min(lion$black), max(lion$black), length.out = 100)
ypoints <- data.frame(predict(z, newdata = data.frame(black = xpoints), interval = "confidence", level = 0.95))
lines(ypoints$lwr ~ xpoints, lty = 2)
lines(ypoints$upr ~ xpoints, lty = 2)
#added confidence bands to scatterplot! note: it widens at the top

plot(age~black, data=lion)
abline(z)
xpoints <- seq(min(lion$black), max(lion$black), length.out = 100)
ypoints <- data.frame(predict(z, newdata = data.frame(black = xpoints),
                              interval = "prediction", level = 0.95))
lines(ypoints$lwr ~ xpoints, lty = 2)
lines(ypoints$upr ~ xpoints, lty = 2)
#add prediction intervals to scatterplot; like CI but for new values
#a lot wider, maybe not great for predicting lion age from noses

library(visreg)
z <- lm(age ~ black, data = lion)
visreg(z)
#visualize the fit of the regression model to the data w/ 95% CI

knees <- read.csv(file="knees.csv", stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
#read new data file called knees

head(knees)
#always check da data

is.factor(knees$treatment)
#not a factor

knees$treatment <- as.factor(knees$treatment)
#change it to a factor!

levels(knees$treatment)
#check the order of levels, default=alphabetical, want control=1st

knees$treatment <- factor(knees$treatment, levels(knees$treatment)[c(1, 3, 2)])
#to get practice rearrange levels to control, knees, eyes (easy!)

ggplot(knees, aes(treatment, shift))+
  geom_point(stat="identity")
#plot the phase shift data, showing individual points for treatment

stripchart(shift~treatment, data=knees, vertical=TRUE)
#plot the phase shift data w individ points, not in ggplot

z <- lm(shift~treatment, data=knees)
#store linear model output into an object like before

abline(z)
#try to fit the model to the plot, get warning message: In
#abline(z): only using the first 2 of the 3 regression coefficients

visreg(z)
#create a graphic that illustrates the fit of the model to data
#i.e. include predicted (fitted) values to your plot

plot(z)
#check assumptions, pretty good, little bit of variation at top

model.matrix(z)
#peek behind the scenes and see that control is left out

summary(z)
#look at parameter coefficients and standard errors with summary
#coefficients are intercept=-0.309 as control mean
#and knee treatment=-0.027 is diff from control mean (small diff)
#and eye treatment=-1.24 is diff from control mean (large diff!)
#pvalues are invalid here, they may be valid in anova table? idk.

confint(z)
#obtain 95% confidence intervals for the three parameters

anova(z)
#test the effect of light treatment on phase shint with ANOVA

library(lsmeans) #load library first
lsmeans(z, "treatment", data=knees)
#make table with treatment means, SE and CIs. Why are these diff?
#b/c uses residual mean square from model fitted to all the data
#smaller SEs and CIs, assuming equal variance within diff groups

setwd("~/Desktop")
fly <- read.csv(file="fruitflies.csv", stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
head(fly) #read in and check out

is.factor(fly$treatment) #not a factor so change it to one
fly$treatment <- as.factor(fly$treatment)

levels(fly$treatment) #reorder levels, with control group first
fly$treatment <- factor(fly$treatment, levels(fly$treatment)[c(5, 2, 4, 1, 3)])

plot(fly$thorax.mm ~ fly$longevity.days, col=fly$treatment)
ggplot(fly, aes(thorax.mm, longevity.days))+
  geom_point()+
  facet_wrap(~treatment) #optional: make some plots and stuff

z <- lm(~ thorax.mm + treatment, data=fly)
#not working even though it did previously, infuriating.
plot(z) #computer crashed before the plot worked...
fly
?lm
is.factor(fly$thorax.mm)
is.factor(fly$treatment)
fly$thorax.mm <- as.factor(fly$thorax.mm)

#this is a lost fucking cause, don't even bother or will go crazy