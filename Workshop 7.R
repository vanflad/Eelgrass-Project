#Biol 501 Workshop 7 on General Linear Models

setwd("~/Desktop")
#set working directory to find file

birds <- read.csv(file="songsparrow.csv", stringsAsFactors=FALSE,
                 strip.white=TRUE, na.strings=c("NA",""))
#read in file

head(birds)
#check file

class(birds$year)
#is year categorical? we'll be comparing years later on

birds$year <- as.factor(birds$year)
#change to a factor for the comparisons

plot(jitter(birds$survival, amount=0.2) ~ birds$tarsus)
#base r plots are awful, idk how to add the trendline...

library(ggplot2)
#load the superior ggplot

ggplot(birds, aes(tarsus, survival))+
  geom_point(position="jitter")+
  geom_smooth()
#create plot, jitter so you can see points, add trendline.

z <- glm(survival ~ tarsus, family = binomial(link = "logit"),
         data=birds)

library(visreg)
#load visreg to use it

visreg(z)
#visualizes the model fit, i think it means long tarsus=survival?

summary(z)
#shows coeff; Y = 24.64 - 1.26X is the model eqn in prev. graph

plot(jitter(birds$survival, amount=0.2) ~ birds$tarsus)
#redraw original scatterplot
abline(z)
#draw the model fit - logistic regression curve line to plot
#it seems that smaller birds are more likely to survive actually

xpoints <- range(birds$tarsus)
yhat <- predict(z, newdata = data.frame(tarsus=xpoints))
lines(yhat ~ xpoints)
#just double checking that this is same as abline(z), looks it

lines(birds$survival, predict(z)) #also the same way to do it.

predict(z, newdata=data.frame(tarsus=20.5))
#predicted survival probability of sparrow sized 20.5mm = -1.149
#there's another number in the answer 0.2407, no idea wtf it is.
#standard error maybe? whatever, i got the first part right.

-24.6361/-1.2578
#the ratio of -intercept/slope estimates point where probability
#of survival is changing most rapidly aka "LD50" = 19.59, right
library(MASS)
#load library for dose.p()
dose.p(z)
#another way to calculate same answer

confint(z)
#calc the likelihood based 95% CI for logistic regression coeffs

z0 <- glm(birds$survival ~ 1, family=binomial(link="logit"))
#reduced model (or is it supposed to be 0?)
z1 <- glm(birds$survival ~ birds$tarsus, family=binomial(link="logit"))
#full model (or is it supposed to be 1?)
anova(z0, z1, test = "Chi")
#anova, i have no idea if this was the right way to do it, w/e.

#if time permits (it never does...) finish Q#10 for sparrows.

crabs <- read.csv(file="satellites.csv", stringsAsFactors=FALSE,
                  strip.white=TRUE, na.strings=c("NA",""))
#read file
head(crabs)
#check it
str(crabs)
#see variables and groups and shit

plot(crabs$nsatellites ~ crabs$width.cm)
#plot num of sat. against width, add curve to see trend (can't)

ggplot(crabs, aes(width.cm, nsatellites))+
  geom_point()+
  geom_smooth()
#plot num of sat. against width, add curve to see trend (done)

z <- glm(nsatellites ~ width.cm, family=poisson(link="log"),
         data=crabs)
#fit general linear model to data

visreg(z)
#visualize the fit of the model

plot(crabs$nsatellites ~ crabs$width.cm)
#replot original scatterplot

abline(z)
#this don't work in this case

xpoints <- range(crabs$width.cm)
yhat <- predict(z, newdata = data.frame(width.cm=xpoints), type = "response")
lines(yhat ~ xpoints)
#this don't work in this case either

plot(crabs$nsatellites ~ crabs$width.cm)
#replot scatterplot to get rid of bad lines

lines(crabs$width.cm, predict(z))
#THIS IS THE CORRECT ONE TO UUUUUSE, it's curvilinear, idk why.

summary(z)
#gives coeff; Y = -3.30476 + 0.16405X is the eqn used for line

CI <- confint(z)
exp(CI)/(1+exp(CI))
#i think i'm doing this right? idk. trying to get CI for coeff.

z0 <- glm(nsatellites ~ 1, family=poisson(link = "log"),
          data=crabs)
#reduced model
z1 <- glm(nsatellites ~ width.cm, family=poisson(link = "log"),
          data=crabs)
#full model
anova (z0, z1, test="Chi")
#anova comparison, with misleading small p-value
anova(z, test="Chi")
#or can just do this one
anova(z, test="Chisq")
#i think these are all the same, they give same p value?

#glm assumes dispersion = 1 (i.e. that var = mean) but we'll see

a <- tapply(crabs$nsatellites, crabs$width.cm, mean)
b <- tapply(crabs$nsatellites, crabs$width.cm, var)
View(cbind(a, b))
#easiest way to compare the means and vars, variation is often much higher it seems...

v <- glm(nsatellites~width.cm, family=quasipoisson, data=crabs)
#new glm model fit for data not under previous assumption

summary(v)
#dispersion parameter = 3.18, nowhere near 1! this glm is better
summary(z)
#to compare side by side
#intercept and slope are the same but v has higher std dev's

visreg(z)
visreg(v)
#to compare. just like summary, it's same but higher variation.

anova(v, test = "F")
#more realistic p value than z gave, and is still significant
#workshop said to use F test here not likelihood ratio test
#which i think is what i did before, can't do F test for poisson

#On to the next section on Prion disease resistance...

kuru <- read.csv(file="kurudata.csv", stringsAsFactors=FALSE,
                  strip.white=TRUE, na.strings=c("NA",""))
#read data

head(kuru)
#check data

kuru$Genotype
#need to split up the 2 diff genotypes here

a <- do.call(rbind, strsplit(kuru$Genotype, ""))
#split and put into data frame/matrix

data <- cbind(a, kuru)
#add the split back to the original info with old vs young cohorts

head(data)
#make sure it worked

table(data$Cohort, data$`1`, data$`2`)
#contingency table, but looking at instructions, split was not necess.

table(kuru$Cohort, kuru$Genotype)
#contingency table for original non-split genotype. MV=high resist.

barplot(table(kuru$Cohort, kuru$Genotype), beside = TRUE)
#optional: make grouped bar plot for freq of genotype in age categ.

myftable <- ftable(kuru, row.vars = c("Cohort", "Genotype"))
#create flat frequency table to eventually use generalized lin. mod.
x1 <- data.frame(myftable)
#save flat frequency table to a data frame
x1
#make sure it worked

z <- glm(Freq ~ Cohort + Genotype, family = poisson(link = "log"),
         data = x1)
#additive/reduced glm including cohort and geno but no interaction*

visreg(z)
#examine the fit of the additive model to frequency data with visreg

summary(z)
#additive model is constrained from fitting exact freq in each categ

z <- glm(Freq ~ Cohort * Genotype, family = poisson(link = "log"),
         data = x1)
#repeat model with interaction term to compare

visreg(z)
#visually compare fit of model to data, this one much better fit?

summary(z)
#notice how this full model really is full, it fits freq exactly..?

anova(z, test = "Chisq")
#using full model test whether geno freq differ between two groups