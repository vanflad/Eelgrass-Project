#Biol 501 Assignment 2 on Linear Models

setwd("~/Desktop")
#set working directory to know where to find file

eelgrass <- read.csv(file="eel.csv", stringsAsFactors=FALSE,
                strip.white=TRUE, na.strings=c("NA",""))
#read in data file for prey contents of fish stomachs to be analyzed

str(eelgrass)
#check that it worked and was read in properly

suppressPackageStartupMessages(library(tidyverse))
#load packages for data manipulation (dplyr) and graphs (ggplot2)

eel <- eelgrass %>%
  group_by(ID) %>%
  summarize(abd=sum(Abundance), Set=first(Set), Site=first(Site), 
            Eelgrass=first(Eelgrass), Date=first(Date))
#grouping by fish ID to combine multiple rows for multiple prey items
#so that we get an overall total abundance of prey for each fish
#summarize function calculates the total and gives info for each fish

str(eel)
#make sure it worked and it has 134 rows (the number of fish samples)

eel$Date <- as.Date(eel$Date)
#change from a character variable to a continuous date format variable

head(eel)
#check that it worked out okay, always important to check each time!

#perform tests on abundance (response) variable
shapiro.test(eel$abd)
#shapiro test for abundance, shows non-normal distribution (p<0.05)

eel$Site <- as.factor(eel$Site)
#need to change from character to factor for fligner test

fligner.test(eel$abd~eel$Site)
#fligner test for abundance, shows data is non-homogeneous (p<0.05)

eel <- mutate(eel, logabd = log(abd+1))
#log transform the data and re-do the tests

head(eel)
#check that it worked out

shapiro.test(eel$logabd)
#log(abundance) shapiro test, still shows non-normal distribution
#however it is expected to have poisson/quasipoisson distribution
#therefore it makes sense and is okay that it is non-normal here

fligner.test(eel$logabd~eel$Site)
#log(abundance) fligner test, shows the data is homogeneous now!

ggplot(eel, aes(Date, logabd))+
  geom_point(aes(color=Site), position="jitter")+
  labs(y="Log(Abundance)")+
  theme_classic()+
  theme(legend.position = c(0.9, 0.75),
        legend.box.background = element_rect())
#scatterplot for abundance over time, categorized by the region
#labs to rename label, position="jitter" to see ovelapping points
#and changed to theme_classic to get rid of distracting elements
#and re-adjusted placement of the legend with borders around it

z <- glm(abd~Site+Eelgrass+Date, family=poisson(link="log"), data=eel)
#first let's attempt a poisson distribution general linear model
summary(z)
#check the coefficients
anova(z, test = "Chisq")
#oh look they're all significant variables! But the assumptions?
#now we will check if the dispersion parameter actually = ~1...

a <- tapply(eel$abd, eel$Site, mean)
b <- tapply(eel$abd, eel$Site, var)
cbind(a, b)
#we can see here that variance is much higher than the means
#which means dispersion parameter does not = 1, can't use poisson

z1 <- glm(abd~Site+Eelgrass+Date, family=quasipoisson(link="log"),
          data=eel)
summary(z1)
anova(z1, test = "F")

z2 <- glm(abd~Site+Eelgrass, family=quasipoisson(link="log"),
          data=eel)
summary(z2)
anova(z2, test = "F")

anova(z1, z2, test = "F")
#date is not signif

z3 <- glm(abd~Eelgrass, family = quasipoisson(link = "log"),
          data=eel)

anova(z2, z3, test = "F")
#site is signif

z4 <- glm(abd~Site, family=quasipoisson(link="log"),
          data=eel)
summary(z4)
anova(z4, test = "F")

anova(z2, z4, test = "F")
#eelgrass is signif

#therefore z2=glm(abd ~ Site + Eelgrass) is the best model option

predicted <- data.frame(abdpred=predict(z2, eel), Date=eel$Date)

ggplot(eel, aes(Date, logabd))+
  geom_point(aes(color=Site), position="jitter")+
  labs(y="Log(Abundance)")+
  theme_classic()+
  theme(legend.position = c(0.9, 0.75),
        legend.box.background = element_rect())+
  geom_line(data=predicted, aes(Date, abdpred))

fit <- data.frame(abdfit=fitted(z2, eel), Date=eel$Date)

ggplot(eel, aes(Date, abd))+
  geom_point(aes(color=Site), position="jitter")+
  labs(y="Abundance")+
  theme_classic()+
  theme(legend.position = c(0.9, 0.75),
        legend.box.background = element_rect())+
  geom_line(data=fit, aes(Date, abdfit))

#dont get it confused but theres multiple z1 and z2s and etc here
z1 <- glm(abd~Site+Eelgrass+Date, family=quasipoisson(link="log"),
          data=eel)
summary(z1)
anova(z1, test = "F")

z2 <- glm(abd~Site+Eelgrass, family=quasipoisson(link="log"),
          data=eel)
summary(z2)
anova(z2, test = "F")

anova(z1, z2, test = "F")
#date is not signif

z3 <- glm(abd~Site+Date, family=quasipoisson(link="log"),
          data=eel)
summary(z3)
anova(z3, test = "F")

anova(z1, z3, test = "F")
#date is not signif again, eelgrass is signif

z4 <- glm(abd~Eelgrass+Date, family=quasipoisson(link="log"),
          data=eel)
summary(z4)
anova(z4, test = "F")

anova(z1, z4, test = "F")
#site is signif

#dont worry about these just yet
z2 <- lm(logabd~Site+Eelgrass, data=eel)
summary(z2)
anova(z2, test = "Chisq")
z5 <- lm(logabd~Site, data=eel)
summary(z5)
anova(z5, test = "Chisq")
anova(z2, z5, test = "Chisq") #eel signif
z6 <- lm(logabd~Eelgrass, data=eel)
summary(z6)
anova(z6, test = "Chisq")
anova(z2, z6, test = "Chisq") #site signif
x <- c(AIC(z2), AIC(z5), AIC(z6))
delta <- x-min(x)
L <- exp(-0.5*delta)
w <- L/sum(L)
w
sum(w)

#dont get confused but theres multiple z1 and z2s: trying lm here
z1 <- lm(logabd~Site+Eelgrass+Date, data=eel)
summary(z1)
anova(z1, test = "Chisq")

z2 <- lm(logabd~Site+Eelgrass, data=eel)
summary(z2)
anova(z2, test = "Chisq")

anova(z1, z2, test = "Chisq")
#date is not signif

z3 <- lm(logabd~Site+Date, data=eel)
summary(z3)
anova(z3, test = "Chisq")

anova(z1, z3, test = "Chisq")
#date is not signif again, eelgrass is signif

z4 <- lm(logabd~Eelgrass+Date, data=eel)
summary(z4)
anova(z4, test = "Chisq")

anova(z1, z4, test = "Chisq")
#site is signif

x <- c(AIC(z1), AIC(z2), AIC(z3), AIC(z4))
delta <- x-min(x)
L <- exp(-0.5*delta)
w <- L/sum(L)
w
sum(w)

x <- c(AIC(z1), AIC(z2), AIC(z3), AIC(z4), AIC(z5), AIC(z6))
delta <- x-min(x)
L <- exp(-0.5*delta)
w <- L/sum(L)
w
sum(w)
#abd ~ Site + Eelgrass is the best model

library(lme4)

mixed.mod<-lmer(logabd~Site + Eelgrass + (1|Date), data=eel)

#Backwards stepwise elimination (same process for linear model without random effects)
mixed.mod2<-lmer(logabd~ Eelgrass + (1|Date), data=eel)

anova(mixed.mod,mixed.mod2) #If significant, then retain variable that was removed
#site is significant so we keep it in the model

#Repeat
mixed.mod3<-lmer(logabd~Site + (1|Date), data=eel)

anova(mixed.mod,mixed.mod3) #If significant, then retain variable that was removed
#eelgrass is not significant so mixed.mod2 is our best model

x <- c(AIC(z2), AIC(mixed.mod2))
delta <- x-min(x)
L <- exp(-0.5*delta)
w <- L/sum(L)
w
sum(w)