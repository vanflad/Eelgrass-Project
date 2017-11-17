#Biol 501 Assignment 2 on Linear Models

eel <- read.csv(file="eel.csv", stringsAsFactors=FALSE,
                strip.white=TRUE, na.strings=c("NA",""))
#read in file

str(eel)
#check that it worked

suppressPackageStartupMessages(library(tidyverse))
#load package for data manipulation and graphs

suppressPackageStartupMessages(library(vegan))
#load package for ecology-specific data transformations

eel <- mutate(eel, Eelsite = paste(Site, Eelgrass, sep = " "))
#add a new column with both eelgrass and site information

eelabd <- eel %>%
  group_by(ID, Group) %>%
  summarise(abd=sum(Abundance))
#add together multiple rows for prey groups within each fish id

head(eelabd)
#make sure it worked

eelwide <- eelabd %>%
  spread(Group, abd, fill = 0, sep = NULL)
#change from long data format with rows for each prey to wide data
#so the prey groups form a matrix where 1 fish = 1 row for analysis

head(eelwide)
#check that it worked, interesting it added an empty NA column...

eelwide <- select(eelwide, -`<NA>`)
#just drop the NA column full of zero's since it's useless to us

head(eelwide)
#see if it looks how it's supposed to now, it does!

#now we need to reattach the site info such as date and eelgrass:

info <- select(eel, ID, Site, Eelgrass, Eelsite, Date)
#take only the relevant info we need to reattach to the dataframe

eelgrass <- left_join(eelwide, info, by = "ID")
#join dataframes together to get site/habitat/date info for fish IDs

head(eelgrass)
#see if it worked, it has duplicated rows that we have to remove

eelgrass <- unique(eelgrass)
#only retain the unique, non-duplicated rows for each fish

head(eelgrass)
#see if it worked, yes now we have only one row for each fish ID!

eelgrass$Date <- format.Date(eelgrass$Date, "%m-%d")
#remove the year from the date format because they're all 2016

head(eelgrass$Date)
#check that it worked out okay, always important to check each time

#create another dataframe for testing presence-absence significance
eelnum <- eelgrass %>%
  ungroup() %>%
  select(Barnacle:Pteropod)
#just the matrix of abundance numbers

head(eelnum)
#check that it is as it should be

eelpa.mat <- decostand(eelnum, method = "pa")
#turn abundance data into presence-absence data (in matrix-like df)

head(eelpa.mat)
#make sure it worked

eelinfo <- select(eelgrass, ID, Site, Eelgrass, Eelsite, Date)
#the relevant ID and site/eelgrass info to match to numberic matrix

eelpa <- bind_cols(eelpa.mat, eelinfo)
#combine presence-absence matrix with info columns to get PA dataset

head(eelpa)
#make sure it worked

typeof(eelnum)
#check whether abundance matrix is correct format for testing

eelnum <- as.matrix(eelnum)
#need to change it from a list to something we can work with

head(eelnum)
#make sure it worked and that the data itself still looks the same

shapiro.test(eelnum)
#perform shapiro test, which shows non-normality distribution

eelpa.mat <- as.matrix(eelpa.mat)
#do the same for the presence absence data as well

shapiro.test(eelpa.mat)
#same p value of significance, still has non-normal distribution

attach(eelinfo)

fligner.test(eelnum~Site)

nrow(eelnum)
nrow(eelinfo)

totalset <- eel %>%
  group_by(Eelsite, Group, Set, Date) %>%
  summarise(total=sum(abd))

aveset <- totalset %>%
  group_by(Eelsite, Group, Date) %>%
  summarise(Ave=mean(total))

View(totalset)
View(aveset)

ggplot(aveset, aes(Date, Ave))+
  geom_path(aes(color=Eelsite))
#add set # stuff, add up abd totals for each set then average then graph!

### STARTING OVER, DELETING ANYTHING NOT RELEVANT HERE: ###

#Biol 501 Assignment 2 on Linear Models

setwd("~/Desktop")
#set working directory to know where to find file

eel <- read.csv(file="eel.csv", stringsAsFactors=FALSE,
                strip.white=TRUE, na.strings=c("NA",""))
#read in file

str(eel)
#check that it worked

suppressPackageStartupMessages(library(tidyverse))
#load package for data manipulation and graphs

suppressPackageStartupMessages(library(vegan))
#load package for ecology-specific data transformations

eel <- mutate(eel, Eelsite = paste(Site, Eelgrass, sep = " "))
#add a new column with both eelgrass and site information

eel <- eel %>%
  group_by(ID) %>%
  summarise(abd=sum(Abundance), bio=sum(Biomass), Site=first(Site),
            Eelgrass=first(Eelgrass), Eelsite=first(Eelsite),
            Set=first(Set), Date=first(Date))
#add together multiple rows for prey groups within each fish id
#actually deleted Group from: "group_by(ID, Group)" line here*

head(eel)
#make sure it worked

eel$Date <- as.Date(eel$Date, "%Y-%m-%d")
#remove the year from the date format because they're all 2016

head(eel)
#check that it worked out okay, always important to check each time

shapiro.test(eel$abd)
#perform shapiro test, which shows non-normality distribution

eel$Site <- as.factor(eel$Site)
#need to change from character to factor for fligner test

fligner.test(eel$abd~eel$Site)
#perform fligner test, which shows data is non-homogeneous

eel <- mutate(eel, logabd = log(abd+1))
#log transform the data and re-do the tests

head(eel$logabd)
#check that it worked out

shapiro.test(eel$logabd)
#perform shapiro test, still shows non-normality distribution

fligner.test(eel$logabd~eel$Site)
#perform fligner test, shows that data is now homogeneous!

eel$Eelgrass <- as.factor(eel$Eelgrass)
#change to a factor as well for analysis

model <- lm(logabd ~ Site, data=eel)
#run the linear model first only with site as explanatory variable

summary(model)
#investigate the coefficients of the linear model

model2 <- lm(logabd ~ Date, data=eel)
#run another linear model with only date as explanatory variable

summary(model2)
#see the coefficients for this linear model

anova(model2, test="Chisq") #?

model5 <- lm(logabd ~ 1, data=eel)

model3 <- lm(logabd ~ Eelgrass, data=eel)

summary(model3)

model4 <- lm(logabd ~ Site + Eelgrass + Date, data=eel)

summary(model4)

anova(model2, model5)

#wait, is date is significant tho?? idk how to tell! p or R^2? or?

#If Date isn't significant, we might instead choose to use it as a random effect in a mixed-effects model
library(lme4)

mixed.mod<-lmer(logabd~Site + Eelgrass + (1|Date), data=eel)

#Backwards stepwise elimination (same process for linear model without random effects)
mixed.mod2<-lmer(logabd~ Eelgrass + (1|Date), data=eel)

anova(mixed.mod,mixed.mod2) #If significant, then retain variable that was removed
#in this case retain site variable

#Repeat
mixed.mod3<-lmer(logabd~ Site + (1|Date), data=eel)

anova(mixed.mod,mixed.mod3) #If significant, then retain variable that was removed
#not sig, do not retain eelgrass variable

#need to graph trendlines to add linear model lines

totalset <- eel %>%
  group_by(Eelsite, Date, Set, Site) %>%
  summarise(totalabd=sum(abd), totalbio=sum(bio))

aveset <- totalset %>%
  group_by(Eelsite, Date, Site) %>%
  summarise(aveabd=mean(totalabd), avebio=mean(totalbio))

sites <- aveset %>%
  group_by(Site, Date) %>%
  summarise(Abd=mean(aveabd), Bio=mean(avebio))

View(totalset)
View(aveset)
View(sites)

ggplot(aveset, aes(Date, log(aveabd)))+
  geom_path(aes(color=Eelsite))+
  geom_abline()
?geom_abline()
ggplot(aveset, aes(Date, avebio))+
  geom_path(aes(color=Eelsite))
#add set # stuff, add up abd totals for each set then average then graph!

z <- glm(abd ~ Site+Eelgrass+Date, family = poisson(link = "log"), data=eel)

?glm

summary(z)

z1 <- glm(abd~Site+Date, family = poisson(link = "log"), data=eel)

summary(z1)

z2 <- glm(abd~Eelgrass+Date, family = poisson(link = "log"), data=eel)

summary(z2)

z3 <- glm(abd~Date, family = poisson(link = "log"), data=eel)

summary(z3)

anova(z, z1, test = "Chisq")
anova(z, z2, test = "Chisq")
anova(z1, z3, test = "Chisq")
anova(z2, z3, test = "Chisq")
anova(z, test = "Chisq")

z4 <- glm(abd~Site+Date+Eelgrass, family=quasipoisson(link="log"), data=eel)

summary(z4)
#dispersion parameter is huuuuuge, not 1.

anova(z4, test = "F")
#significant for site and not date or eelgrass

z5 <- glm(abd~Site+Date, family=quasipoisson(link="log"), data=eel)

summary(z5)

anova(z5, test = "F")

z6 <- glm(abd~Site, family=quasipoisson(link="log"), data=eel)

summary(z6)

anova(z6, test = "F")

predicted <- data.frame(abdpred=predict(z6, eel), Date=as.Date(eel$Date, "%Y-%m-%d"))

ggplot(sites, aes(Date, log(Abd)))+
  geom_path(aes(color=Site))+
  geom_line(data=predicted, aes(Date, abdpred))

#do same for biomass and see what it looks like?????

z7 <- lm(bio~Site+Eelgrass+Date, data=eel)
summary(z)
anova(z, test = "Chisq")
pred <- data.frame(biopred=predict(z7, eel), Date=eel$Date)

ggplot(sites, aes(Date, Bio))+
  geom_path(aes(color=Site))+
  geom_line(data=pred, aes(Date, biopred))

z8 <- lm(bio~Site, data=eel)
summary(z8)
anova(z8, test="Chisq")
pred <- data.frame(biopred=predict(z8, aveset), Date=aveset$Date)

ggplot(sites, aes(Date, Bio))+
  geom_path(aes(color=Site))+
  geom_line(data=pred, aes(Date, biopred))

#okay, realized I shouldn't graph glm on the trendline graph
#i should plot the linear model on a scatterplot of data!

ggplot(eel, aes(Date, log(abd+1)))+
  geom_point(aes(color=Site), position="jitter")+
  geom_line(data=predicted, aes(Date, abdpred))

eel %>%
  filter(bio<400) %>% 
  ggplot(aes(Date, bio))+
  geom_point(aes(color=Site), position="jitter")+
  geom_line(data=pred, aes(Date, biopred))

?glm

z9 <- glm(bio~Site, family = quasipoisson(link = "log"), data = eel)
summary(z9)
anova(z9, test = "F")
predb <- data.frame(bpred=predict(z9, eel), Date=eel$Date)
ggplot(eel, aes(Date, log(bio+1)))+
  geom_point(aes(color=Site), position="jitter")+
  geom_line(data=predb, aes(Date, bpred))

#have to re-do data by sum up total abd/bio, not looking at sp
#supposed to have one data point per each fish? not each prey!

#add together overall abundance/biomass of all prey items for each fish
#now we have one row for each fish, not one row for each diff prey item

eel <- eel %>%
  group_by(ID) %>%
  summarise(abd=sum(Abundance), bio=sum(Biomass), Site=first(Site),
            Eelgrass=first(Eelgrass), Set=first(Set), Date=first(Date))
#grouping by fish ID to combine multiple rows for multiple prey items
#so that we get a total abundance and total biomass of prey per fish

#more scrapped code because should do abundance only:
#stop making it so fucking complicated dude!
str(eel)
#make sure it worked and it has 134 rows (the number of fish samples)

eel$Date <- as.Date(eel$Date)
#change from a character variable to a continuous date format variable

head(eel)
#check that it worked out okay, always important to check each time

#perform tests on abundance variable
shapiro.test(eel$abd)
#shapiro test for abundance, which shows non-normal distribution

eel$Site <- as.factor(eel$Site)
#need to change from character to factor for fligner test

fligner.test(eel$abd~eel$Site)
#fligner test for abundance, which shows data is non-homogeneous

eel <- mutate(eel, logabd = log(abd+1))
#log transform the data and re-do the tests

head(eel)
#check that it worked out

shapiro.test(eel$logabd)
#log(abundance) shapiro test, still shows non-normal distribution
#but fligner test of homogeneity of data is more important
#and if it has quasi-poisson or poisson distribution, use glm()!

fligner.test(eel$logabd~eel$Site)
#log(abundance) fligner test, shows the data is homogeneous now!

#perform tests on biomass
##### HOWEVER, CAN ONLY HAVE ONE RESPONSE VARIABLE FOR HW #####
shapiro.test(eel$bio)
#perform shapiro test for biomass, shows non-normal distribution

fligner.test(eel$bio~eel$Site)
#perform fligner test for biomass, shows data is non-homogeneous

eel <- mutate(eel, logbio = log(bio+1))
#log transform the data and re-do the tests

head(eel)
#check that it worked out

shapiro.test(eel$logbio)
#log(biomass) shapiro test, which shows normal distribution now!

fligner.test(eel$logbio~eel$Site)
#log(biomass) fligner test, shows that data is homogeneous now!

#now the data is transformed, let's graph it before modelling:
ggplot(eel, aes(Date, logabd))+
  geom_point(aes(color=Site), position="jitter")
#scatterplot for abundance over time, categorized by the region

ggplot(eel, aes(Date, logbio))+
  geom_point(aes(color=Site), position="jitter")
#scatterplot for biomass over time, categorized by the region

#add graphs showing overall trends here as well????? *****