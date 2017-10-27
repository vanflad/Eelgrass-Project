#Biol 501 Workshop 8 on Model Selection

bmr <- read.csv(file="bmr.csv", stringsAsFactors=FALSE,
                strip.white=TRUE, na.strings=c("NA",""))
#read in file

head(bmr)
#check it

plot(bmr$mass.g, bmr$bmr.w)
#plot the data

plot(log(bmr$mass.g), log(bmr$bmr.w))
#relationship between mass and metabolic rate linear on log scale? yes

transbmr <- log(bmr)
#log transform data since original data are not on the log scale

head(transbmr)
#check it out, see it worked

z <- lm(bmr.w ~ mass.g, transbmr)
#fit linear model to log trans data, what is the estimate of slope?

summary(z)
#estimate of slope is 1.33809

confint(z)
#95 CI for est slope, does this interval include either of the
#candidate values for the scaling parameter beta? IDK. 1.29-1.38. N?

abline(z)
#add best fit regression line to prev plot and it looks nice now!

z1 <- lm(bmr.w ~ 1 + offset(0.75*mass.g), transbmr)
#force linear model with chosen slope of 3/4 for beta estimate

z2 <- lm(bmr.w ~ 1 + offset(0.667*mass.g), transbmr)
#force linear model with chosen slope of 2/3 for beta estimate

abline(z1)
abline(z2)
#compare the fit of these two models, they're the same and suck
#but z1 is ever so slightly lower which means it's kind of better?
#^that "z1=better" was with x&y swapped, here there is no abline(z1)?

summary(z1)
summary(z2)
#compare models, z1 has less residuals so it's better. agrees w above

logLik(z1)
logLik(z2)
#calculate log likehood of models, z1 has the higher (less neg) value

AIC(z1)
AIC(z2)
#calculate AIC values and then compare them
AIC(z2) - AIC(z1)
#diff = 23.5472, z1 is better b/c smaller AIC value (= better model)

x <- c(AIC(z1), AIC(z2))
delta <- x-min(x)
L <- exp(-0.5*delta)
w <- L/sum(L)
w
sum(w)
#calc Akaike weights (0.9 and 0.000008, z1=better) and check sum=1

#summarize findings: z2 sucks just way to bad to have any support

#why is it not possible to compare with reg log likelihood test? IDK!

#optional: theories predict mass and bmr conform to power law... skip

#bird abundance in forest fragments part now:

birds <- read.csv(file="birdabund.csv", stringsAsFactors=FALSE,
                  strip.white=TRUE, na.strings=c("NA",""))
#read in bird file

head(birds)
#check out data

#using histograms explore freq dist of the variables, trans if need:

hist(birds$abund)
#distributed all over the place (many points skew right & many = 0)

hist(log(birds$area))
#kinda normally distributed on log scale, but not perfect though

hist(birds$yr.isol)
#very right skewed

hist(log(birds$dist))
#right skewed slightly

hist(log(birds$ldist))
#kinda normally distributed? kinda flatter shaped tho, all over

hist(birds$graze)
#most all over the place distribution, high left, high right, weird

hist(birds$alt)
#slightly left skewed

round(cor(birds), digits=2)
#what are correlated? graze + abund, graze + yr.isol, yr.iso + abund

library(leaps)
#load needed library

x <- birds[, c(2:7)]
#first step create dataframe with only explanatory var, no response
head(x)
#check it worked
z <- leaps(x, birds$abund, names = names(x))
#use leaps to find which model best predicts bird abundance
i <- which(z$Cp==min(z$Cp))
#this says model #1 is best. Whatever the heck that means

plot(z$size, z$Cp)
#plot mallows Cp val. against predictors p, how many p for best mod?
lines(z$size, z$size)
#add lines to preevious plot where Cp=p. best model has 4 predictors

vars <- which(z$which[i,])
vars
#this says 5...

z$size[which(z$Cp < 2)]
#this says 2...

#so I have no idea why best model # of predictors = 4... mistake?