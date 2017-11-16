#Biol 501 Workshop 10 on Bootstrapping Nov 9th 2017

setwd("~/Desktop")
#set working directory

ant <- read.csv(file="antilles.csv", stringsAsFactors=FALSE,
								strip.white=TRUE, na.strings=c("NA",""))
#read in file

head(ant)
#check file

hist(ant$immigration.date)
#left skewed data

mean(ant$immigration.date)
#8.7

median(ant$immigration.date)
#3.5, diff values because it's so left skewed

boot <- sample(ant$immigration.date, 10, replace = TRUE)
#take a single bootstrap replicate
median(boot)
median(sample(ant$immigration.date, 10, replace = TRUE))
hist(boot)
#weird distribution, gap in middle and left (mostly) and right skewed!

x <- vector("list", 10000)
medians <- vector("numeric", length = 10000)
for(i in seq(1:10000)){
	x[[i]] <- sample(ant$immigration.date, 10, replace = TRUE)
	medians[i] <- median(x[[i]])
}
medians
#wow it only took over an hour to get a for loop working...

hist(medians)
#visualizes the most frequent medians values for bootstrapped data

sd(medians)
#3.306 when answer is asking for 2.5... sooooo this is wrong af then.

library(boot)
#load bootstrapping library for a less stupid way to bootstrap data

boot.med <- function(x,i){boot.med <- median(x[i])}
#write a function
z <- boot(ant$immigration.date, boot.med, R=10000)
#bootstrap
z
#print summarized results
hist(z$t)
#visualize the shit
sd(z$t)
#standard error of the sample median = 2.56! why diff tho?????

mean(z$t)
#4.4=mean median value, over-est/biased by ~0.9 b/c orig. median=0.35

quantile(z$t, probs = c(.025, 0.975))
#11.1, 1.8 are the 95% confidence intervals

boot.ci(z, type = "bca")
#10.6, 1.8 are 95% confidence intervals using more accurate method

#stopping here before new section on trillium fragmentation questions