#Biol 501 Workshop 11 on Meta-analysis

setwd("~/Desktop")
#set working directory

birds <- read.csv(file="sparrow.csv", stringsAsFactors=FALSE,
         strip.white=TRUE, na.strings=c("NA",""))
#read in file

head(birds)
#give the data a look

library(meta)
#load library for funnel graphs and stuff

sdz <- sd(birds$r)
#save standard deviation to a vector

SEz <- vector(length = length(birds$n))
#create empty vector for loop

for (i in 1:length(birds$n)){
  SEz[i] <- sdz/sqrt(birds$n[i])
}
#write loop to fill in vector with standard error values

SEz
#standard error values, it worked!

w <- 1/((SEz)^2)
#calculate weights for stuff or whatever

weighted.mean(birds$r, w)
#weighted mean is 0.425874

mean(birds$r)
#compared to regular mean which is 0.4358667

sewm <- sqrt(1/sum(w))
#standard error of the weighted mean

sewm
#0.01567626

z <- metagen(r, SEz, data=birds)
#use metagen to make funnel plot with

summary(z)
#see what it's all about

funnel(z)
#funnel plot, looks like no bias, said to add line at r=0 but that's axis

plot(x=birds$n, y=birds$r)
#a non-funnel graph to compare, sample size versus effect size, no bias

fishz = 0.5*log((1 + birds$r)/(1 - birds$r))
#converts to fishers z scale or something or other

SEfz <- 1/(sqrt(birds$n-3))
#standard error of the fishers z scale stuff

zupper <- fishz + 1.96 * SEfz
zlower <- fishz - 1.96 * SEfz
zupper
zlower
#calculate approximate CI for mean of transformed effect sizes using
#normal approximation. No idea how, might just quick right here...