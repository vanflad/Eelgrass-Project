#Biol 501 Workshop 3 Sept 21st on Planning experiments

#note that this workshop will be a long one... yikes!

#"there are many ways to do the same thing in R
#the right way is whatever way you figure out!"

#simulating data portion of workshop:

sample(c("diseased","healthy"), size=20, replace=TRUE, prob=c(.5,.5))
#simulating data to sample 20 ppl from population where 50% diseased

table(sample(c("diseased","healthy"), size=20, replace=TRUE, prob=c(.5,.5)))
#make a frequency table of the results, then repeat 5 times to see
#differences are... 11:9, 11:9, 10:10, 5:15, 14:6, it varies a lot!

sample(c("mated","unmated"), size=18, replace=TRUE, prob=c(.7,.3))
#sample 18 ppl from population where unmated proportion = 0.7

table(sample(c("mated","unmated"), size=18, replace=TRUE, prob=c(.7,.3)))
#summarize these results in a table too (13:5, which is accurate)
#repeat five more times... 11:7, 12:6, 12:6, 12:6, 16:2, it varies.

myrand <- rnorm(30, mean = 0, sd = 2)
#sample 30 observations from a normally distrib'd pop'n, mean=0 SD=2

hist(myrand)
#plot these results in a histogram. usually skewed one way, not perf

mean(rnorm(30, mean = 0, sd = 2))
#repeat the random simulation 5 times and calculate the mean
#results are... -0.37, -0.20, -0.29, -0.77, -0.43, off from zero!

#simulated/hypothetical spider mating experiment:

sample(c("success","failure"), size=10, replace=TRUE, prob=c(.5,.5))
#randomly choose 10 females and see if they choose male from sp.
#result was 6:4 success:failure, so they chose own sp. more here

install.packages("binom", dependencies = TRUE)
#install the binom package to calculate confidence intervals

library(binom)
#need to load library before we can use it, duh

myCI <- binom.confint(6, 10, method = "ac")
#gets the confidence interval, where x=6 success in generated sample

print(myCI)
#shows the results

myCI$lower
#the lower limit of the confidence interval

myCI$upper
#the upper limit of the confidence interval

myCI$upper-myCI$lower
#the span of the confidence interval 0.83 - 0.31 = 0.52
#can't be confident at all that the mean = 0.5 with that CI
#because CI spans half of results since it can be from 0-1.

#repeat step five times, what is the smallest CI span?
sample(c("success","failure"), size=10, replace=TRUE, prob=c(.5,.5))
myCI <- binom.confint(x, 10, method = "ac")
myCI$upper-myCI$lower
#results... 0.7 success = 0.50 span, 0.6=0.52, 0.7 again...
#more results... 0.3=0.50 (same dist. to mean as 0.7), 0.4=0.52
#therefore 0.504 (more decimals) is the smallest span we can get!

for (i in 1:10){
  random <- sample(c("success","failure"), size=10, replace=TRUE, prob=c(.5,.5))
  x <- length(which(random == "success"))
              myCI <- binom.confint(x, 10, method = "ac")
              print(myCI$upper-myCI$lower)
              }
#speeds up the process to "loop" the iterations of repeating this
#I also streamlined manually punching in x to be automatic (better)
#for one of the iterations, I got a CI span of 0.474! still high tho

for (i in 1:10){
  random <- sample(c("success","failure"), size=20, replace=TRUE, prob=c(.5,.5))
  x <- length(which(random == "success"))
  myCI <- binom.confint(x, 20, method = "ac")
  print(myCI$upper-myCI$lower)
}
#change sample size to 20 and check out the results! Which are...
#ranging from 0.378-0.401, still fairly high but slightly better

for (i in 1:10){
  random <- sample(c("success","failure"), size=100, replace=TRUE, prob=c(.5,.5))
  x <- length(which(random == "success"))
  myCI <- binom.confint(x, 100, method = "ac")
  print(myCI$upper-myCI$lower)
}
#after a few tries it seems that 100 is best sample size for result
#of CI span to be less than 0.2 fairly consistently so CI=0.4-0.6
#I would say we could confidently say here that mean roughly = 0.5

#optional way to speed things up further or if lots of iterations
# first start by initializing vector to store the results in
result <- vector("numeric", length = 10)
for(i in 1:10){
  random <- sample(c("success","failure"), size=100, replace=TRUE, prob=c(.5,.5))
  x <- length(which(random == "success"))
  myCI <- binom.confint(x, 100, method = "ac")
  result[i] <- (myCI$upper-myCI$lower)
}
result
#it is much easier to visualize the results as a vector

result <- vector("numeric", length = 10)
for(i in 1:10){
  random <- sample(c("success","failure"), size=20, replace=TRUE, prob=c(.5,.5))
  x <- length(which(random == "success"))
  myCI <- binom.confint(x, 20, method = "ac")
  result[i] <- (myCI$upper-myCI$lower)
}
result
#aiming for a span of 0.4 rather than 0.2 to be realistic
#20 sample size gives 0.401 as a CI span, 25 gives 0.365 as highest

random <- sample(c("success","failure"), size=20, replace=TRUE, prob=c(.7,.3))
#sample 20 females from population where success = 0.7 this time

x <- length(which(random == "success"))
z <- binom.test(x, 20, p = 0.5)
print(z)
z$p.value
#perform a binomial test on new randomized data, p value = 0.115
#therefore cannot reject the null of mean = 0.5 unfortunately

result <- vector("numeric", length = 10)
for (i in 1:10){
  random <- sample(c("success","failure"), size=20, replace=TRUE, prob=c(.7,.3))
  x <- length(which(random == "success"))
  z <- binom.test(x, 20, p = 0.5)
  result[i] <- z$p.value
  result <- round(result, 3)
}
result
#four out of ten iterations could I reject the null (p > 0.05)

result <- vector("numeric", length = 100)
for (i in 1:100){
  random <- sample(c("success","failure"), size=75, replace=TRUE, prob=c(.7,.3))
  x <- length(which(random == "success"))
  z <- binom.test(x, 75, p = 0.5)
  result[i] <- z$p.value
  result <- round(result, 3)
}
result
#seeing what sample size (~75) you can usually reject the null
#out of 100 iterations, only 2 outliers couldn't reject the null
#it's a slightly smaller sample size than the CI span gave us (100)

library(pwr)
#load pwr to try out some power calculations now

h <- ES.h(0.5, 0.6)
#po (null hypothesis) = 0.5 (no diff), pa (alt. hyp.) = 0.6 (real)

pwr.p.test(h, power = 0.8)
#finds the sample size needed for a power of 0.8 to detect pa=0.6
#result is 194

h <- ES.h(0.5, 0.7)
pwr.p.test(h, power = 0.8)
#repeat for different pa=0.7, 0.8, 0.9... 0.7 result is 46 samples

h <- ES.h(0.5, 0.8)
pwr.p.test(h, power = 0.8)
#result for needed sample size is 19

h <- ES.h(0.5, 0.9)
pwr.p.test(h, power = 0.8)
#result is 9, the higher the real diff, the easier to detect it!

y <- c(0.6, 0.7, 0.8, 0.9)
x <- c(194, 46, 19, 9)
plot(y ~ x, pch=16)
lines(y[order(x)] ~ x[order(x)])
#create a line plot for relationship between sample size and pa
#for a power = 0.8, are the sample sizes realistic? idk! sometimes?

birds <- rep(c("treat","control"), c(30,30))
s1 <- sample(c("infected","healthy"), 30, replace = TRUE, prob = c(.2,.8))
s2 <- sample(c("infected","healthy"), 30, replace = TRUE, prob = c(.5,.5))
mydat <- data.frame(treatment=c(s1,s2), birds, stringsAsFactors = FALSE)
table(mydat)
#randomly sample 30 birds where 0.2 are infected, then 30 from 0.5
#add together in a dataframe and add a variable for treatment
#2x2 frequency table results shows there is an association

barplot(table(mydat), beside=TRUE, legend.text=TRUE, space=c(.1,.3))
#barplot of the results shown in table
#running the simulation a few times shows slightly diff results

library(pwr)
#re-load the relevant library here

h <- ES.h(0.5, 0.2)
pwr.p.test(h, power = 0.8)
#19 is the sample size needed for 0.8 power if i did this right

for (i in seq(from=0.3, to=0.9, by=0.1)){
  h <- ES.h(0.5, i)
  z <- pwr.p.test(h, power = 0.8)
  result[i] <- z$n
}
#figure out what the heck im supposed to do here later, ugh