#Biol 501 Workshop 6 (because fuck Workshop 5) on Likelihood

dbinom(5, 10, 0.5)
#probability of obtaining 5 heads and 5 tails in 10 coin flips=0.25

dbinom(10, 20, 0.512)
#probability of finding 10 boys in 20 kids with p=0.512 for a boy

dist <- vector(length = 6)
for (i in X) {
  X <- seq(0:5)
  dist[i] <- dbinom(X[i], 6, 0.512)
  print(dist)
}

barplot(dist)
#plot distribution of probability of # of boys in a family of 6
#can't get x axis to have numeric label for 1 to 6 but whatever...

dgeom(10, 0.1)
#if probability of dying = 0.1 per year, what is prob of 11yr dead

dead <- vector(length = 6)
for (i in X) {
  X <- seq(0:6)
  dead[i] <- round(dgeom(X[i], 0.1), digits = 5)
}
#tried this but it didn't work because i starts at 1, not zero!
sum(dgeom(seq(0, 5), 0.1))
#gets the right answer! 0.468559 is the proportion dead before 6

dexp(2, 2)
#X is waiting time (2 hours), rate=1/0.5=2 b/c mean time = 0.5 hrs
#answer gives 0.03663 as the probability density for this question

prey <- dexp(seq(0,5), 2)
num <- seq(0, 5)
prob <- data.frame(cbind(num, prey))
prob

library(ggplot2)
ggplot(prob, aes(num, prey))+
  geom_line()
#line plot of exponential probability curve for search time b/t pr

p <- seq(0.01, 0.99, by = 0.01)
loglike <- dbinom(46, 50, p, log = TRUE)

dolla <- data.frame(cbind(p, loglike))

ggplot(dolla, aes(p, loglike))+
  geom_line()

dolla$p[loglike == max(loglike)]

#resume at question 4 under illegal tender and add comments here!