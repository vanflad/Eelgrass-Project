#Biol 501 workshop 2 on graphs sept 14th

mydata <- read.csv(file.choose(), stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c(""))
#fixed prev eqn to na.strings="" because NA=North America here

head(mydata)
#see the data, each row is for a different mammal species

table(mydata$continent, mydata$status)
#two most interesting character variables, frequency of cases

mydata$continent[which(mydata$continent == "Af")] <- "AF"
#Fixing a typo from Af to AF to get rid of extra category

logmass <- log10(mydata$mass.grams)
#make a vector with log values of mass

mydata$mass.log <- logmass
#attach the vector to the data frame

str(mydata)
#to check that it worked

mydata$continent[which(mydata$continent == "Oceanic")] <- "OC"
mydata$continent[which(mydata$continent == "Insular")] <- "IN"
#shorten names so they show up on graph too

barplot(table(mydata$continent))
#creates graph to see that IN = highest # sp, OC = smallest # sp

mydata$continent[which(mydata$continent == "OC")] <- "Oceanic"
mydata$continent[which(mydata$continent == "IN")] <- "Insular"
#I jumped the gun and was not supposed to change the names

barplot(table(mydata$continent), cex.names=0.8)
#change label size to fit "oceanic" and "insular" as normal

barplot(table(mydata$continent), cex.names=0.8, col="purple")
#change the color of the graph, very cool

barplot(table(mydata$continent), cex.names=0.8, col="purple", ylab="Frequency")
#label the y axis

barplot(table(mydata$continent), cex.names=0.8, col="purple", ylab="Frequency", ylim=c(0,1600))
#change the limit of y to a higher value

barplot(sort(table(mydata$continent), decreasing = TRUE), cex.names=0.8, col="purple", ylab="Frequency", ylim=c(0,1600))
#change from alphabetical to decreasing frequency of sp.

hist(mydata$mass.grams)
#looks like poop, not informative

hist(mydata$mass.log)
#much better, more informative

hist(mydata$mass.log, col = "purple", right = FALSE, breaks = seq(from=0, to=10, by=2))
#histogram with a bin width of 2

hist(mydata$mass.log, col = "purple", right = FALSE, breaks = seq(from=0, to=10, by=1))
#can vary the bin width to whatever for more or less detail

hist(mydata$mass.log, col = "purple", right = FALSE, breaks = seq(from=0, to=10, by=1), probability = TRUE)
#changes it from frequency to probability density instead

#how does it differ from normal distribution? skewed to the left
qqnorm(mydata$mass.log, col = "purple")
qqline(mydata$mass.log)
#this also differs from normal in the beginning, more informative?

qqnorm(mydata$mass.log, col = "purple", pch=".")
qqline(mydata$mass.log)
#uses a smaller plotting symbol so looks more clean and narrow

hist(mydata$mass.log, col = "purple", right = FALSE, breaks = seq(from=0, to=10, by=1), probability = TRUE)
m <- mean(mydata$mass.log, na.rm = TRUE)
s <- sd(mydata$mass.log, na.rm = TRUE)
xpts <- seq(from = min(mydata$mass.log, na.rm=TRUE), to = max(mydata$mass.log, na.rm = TRUE), length.out = 101)
lines(dnorm(xpts, mean=m, sd=s) ~ xpts, col="red", lwd=2)
#histogram with a normal density curve superimposed for comparing

boxplot(mydata$mass.log ~ mydata$status)
#box plot for log of masses depending on diff extintion status

boxplot(mydata$mass.log ~ mydata$status, cex.axis=0.8)
#now all the labels will fit. Extinct mammals way bigger than living

boxplot(mydata$mass.log ~ mydata$status, cex.axis=0.8, varwidth = TRUE)
#box width proportional to the square root of the sample size

boxplot(mydata$mass.log ~ mydata$status, cex.axis=0.8, varwidth = TRUE, main = "Mass vs. Status")
#add a title to the plot

tapply(mydata$mass.log, INDEX=mydata$status, FUN = median, na.rm=TRUE)
#median value of log(mass) for each status, same as box plot

tapply(mydata$mass.log, INDEX=mydata$status, FUN = mean, na.rm=TRUE)
#mean value, slightly diff than median... b/c of diff sample size??
#5388 extant and 242 extinct. must skew median & mean evens it out

extant <- mydata$mass.log[which(mydata$status == "extant")]
#make a vector as to not confuse the hist function
hist(extant)
#plot a histogram, see that it is skewed to the left
extinct <- mydata$mass.log[which(mydata$status == "extinct")]
#make a vector first as before
hist(extinct)
#plot a histogram to see that it is skewed to the right!
#therefore medians are further away and means relatively closer

table(mydata$continent, mydata$status)
#two way frequency table aka contingency table
#NA most extinct # of sp., AUS most extinct relative to extant sp.

mosaicplot(table(mydata$status,mydata$continent), color = TRUE, las = 2, cex.axis = 0.8)
#mosaic plot, looks funny because of low observations in categories

mosaicplot(table(mydata$continent,mydata$status), color = TRUE, las = 2, cex.axis = 0.8)
#mosaic plot with explanatory/response variables flipped

mydata <- read.csv(file.choose(), stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
#read fruit fly file

head(mydata)
#get familiar with the data, names, etc.

boxplot(mydata$longevity.days ~ mydata$treatment)
#boxplot to examine distribution of longevity in treatment groups

boxplot(mydata$longevity.days ~ mydata$treatment, ylab="Longevity (days)")
#add label to y axis

boxplot(mydata$longevity.days ~ mydata$treatment, ylab="Longevity (days)", cex.axis=0.3)
#do treatment groups differ in longevity? Describe pattern.
#changed axis label size, titles are too long to see...
#pattern is longevity goes down with virgin partners

stripchart(mydata$longevity.days ~ mydata$treatment, vertical=TRUE)
#use strip chart to examine distributions of longevities, treatment

stripchart(mydata$longevity.days ~ mydata$treatment, vertical = TRUE, method = "jitter", jitter = 0.2)
#try jitter method to reduce overlap between points

stripchart(mydata$longevity.days ~ mydata$treatment, vertical = TRUE, method = "jitter", jitter = 0.2, las=2, cex.axis=0.5)
#adjust treatment label sizes so they all fit, rotate labels
#comparing with boxplot, stripchart is more informative

plot(mydata$thorax.mm, mydata$longevity.days)
#scatterplot of thorax and longevity (response var, y axis)
#is there a relationship? yes for sure, almost linear

scatter.smooth(mydata$thorax.mm, mydata$longevity.days)
lines(lowess(mydata$thorax.mm, mydata$longevity.days))
#use the lowess smoother to draw a smooth curve through the plot

plot(mydata$thorax.mm, mydata$longevity.days, pch = as.numeric(factor(mydata$treatment)), col=as.numeric(factor(mydata$treatment)))
#redraw the scatterplot with color or symbols for treatm. groups

legend(locator(1), legend = as.character(levels(factor(mydata$treatment))), pch = 1:length(levels(factor(mydata$treatment))), col = 1:length(levels(factor(mydata$treatment))))
#add a legend and describe the differences
#high for 1 preg, no fem, 8 preg, then ~ lower for 1 virg, 8 virg.

ggplot(mydata, aes(mydata$longevity.days)) + xlab("Male Longevity") + ylab("Frequency") + 
  geom_histogram(closed = "left", breaks = seq(from=10, to=100, by=10), fill = "red") +
  facet_wrap(~mydata$treatment, ncol = 2)
#plot frequency distribution of longevity for all treatment groups
#how easy is it to visualize diff among treatments in distribution?
#fairly easy! Skewed left for low longevity and right for higher

histogram(~ mydata$longevity.days | mydata$treatment, data = mydata, layout = c(1,5), right = FALSE)
#repeat the previous command but stack the panels over one another
#consider how this affects your ability to compare vs side by side
#much easier to compare when plots are stacked on top of each other

#visualize relationships between longevity & size for diff treatm:
#make a multipanel plot for each group, all on the same scale
library(lattice)
#first have to load package
xyplot(mydata$longevity.days ~ mydata$thorax.mm | mydata$treatment)
#scatterplot using lattice
xyplot(mydata$longevity.days ~ mydata$thorax.mm | mydata$treatment, data = mydata, pch = 16, aspect = 0.7)
#make it look nicer
xyplot(mydata$longevity.days ~ mydata$thorax.mm | mydata$treatment, data = mydata, pch = 16, aspect = 0.7,
       panel = function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)})
#add regression line to each plot

library(ggplot2)
# load the ggplot2 package
theme_set(theme_bw())
# to employ the simplest ggplot2 style
ggplot(mydata, aes(mydata$thorax.mm, mydata$longevity.days)) + xlab("Male size") + ylab("Male longevity") +
  geom_point(col = "red", size = I(2)) +
  geom_smooth(method = lm, size = I(1), se = FALSE, col = "black") +
  facet_wrap(~mydata$treatment, ncol = 2)
#definitely easier to see trends than combining in one plot
#note: ggplot is an alternative to lattice, they are similar here