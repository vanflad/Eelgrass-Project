mydata <- read.csv(file.choose(), stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
#keep a script file when working in R to refer to later!

#will go through workshop piece by piece, deleting useless steps

log(2)
#can do any calculator functions

x <- 3
#can assign numbers to vectors

z <- "Wake up Neo"
#assign character strings to vectors

y <- 5
z <- x*y
#store result of an operation in a vector

coffee <- "room 236"
#coffee co-op for biodiv building in room 236

z
#or print(z) to print result

2+2==4
#logical operations give TRUE or FALSE

3<=2
#means less than or equal to

"A">"a"
#greater than works on characters too I suppose

"Hi" ! "hi"
#character sensitive

x <- c(1, -2, 3, 40, 5, 66, 777, -88, 9, -10)
#concatenate values into a vector

x[5]
#fifth element

x[1:3]
#values 1 thru 3

x[c(2, 4, 9)]
#2nd, fourth and nineth values

x[c(3, 6)]
#print the 3rd and 6th elements of x with a single command

length(x)
#some functions yield integer results, can be used as indices

x[length(x)]
#gives the last value of x

x>0
#logical operations can be used to generate indices, for example:
x[x>0]

x[x>=0]
#print values that are non-negative
which(x>0)
#also works for same purpose

x[5] <- 0
#change an element within a vector

x[10] <- 5
#change the last value to a diff #

x[c(2, 6, 10)] <- c(1, 2, 3)
#change 2nd, 6th and 10th values to new #s with 1 command

x[2] <- NA
x
#change a value to NA and see the result

y <- seq(5, 50, by=5)
#make a y vector same length as x

z <- x*y
#can calculate an array of numbers, this multiplies each component

z <- y - 2 * x
#doubles x and then is subtracted from y

z <- x >= y
#logical operation to see when x is greater than or equal to y

z <- x[abs(x) < abs(y)]
#absolute values and a logical operation

z <- 2 * x
#single value vector gets recycled for each value of x

mydata <- data.frame(x, y)
#data frame with two vectors

rm(x)
rm(y)
#delete vectors from r environment, they exist only in data frame

names(mydata)
#names of stored variables (x and y)

length(mydata)
#diff answer than the length of vectors

length(mydata$x)
#gives the length of original vector, now column within dataframe

snek <- c(0.9, 1.4, 1.2, 1.2, 1.3, 2.0, 1.4, 1.6)
#put data values into a named vector

hist(snek)
#prints histogram of data

hist(snek, right=FALSE)
#fixes observations by making it left-closed, right-open intervals

snake <- snek*2*pi
#changes from hertz to radians, messed up this step originally

sum(snake)/length(snake)
#gives mean of snake

mean(snake)
#to compare

sqrt(sum(snake-mean(snake))^2)/length(snake-1)
#calculates SD, apparently SD uses n-1 for final mean and not n

sd(snake)
#to compare

sort(snake)
#sorts observations, but doesn't overwrite original w/o assigning

mean(sort(snake)[c(4,5)])
#median

median(snake)
#to compare

sd(snake)/sqrt(length(snake))
#standard error

newsnake <- c(snake, NA)
#adding NA to end of vector

length(newsnake)
#9 rather than 8 because of new NA value

mean(newsnake)
#returns NA

sd(newsnake)/(sqrt(length(newsnake)))
#trying to calculate standard error returns NA as well

sd(na.omit(newsnake))/sqrt(length(na.omit(newsnake)))
#standard error with NAs removed

mydata <- read.csv(file.choose())
#read anolis lizard file

str(mydata)
#see the structure of the data

is.factor(mydata$Island)
#a way to check the type of variable in the data frame

class(mydata$Island)
#returns the type of data, in this case "character"

class(mydata)
#returns data.frame as data type

head(mydata)
#first few rows of the data frame

tail(mydata)
#last few rows

levels(mydata$Ecomorph)
#since ecomorph is a factor, levels returns all the diff groups

table(mydata$Ecomorph)
#see the frequency/number of species belonging to each group

which(mydata$Ecomorph == "Trunk Crown ")
#find where the typo is in the data frame

mydata$Ecomorph[118] <- "Trunk-Crown"
#fixing the typo

which(mydata$Ecomorph == "Trunk Crown ") <- "Trunk Crown"
#another way to fix typo in one command, but untested

table(mydata$Ecomorph)
#see if its fixed but actually typo group just = 0 now

mydata$Ecomorph <- droplevels(mydata$Ecomorph)
#deletes that unused level with the typo

table(mydata$Ecomorph)
#always double check that R did what you want

mydata <- read.csv(file.choose(), stringsAsFactors = FALSE, strip.white=TRUE, na.strings = c("NA",""))
#re-read data file with characters instead of factors, typos fixed
#and it treats empty fields as missing not words with no letters

?read.csv
#question mark before functions gives explanations/help/options

table(mydata$Ecomorph)
#see if problems from before are fixed, they are

table(mydata$Ecomorph, useNA="ifany")
#displays lizard species that don't belong to an ecomorph category

mydata$Ecomorph <- replace(mydata$Ecomorph, is.na(mydata$Ecomorph), "None")
#how to replace the NA values with the category group name "None"

length(which(c(mydata$Island == "Jamaica", mydata$Species == "Anolis")))
#how many Anolis sp. inhabit Jamaica exclusively? 6!

length(grep("Cuba", mydata$Island))
#how many Anolis on Cuba? Not exclusively, some live elsewhere too

#solve: how many sp. belong to each ecomorph on 4 main islands?
newdata <- mydata[grep("Cuba|Jamaica|Hispaniola|Puerto Rico", mydata$Island), ]
#new data frame with only four main islands data

table(newdata[ ,c(2,3)])
#table to see what values are for each island and ecomorph

cuba <- c(6, 15, 15, 1, 7, 14, 5)
hisp <- c(3, 7, 8, 6, 4, 9, 4)
jama <- c(1, 0, 1, 0, 2, 1, 1)
puer <- c(2, 3, 0, 0, 2, 3, 1)
islands <- c(cuba, hisp, jama, puer)
#make vectors with values for each island and their ecomorph sp. #s
#counted manually but i'm sure there is quicker code... using grep?

xmat <- matrix(islands, nrow=4, byrow=TRUE)
#turn it into a matrix with ecomorph column names still unnamed

islandmat <- as.data.frame(xmat, stringsAsFactors=FALSE)
#changing the matrix into a dataframe cause it's easier/better

row.names(islandmat) <- c("Cuba", "Hispaniola", "Jamaica", "Puerto Rico")
print(islandmat)
#ecomorph column names done, giving a complete table for an answer

#what is most frequent ecomorph for species not on 4 big islands?
ecodata <- mydata[ , c(2, 3)]
#create a data frame with only ecomorph and island data

newislands <- ecodata$Island
ecomorphs <- ecodata$Ecomorph
newmat <- matrix(c(newislands, ecomorphs))
#make a matrix with island and ecomorph data, only values no names

neweco <- ecodata[(grep("Cuba|Hispaniola|Jamaica|Puerto Rico", newmat, invert=TRUE)), ]
#new data frame with four main islands removed

table(neweco$Ecomorph)
#use table to find answer: Trunk-Crown is most frequent ecomorph
#unsure if quickest way to code that answer but oh well, I got it!