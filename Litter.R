#Towards Cleaner Shores Group Project Data for FISH 507
#Full of ditched graphs and code, organized file elsewhere

#deleted: abbostford, aldergrove, atlin, castlegar, chase, chilliwack,
#coquitlam, cranbrook, crawford, creston, cultus lake, d'arcy, dawson creek,
#duncan, elkford, fernie, fort nelson, fort st john, gellatly, grand forks,
#harrison, harrison mills, hope, invermere, kamloops, kelowna, kitimat,
#langford, langley, lillooet, lumby, madeira park, maple ridge, 
#mission, naramata, nelson, new westminster, oyama, pitt meadows,
#peachland, penticton, port coquitlam, prince george, princeton,
#riondel, salmon arm, shawnigan lake, sicamous, smithers, sparwood, 
#telkwa, terrace, township of langley, vernon, wasa, westbank, whistler,
#& williams lake. fixed typos: herlot bay to heriot bay & QCI to HG!

#trash$Material <- as.factor(trash$Material)
#trash$Source <- as.factor(trash$Source)
#make sure columns are factors so they can be changed
#trash$Material[which(trash$Material=="Rubber")] <- "Other"
#trash$Material[which(trash$Material=="Wood")] <- "Other"
#trash$Source[which(trash$Source=="Medical/hygiene")] <- "Shoreline Rec."
#replace relevant values (small categories moved to bigger ones)
#trash$Material <- as.character(trash$Material)
#trash$Source <- as.character(trash$Source)
#trash$Material <- as.factor(trash$Material)
#trash$Source <- as.factor(trash$Source)
#change to a character and back to a factor to drop levels
#dealt with this manually instead!

setwd("~/Desktop")
#set working directory to find file

trash <- read.csv(file="Ocean_Data_Excel.csv", stringsAsFactors=FALSE,
            strip.white=TRUE, na.strings=c("NA",""))
#read in file

head(trash)
#check the data

library(tidyr)
#load library for data manipulation

library(gridExtra)
#load library for grid.arrange()

library(cowplot)
#load library for nice graphs

debris <- gather(trash, Litter, Number, Cigarettes:Bait_Conta)
#change from wide to long format!

head(debris)
#make sure it worked

categories <- read.csv(file="categories.csv", stringsAsFactors=FALSE,
            strip.white=TRUE, na.strings=c("NA",""))
#read in category file

garbage <- full_join(debris, categories)
#join the files to add category data

head(garbage)
#check that it worked, it did!

garbage$Litter <- as.factor(garbage$Litter)
#change to factor for grouping

levels(garbage$Litter)
#check the levels to make sure they're all there

garbage$Material[which(garbage$Material=="Rubber")] <- "Other"
garbage$Material[which(garbage$Material=="Wood")] <- "Other"
garbage$Source[which(garbage$Source=="Medical/hygiene")] <- "Shoreline Rec."
#changing small categories into appropriate larger categories
#keep code when plotting but delete when performing full analysis *****

yr <- garbage %>%
  group_by(Year, Litter, Source, Material) %>%
  summarise(num=sum(Number))
#summarizing the data by each year

p1 <- ggplot(yr, aes(Year, num))+
  geom_bar(aes(fill=Source), stat="identity")
#quick bar graph on source of litter over time
p2 <- ggplot(yr, aes(Year, num))+
  geom_bar(aes(fill=Material), stat="identity")
#quick bar graph on material of litter over time
plot_grid(p1, p2, nrow=2, align = "v")
#source and material and overall number of litter over time

region <- garbage %>%
  group_by(ED_NAME, Litter, Source, Material, Year) %>%
  summarise(num=sum(Number))
#summarizing the data by each region

ggplot(region, aes(ED_NAME, num))+
  geom_bar(aes(fill=Source), position = "fill", stat="identity")+
  coord_flip()
#quick stacked bar graph on source of litter by region

ggplot(region, aes(ED_NAME, num))+
  geom_bar(aes(fill=Material), position = "fill", stat="identity")+
  coord_flip()
#quick stacked bar graph on material of litter by region

ggplot(region, aes(fct_reorder(ED_NAME, num, sum, na.rm=TRUE), num))+
  geom_bar(aes(fill=Source), stat="identity")+
  coord_flip()
#quick bar graph on source of litter by region, levels reordered

ggplot(region, aes(fct_reorder(ED_NAME, num, sum, na.rm=TRUE), num))+
  geom_bar(aes(fill=Material), stat="identity")+
  coord_flip()
#quick bar graph on material of litter by region, levels reordered

gar <- read.csv(file="Regions.csv", stringsAsFactors=FALSE,
            strip.white=TRUE, na.strings=c("NA",""))
#read in file that groups together regions

van <- full_join(region, gar)
#add on to current working dataframe

head(van)
#check that it worked

van$Region <- as.factor(van$Region)

van$Region <- factor(van$Region, levels(van$Region)[c(1, 3, 2)])

p5 <- ggplot(van, aes(Region, num))+
  geom_bar(aes(fill=Source), stat="identity")
#quick bar graph on source of litter by region

p6 <- ggplot(van, aes(Region, num))+
  geom_bar(aes(fill=Source), position = "fill", stat="identity")
#quick stacked bar graph on source of litter by region

plot_grid(p5, p6, ncol = 2)
#combine two source plots

p7 <- ggplot(van, aes(Region, num))+
  geom_bar(aes(fill=Material), stat="identity")
#quick bar graph on material of litter by region

p8 <- ggplot(van, aes(Region, num))+
  geom_bar(aes(fill=Material), position = "fill", stat="identity")
#quick stacked bar graph on material of litter by region

plot_grid(p7, p8, ncol = 2)
#combine two material plots