#Towards Cleaner Shores Group Project Data for FISH 507

setwd("~/Desktop")
#set working directory to find file

data <- read.csv(file="Data_WMunic.csv", stringsAsFactors=FALSE,
                  strip.white=TRUE, na.strings=c("NA",""))
#read in main data file (see Litter.R for notes and scrapped code)

categories <- read.csv(file="categories.csv", stringsAsFactors=FALSE,
                       strip.white=TRUE, na.strings=c("NA",""))
#read in category file

area <- read.csv(file="Regions.csv", stringsAsFactors=FALSE,
                       strip.white=TRUE, na.strings=c("NA",""))

library(tidyverse)
#load library for data manipulation

library(cowplot)
#load library for nice graphs

library(scales)
#package for axis label reformatting

library(RColorBrewer)
#color library

data <- filter(data, Province == "British Columbia" & Year > 2007)

litter <- left_join(data, area, by="ED_NAME")

#write_csv(litter, "Data_WRegion.csv")
#write to file to upload to google drive

van <- gather(litter, Litter, Number,
               Cigarettes_Cigarette_Filters:Shotgun_Shells)
#change from wide to long format!

trash <- left_join(van, categories, by="Litter") %>%
  filter(!is.na(Number) & Number!=0 & !is.na(Region))
#join the files to add category data and remove NA and 0 rows

#write_csv(trash, "LongData_WCategories.csv")
#write to file to upload to google drive

View(trash)
#make sure it all worked, will use for plotting

trash$Region <- as.factor(trash$Region)
#make a factor as well to change levels
trash$Region <- factor(trash$Region, levels(trash$Region)[c(1, 3, 2)])
#new order: Van, Van Island, North Coast

yr <- trash %>%
  group_by(Year, Litter, Source, Material) %>%
  summarise(num=sum(Number))
#summarizing the data by each year

yr$Source <- as.factor(yr$Source)

yr$Source <- factor(yr$Source, levels(yr$Source)[c(4, 3, 2, 1)])

yr$Material <- as.factor(yr$Material)

yr$Material <- factor(yr$Material, levels(yr$Material)[c(6, 2, 3, 5, 1, 4)])

p1 <- ggplot(yr, aes(Year, num))+
  geom_bar(aes(fill=Source), stat="identity")+
  labs(x=NULL, y="Number of items", title="Source of Litter over Time")+
  scale_y_continuous(label = comma)+
  scale_x_continuous(limits = c(2007.5, 2016.5), breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016))+
  scale_fill_manual(values = brewer.pal(n=4, "Set1"))
#quick bar graph on source of litter over time
p2 <- ggplot(yr, aes(Year, num))+
  geom_bar(aes(fill=Material), stat="identity")+
  labs(x=NULL, y="Number of items", title="Material of Litter over Time")+
  scale_y_continuous(label = comma)+
  scale_x_continuous(limits = c(2007.5, 2016.5), breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016))+
  scale_fill_manual(values = brewer.pal(n=6, "Dark2"))
#quick bar graph on material of litter over time
plot_grid(p1, p2, nrow=2, align = "v", labels = c("A", "B"))
#source and material and overall number of litter over time
ggsave("Time.pdf")

gar <- trash %>%
  group_by(Region, Litter, Source, Material) %>%
  summarise(num=sum(Number))
#summarizing the data by each region

gar$Source <- as.factor(gar$Source)

gar$Source <- factor(gar$Source, levels(gar$Source)[c(4, 3, 2, 1)])

levels(gar$Source)

p3 <- ggplot(gar, aes(Region, num))+
  geom_bar(aes(fill=Source), stat="identity")+
  labs(x=NULL, y="Number of items", title="Number of Litter by Source")+
  scale_y_continuous(label = comma)+
  scale_x_discrete(labels = c("Greater Van.", "Van. Island", "North Coast"))+
  scale_fill_manual(values = brewer.pal(n=4, "Set1"))+
  theme(legend.position = c(0.6, 0.75))
#quick bar graph on source of litter by region
p4 <- ggplot(gar, aes(Region, num))+
  geom_bar(aes(fill=Source), position = "fill", stat="identity")+
  labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Source")+
  theme(legend.position = "none")+
  scale_y_continuous(labels=scales::unit_format("", 100))+
  scale_x_discrete(labels = c("Greater Van.", "Van. Island", "North Coast"))+
  scale_fill_manual(values = brewer.pal(n=4, "Set1"))
#quick stacked bar graph on source of litter by region
plot_grid(p3, p4, ncol = 2, align = "h", labels = c("A", "B"))
#combine two source plots
ggsave("Source.pdf")

gar$Material <- as.factor(gar$Material)

gar$Material <- factor(gar$Material, levels(gar$Material)[c(6, 2, 3, 5, 1, 4)])

levels(gar$Material)

p5 <- ggplot(gar, aes(Region, num))+
  geom_bar(aes(fill=Material), stat="identity")+
  labs(x=NULL, y="Number of items", title="Number of Litter by Material")+
  scale_y_continuous(label = comma)+
  scale_x_discrete(labels = c("Greater Van.", "Van. Island", "North Coast"))+
  scale_fill_manual(values = brewer.pal(n=6, "Dark2"))+
  theme(legend.position = c(0.6, 0.75))
#quick bar graph on material of litter by region
p6 <- ggplot(gar, aes(Region, num))+
  geom_bar(aes(fill=Material), position = "fill", stat="identity")+
  labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Material")+
  theme(legend.position = "none")+
  scale_y_continuous(labels=scales::unit_format("", 100))+
  scale_x_discrete(labels = c("Greater Van.", "Van. Island", "North Coast"))+
  scale_fill_manual(values = brewer.pal(n=6, "Dark2"))
#quick stacked bar graph on material of litter by region
plot_grid(p5, p6, ncol = 2, labels = c("A", "B"))
#combine two material plots
ggsave("Material.pdf")
?plot_grid
#things still to add/change on plots:
#labels, axis, legend, color, reordering.
#still need to do quantitative analysis too:
#ndms? cluster? anova? simper? pca? bio div index?