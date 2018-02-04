#Towards Cleaner Shores Group Project Data for FISH 507

##### SET-UP #####

rm(list=ls())
#remove any extra stuff floating around R environment

setwd("~/Desktop/2017-2018/FISH 507")
#set working directory to find file

data <- read.csv(file="GCSC_Data_22NOV2017.csv", stringsAsFactors=FALSE,
								 strip.white=TRUE, na.strings=c("NA",""))
#read in main data file (see Litter.R for notes and scrapped code)

categories <- read.csv(file="categories.csv", stringsAsFactors=FALSE,
											 strip.white=TRUE, na.strings=c("NA",""))
#read in category file

library(tidyverse)
#load library for data manipulation

library(cowplot)
#load library for nice graphs

library(scales)
#package for axis label reformatting

library(RColorBrewer)
#color library

library(forcats)
#for factor reordering

library(vegan)
#for nmds and cluster analysis

#library(kableExtra)
#for making nice tables

#library(ggdendro)
#load library for nice dendrograms

library(stringr)
#for string manipulating

litter <- gather(data, Litter, Number, Cigarettes:Plastic_Pi) %>%
  filter(Number != 0, is.na(Number) != TRUE)
#change from wide to long format!

trash <- left_join(litter, categories, by="Litter") %>%
	filter(!is.na(Number) & Number!=0) %>%
	droplevels()
#join the files to add category data and remove NA and 0 rows

#View(litter)
#View(trash)
#make sure it all worked, will use for plotting!

#trash %>%
#	filter(Region=="4") %>%
#	group_by(Litter) %>%
#	summarise(num=sum(Number)) %>%
#	arrange(num) %>%
#	View()

#write_csv(trash, "trash.csv")

##### TRASH OVER TIME PLOT #####

yr <- trash %>%
	group_by(Year_, Litter, Source, Material) %>%
	summarise(num=sum(Number))
#summarizing the data by each year

yr$Material[which(yr$Material=="Wood")] <- "Other"
#get rid of the too small to see on graph category for wood
yr$Material[which(yr$Material=="Rubber")] <- "Other"
#get rid of the too small to see on graph category for rubber
yr$Source[which(yr$Source=="Medical/Hygiene")] <- "Shoreline Rec."
#get rid of the too small to see on graph category for medical/hygiene

yr$Source <- as.factor(yr$Source)
#changing source to a factor for level reordering

levels(yr$Source)
#check the original levels to find out order numbering

yr$Source <- factor(yr$Source, levels(yr$Source)[c(4, 3, 2, 1)])
#relevel the factors to where needed

yr$Material <- as.factor(yr$Material)
#do the same for material as well as source

levels(yr$Material)
#check levels

yr$Material <- factor(yr$Material, levels(yr$Material)[c(7, 6, 2, 3, 5, 1, 4)])
#change levels

p1 <- ggplot(yr, aes(Year_, num))+
	geom_bar(aes(fill=Source), stat="identity")+
	labs(x=NULL, y="Number of items", title="Source of Litter over Time")+
	scale_y_continuous(label = comma)+
	scale_x_continuous(limits = c(2012.5, 2016.5), breaks = c(2013, 2014, 2015, 2016))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))
#quick bar graph on source of litter over time
p2 <- ggplot(yr, aes(Year_, num))+
	geom_bar(aes(fill=Material), stat="identity")+
	labs(x=NULL, y="Number of items", title="Material of Litter over Time")+
	scale_y_continuous(label = comma)+
	scale_x_continuous(limits = c(2012.5, 2016.5), breaks = c(2013, 2014, 2015, 2016))+
	scale_fill_manual(values = brewer.pal(n=8, "Dark2"))
#quick bar graph on material of litter over time
plot_grid(p1, p2, nrow=2, align = "v", labels = c("A", "B"))
#source and material and overall number of litter over time

##### TRASH PROFILE PLOTS #####

trash$Region <- as.factor(trash$Region)
#make sure the 1-4 system for regions are factors not numbers

gar <- trash %>%
	group_by(Nearest_Ci, Region, Litter, Source, Material) %>%
	summarise(num=sum(Number))
#summarizing the data by each region

gar$Source[which(gar$Source=="Shoreline Rec.")] <- "Recreation"
#gar$Source[which(gar$Source=="Medical/Hygiene")] <- "Hygiene"
gar$Source[which(gar$Source=="Medical/Hygiene")] <- "Dumping"

gar$Source <- as.factor(gar$Source)
#exact same process as above, but for gar dataset now

gar$Source <- factor(gar$Source, levels(gar$Source)[c(4, 3, 2, 1)])
#change source levels

levels(gar$Source)
#check that it worked

gar$Region <- as.character(gar$Region)
#change to characters before renaming!

gar$Region[which(gar$Region=="1")] <- "NCBC"
gar$Region[which(gar$Region=="2")] <- "ICVI"
gar$Region[which(gar$Region=="3")] <- "WCVI"
gar$Region[which(gar$Region=="4")] <- "SSOG"
#rename regions to something meaningful

gar$Region <- as.factor(gar$Region)
#change regions to factors for this dataframe too

gar$Region <- factor(gar$Region, levels(gar$Region)[c(2, 1, 4, 3)])
#reorder levels from alphabetical to the correct order of regions

levels(gar$Region)
#check the levels

p3 <- ggplot(gar, aes(Region, num))+
	geom_bar(aes(fill=Source), stat="identity")+
	labs(x=NULL, y="Number of items", title="Number of Litter by Source")+
	scale_y_continuous(label = comma)+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	theme(legend.position = c(0.5, 0.6))+
	coord_flip()
#quick bar graph on source of litter by region
p4 <- ggplot(gar, aes(Region, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
	labs(x="Region", y=NULL, title="Source of Litter (by count)")+
	theme(legend.position = "right", axis.title.x = element_text(size=14),
	      axis.text.y = element_text(size = 14), axis.title = element_text(size=14),
	      legend.text = element_text(size=14), title = element_text(size=16),
	      legend.title = element_text(size=14), axis.text.x = element_text(size=14))+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	scale_fill_manual(values = brewer.pal(n=4, "Set1"))
#quick stacked bar graph on source of litter by region
plot_grid(p3, p4, ncol = 2, align = "h", labels = c("A", "B"))
#combine two source plots
#note that you fucked up the labels and got confused, don't do that shit again

gar$Material[which(gar$Material=="Cloth")] <- "Other"
#get rid of the too small to see on graph category for cloth

gar$Material[which(gar$Material=="Rubber")] <- "Other"
#get rid of the too small to see on graph category for rubber

gar$Material <- as.factor(gar$Material)
#change from character to factor

gar$Material <- factor(gar$Material, levels(gar$Material)[c(5, 1, 2, 4, 3)])
#change the level order of materials for this plot

levels(gar$Material)
#check that it worked out right

p5 <- ggplot(gar, aes(Region, num))+
	geom_bar(aes(fill=Material), stat="identity")+
	labs(x=NULL, y="Number of items", title="Number of Litter by Material")+
	theme(legend.position = c(0.5, 0.5))+
	scale_y_continuous(label = comma)+
	scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
	coord_flip()
#quick bar graph on material of litter by region
p6 <- ggplot(gar, aes(Region, num))+
	geom_bar(aes(fill=Material), position = "fill", stat="identity")+
	labs(x="Region", y=NULL, title="Material of Litter (by count)")+
	scale_y_continuous(labels=scales::unit_format("%", 100))+
	scale_fill_manual(values = brewer.pal(n=5, "Dark2"))+
	theme(legend.position = "right", axis.title.x = element_text(size=14),
	      axis.text.y = element_text(size = 14), axis.title = element_text(size=14),
	      legend.text = element_text(size=14), title = element_text(size=16),
	      legend.title = element_text(size=14), axis.text.x = element_text(size=14))
#quick stacked bar graph on material of litter by region
plot_grid(p5, p6, ncol = 2, labels = c("A", "B"))
#combine two material plots

plot_grid(p4, p6, ncol = 2, labels = c("A", "B"))
#MAAAAAIN PLOT WE CARE ABOUT IS THIS ONE HERE!#

##### EXTRA STUFF/WHATEVER/GET RID OF #####
#make simple dataframes for pie chart map!
pie <- gar %>%
	ungroup() %>% 
	select(-Litter) %>%
	group_by(Region, Source, Material) %>%
	summarise(num = sum(num))

pies <- pie %>%
	select(Region, Source, num) %>%
	summarise(num=sum(num)) %>% 
	spread(Source, num)

piem <- pie %>%
	ungroup() %>% 
	select(Region, Material, num) %>%
	group_by(Region, Material) %>%
	summarise(num=sum(num)) %>% 
	spread(Material, num)

write_csv(pies, "summed-source-data.csv")
write_csv(piem, "summed-material-data.csv")
#write them to file to give to Cassandra!

#Trash profiles over time for each region:
newgar <- trash %>%
	group_by(Year_, Region, Litter, Source, Material) %>%
	summarise(num=sum(Number))
#summarizing the data by each region

newgar$Source <- as.factor(newgar$Source)
#exact same process as above, but for gar dataset now

newgar$Source <- factor(newgar$Source, levels(newgar$Source)[c(5, 4, 2, 1, 3)])
#change source levels

levels(newgar$Source)
#check that it worked

newgar$Region <- as.factor(newgar$Region)
#change regions to factors for this dataframe too

levels(newgar$Region)
#check the levels

newgar$Material[which(newgar$Material=="Wood")] <- "Other"
#get rid of the too small to see on graph category for wood

newgar$Material[which(newgar$Material=="Cloth")] <- "Other"
#get rid of the too small to see on graph category for wood

newgar$Material[which(newgar$Material=="Rubber")] <- "Other"
#get rid of the too small to see on graph category for wood

newgar$Material <- as.factor(newgar$Material)
#change from character to factor

newgar$Material <- factor(newgar$Material, levels(newgar$Material)[c(7, 6, 2, 3, 5, 1, 8, 4)])
#change the level order of materials for this plot

levels(newgar$Material)
#check that it worked out right

gar3 <- filter(newgar, Year_=="2013")
gar4 <- filter(newgar, Year_=="2014")
gar5 <- filter(newgar, Year_=="2015")
gar6 <- filter(newgar, Year_=="2016")

p7 <- ggplot(gar3, aes(Region, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Source")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	theme(legend.position = "none")+
	coord_flip()
p8 <- ggplot(gar4, aes(Region, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Source")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	theme(legend.position = "none")+
	coord_flip()
p9 <- ggplot(gar5, aes(Region, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Source")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	theme(legend.position = "none")+
	coord_flip()
p10 <- ggplot(gar6, aes(Region, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Source")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	theme(legend.position = "none")+
	coord_flip()
plot_grid(p7, p8, p9, p10, ncol = 2, labels = c("2013", "2014", "2015", "2016"))
#SOURCE

p11 <- ggplot(gar3, aes(Region, num))+
	geom_bar(aes(fill=Material), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Material")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
	theme(legend.position = "none")+
	coord_flip()
p12 <- ggplot(gar4, aes(Region, num))+
	geom_bar(aes(fill=Material), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Material")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
	theme(legend.position = "none")+
	coord_flip()
p13 <- ggplot(gar5, aes(Region, num))+
	geom_bar(aes(fill=Material), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Material")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
	theme(legend.position = "none")+
	coord_flip()
p14 <- ggplot(gar6, aes(Region, num))+
	geom_bar(aes(fill=Material), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Material")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
	theme(legend.position = "none")+
	coord_flip()
plot_grid(p11, p12, p13, p14, ncol = 2, labels = c("2013", "2014", "2015", "2016"))
#MATERIAL

gar %>%
	filter(Region=="3") %>%
	ggplot(aes(Nearest_Ci, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
					 	labs(x=NULL, y="Proportion of items (%)", title="WCVI (3) Source")+
					 	scale_y_continuous(labels=scales::unit_format("", 100))+
					 	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
					 	#theme(legend.position = "none")+
					 	coord_flip()

gar %>%
  filter(Region=="3") %>%
  ggplot(aes(Nearest_Ci, num))+
  geom_bar(aes(fill=Material), position = "fill", stat="identity")+
  labs(x=NULL, y="Proportion of items (%)", title="WCVI (3) Material")+
  scale_y_continuous(labels=scales::unit_format("", 100))+
  scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
  #theme(legend.position = "none")+
  coord_flip()

gar %>%
	filter(Region=="4") %>%
	ggplot(aes(Nearest_Ci, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Van/Vic (4) Source")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	#theme(legend.position = "none")+
	coord_flip()

gar %>%
  filter(Region=="4") %>%
  ggplot(aes(Nearest_Ci, num))+
  geom_bar(aes(fill=Material), position = "fill", stat="identity")+
  labs(x=NULL, y="Proportion of items (%)", title="Van/Vic (4) Material")+
  scale_y_continuous(labels=scales::unit_format("", 100))+
  scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
  #theme(legend.position = "none")+
  coord_flip()

gar %>%
	filter(Region=="1") %>%
	ggplot(aes(Nearest_Ci, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="North (1) Source")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	#theme(legend.position = "none")+
	coord_flip()

gar %>%
  filter(Region=="1") %>%
  ggplot(aes(Nearest_Ci, num))+
  geom_bar(aes(fill=Source), position = "fill", stat="identity")+
  labs(x=NULL, y="Proportion of items (%)", title="North (1) Material")+
  scale_y_continuous(labels=scales::unit_format("", 100))+
  scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
  #theme(legend.position = "none")+
  coord_flip()

gar %>%
	filter(Region=="2") %>%
	ggplot(aes(Nearest_Ci, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="JS/NSoG (2) Source")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	#theme(legend.position = "none")+
	coord_flip()

gar %>%
  filter(Region=="2") %>%
  ggplot(aes(Nearest_Ci, num))+
  geom_bar(aes(fill=Material), position = "fill", stat="identity")+
  labs(x=NULL, y="Proportion of items (%)", title="JS/NSoG (2) Material")+
  scale_y_continuous(labels=scales::unit_format("", 100))+
  scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
  #theme(legend.position = "none")+
  coord_flip()

gar %>%
	filter(Nearest_Ci=="Courtenay") %>%
	ggplot(aes(Year_, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Source")+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	theme(legend.position = "none")+
	coord_flip()
#only proportions! for material and source!

##### NMDS! #####

ecoyr <- litter %>%
	filter(!is.na(Number) & Number!=0) %>%
	select(Region, Year_, Litter, Number) %>%
	group_by(Region, Year_, Litter) %>%
	summarise(num=sum(Number)) %>%
	spread(Litter, num, fill = 0)
#this is total items per site per year, need to calc proportions!

mat <- ecoyr %>%
	ungroup() %>%
	select(Appliances:Toys)
#create a dataframe with only litter categories

eco <- ecoyr$Region
#create a vector for ecosection info to be reattached after calculations

Total <- vector(length = nrow(mat))
#create an empty vector

Total <- rowSums(mat)
#fill that vector with calculated row totals (total per cleanup)

mat <- cbind(mat, Total)
#add that vector as a column to the matrix

proportions <- data.frame()
#create empty dataframe to fill with transformed data!

proportions <- mat/mat$Total*100
#divide entire dataframe by row totals (total column should all = 1)

mat1 <- cbind(eco, proportions)
#reattach the ecosection labels now that total calculation is done

smallermat <- mat1 %>%
#	filter(eco != "Dogfish Bank Frontal Region" & eco != "Eastern Queen Charlotte Sound" & eco != "Cape Scott Tidal Mixing" & eco != "Southern Strait of Georgia") %>%
	select(-Total)
#filter those out with less than three datapoints and drop total=1 column!

smallermat <- droplevels(smallermat)
#get rid of ditched levels

smalleco <- smallermat$eco
#make vector for ecosections as before

smalleco <- as.factor(smalleco)
#make factor to avoid weird errors because they're numbers

#create a matrix with ecosections as row names
matrix2<-as.matrix(smallermat)
row.names(matrix2) <- matrix2[,1]
new.matrix <- matrix2[,-1]
class(new.matrix)<-"numeric"

rankindex(smalleco, new.matrix, indices = c("euc", "man", "gow", "bra", "kul"))
#now it says euclidean is best and tied for second are bray, man and kul!

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(new.matrix,distance="bray",labels=smalleco, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

#region, proportion based dissimilarity - euclidean
eco.nmds.eu<- metaMDS(new.matrix,distance="euclidean",labels=smalleco, trymax = 100, autotransform = FALSE)
eco.nmds.eu
plot(eco.nmds.eu)
#found convergence easily (<20), stress is rel. low = 0.05774715

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(new.matrix ~ smalleco, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

permanova_eco.eu<-adonis(new.matrix ~ smalleco, permutations = 999, method="euclidean")
permanova_eco.eu #significant! p = 0.001 so plot it (has lower mean sqs and sum of sqs tho)

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=smalleco)
#plot NMDS, only once (picking Bray because all similar), no presence absence

NMDS.eu<-data.frame(NMDS1.eu=eco.nmds.eu$points[,1],NMDS2.eu=eco.nmds.eu$points[,2],group=smalleco)
#plot NMDS, only once (picking Gower because bray can't converge), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,smalleco,display="sites",kind="sd", conf = 0.95, label=T)
#Ellipses are standard deviation, no scaling of data (can use standard error, scaling, and confidence limit options)

ord.eu<-ordiellipse(eco.nmds.eu,smalleco,display="sites",kind="sd", conf = 0.95, label=T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
	theta <- (0:npoints) * 2 * pi/npoints
	Circle <- cbind(cos(theta), sin(theta))
	t(center + scale * t(Circle %*% chol(cov)))
}

df_ell.bc <- data.frame()
for(g in levels(NMDS.bc$group)){
	df_ell.bc <- rbind(df_ell.bc, cbind(as.data.frame(with(NMDS.bc[NMDS.bc$group==g,],
																												 veganCovEllipse(ord.bc[[g]]$cov,ord.bc[[g]]$center))),group=g))
}

df_ell.eu <- data.frame()
for(g in levels(NMDS.eu$group)){
	df_ell.eu <- rbind(df_ell.eu, cbind(as.data.frame(with(NMDS.eu[NMDS.eu$group==g,],
																												 veganCovEllipse(ord.eu[[g]]$cov,ord.eu[[g]]$center))),group=g))
}

ggplot(NMDS.bc, aes(NMDS1.bc, NMDS2.bc))+
	geom_point(stat = "identity", aes(fill=group), size=3, shape=21, color = "black")+
	geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
	scale_fill_manual(values=c("#984ea3", "#4daf4a", "#377eb8", "#ff7f00"),
										name="Region", guide="legend") +
	guides(fill= guide_legend(override.aes = list(size=4)))+
	scale_colour_manual(values=c("#984ea3", "#4daf4a", "#377eb8", "#ff7f00"),
											guide=FALSE) +
	scale_y_continuous(limits=c(-0.8,0.6),breaks=seq(-0.8,0.6,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
	scale_x_continuous(limits=c(-0.9,0.7),breaks=seq(-0.9,0.7,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
	theme_bw()+
	theme(axis.text.x=element_text(size=12),
				axis.title.x=element_text(size=12),
				axis.title.y=element_text(angle=90,size=12),
				axis.text.y=element_text(size=12),
				panel.grid.minor=element_blank(),panel.grid.major=element_blank()) + coord_fixed() +
	annotate("text",x=-0.75,y=0.6,label="stress = 0.07",size=4)
#NMDS graph for the different ecosections!

##### CLUSTER TIME #####

ecosite <- litter %>%
	filter(!is.na(Number) & Number!=0) %>%
	select(Region, Litter, Number) %>%
	group_by(Region, Litter) %>%
	summarise(num=sum(Number)) %>%
	spread(Litter, num, fill = 0)
#this is total items per site, need to calc proportions!

ecomat <- ecosite %>%
	ungroup() %>%
	select(Appliances:Toys)
#create a dataframe with only litter categories

ecoinfo <- ecosite$Region
#create a vector for ecosection info to be reattached after calculations

total <- vector(length = nrow(ecomat))
#create an empty vector

total <- rowSums(ecomat)
#fill that vector with calculated row totals (total per cleanup)

mat <- cbind(ecomat, total)
#add that vector as a column to the matrix

proportion <- data.frame()
#create empty dataframe to fill with transformed data!

proportion <- mat/mat$total*100
#divide entire dataframe by row totals for proportions (%)

neweco <- cbind(ecoinfo, proportion)
#reattach the ecosection labels now that total calculation is done

neweco <- select(neweco, -total)
#still need to drop total column

View(neweco)
#check that it looks as it should

#create a matrix with ecosections as row names
fillermat<-as.matrix(neweco)
row.names(fillermat) <- fillermat[,1]
clustmat <- fillermat[,-1]
class(clustmat)<-"numeric"

rankindex(ecoinfo, clustmat, indices = c("euc", "man", "gow", "bra", "kul"))
#not working for some reason! just fuck around with all different distance measures

eudist <- vegdist(clustmat, method = "euclidean")
euclust <- hclust(eudist)
plot(euclust, labels=ecoinfo)

kudist <- vegdist(clustmat, method = "kulczynski")
kuclust <- hclust(kudist)
plot(kuclust, labels=ecoinfo)

Bray_Curtis_Dissimilarity <- vegdist(clustmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
plot(bcclust, labels=ecoinfo)
abline(0.46, 0, col="red")
#cluster for proportion of trash items

dendr <- dendro_data(bcclust, type = "rectangle")

ggplot()+
	geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
	geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=3) +
	coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
	theme(axis.line.y=element_blank(),
				axis.ticks.y=element_blank(),
				axis.text.y=element_blank(),
				axis.title.y=element_blank(),
				panel.background=element_rect(fill="white"),
				panel.grid=element_blank())

##### CLUSTERS BY YEAR #####

ecoyr <- litter %>%
	filter(!is.na(Number) & Number!=0) %>%
	select(Region, Litter, Number, Year_) %>%
	group_by(Region, Litter, Year_) %>%
	summarise(num=sum(Number)) %>%
	spread(Litter, num, fill = 0)
#this is total items per site, need to calc proportions!

#filter out each year to calculate clusters
eco13 <- ecoyr %>%
	filter(Year_=="2013")

eco14 <- ecoyr %>%
	filter(Year_=="2014")

eco15 <- ecoyr %>%
	filter(Year_=="2015")

eco16 <- ecoyr %>%
	filter(Year_=="2016")

#create a dataframe with only litter categories
mat13 <- eco13 %>%
	ungroup() %>%
	select(Appliances:Toys)

mat14 <- eco14 %>%
	ungroup() %>%
	select(Appliances:Toys)

mat15 <- eco15 %>%
	ungroup() %>%
	select(Appliances:Toys)

mat16 <- eco16 %>%
	ungroup() %>%
	select(Appliances:Toys)

#create a vector for ecosection info to be reattached after calculations
info13 <- eco13$Region

info14 <- eco14$Region

info15 <- eco15$Region

info16 <- eco16$Region

#create an empty vector
total13 <- vector(length = nrow(mat13))

total14 <- vector(length = nrow(mat14))

total15 <- vector(length = nrow(mat15))

total16 <- vector(length = nrow(mat16))

#fill that vector with calculated row totals (total per cleanup)
total13 <- rowSums(mat13)

total14 <- rowSums(mat14)

total15 <- rowSums(mat15)

total16 <- rowSums(mat16)

#add that vector as a column to the matrix
mat13 <- cbind(mat13, total13)

mat14 <- cbind(mat14, total14)

mat15 <- cbind(mat15, total15)

mat16 <- cbind(mat16, total16)

#create empty dataframe to fill with transformed data!
proportion13 <- data.frame()

proportion14 <- data.frame()

proportion15 <- data.frame()

proportion16 <- data.frame()

#divide entire dataframe by row totals for proportions (%)
proportion13 <- mat13/mat13$total*100

proportion14 <- mat14/mat14$total*100

proportion15 <- mat15/mat15$total*100

proportion16 <- mat16/mat16$total*100

#reattach the ecosection labels now that total calculation is done
neweco13 <- cbind(info13, proportion13)

neweco14 <- cbind(info14, proportion14)

neweco15 <- cbind(info15, proportion15)

neweco16 <- cbind(info16, proportion16)

#still need to drop total column
neweco13 <- select(neweco13, -total13)

neweco14 <- select(neweco14, -total14)

neweco15 <- select(neweco15, -total15)

neweco16 <- select(neweco16, -total16)

#check that it looks as it should
head(neweco13)

head(neweco14)

head(neweco15)

head(neweco16)

#create a matrix with ecosections as row names
fillermat<-as.matrix(neweco13)
row.names(fillermat) <- fillermat[,1]
clustmat13 <- fillermat[,-1]
class(clustmat13)<-"numeric"

fillermat<-as.matrix(neweco14)
row.names(fillermat) <- fillermat[,1]
clustmat14 <- fillermat[,-1]
class(clustmat14)<-"numeric"

fillermat<-as.matrix(neweco15)
row.names(fillermat) <- fillermat[,1]
clustmat15 <- fillermat[,-1]
class(clustmat15)<-"numeric"

fillermat<-as.matrix(neweco16)
row.names(fillermat) <- fillermat[,1]
clustmat16 <- fillermat[,-1]
class(clustmat16)<-"numeric"

#see which dist works best
rankindex(ecoinfo, clustmat13, indices = c("euc", "man", "gow", "bra", "kul"))
#euc
rankindex(ecoinfo, clustmat14, indices = c("euc", "man", "gow", "bra", "kul"))
#gow
rankindex(ecoinfo, clustmat15, indices = c("euc", "man", "gow", "bra", "kul"))
#bray/kul/man
rankindex(ecoinfo, clustmat16, indices = c("euc", "man", "gow", "bra", "kul"))
#bray/kul/man

dist13 <- vegdist(clustmat13, method = "bray")
clust13 <- hclust(dist13)
a <- plot(clust13, labels=info13)

dist14 <- vegdist(clustmat14, method = "gower")
clust14 <- hclust(dist14)
b <- plot(clust14, labels=info14)

dist15 <- vegdist(clustmat15, method = "bray")
clust15 <- hclust(dist15)
c <- plot(clust15, labels=info15)

dist16 <- vegdist(clustmat16, method = "bray")
clust16 <- hclust(dist16)
d <- plot(clust16, labels=info16)
#clusters for proportion of trash items by year

#can't use plot_grid for non-ggplot plots unfortunately, hmm...

##### NMDS BY YEAR #####

yr3 <- litter %>%
  filter(!is.na(Number) & Number!=0 & Year_=="2013") %>%
  select(Region, Municipali, Litter, Number) %>%
  group_by(Region, Municipali, Litter) %>%
  summarise(num=sum(Number)) %>%
  spread(Litter, num, fill = 0)
#this is total items per site per year, need to calc proportions!

mat3 <- yr3 %>%
  ungroup() %>%
  select(Appliances:Toys)
#create a dataframe with only litter categories

eco3 <- yr3$Region
#create a vector for ecosection info to be reattached after calculations

Total3 <- vector(length = nrow(mat3))
#create an empty vector

Total3 <- rowSums(mat3)
#fill that vector with calculated row totals (total per cleanup)

mat3 <- cbind(mat3, Total3)
#add that vector as a column to the matrix

proportions3 <- data.frame()
#create empty dataframe to fill with transformed data!

proportions3 <- mat3/mat3$Total3*100
#divide entire dataframe by row totals (total column should all = 1)

mat13 <- cbind(eco3, proportions3)
#reattach the ecosection labels now that total calculation is done

smallmat3 <- mat13 %>%
  #	filter(eco != "Dogfish Bank Frontal Region" & eco != "Eastern Queen Charlotte Sound" & eco != "Cape Scott Tidal Mixing" & eco != "Southern Strait of Georgia") %>%
  select(-Total3)
#filter those out with less than three datapoints and drop total=1 column!

smallmat3 <- droplevels(smallmat3)
#get rid of ditched levels

smalleco3 <- smallmat3$eco3
#make vector for ecosections as before

smalleco3 <- as.factor(smalleco3)
#make factor to avoid weird errors because they're numbers

#create a matrix with ecosections as row names
matrix3<-as.matrix(smallmat3)
row.names(matrix3) <- matrix3[,1]
new.matrix3 <- matrix3[,-1]
class(new.matrix3)<-"numeric"

rankindex(smalleco3, new.matrix3, indices = c("euc", "man", "gow", "bra", "kul"))
#now it says euclidean is best and tied for second are bray, man and kul!

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc3<- metaMDS(new.matrix3,distance="bray",labels=smalleco3, trymax = 100, autotransform = FALSE)
eco.nmds.bc3
plot(eco.nmds.bc3)
#found convergence easily (<20), stress is rel. low = 0.06764617

#region, proportion based dissimilarity - kulczynski
eco.nmds.ku3<- metaMDS(new.matrix3,distance="kulczynski",labels=smalleco3, trymax = 100, autotransform = FALSE)
eco.nmds.ku3
plot(eco.nmds.ku3)
#found convergence easily (<20), stress is rel. low = 0.05774715

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc3<-adonis(new.matrix3 ~ smalleco3, permutations = 999, method="bray")
permanova_eco.bc3 #significant! p = 0.001 so plot it

permanova_eco.ku3<-adonis(new.matrix3 ~ smalleco3, permutations = 999, method="kulczynski")
permanova_eco.ku3 #significant! p = 0.001 so plot it (has lower mean sqs and sum of sqs tho)

NMDS.bc3<-data.frame(NMDS1.bc3=eco.nmds.bc3$points[,1],NMDS2.bc3=eco.nmds.bc3$points[,2],group=smalleco3)
#plot NMDS, only once (picking Bray because all similar), no presence absence

NMDS.ku3<-data.frame(NMDS1.ku3=eco.nmds.ku3$points[,1],NMDS2.ku3=eco.nmds.ku3$points[,2],group=smalleco3)
#plot NMDS, only once (picking Gower because bray can't converge), no presence absence

ord.bc3<-ordiellipse(eco.nmds.bc3,smalleco3,display="sites",kind="sd", conf = 0.95, label=T)
#Ellipses are standard deviation, no scaling of data (can use standard error, scaling, and confidence limit options)

ord.ku3<-ordiellipse(eco.nmds.ku3,smalleco3,display="sites",kind="sd", conf = 0.95, label=T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell.bc3 <- data.frame()
for(g in levels(NMDS.bc3$group)){
  df_ell.bc3 <- rbind(df_ell.bc3, cbind(as.data.frame(with(NMDS.bc3[NMDS.bc3$group==g,],
                                                         veganCovEllipse(ord.bc3[[g]]$cov,ord.bc3[[g]]$center))),group=g))
}

df_ell.ku3 <- data.frame()
for(g in levels(NMDS.ku3$group)){
  df_ell.ku3 <- rbind(df_ell.ku3, cbind(as.data.frame(with(NMDS.ku3[NMDS.ku3$group==g,],
                                                         veganCovEllipse(ord.ku3[[g]]$cov,ord.ku3[[g]]$center))),group=g))
}

ggplot(NMDS.bc3, aes(NMDS1.bc3, NMDS2.bc3))+
  geom_point(stat = "identity", aes(fill=group), size=3, shape=21, color = "black")+
  geom_path(data=df_ell.bc3, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_fill_manual(values=c("rosybrown4", "magenta2", "goldenrod1", "tomato", "green", "darkorchid4", "lightslateblue", "lightblue"),
                    name="Ecosection", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  scale_colour_manual(values=c("rosybrown4", "magenta2", "goldenrod1", "tomato", "green", "darkorchid4", "lightslateblue", "lightblue"),
                      guide=FALSE) +
  scale_y_continuous(limits=c(-1,1.5),breaks=seq(-1,1.5,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
  scale_x_continuous(limits=c(-3,1),breaks=seq(-3,1,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()) + coord_fixed() +
  annotate("text",x=-0.75,y=1,label="stress = 0.11",size=4)
#NMDS graph for the different regions, 2013

one4 <- litter %>%
  filter(Year_=="2014", Region=="1") %>%
  select(Region, ED_NAME, Site_Name, Litter, Number) %>%
  group_by(Region, Site_Name, Litter) %>%
  summarise(num=sum(Number)) %>%
  spread(Litter, num, fill = 0)

rest4 <- litter %>%
  filter(Year_=="2014", Region!="1") %>%
  select(Region, ED_NAME, Site_Name, Litter, Number) %>%
  group_by(Region, ED_NAME, Litter) %>%
  summarise(num=sum(Number)) %>%
  spread(Litter, num, fill = 0)

yr4 <- rbind(one4, rest4)
#this is total items per site per year, need to calc proportions!

yr4$Fishing_Lu[which(is.na(yr4$Fishing_Lu))] <- 0

mat4 <- yr4 %>%
  ungroup() %>%
  select(Appliances:Toys, Fishing_Lu)
#create a dataframe with only litter categories

eco4 <- yr4$Region
#create a vector for ecosection info to be reattached after calculations

Total4 <- vector(length = nrow(mat4))
#create an empty vector

Total4 <- rowSums(mat4)
#fill that vector with calculated row totals (total per cleanup)

mat4 <- cbind(mat4, Total4)
#add that vector as a column to the matrix

proportions4 <- data.frame()
#create empty dataframe to fill with transformed data!

proportions4 <- mat4/mat4$Total4*100
#divide entire dataframe by row totals (total column should all = 1)

mat14 <- cbind(eco4, proportions4)
#reattach the ecosection labels now that total calculation is done

smallmat4 <- mat14 %>%
  #	filter(eco != "Dogfish Bank Frontal Region" & eco != "Eastern Queen Charlotte Sound" & eco != "Cape Scott Tidal Mixing" & eco != "Southern Strait of Georgia") %>%
  select(-Total4)
#filter those out with less than three datapoints and drop total=1 column!

smallmat4 <- droplevels(smallmat4)
#get rid of ditched levels

smalleco4 <- smallmat4$eco4
#make vector for ecosections as before

smalleco4 <- as.factor(smalleco4)
#make factor to avoid weird errors because they're numbers

#create a matrix with ecosections as row names
matrix4<-as.matrix(smallmat4)
row.names(matrix4) <- matrix4[,1]
new.matrix4 <- matrix4[,-1]
class(new.matrix4)<-"numeric"

rankindex(smalleco4, new.matrix4, indices = c("euc", "man", "gow", "bra", "kul"))
#now it says euclidean is best and tied for second are bray, man and kul!

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc4<- metaMDS(new.matrix4,distance="bray",labels=smalleco4, trymax = 100, autotransform = FALSE)
eco.nmds.bc4
plot(eco.nmds.bc4)
#found convergence easily (<20), stress is rel. low = 0.06764617

#region, proportion based dissimilarity - kulczynski
eco.nmds.ku4<- metaMDS(new.matrix4,distance="kulczynski",labels=smalleco4, trymax = 100, autotransform = FALSE)
eco.nmds.ku4
plot(eco.nmds.ku4)
#found convergence easily (<20), stress is rel. low = 0.05774715

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc4<-adonis(new.matrix4 ~ smalleco4, permutations = 999, method="bray")
permanova_eco.bc4 #significant! p = 0.001 so plot it

permanova_eco.ku4<-adonis(new.matrix4 ~ smalleco4, permutations = 999, method="kulczynski")
permanova_eco.ku4 #significant! p = 0.001 so plot it (has lower mean sqs and sum of sqs tho)

NMDS.bc4<-data.frame(NMDS1.bc4=eco.nmds.bc4$points[,1],NMDS2.bc4=eco.nmds.bc4$points[,2],group=smalleco4)
#plot NMDS, only once (picking Bray because all similar), no presence absence

NMDS.ku4<-data.frame(NMDS1.ku4=eco.nmds.ku4$points[,1],NMDS2.ku4=eco.nmds.ku4$points[,2],group=smalleco4)
#plot NMDS, only once (picking Gower because bray can't converge), no presence absence

ord.bc4<-ordiellipse(eco.nmds.bc4,smalleco4,display="sites",kind="sd", conf = 0.95, label=T)
#Ellipses are standard deviation, no scaling of data (can use standard error, scaling, and confidence limit options)

ord.ku4<-ordiellipse(eco.nmds.ku4,smalleco4,display="sites",kind="sd", conf = 0.95, label=T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell.bc4 <- data.frame()
for(g in levels(NMDS.bc4$group)){
  df_ell.bc4 <- rbind(df_ell.bc4, cbind(as.data.frame(with(NMDS.bc4[NMDS.bc4$group==g,],
                                                           veganCovEllipse(ord.bc4[[g]]$cov,ord.bc4[[g]]$center))),group=g))
}

df_ell.ku4 <- data.frame()
for(g in levels(NMDS.ku4$group)){
  df_ell.ku4 <- rbind(df_ell.ku4, cbind(as.data.frame(with(NMDS.ku4[NMDS.ku4$group==g,],
                                                           veganCovEllipse(ord.ku4[[g]]$cov,ord.ku4[[g]]$center))),group=g))
}

ggplot(NMDS.bc4, aes(NMDS1.bc4, NMDS2.bc4))+
  geom_point(stat = "identity", aes(fill=group), size=3, shape=21, color = "black")+
  geom_path(data=df_ell.bc4, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_fill_manual(values=c("rosybrown4", "magenta2", "goldenrod1", "tomato", "green", "darkorchid4", "lightslateblue", "lightblue"),
                    name="Ecosection", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  scale_colour_manual(values=c("rosybrown4", "magenta2", "goldenrod1", "tomato", "green", "darkorchid4", "lightslateblue", "lightblue"),
                      guide=FALSE) +
  scale_y_continuous(limits=c(-1.5,1),breaks=seq(-1.5,1,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
  scale_x_continuous(limits=c(-1.5,1.5),breaks=seq(-1.5,1.5,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()) + coord_fixed() +
  annotate("text",x=-0.75,y=1,label="stress = 0.11",size=4)
#NMDS graph for the different regions, 2014

one5 <- litter %>%
  filter(Year_=="2015", Region=="1") %>%
  select(Region, ED_NAME, Site_Name, Litter, Number) %>%
  group_by(Region, Site_Name, Litter) %>%
  summarise(num=sum(Number)) %>%
  spread(Litter, num, fill = 0)

rest5 <- litter %>%
  filter(Year_=="2015", Region!="1") %>%
  select(Region, ED_NAME, Site_Name, Litter, Number) %>%
  group_by(Region, ED_NAME, Litter) %>%
  summarise(num=sum(Number)) %>%
  spread(Litter, num, fill = 0)

yr5 <- rbind(one5, rest5)

yr5$Syringes[which(is.na(yr5$Syringes))] <- 0

mat5 <- yr5 %>%
  ungroup() %>%
  select(Appliances:Toys, Syringes)
#create a dataframe with only litter categories

eco5 <- yr5$Region
#create a vector for ecosection info to be reattached after calculations

Total5 <- vector(length = nrow(mat5))
#create an empty vector

Total5 <- rowSums(mat5)
#fill that vector with calculated row totals (total per cleanup)

mat5 <- cbind(mat5, Total5)
#add that vector as a column to the matrix

proportions5 <- data.frame()
#create empty dataframe to fill with transformed data!

proportions5 <- mat5/mat5$Total5*100
#divide entire dataframe by row totals (total column should all = 1)

mat15 <- cbind(eco5, proportions5)
#reattach the ecosection labels now that total calculation is done

smallmat5 <- mat15 %>%
  #	filter(eco != "Dogfish Bank Frontal Region" & eco != "Eastern Queen Charlotte Sound" & eco != "Cape Scott Tidal Mixing" & eco != "Southern Strait of Georgia") %>%
  select(-Total5)
#filter those out with less than three datapoints and drop total=1 column!

smallmat5 <- droplevels(smallmat5)
#get rid of ditched levels

smalleco5 <- smallmat5$eco5
#make vector for ecosections as before

smalleco5 <- as.factor(smalleco5)
#make factor to avoid weird errors because they're numbers

#create a matrix with ecosections as row names
matrix5<-as.matrix(smallmat5)
row.names(matrix5) <- matrix5[,1]
new.matrix5 <- matrix5[,-1]
class(new.matrix5)<-"numeric"

rankindex(smalleco5, new.matrix5, indices = c("euc", "man", "gow", "bra", "kul"))
#now it says euclidean is best and tied for second are bray, man and kul!

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc5<- metaMDS(new.matrix5,distance="bray",labels=smalleco5, trymax = 100, autotransform = FALSE)
eco.nmds.bc5
plot(eco.nmds.bc5)
#found convergence easily (<20), stress is rel. low = 0.06764617

#region, proportion based dissimilarity - gower
eco.nmds.ku5<- metaMDS(new.matrix5,distance="gower",labels=smalleco5, trymax = 100, autotransform = FALSE)
eco.nmds.ku5
plot(eco.nmds.ku5)
#found convergence easily (<20), stress is rel. low = 0.05774715

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc5<-adonis(new.matrix5 ~ smalleco5, permutations = 999, method="bray")
permanova_eco.bc5 #significant! p = 0.001 so plot it

permanova_eco.ku5<-adonis(new.matrix5 ~ smalleco5, permutations = 999, method="kulczynski")
permanova_eco.ku5 #significant! p = 0.001 so plot it (has lower mean sqs and sum of sqs tho)

NMDS.bc5<-data.frame(NMDS1.bc5=eco.nmds.bc5$points[,1],NMDS2.bc5=eco.nmds.bc5$points[,2],group=smalleco5)
#plot NMDS, only once (picking Bray because all similar), no presence absence

NMDS.ku5<-data.frame(NMDS1.ku5=eco.nmds.ku5$points[,1],NMDS2.ku5=eco.nmds.ku5$points[,2],group=smalleco5)
#plot NMDS, only once (picking Gower because bray can't converge), no presence absence

ord.bc5<-ordiellipse(eco.nmds.bc5,smalleco5,display="sites",kind="sd", conf = 0.95, label=T)
#Ellipses are standard deviation, no scaling of data (can use standard error, scaling, and confidence limit options)

ord.ku5<-ordiellipse(eco.nmds.ku5,smalleco5,display="sites",kind="sd", conf = 0.95, label=T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell.bc5 <- data.frame()
for(g in levels(NMDS.bc5$group)){
  df_ell.bc5 <- rbind(df_ell.bc5, cbind(as.data.frame(with(NMDS.bc5[NMDS.bc5$group==g,],
                                                           veganCovEllipse(ord.bc5[[g]]$cov,ord.bc5[[g]]$center))),group=g))
}

df_ell.ku5 <- data.frame()
for(g in levels(NMDS.ku5$group)){
  df_ell.ku5 <- rbind(df_ell.ku5, cbind(as.data.frame(with(NMDS.ku5[NMDS.ku5$group==g,],
                                                           veganCovEllipse(ord.ku5[[g]]$cov,ord.ku5[[g]]$center))),group=g))
}

ggplot(NMDS.bc5, aes(NMDS1.bc5, NMDS2.bc5))+
  geom_point(stat = "identity", aes(fill=group), size=3, shape=21, color = "black")+
  geom_path(data=df_ell.bc5, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_fill_manual(values=c("rosybrown4", "magenta2", "goldenrod1", "tomato", "green", "darkorchid4", "lightslateblue", "lightblue"),
                    name="Ecosection", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  scale_colour_manual(values=c("rosybrown4", "magenta2", "goldenrod1", "tomato", "green", "darkorchid4", "lightslateblue", "lightblue"),
                      guide=FALSE) +
  scale_y_continuous(limits=c(-2,1),breaks=seq(-2,1,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
  scale_x_continuous(limits=c(-1,2),breaks=seq(-1,2,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()) + coord_fixed() +
  annotate("text",x=-0.75,y=1,label="stress = 0.11",size=4)
#NMDS graph for the different regions, 2015

#okay I can't figure this shit out, giving up for 2016... ugh

##### NMDS VANCOUVER? #####

ecovan <- litter %>%
  filter(!is.na(Number) & Number!=0, Region=="4") %>%
  select(ED_NAME, Year_, Litter, Number) %>%
  group_by(ED_NAME, Year_, Litter) %>%
  summarise(num=sum(Number)) %>%
  spread(Litter, num, fill = 0)
#this is total items per site per year, need to calc proportions!

mat <- ecovan %>%
  ungroup() %>%
  select(Appliances:Toys)
#create a dataframe with only litter categories

eco <- ecovan$ED_NAME
#create a vector for ecosection info to be reattached after calculations

Total <- vector(length = nrow(mat))
#create an empty vector

Total <- rowSums(mat)
#fill that vector with calculated row totals (total per cleanup)

mat <- cbind(mat, Total)
#add that vector as a column to the matrix

proportions <- data.frame()
#create empty dataframe to fill with transformed data!

proportions <- mat/mat$Total*100
#divide entire dataframe by row totals (total column should all = 1)

mat1 <- cbind(eco, proportions)
#reattach the ecosection labels now that total calculation is done

smallermat <- mat1 %>%
  #	filter(eco != "Dogfish Bank Frontal Region" & eco != "Eastern Queen Charlotte Sound" & eco != "Cape Scott Tidal Mixing" & eco != "Southern Strait of Georgia") %>%
  select(-Total)
#filter those out with less than three datapoints and drop total=1 column!

smallermat <- droplevels(smallermat)
#get rid of ditched levels

smalleco <- smallermat$eco
#make vector for ecosections as before

smalleco <- as.factor(smalleco)
#make factor to avoid weird errors because they're numbers

#create a matrix with ecosections as row names
matrix2<-as.matrix(smallermat)
row.names(matrix2) <- matrix2[,1]
new.matrix <- matrix2[,-1]
class(new.matrix)<-"numeric"

rankindex(smalleco, new.matrix, indices = c("euc", "man", "gow", "bra", "kul"))
#now it says euclidean is best and tied for second are bray, man and kul!

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(new.matrix,distance="bray",labels=smalleco, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

#region, proportion based dissimilarity - euclidean
eco.nmds.eu<- metaMDS(new.matrix,distance="euclidean",labels=smalleco, trymax = 100, autotransform = FALSE)
eco.nmds.eu
plot(eco.nmds.eu)
#found convergence easily (<20), stress is rel. low = 0.05774715

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(new.matrix ~ smalleco, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

permanova_eco.eu<-adonis(new.matrix ~ smalleco, permutations = 999, method="euclidean")
permanova_eco.eu #significant! p = 0.001 so plot it (has lower mean sqs and sum of sqs tho)

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=smalleco)
#plot NMDS, only once (picking Bray because all similar), no presence absence

NMDS.eu<-data.frame(NMDS1.eu=eco.nmds.eu$points[,1],NMDS2.eu=eco.nmds.eu$points[,2],group=smalleco)
#plot NMDS, only once (picking Gower because bray can't converge), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,smalleco,display="sites",kind="sd", conf = 0.95, label=T)
#Ellipses are standard deviation, no scaling of data (can use standard error, scaling, and confidence limit options)

ord.eu<-ordiellipse(eco.nmds.eu,smalleco,display="sites",kind="sd", conf = 0.95, label=T)

veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell.bc <- data.frame()
for(g in levels(NMDS.bc$group)){
  df_ell.bc <- rbind(df_ell.bc, cbind(as.data.frame(with(NMDS.bc[NMDS.bc$group==g,],
                                                         veganCovEllipse(ord.bc[[g]]$cov,ord.bc[[g]]$center))),group=g))
}

df_ell.eu <- data.frame()
for(g in levels(NMDS.eu$group)){
  df_ell.eu <- rbind(df_ell.eu, cbind(as.data.frame(with(NMDS.eu[NMDS.eu$group==g,],
                                                         veganCovEllipse(ord.eu[[g]]$cov,ord.eu[[g]]$center))),group=g))
}

ggplot(NMDS.bc, aes(NMDS1.bc, NMDS2.bc))+
  geom_point(stat = "identity", aes(fill=group), size=3, shape=21, color = "black")+
  geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_fill_manual(values=c("rosybrown4", "magenta2", "goldenrod1", "tomato", "green", "darkorchid4", "lightslateblue", "lightblue"),
                    name="Ecosection", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  scale_colour_manual(values=c("rosybrown4", "magenta2", "goldenrod1", "tomato", "green", "darkorchid4", "lightslateblue", "lightblue"),
                      guide=FALSE) +
  scale_y_continuous(limits=c(-1,1),breaks=seq(-1,1,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
  scale_x_continuous(limits=c(-1,1.1),breaks=seq(-1,1.1,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()) + coord_fixed() +
  annotate("text",x=-0.75,y=1,label="stress = 0.07",size=4)
#copied and pasted everything but it won't converge... lost cause.

##### PCA? #####

lit <- trash %>%
  select(Region, Nearest_Ci, Year_, Litter, Number) %>%
  group_by(Region, Nearest_Ci, Year_, Litter) %>%
  summarise(num=sum(Number)) %>%
  spread(Litter, num, fill=0)

mat <- lit %>%
  ungroup() %>%
	select(Appliances:Toys)
#  select(Cigarettes, Food_Wrapp, Bags__Pape, Bags__Plas, Beverage_B, Beverage_C, Bottle_Cap, Building_M, Buoys_Floa, Cups_and_1, Cups_and_2, Fishing_Li, Fishing_Ne, Foam_Piece, Glass_Beve, Glass_Piec, Lids__Plas, Other_Pl_1, Other_Pl_2, Other_plas, Plastic_Pi, Rope, Straws__St, Tobacco_Pa)
#create a dataframe with only litter categories

eco <- lit$Region
#create a vector for ecosection info to be reattached after calculations

Total <- vector(length = nrow(mat))
#create an empty vector

Total <- rowSums(mat)
#fill that vector with calculated row totals (total per cleanup)

mat <- cbind(mat, Total)
#add that vector as a column to the matrix

proportions <- data.frame()
#create empty dataframe to fill with transformed data!

proportions <- mat/mat$Total*100
#divide entire dataframe by row totals (total column should all = 1)

mat1 <- cbind(eco, proportions)
#reattach the ecosection labels now that total calculation is done

#see which categories to take out because this PCA has 45 variables! Too many!
max(mat1$Total)
#less than 1%: appliances
#less than 5%: batteries, condoms, diapers, 6 pack holders, fireworks, fishing lures, cutlery, syringes, foam take out container, tires, toys
#less than 10%: balloons, plastic bottle caps, cigar tips, cig lighters, clothing, paper cups and plates, strapping bands, plastic take out containers, tampons

unique(trash$Nearest_Ci[which(trash$Region=="1")])
#where is it 100% beverage bottles?! Sandspit! Cuz no one lives there (it sux), only debris!
#and where in Region 4 is it 100% beverage cans?? Bowen not counting properly in 2015... >:(
cities <- vector()

cities <- lit$Nearest_Ci

wcity <- cbind(cities, mat1)
#a way to see what's going on / where outliers are coming from!

matrix1 <- filter(mat1, Beverage_C!=100)
#remove outlier where Bowen only counted metal beverage cans for recycling...

smallermat <- matrix1 %>%
  select(-Total)
#filter those out with less than three datapoints and drop total=1 column!

smallermat <- droplevels(smallermat)
#get rid of ditched levels

smalleco <- smallermat$eco
#make vector for ecosections as before

smalleco <- as.factor(smalleco)
#make factor to avoid weird errors because they're numbers

#create a matrix with ecosections as row names
matrix2<-as.matrix(smallermat)
row.names(matrix2) <- matrix2[,1]
new.matrix1 <- matrix2[,-1]
class(new.matrix1)<-"numeric"

View(new.matrix1)

za <- prcomp(new.matrix1)

summary(za)

#screeplot(za)

#biplot(za, cex=0.7, xlim = c(-0.25, 0.23))

za$rotation

#same for source

sour <- trash %>%
  select(Region, Nearest_Ci, Year_, Source, Number) %>%
  group_by(Region, Nearest_Ci, Year_, Source) %>%
  summarise(num=sum(Number)) %>%
  spread(Source, num, fill=0)

mat <- sour %>%
  ungroup() %>%
  select(Dumping:Smoking)
#create a dataframe with only litter categories

colnames(mat) <- c("Dumping", "Fishing", "Hygiene", "Recreation", "Smoking")

eco <- sour$Region
#create a vector for ecosection info to be reattached after calculations

Total <- vector(length = nrow(mat))
#create an empty vector

Total <- rowSums(mat)
#fill that vector with calculated row totals (total per cleanup)

mat <- cbind(mat, Total)
#add that vector as a column to the matrix

proportions <- data.frame()
#create empty dataframe to fill with transformed data!

proportions <- mat/mat$Total*100
#divide entire dataframe by row totals (total column should all = 1)

mat1 <- cbind(eco, proportions)
#reattach the ecosection labels now that total calculation is done

matrix1 <- mat1[-82, ]

which(matrix1$`Shoreline Rec.` == 100)

nrow(matrix1)

smallermat <- matrix1 %>%
  #	filter(eco != "Dogfish Bank Frontal Region" & eco != "Eastern Queen Charlotte Sound" & eco != "Cape Scott Tidal Mixing" & eco != "Southern Strait of Georgia") %>%
  select(-Total)
#filter those out with less than three datapoints and drop total=1 column!

smallermat <- droplevels(smallermat)
#get rid of ditched levels

smalleco <- smallermat$eco
#make vector for ecosections as before

smalleco <- as.factor(smalleco)
#make factor to avoid weird errors because they're numbers

#create a matrix with ecosections as row names
matrix2<-as.matrix(smallermat)
row.names(matrix2) <- matrix2[,1]
new.matrix2 <- matrix2[,-1]
class(new.matrix2)<-"numeric"

View(new.matrix2)

zs <- prcomp(new.matrix2)

summary(zs)

#screeplot(zs)

biplot(zs, xlim = c(-0.16, 0.25), ylim = c(-0.21, 0.21))
title("PCA: Source of Litter", line = 3)

zs$rotation

#same for material

mater <- trash %>%
	select(Region, Nearest_Ci, Year_, Material, Number) %>%
	group_by(Region, Nearest_Ci, Year_, Material) %>%
	summarise(num=sum(Number)) %>%
	spread(Material, num, fill=0)

mat <- mater %>%
	ungroup() %>%
	select(Cloth:Rubber)
#create a dataframe with only litter categories

city <- mater$Nearest_Ci


eco <- mater$Region
#create a vector for ecosection info to be reattached after calculations

Total <- vector(length = nrow(mat))
#create an empty vector

Total <- rowSums(mat)
#fill that vector with calculated row totals (total per cleanup)

mat <- cbind(mat, Total)
#add that vector as a column to the matrix

proportions <- data.frame()
#create empty dataframe to fill with transformed data!

proportions <- mat/mat$Total*100
#divide entire dataframe by row totals (total column should all = 1)

mat1 <- cbind(eco, proportions)
#reattach the ecosection labels now that total calculation is done

matrix1 <- filter(mat1, Metal!=100)
#remove huge outlier where Bowen only counted metal beverage cans for recycling...

smallermat <- matrix1 %>%
	select(-Total)
#filter those out with less than three datapoints and drop total=1 column!

smallermat <- droplevels(smallermat)
#get rid of ditched levels

smalleco <- smallermat$eco
#make vector for ecosections as before

smalleco <- as.factor(smalleco)
#make factor to avoid weird errors because they're numbers

#create a matrix with ecosections as row names
matrix2<-as.matrix(smallermat)
row.names(matrix2) <- matrix2[,1]
new.matrix3 <- matrix2[,-1]
class(new.matrix3)<-"numeric"

View(new.matrix3)

zm <- prcomp(new.matrix3)

summary(zm)

#screeplot(zm)

biplot(zm, cex=0.7, xlim = c(-0.4, 0.11))

zm$rotation

#try to mash all 3 together?

one <- new.matrix1[, c(6, 14, 26)]
#cigs, bevies and foam pieces

two <- new.matrix2[, c(1, 4, 5)]
#smoke, dump n shoreline rec

three <- new.matrix3[, c(2, 6)]
#glass n plastics

allofem <- cbind(one, two, three)

zw <- prcomp(allofem)

summary(zw)

#screeplot(zw)

biplot(zw, cex=0.7)

zw$rotation

#par(mfrow = c(1, 2))
#biplot(za, cex=0.7, xlim = c(-0.25, 0.23), ylim = c(-0.3, 0.25))
#mtext("PCA: All Litter Items", side = 3, line = 3)
#biplot(zs, cex=0.7, xlim = c(-0.2, 0.25), ylim = c(-0.25, 0.23))
#mtext("PCA: Source of Litter", side = 3, line = 3)

##### TABLE #####

#TABLE CREATION SECTION#

categories$Source[which(categories$Source == "Shoreline Rec.")] <- "Shoreline Recreation"
categories$Litter[which(categories$Litter == "Cigarettes")] <- "Cigarettes & Cigarette Filters"
categories$Litter[which(categories$Litter == "Food_Wrapp")] <- "Food Wrappers & Containers"
categories$Litter[which(categories$Litter == "Takeout_Co")] <- "Takeout Containers (Plastic)"
categories$Litter[which(categories$Litter == "Takeout__1")] <- "Takeout Containers (Foam)"
categories$Litter[which(categories$Litter == "Bottle_Cap")] <- "Bottle Caps (Plastic)"
categories$Litter[which(categories$Litter == "Bottle_C_1")] <- "Bottle Caps (Metal)"
categories$Litter[which(categories$Litter == "Lids__Plas")] <- "Lids (Plastic)"
categories$Litter[which(categories$Litter == "Straws__St")] <- "Straws & Stirrers"
categories$Litter[which(categories$Litter == "Forks__Kni")] <- "Forks, Knives & Spoons"
categories$Litter[which(categories$Litter == "Beverage_B")] <- "Beverage Bottles (Plastic, 2L or less)"
categories$Litter[which(categories$Litter == "Glass_Beve")] <- "Glass Beverage Bottles"
categories$Litter[which(categories$Litter == "Beverage_C")] <- "Beverages (Cans)"
categories$Litter[which(categories$Litter == "Bags__Plas")] <- "Bags (Plastic)"
categories$Litter[which(categories$Litter == "Other_plas")] <- "Other Plastic Bags"
categories$Litter[which(categories$Litter == "Bags__Pape")] <- "Bags (Paper)"
categories$Litter[which(categories$Litter == "Cups_and_P")] <- "Cups & Plates (Paper)"
categories$Litter[which(categories$Litter == "Cups_and_1")] <- "Cups & Plates (Plastic)"
categories$Litter[which(categories$Litter == "Cups_and_2")] <- "Cups & Plates (Foam)"
categories$Litter[which(categories$Litter == "Buoys_Floa")] <- "Buoys & Floats"
categories$Litter[which(categories$Litter == "Fishing_Li")] <- "Fishing Line"
categories$Litter[which(categories$Litter == "Fishing_Ne")] <- "Fishing Nets"
categories$Litter[which(categories$Litter == "Fishing_Lu")] <- "Fishing Lures & Light Sticks"
categories$Litter[which(categories$Litter == "F6_Pack_Ho")] <- "6-Pack Holders"
categories$Litter[which(categories$Litter == "Other_Pl_1")] <- "Other Plastic/Foam Packaging"
categories$Litter[which(categories$Litter == "Other_Pl_2")] <- "Other Plastic Bottles (Oil, Bleach, etc.)"
categories$Litter[which(categories$Litter == "Strapping_")] <- "Strapping Bands"
categories$Litter[which(categories$Litter == "Tobacco_Pa")] <- "Tobacco Packaging Wrappers"
categories$Litter[which(categories$Litter == "Appliances")] <- "Appliances (Refrigerators, Washers, etc.)"
categories$Litter[which(categories$Litter == "Cigar_Tips")] <- "Cigar Tips"
categories$Litter[which(categories$Litter == "Cigarette_")] <- "Cigarette Lighters"
categories$Litter[which(categories$Litter == "Building_M")] <- "Building Materials"
categories$Litter[which(categories$Litter == "Clothing__")] <- "Clothing & Shoes"
categories$Litter[which(categories$Litter == "Tampons_Ta")] <- "Tampons & Tampon Applicators"
categories$Litter[which(categories$Litter == "Foam_Piece")] <- "Foam Pieces"
categories$Litter[which(categories$Litter == "Glass_Piec")] <- "Glass Pieces"
categories$Litter[which(categories$Litter == "Plastic_Pi")] <- "Plastic Pieces"

a <- knitr::kable(categories, format = "latex", booktabs=TRUE) %>%
	row_spec(0, bold = TRUE) %>% 
	kable_styling(latex_options = "striped")

#kable_as_image(a, "litter-categories")

shore <- filter(categories, Source == "Shoreline Recreation")
dumping <- filter(categories, Source == "Dumping")
smoke <- filter(categories, Source == "Smoking")
fish <- filter(categories, Source == "Fishing")
medical <- filter(categories, Source == "Medical/Hygiene")

nrow(shore)
nrow(dumping)
nrow(smoke)
nrow(fish)
nrow(medical)
#most to least categories: shore, dump, fish, smoke and medical

shore$Litter[which(shore$Litter == "Takeout Containers (Plastic)")] <- "Takeout Containers"
shore$Litter[which(shore$Litter == "Takeout Containers (Foam)")] <- "Takeout Containers"
shore$Litter[which(shore$Litter == "Bottle Caps (Plastic)")] <- "Bottle Caps"
shore$Litter[which(shore$Litter == "Bottle Caps (Metal)")] <- "Bottle Caps"
shore$Litter[which(shore$Litter == "Beverage Bottles (Plastic, 2L or less)")] <- "Beverage Bottles"
shore$Litter[which(shore$Litter == "Glass Beverage Bottles")] <- "Beverage Bottles"
shore$Litter[which(shore$Litter == "Bags (Plastic)")] <- "Bags"
shore$Litter[which(shore$Litter == "Other Plastic Bags")] <- "Bags"
shore$Litter[which(shore$Litter == "Bags (Paper)")] <- "Bags"
shore$Litter[which(shore$Litter == "Cups & Plates (Paper)")] <- "Cups & Plates"
shore$Litter[which(shore$Litter == "Cups & Plates (Plastic)")] <- "Cups & Plates"
shore$Litter[which(shore$Litter == "Cups & Plates (Foam)")] <- "Cups & Plates"
shore$Litter[which(shore$Litter == "Lids (Plastic)")] <- "Plastic Lids"
shore$Litter[which(shore$Litter == "Beverages (Cans)")] <- "Beverage Cans"

shore <- select(shore, -Material)

shore <- distinct(shore)

shore <- str_c(shore$Litter, collapse = ", ")
dumping <- str_c(dumping$Litter, collapse = ", ")
smoke <- str_c(smoke$Litter, collapse = ", ")
fish <- str_c(fish$Litter, collapse = ", ")
medical <- str_c(medical$Litter, collapse = ", ")

sources <- cbind(shore, dumping, smoke, fish, medical)

colnames(sources) <- c("Shoreline Recreation", "Dumping", "Smoking", "Fishing", "Medical")

b <- knitr::kable(sources, format = "latex", booktabs=TRUE) %>%
	row_spec(0, bold = TRUE) %>% 
	column_spec(1:5, width = "6em") %>% 
	kable_styling(latex_options = "striped")

kable_as_image(b, "litter-source")

##### NOTES TO SELF / TO DO LIST #####

#look into dead animal entanglement and/or tsunami comments - for next step of project!

zs$rotation
summary(zs)

