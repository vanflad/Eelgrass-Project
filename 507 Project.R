#Towards Cleaner Shores Group Project Data for FISH 507

rm(list=ls())
#remove any extra stuff floating around R environment

setwd("~/Desktop")
#set working directory to find file

data <- read.csv(file="GCSC_Data_10Nov2017.csv", stringsAsFactors=FALSE,
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

library(kableExtra)
#for making nice tables

van <- filter(data, Province == "British Columbia" & Year_ > 2007 & Coastal == "Yes")
#filter out the relevant data for BC 2008-2016

van$Ecosections.Region[which(van$Ecosections.Region == "(10 Aristazabal Banks Upwelling")] <- "(10) Aristazabal Banks Upwelling"
#rename messed up bracket to strip away brackets properly

a <- do.call(rbind, strsplit(van$Ecosections.Region, ") "))
#separate "(#) Region" so that we just have "Region" without annoying (#)

garbage <- cbind(a, van)
#attach new column to dataframe

garbage <- garbage %>%
	rename("Ecosections"=`2`, "Year"="Year_") %>%
	select(-`1`)
#drop "(#" column with no name and rename new column

litter <- gather(garbage, Litter, Number, Cigarettes:Shotgun_Sh)
#change from wide to long format!

trash <- left_join(litter, categories, by="Litter") %>%
	filter(!is.na(Number) & Number!=0 & !is.na(Ecosections)) %>%
	droplevels()
#join the files to add category data and remove NA and 0 rows

View(trash)
#make sure it all worked, will use for plotting!

#TRASH OVER TIME PLOT#

yr <- trash %>%
	group_by(Year, Litter, Source, Material) %>%
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

yr$Material <- factor(yr$Material, levels(yr$Material)[c(6, 2, 3, 5, 1, 4)])
#change levels

p1 <- ggplot(yr, aes(Year, num))+
	geom_bar(aes(fill=Source), stat="identity")+
	labs(x=NULL, y="Number of items", title="Source of Litter over Time")+
	scale_y_continuous(label = comma)+
	scale_x_continuous(limits = c(2007.5, 2016.5), breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))
#quick bar graph on source of litter over time
p2 <- ggplot(yr, aes(Year, num))+
	geom_bar(aes(fill=Material), stat="identity")+
	labs(x=NULL, y="Number of items", title="Material of Litter over Time")+
	scale_y_continuous(label = comma)+
	scale_x_continuous(limits = c(2007.5, 2016.5), breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016))+
	scale_fill_manual(values = brewer.pal(n=8, "Dark2"))
#quick bar graph on material of litter over time
plot_grid(p1, p2, nrow=2, align = "v", labels = c("A", "B"))
#source and material and overall number of litter over time

#TRASH PROFILE PLOTS#

gar <- trash %>%
	group_by(Ecosections, Litter, Source, Material) %>%
	summarise(num=sum(Number))
#summarizing the data by each region

gar$Source <- as.factor(gar$Source)
#exact same process as above, but for gar dataset now

gar$Source <- factor(gar$Source, levels(gar$Source)[c(5, 4, 2, 1, 3)])
#change source levels

levels(gar$Source)
#check that it worked

gar$Ecosections <- as.factor(gar$Ecosections)
#change ecosections to factors

levels(gar$Ecosections)
#check the levels

gar$Ecosections <- factor(gar$Ecosections, levels(gar$Ecosections)[c(5, 13, 1, 2, 4, 8, 10, 9, 6, 12, 7, 11, 3)])
#reorder the levels to what's appropriate for graphs

p3 <- ggplot(gar, aes(Ecosections, num))+
	geom_bar(aes(fill=Source), stat="identity")+
	labs(x=NULL, y="Number of items", title="Number of Litter by Source")+
	scale_y_continuous(label = comma)+
	scale_x_discrete(label=c("EQCS", "SSoG", "ArBU", "CSTM", "DBFR", "JhS", "LwFN", "HSaRP", "JdFS", "NSoG", "IGI", "MnF", "CSoG"))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	theme(legend.position = c(0.5, 0.6))+
	coord_flip()
#quick bar graph on source of litter by region
p4 <- ggplot(gar, aes(Ecosections, num))+
	geom_bar(aes(fill=Source), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Source")+
	theme(legend.position = "none")+
	scale_x_discrete(label=c("EQCS", "SSoG", "ArBU", "CSTM", "DBFR", "JhS", "LwFN", "HSaRP", "JdFS", "NSoG", "IGI", "MnF", "CSoG"))+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=5, "Set1"))+
	coord_flip()
#quick stacked bar graph on source of litter by region
plot_grid(p3, p4, ncol = 2, align = "h", labels = c("A", "B"))
#combine two source plots
#note that you fucked up the labels and got confused, don't do that shit again

gar$Material[which(gar$Material=="Wood")] <- "Other"
#get rid of the too small to see on graph category for wood

gar$Material <- as.factor(gar$Material)
#change from character to factor

gar$Material <- factor(gar$Material, levels(gar$Material)[c(6, 2, 3, 5, 1, 7, 4)])
#change the level order of materials for this plot

levels(gar$Material)
#check that it worked out right

p5 <- ggplot(gar, aes(Ecosections, num))+
	geom_bar(aes(fill=Material), stat="identity")+
	labs(x=NULL, y="Number of items", title="Number of Litter by Material")+
	theme(legend.position = c(0.5, 0.5))+
	scale_x_discrete(label=c("EQCS", "SSoG", "ArBU", "CSTM", "DBFR", "JhS", "LwFN", "HSaRP", "JdFS", "NSoG", "IGI", "MnF", "CSoG"))+
	scale_y_continuous(label = comma)+
	scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
	coord_flip()
#quick bar graph on material of litter by region
p6 <- ggplot(gar, aes(Ecosections, num))+
	geom_bar(aes(fill=Material), position = "fill", stat="identity")+
	labs(x=NULL, y="Proportion of items (%)", title="Proportion of Litter by Material")+
	theme(legend.position = "none")+
	scale_x_discrete(label=c("EQCS", "SSoG", "ArBU", "CSTM", "DBFR", "JhS", "LwFN", "HSaRP", "JdFS", "NSoG", "IGI", "MnF", "CSoG"))+
	scale_y_continuous(labels=scales::unit_format("", 100))+
	scale_fill_manual(values = brewer.pal(n=8, "Dark2"))+
	coord_flip()
#quick stacked bar graph on material of litter by region
plot_grid(p5, p6, ncol = 2, labels = c("A", "B"))
#combine two material plots

#NMDS analysis section!#

ecoyr <- litter %>%
	filter(!is.na(Number) & Number!=0 & !is.na(Ecosections)) %>% 
	select(Ecosections, Year, Litter, Number) %>%
	group_by(Ecosections, Year, Litter) %>%
	summarise(num=sum(Number)) %>%
	spread(Litter, num, fill = 0)
#this is total items per site per year, need to calc proportions!

mat <- ecoyr %>%
	ungroup() %>% 
	select(Appliances:Toys)
#create a dataframe with only litter categories

eco <- ecoyr$Ecosections
#create a vector for ecosection info to be reattached after calculations

Total <- vector(length = nrow(mat))
#create an empty vector

Total <- rowSums(mat)
#fill that vector with calculated row totals (total per cleanup)

mat <- cbind(mat, Total)
#add that vector as a column to the matrix

proportions <- data.frame()
#create empty dataframe to fill with transformed data!

proportions <- mat/mat$Total
#divide entire dataframe by row totals (total column should all = 1)

mat1 <- cbind(eco, proportions)
#reattach the ecosection labels now that total calculation is done

smallermat <- mat1 %>%
	filter(eco != "Aristazabal Banks Upwelling" & eco != "Eastern Queen Charlotte Sound" & eco != "Cape Scott Tidal Mixing" & eco != "Southern Strait of Georgia") %>%
	select(-Total)
#filter those out with less than three datapoints and drop total=1 column!

smallermat <- droplevels(smallermat)
#get rid of ditched levels

smalleco <- smallermat$eco
#make vector for ecosections as before

#create a matrix with ecosections as row names
matrix2<-as.matrix(smallermat)
row.names(matrix2) <- matrix2[,1]
new.matrix <- matrix2[,-1]
class(new.matrix)<-"numeric"

rankindex(smalleco, new.matrix, indices = c("euc", "man", "gow", "bra", "kul"))
#interesting it now says euclidean distance is most appropriate!
#but maybe let's try both euclidean and bray because they're similar
#note manhattan and kulczynski are also exact same as bray-curtis

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(new.matrix,distance="bray",labels=smalleco, trymax = 100, autotransform = FALSE)
eco.nmds.bc  
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.112477

#region, proportion based dissimilarity - euclidean
eco.nmds.eu<- metaMDS(new.matrix,distance="euclidean",labels=smalleco, trymax = 100, autotransform = FALSE)
eco.nmds.eu 
#plot(eco.nmds.eu)
#found convergence easily (<20), stress is rel. low = 0.09524341
#but the plot itself is not as nice as bray curtis plot... hmm.

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(new.matrix ~ smalleco, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

permanova_eco.eu<-adonis(new.matrix ~ smalleco, permutations = 999, method="euclidean")
permanova_eco.eu #significant! p = 0.001 so plot it (has higher mean sqs and sum of sqs tho)

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=smalleco)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,smalleco,display="sites",kind="sd", conf = 0.95, label=T)
#Ellipses are standard deviation, no scaling of data (can use standard error, scaling, and confidence limit options)

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

hsarp <- filter(NMDS.bc, group=="Haro Strait and Rosario Passage")
csog <- filter(NMDS.bc, group=="Central Strait of Georgia")
jdfs <- filter(NMDS.bc, group=="Juan de Fuca Strait")
mnf <- filter(NMDS.bc, group=="Mainland Fjords")
igi <- filter(NMDS.bc, group=="Interior Gulf Islands")
jhs <- filter(NMDS.bc, group=="Johnstone Strait")
nsog <- filter(NMDS.bc, group=="Northern Strait of Georgia")
lwfn <- filter(NMDS.bc, group=="Low Flow Nearshore")
dbfr <- filter(NMDS.bc, group=="Dogfish Bank Frontal Region")

NMDS.bc.mod <- rbind(dbfr, lwfn, jhs, jdfs, nsog, igi, hsarp, mnf, csog)
#need to reorder levels AND manually rearranged the order in dataframe itself...

NMDS.bc.mod$group <- factor(NMDS.bc.mod$group, levels(NMDS.bc.mod$group)[c(2, 7, 5, 6, 9, 4, 3, 8, 1)])
levels(NMDS.bc.mod$group)

df_ell.bc$group <- factor(df_ell.bc$group, levels(df_ell.bc$group)[c(2, 7, 5, 6, 9, 4, 3, 8, 1)])
levels(df_ell.bc$group)

ggplot(NMDS.bc.mod, aes(NMDS1.bc, NMDS2.bc))+
	geom_point(stat = "identity", aes(fill=group), size=2, shape=21, color = "black")+
	geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
	scale_fill_manual(values=c("antiquewhite4", "darkorchid4", "tomato", "green", "aquamarine4", "goldenrod1", "magenta2", "lightslateblue", "rosybrown4"),
										name="Ecosection", guide="legend") +
	guides(fill= guide_legend(override.aes = list(size=4)))+
	scale_colour_manual(values=c("antiquewhite4", "darkorchid4", "tomato", "green", "aquamarine4", "goldenrod1", "magenta2", "lightslateblue", "rosybrown4"),
											guide=FALSE) +
	scale_y_continuous(limits=c(-0.9,1.35),breaks=seq(-0.9,1.35,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
	scale_x_continuous(limits=c(-0.9,1.4),breaks=seq(-0.9,1.4,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
	theme_bw()+
	theme(axis.text.x=element_text(size=12),
				axis.title.x=element_text(size=12),
				axis.title.y=element_text(angle=90,size=12), 
				axis.text.y=element_text(size=12), 
				panel.grid.minor=element_blank(),panel.grid.major=element_blank()) + coord_fixed() +
	annotate("text",x=-0.55,y=1.35,label="stress = 0.11",size=4)
#NMDS graph for the different ecosections!

# CLUSTER TIME #

#still need to possibly remove SSoG (too small in size and cleanups) & redo graph!

ecosite <- litter %>%
	filter(!is.na(Number) & Number!=0 & !is.na(Ecosections)) %>% 
	select(Ecosections, Litter, Number) %>%
	group_by(Ecosections, Litter) %>%
	summarise(num=sum(Number)) %>%
	spread(Litter, num, fill = 0)
#this is total items per site, need to calc proportions!

ecomat <- ecosite %>%
	ungroup() %>% 
	select(Appliances:Toys)
#create a dataframe with only litter categories

ecoinfo <- ecosite$Ecosections
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
abline(0.8, 0, col="red")
#cluster for proportion of trash items
z <- 0.8
?abline

View(bcclust)

ggplot(bcclust, aes())

#should do cluster for proportion of trash source?

#NMDS on Clusters!#

#NOTE MODIFYING LITTER DATAFRAME HERE, DO NOT USE BELOW HERE IF WANT ORIGINAL!#

#north coast = DBFR, ArBU, EQCS
#south coast = IGI, CSoG, HSaRP, MnF, JdFS, JhS, NSoG
#west coast van. is. = SSoG, CSTM, LwFN
#exceptions to these classifications tho ie HSaRP & JdFS are west coast van is
#and LwFN is partly west coast van is but partly north coast too. SSoG is kinda east... w/e

litter$Ecosections <- as.character(litter$Ecosections)

litter$Ecosections[which(litter$Ecosections == "Aristazabal Banks Upwelling")] <- "North Coast"
litter$Ecosections[which(litter$Ecosections == "Dogfish Bank Frontal Region")] <- "North Coast"
litter$Ecosections[which(litter$Ecosections == "Eastern Queen Charlotte Sound")] <- "North Coast"
litter$Ecosections[which(litter$Ecosections == "Interior Gulf Islands")] <- "South Coast"
litter$Ecosections[which(litter$Ecosections == "Central Strait of Georgia")] <- "South Coast"
litter$Ecosections[which(litter$Ecosections == "Haro Strait and Rosario Passage")] <- "South Coast"
litter$Ecosections[which(litter$Ecosections == "Mainland Fjords")] <- "South Coast"
litter$Ecosections[which(litter$Ecosections == "Juan de Fuca Strait")] <- "South Coast"
litter$Ecosections[which(litter$Ecosections == "Johnstone Strait")] <- "South Coast"
litter$Ecosections[which(litter$Ecosections == "Northern Strait of Georgia")] <- "South Coast"
litter$Ecosections[which(litter$Ecosections == "Southern Strait of Georgia")] <- "NW Coast Vanc Isl + SSoG"
litter$Ecosections[which(litter$Ecosections == "Cape Scott Tidal Mixing")] <- "NW Coast Vanc Isl + SSoG"
litter$Ecosections[which(litter$Ecosections == "Low Flow Nearshore")] <- "NW Coast Vanc Isl + SSoG"

ecoclust <- litter %>%
	filter(!is.na(Number) & Number!=0 & !is.na(Ecosections)) %>% 
	select(Ecosections, Year, Litter, Number) %>%
	group_by(Ecosections, Year, Litter) %>%
	summarise(num=sum(Number)) %>%
	spread(Litter, num, fill = 0)
#this is total items per site per year, need to calc proportions!

litmat <- ecoclust %>%
	ungroup() %>% 
	select(Appliances:Toys)
#create a dataframe with only litter categories

info <- ecoclust$Ecosections
#make vector for ecosections as before

totals <- vector(length = nrow(litmat))
#create an empty vector

totals <- rowSums(litmat)
#fill that vector with calculated row totals (total per cleanup)

litmat <- cbind(litmat, totals)
#add that vector as a column to the matrix

propor <- data.frame()
#create empty dataframe to fill with transformed data!

propor <- litmat/litmat$totals
#divide entire dataframe by row totals (total column should all = 1)

litmat1 <- cbind(info, propor)
#reattach the ecosection labels now that total calculation is done

litmat1 <- litmat1 %>%
	select(-totals) %>%
	droplevels()
#drop total column and any unused levels like SSoG or Dixon Entrance, etc.

#create a matrix with ecosections as row names
fillmat<-as.matrix(litmat1)
row.names(fillmat) <- fillmat[,1]
groupmat <- fillmat[,-1]
class(groupmat)<-"numeric"

rankindex(info, groupmat, indices = c("euc", "man", "gow", "bra", "kul"))

eco.nmds.bc<- metaMDS(groupmat,distance="bray",labels=info, trymax = 100, autotransform = FALSE)
eco.nmds.bc  
plot(eco.nmds.bc)

eco.nmds.eu<- metaMDS(groupmat,distance="euclidean",labels=info, trymax = 100, autotransform = FALSE)
eco.nmds.eu 
plot(eco.nmds.eu)

permanova_eco.bc<-adonis(groupmat ~ info, permutations = 999, method="bray")
permanova_eco.bc

permanova_eco.eu<-adonis(groupmat ~ info, permutations = 999, method="euclidean")
permanova_eco.eu

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=info)
NMDS.eu<-data.frame(NMDS1.eu=eco.nmds.eu$points[,1],NMDS2.eu=eco.nmds.eu$points[,2],group=info)

ord.bc<-ordiellipse(eco.nmds.bc,info,display="sites",kind="sd", conf = 0.95, label=T)
ord.eu<-ordiellipse(eco.nmds.eu,info,display="sites",kind="sd", conf = 0.95, label=T)

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
	geom_point(stat = "identity", aes(fill=group), size=2, shape=21, color = "black")+
	geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
	scale_fill_manual(values=c("darkorchid4", "green", "tomato"),
										name="Ecosection", guide="legend") +
	guides(fill= guide_legend(override.aes = list(size=4)))+
	scale_colour_manual(values=c("darkorchid4", "green", "tomato"),
											guide=FALSE) +
	scale_y_continuous(limits=c(-1.1,1.4),breaks=seq(-1.1,1.4,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
	scale_x_continuous(limits=c(-1.3,0.9),breaks=seq(-1.3,0.9,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
	theme_bw()+
	theme(axis.text.x=element_text(size=12),
				axis.title.x=element_text(size=12),
				axis.title.y=element_text(angle=90,size=12), 
				axis.text.y=element_text(size=12), 
				panel.grid.minor=element_blank(),panel.grid.major=element_blank()) + coord_fixed() +
	annotate("text",x=-0.9,y=1.4,label="stress = 0.11",size=4)
#NMDS graph for the different clusters!

ggplot(NMDS.eu, aes(NMDS1.eu, NMDS2.eu))+
	geom_point(stat = "identity", aes(fill=group), size=2, shape=21, color = "black")+
	geom_path(data=df_ell.eu, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
	scale_fill_manual(values=c("darkorchid4", "green", "tomato"),
										name="Ecosection", guide="legend") +
	guides(fill= guide_legend(override.aes = list(size=4)))+
	scale_colour_manual(values=c("darkorchid4", "green", "tomato"),
											guide=FALSE) +
	scale_y_continuous(limits=c(-0.4,0.55),breaks=seq(-0.4,0.55,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
	scale_x_continuous(limits=c(-0.5,0.5),breaks=seq(-0.5,0.5,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
	theme_bw()+
	theme(axis.text.x=element_text(size=12),
				axis.title.x=element_text(size=12),
				axis.title.y=element_text(angle=90,size=12), 
				axis.text.y=element_text(size=12), 
				panel.grid.minor=element_blank(),panel.grid.major=element_blank()) + coord_fixed() +
	annotate("text",x=-0.37,y=0.55,label="stress = 0.11",size=4)
#NMDS graph for the different clusters!

#need to redo it slightly differently tho... can't just relabel previously calculated one
#calculate total for each year for each cluster then calculate proportions for them
#redo dendrogram to look nice and have color labels corresponding to NMDS color ellipses

#TABLE CREATION SECTION#

categories$Source[which(categories$Source == "Shoreline Rec.")] <- "Shoreline Recreation"
categories$Litter[which(categories$Litter == "Cigarettes")] <- "Cigarettes, Cigarette Filters"
categories$Litter[which(categories$Litter == "Food_Wrapp")] <- "Food Wrappers, Containers"
categories$Litter[which(categories$Litter == "Takeout_Co")] <- "Takeout Containers, Plastic"
categories$Litter[which(categories$Litter == "Takeout__1")] <- "Takeout Containers, Foam"
categories$Litter[which(categories$Litter == "Bottle_Cap")] <- "Bottle Caps, Plastic"
categories$Litter[which(categories$Litter == "Bottle_C_1")] <- "Bottle Caps, Metal"
categories$Litter[which(categories$Litter == "Lids__Plas")] <- "Lids, Plastic"
categories$Litter[which(categories$Litter == "Straws__St")] <- "Straws, Stirrers"
categories$Litter[which(categories$Litter == "Forks__Kni")] <- "Forks, Knives, Spoons"
categories$Litter[which(categories$Litter == "Beverage_B")] <- "Beverage Bottles, Plastic (2L or less)"
categories$Litter[which(categories$Litter == "Glass_Beve")] <- "Glass Beverage Bottles"
categories$Litter[which(categories$Litter == "Beverage_C")] <- "Beverages, Cans"
categories$Litter[which(categories$Litter == "Bags__Plas")] <- "Bags, Plastic"
categories$Litter[which(categories$Litter == "Other_plas")] <- "Other Plastic Bags"
categories$Litter[which(categories$Litter == "Bags__Pape")] <- "Bags, Paper"
categories$Litter[which(categories$Litter == "Cups_and_P")] <- "Cups and Plates, Paper"
categories$Litter[which(categories$Litter == "Cups_and_1")] <- "Cups and Plates, Plastic"
categories$Litter[which(categories$Litter == "Cups_and_2")] <- "Cups and Plates, Foam"
categories$Litter[which(categories$Litter == "Buoys_Floa")] <- "Buoys and Floats"
categories$Litter[which(categories$Litter == "Fishing_Li")] <- "Fishing Line"
categories$Litter[which(categories$Litter == "Fishing_Ne")] <- "Fishing Nets"
categories$Litter[which(categories$Litter == "Fishing_Lu")] <- "Fishing Lures, Light Sticks"
categories$Litter[which(categories$Litter == "F6_Pack_Ho")] <- "6-Pack Holders"
categories$Litter[which(categories$Litter == "Other_Pl_1")] <- "Other Plastic/Foam Packaging"
categories$Litter[which(categories$Litter == "Other_Pl_2")] <- "Other Plastic Bottles (Oil, Bleach, etc.)"
categories$Litter[which(categories$Litter == "Strapping_")] <- "Strapping Bands"
categories$Litter[which(categories$Litter == "Tobacco_Pa")] <- "Tobacco Packaging Wrappers"
categories$Litter[which(categories$Litter == "Appliances")] <- "Appliances (Refrigerators, Washers, etc.)"
categories$Litter[which(categories$Litter == "Cigar_Tips")] <- "Cigar Tips"
categories$Litter[which(categories$Litter == "Cigarette_")] <- "Cigarette Lighters"
categories$Litter[which(categories$Litter == "Building_M")] <- "Building Materials"
categories$Litter[which(categories$Litter == "Clothing__")] <- "Clothing, Shoes"
categories$Litter[which(categories$Litter == "Tampons_Ta")] <- "Tampons, Tampon Applicators"
categories$Litter[which(categories$Litter == "Foam_Piece")] <- "Foam Pieces"
categories$Litter[which(categories$Litter == "Glass_Piec")] <- "Glass Pieces"
categories$Litter[which(categories$Litter == "Plastic_Pi")] <- "Plastic Pieces"
categories$Litter[which(categories$Litter == "F55_Gallon")] <- "55-Gallon Drums"
categories$Litter[which(categories$Litter == "Car_Car_Pa")] <- "Car, Car Parts"
categories$Litter[which(categories$Litter == "Light_Bulb")] <- "Light Bulbs"
categories$Litter[which(categories$Litter == "Plastic_Sh")] <- "Plastic Sheeting"
categories$Litter[which(categories$Litter == "Pull_Tabs")] <- "Pull Tabs"
categories$Litter[which(categories$Litter == "Bait_Conta")] <- "Bait Containers"
categories$Litter[which(categories$Litter == "Crab_Lobst")] <- "Crab, Lobster, Fish Traps"
categories$Litter[which(categories$Litter == "Shotgun_Sh")] <- "Shotgun Shells"

recent <-categories[c(1:45), c(1:3)]

old <- data.frame()
lit <- c("55-Gallon Drums", "Car, Car Parts", "Light Bulbs", "Pallets", "Plastic Sheeting", "Crates", "Pull Tabs", "Bait Containers", "Crab, Lobster, Fish Traps", "Shotgun Shells")
sour <- c("Dumping", "Dumping", "Dumping", "Dumping", "Dumping", "Dumping", "Dumping", "Fishing", "Fishing", "Shoreline Recreation")
mater <- c("Metal", "Metal", "Glass", "Wood", "Plastic", "Plastic", "Plastic", "Plastic", "Other", "Metal")
old <- cbind(lit, sour, mater)
colnames(old) <- c("Litter", "Source", "Material")

a <- knitr::kable(categories, format = "latex", booktabs=TRUE, caption = "Litter Category Classification") %>%
	row_spec(0, bold = TRUE) %>% 
	kable_styling(latex_options = "striped")

kable_as_image(a, "litter_categories")

b <- knitr::kable(old, format = "latex", booktabs=TRUE, caption = "2008") %>%
	row_spec(0, bold = TRUE) %>% 
	kable_styling(latex_options = "basic")

kable_as_image(b, "litter_categories_2008")
