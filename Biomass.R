# BIOMASS SIZE SPECTRAAAAA #
# Or whatever the fuck it currently is #

##### PREP DATA #####
rm(list=ls())
#remove any extra stuff floating around R environment

setwd("~/Desktop/2017-2018/EOSC 575")
#so it knows where to find files

biomass <- read.csv(file="biomass.csv", stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
#upload raw data file

library(tidyverse)
#load library for ggplot and whatnot

library(cowplot)
#for nice graphs or whatever

library(leaflet)
#for plotting datapoints

library(vegan)
#for analysis stuff

library(ggdendro)
#for dendrograms

biom <- filter(biomass, region_name != "BC Inlets"
               #& region_name != "Northern Vancouver Island Offshore"
               #& region_name != "Broughton"
               & region_name != "Vancouver Island Inlets"
               & region_name != "Hecate Strait" & region_name != "Haida Basin"
               & Station %in% c("QC", "63", "QCS4", "JI23", "60", "CPE1", "54", "COOK B",
                                "56", "31", "UBC7", "UBC07", "68", "46", "37", "CS3B",
                                "CI07", "SS6", "SS7", "T01", "T02", "T03", "T04", "T05", "T06",
                                "T07", "T08", "T09", "CS10", "CS09", "CS07", "JS02", "CS1B",
                                "CS03", "T10", "CS02", "CS04", "CS05", "CS06", "CS08", "CS01",
                                "50", "34", "EP07", "EP06", "CHELSEA", "EP05", "T11", "CS00",
                                "T12", "T13", "JI22", "ODAS", "CS2B", "SPASS1", "SPASS5", "SP 5",
                                "SE11H", "SE11.3V", "SE11.2V", "SE11.1V", "SE10.1V", "SE10.2V",
                                "SE10.3V", "SE10H", "NH11", "FOX IS 2", "WP7", "E11H", "W11.3V",
                                "CYPRESS", "GH11", "FOX IS 1", "E11.3V", "N11H", "W11.2V",
                                "CYPRESS5", "G11H", "N11.3V", "E11.2V", "W11.1V", "CYPRESS4",
                                "G11.3V", "N11.2V", "E10.2V", "W10H", "CYPRESS3", "G11.2V",
                                "N11.1V", "E11.1V", "W10.3V", "CYPRESS2", "G11.1V", "W10.2V",
                                "CYPRESS1", "E10H", "E10.1V", "E10.3V", "E11H", "G10H", "G10.1V",
                                "G10.2V", "G10.3V", "N10H", "N10.1V", "N10.2V", "N10.3V", "W10H",
                                "W10.1V", "W11H"))
#get rid of some data poitns that I don't want

missing <- anti_join(biomass, biom, by="Key") %>%
  filter(lat<51)
#cpe2 50.7 gi7 51.15 (make dataset of west coast vancouver island data points)

stations <- filter(biomass, !Station %in% missing$Station)
#get rid of west coast vancouver island data points (to see which stations I do need!)

transect <- filter(biomass, Station %in% c("JS02", "QC", "63", "QCS4", "JI23", "60", "DFO2",
                                           "FZH01", "PRUTH", "RI1", "RI2", "RI3", "RI4", "RI5",
                                           "UBC07", "UBC7", "SS7", "SS6", "T01", "T02", "T03",
                                           "T04", "T05", "T06", "T07", "T08", "T09", "T10", "T11",
                                           "T12", "T13", "50", "CS00", "CS01", "CS02", "CS03",
                                           "CS04", "CS05", "CS06", "CS07", "CS08", "CS09", "CS10",
                                           "CHELSEA", "RI6", "34", "EP05", "EP06", "EP07", "CS2B"))
#these are the stations that I want to look at!

notrans <- anti_join(biomass, transect, by="Key")
#all the stations I'm not using (to see if I missed any that I need)

m <- leaflet(notrans) %>%
  addTiles() %>% 
  addCircles(data=notrans, lng = notrans$lon, lat = notrans$lat, popup = notrans$region_name)
#make map of (not used) datapoints

m
#print map of (not used) datapoints

n <- leaflet(transect) %>%
  addTiles() %>% 
  addCircles(data=transect, lng = transect$lon, lat = transect$lat, popup = transect$region_name)
#make map of datapoints

n
#print map of datapoints

o <- leaflet(biomass) %>%
  addTiles() %>% 
  addCircles(data=biomass, lng = biomass$lon, lat = biomass$lat, popup = biomass$region_name)

o

fifteen <- filter(biomass, Station %in% c("CS00", "CS01", "CS02", "CS03", "CS04", "CS05", "CS06", "CS07", "CS08", "CS09", "CS10", "CS1B", "CS2B", "CS3B"))

l <- leaflet(fifteen) %>%
  addTiles() %>% 
  addCircles(data=fifteen, lng = fifteen$lon, lat = fifteen$lat, popup = fifteen$Station)

l

#write_csv(transect, "biomass_chosen_sites.csv")
#make csv of chosen sites to include!
#manually deleted empty columns:
#ARCR.AMPH.....AMPHI.s3
#ARCR.CUMA.....CUMAC.s1
#ARCR.DECA.MYSI....MYSID.s1 - eliminated from main dataset as well (because = 0)
#ARCR.ISOP.....ISOPOD.s3
#CNID.ANTH.....ANTH.s2
#CNID.HYDR.SIPH....PHYS.s3 - eliminated from main dataset as well (because = 0)
#CNID.SCYF.....SCYPH.s1
#MOGA.HETE.....HETERO.s2 - eliminated from main dataset as well (because = 0)
#MOGA.HETE.....HETERO.s3 - eliminated from main dataset as well (because = 0)
#PROT......PROTO.s2
#UROC.THAL.....SALP.s1

##### CLUSTER ANALYSIS - SITES #####

mydatalong <- gather(biomass, Group, Biomass, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder) %>%
  filter(Biomass != 0)
#change from wide to long

simpledata <- mydatalong %>%
  select(Station, year, Group, Biomass, region_name) %>%
  group_by(Station, year, Group, region_name) %>% 
  summarise(Biomass=mean(Biomass))
#group by year and site

sites <- simpledata %>%
  group_by(Station, Group) %>% 
  summarise(Biomass=mean(Biomass), Region=first(region_name)) %>%
  ungroup()
#group by only site

years <- simpledata %>%
  group_by(year, Group) %>%
  summarise(Biomass=mean(Biomass))
#group by only year

#change all 3 datasets back into wide format for analysis:
simplewide <- spread(simpledata, Group, Biomass, fill=0)

sitewide <- spread(sites, Group, Biomass, fill=0)

yearwide <- spread(years, Group, Biomass, fill=0)

sitewide$Station[which(sitewide$Station=="T10")] <- c("T10Of", "T10Sh")

sitewide$Station[which(sitewide$Station=="UBC07")] <- c("UBC07In", "UBC07Sh")

sitewide$Region[which(sitewide$Region=="Haida Basin")] <- "Hecate Strait"

sitewide$Region[which(sitewide$Region=="Johnstone Discovery")] <- "Queen Charlotte Strait"

#let's focus on sites first:

#sitewide$Region <- as.factor(sitewide$Region)

siteinfo <- select(sitewide, Region)
#vertical vector with station names relating to matrix of biomass values

siteinfo <- sitewide$Region

sitewide$Station <- as.factor(sitewide$Station)

sitestat <- select(sitewide, Station)

sitewide <- as.data.frame(sitewide)

row.names(sitewide) <- sitewide[, "Station"]

row.names(sitewide)

colnames(sitewide)

sitemat <- sitewide %>%
  ungroup() %>% 
  select(-c(Region, Station))

clustmat <- as.matrix(sitemat)

clustmat <- data.matrix(sitemat)

str(clustmat)

str(sitewide)

#create a matrix with stations as row names
fillermat<-as.matrix(sitemat)
fillermat <- data.matrix(sitemat)
row.names(fillermat) <- sitewide[,1]
class(clustmat)<-"numeric"

rankindex(siteinfo, clustmat, indices = c("euc", "man", "gow", "bra", "kul"))

Bray_Curtis_Dissimilarity <- vegdist(clustmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)

dendr <- dendro_data(bcclust, type = "rectangle")

regsite <- select(sitewide, Region, Station)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "Station")

lab <- left_join(labs, regsite, by = "Station")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Region), size=3, angle=90) +
  #coord_flip() +
  #scale_y_reverse(expand=c(0.2, 0)) +
  scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank(), legend.position = "bottom")

##### CLUSTER ANALYSIS - YEARS #####

mydatalong <- gather(biomass, Group, Biomass, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder) %>%
  filter(Biomass != 0)
#change from wide to long

##### NMDS #####

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(clustmat,distance="bray",labels=siteinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(clustmat ~ siteinfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=siteinfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,siteinfo,display="sites",kind="sd", conf = 0.95, label=T)
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

a <- ggplot(NMDS.bc, aes(NMDS1.bc, NMDS2.bc))+
  geom_point(stat = "identity", aes(fill=group), size=3, shape=21, color = "black")+
  geom_path(data=df_ell.bc, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2) +
  scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
                    name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
                      guide=FALSE) +
  scale_y_continuous(limits=c(-2,2.5),breaks=seq(-2,2.5,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
  scale_x_continuous(limits=c(-3,2.5),breaks=seq(-3,2.5,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()#,
        #legend.position = "none"
        ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
#NMDS graph for the different ecosections!

a

##### REJECTED CODE #####
datamod <- read_csv(file="biomass_chosen_modified.csv")
#read it file with 5-10 size class separated into two

size4 <- read_csv(file="four_sizes.csv")
#read it file to group sizes classes together (<3, <5, 5-10, >10)

size9 <- read_csv(file="nine_sizes.csv")
#read it file to group sizes classes together (0.25-1, 1-2, 2-5, 5-7.5, ... etc. up to 250)

#change from long to wide:
mydatalong <- gather(mydata, group, Biomass, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder) %>%
  filter(Biomass != 0)
#four size groups dataset

modifiedlong <- gather(datamod, group, Biomass, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainderc) %>%
  filter(Biomass != 0)
#nine size groups dataset

#combine size group categories:
#note: changed it to A, B, C, instead of 1, 2, 3 to min. errors
four <- left_join(mydatalong, size4, by="group")
#combined dataset with four groups of size classes

nine <- left_join(modifiedlong, size9, by="group")
#combined dataset with nine groups of size classes

sums4 <- group_by(four, Key, size_class) %>%
  summarise(Biomass=sum(Biomass))

View(sums4)

sums4 %>%
  group_by(size_class) %>%
  summarise(ave=mean(Biomass))

sums9 <- group_by(nine, Key, size_class) %>%
  summarise(Biomass=sum(Biomass))

View(sums9)

sums9 %>%
  group_by(size_class) %>%
  summarise(ave=mean(Biomass))
