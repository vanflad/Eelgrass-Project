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

library(ggfortify)
#for PCA in ggplot

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

fifteen <- filter(biomass, year=="2015")

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
  select(region_name, Station, year, Group, Biomass, region_name) %>%
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

length(sitewide$Region)

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

##### CLUSTER ANALYSIS - REGION #####

regzoop <- simplewide %>%
  group_by(region_name) %>%
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder))

regioninfo <- as.data.frame(regzoop$region_name)

regionmat <- regzoop %>%
  ungroup() %>% 
  select(-region_name)

regionmat <- as.matrix(regionmat)

rownames(regionmat) <- regioninfo[, 1]

class(regionmat) <- "numeric"

rankindex(regioninfo, regionmat, indices = c("euc", "man", "gow", "bra", "kul"))

Bray_Curtis_Dissimilarity <- vegdist(regionmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)

dendr <- dendro_data(bcclust, type = "rectangle")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=3) +
  coord_flip() +
  scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank(), legend.position = "bottom")

##### CLUSTER ANALYSIS - YEARS #####

yearinfo <- as.data.frame(yearwide$year)

yearmat <- yearwide %>%
  ungroup() %>% 
  select(-year)

yearmat <- as.matrix(yearmat)

rownames(yearmat) <- yearinfo[, 1]

class(yearmat) <- "numeric"

rankindex(yearinfo, yearmat, indices = c("euc", "man", "gow", "bra", "kul"))

Bray_Curtis_Dissimilarity <- vegdist(yearmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)

dendr <- dendro_data(bcclust, type = "rectangle")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=3) +
  coord_flip() +
  scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank(), legend.position = "bottom")

#how to try to interpret this later - cold vs warm ENSO cycle years? or look at dom zoop types

##### CLUSTER ANALYSIS - YEAR + REGION #####

regyear <- simplewide %>%
  group_by(year, region_name) %>%
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder))
  
yearinfo <- as.data.frame(regyear$year)

reginfo <- as.data.frame(regyear$region_name)

ry <- mutate(regyear, RY=paste(region_name, year, sep=" "))

ryinfo <- as.data.frame(ry$RY)

rymat <- regyear %>%
  ungroup() %>% 
  select(-c(year, region_name))

rymat <- as.matrix(rymat)

rownames(rymat) <- ryinfo[, 1]

class(rymat) <- "numeric"

rankindex(reginfo, rymat, indices = c("euc", "man", "gow", "bra", "kul"))

Bray_Curtis_Dissimilarity <- vegdist(rymat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)

dendr <- dendro_data(bcclust, type = "rectangle")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=3) +
  coord_flip() +
  scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank(), legend.position = "bottom")
  
##### LENGTH DATA #####

lengthdata <- simpledata
#make a copy of dataset to modify by size, ignoring groups

lengthdata$Group[which(lengthdata$Group=="ANNE.POLY.....POLY.larvae.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ANNE.POLY.....POLY.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ANNE.POLY.....POLY.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ANNE.POLY.....POLY.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ARCR......CIRRI.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR......CRUST.larvae.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.AMPH.....AMPHI.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.AMPH.....AMPHI.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.AMPH.....AMPHI.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ARCR.AMPH.GAMM....GAMM.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.AMPH.GAMM....GAMM.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.AMPH.GAMM....GAMM.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ARCR.AMPH.HYPE....HYPER.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.AMPH.HYPE....HYPER.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.AMPH.HYPE....HYPER.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ARCR.CLAD.....CLADO.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.COPE.....COPEN.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.COPE.....COPE.other..2mm")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.COPE.....COPE.other..2mm.1")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.COPE.CALA....CALA..1mm")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.COPE.CALA....CALA..3mm.1mm")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.COPE.CALA....CALA..5mm.3mm")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.COPE.CALA....CALA..5mm")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.CUMA.....CUMAC.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.CUMA.....CUMAC.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.CUMA.....CUMAC.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ARCR.DECA.....DECA.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.DECA.....DECA.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.DECA.....DECA.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ARCR.DECA.CARI....CARID.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.DECA.CARI....CARID.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.DECA.CARI....CARID.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ARCR.DECA.MYSI....MYSID.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.DECA.MYSI....MYSID.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ARCR.EUPH.....EUPH.eggs")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.EUPH.....EUPH.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.EUPH.....EUPH.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.EUPH.....EUPH.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ARCR.ISOP.....ISOPOD.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.ISOP.....ISOPOD.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="ARCR.ISOP.....ISOPOD.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ARCR.OSTR.....OSTRA..3mm")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ARCR.OSTR.....OSTRA..3mm.1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="CHAE......CHAET.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="CHAE......CHAET.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="CHAE......CHAET.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="CHAE......EUKHA.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="CHAE......EUKHA.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="CHAE......PATEL.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="CHAE......PATEL.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="CNID.ANTH.....ANTH.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="CNID.ANTH.....ANTH.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="CNID.HYDR.....MEDUS.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="CNID.HYDR.....MEDUS.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="CNID.HYDR.....MEDUS.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="CNID.HYDR.SIPH....CALYC.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="CNID.HYDR.SIPH....CALYC.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="CNID.HYDR.SIPH....CALYC.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="CNID.HYDR.SIPH....PHYS.float..5mm")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="CNID.HYDR.SIPH....PHYS.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="CNID.HYDR.SIPH....PHYS.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="CNID.SCYF.....SCYPH.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="CNID.SCYF.....SCYPH.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="CNID.SCYF.....SCYPH.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="CTEN......CTENO.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="CTEN......CTENO.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="CTEN......CTENO.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="ECHI......ECHIN.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="ECTO......BRYOC.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="MOCE.OCTO.....CEPH.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="MOCE.OCTO.....CEPH.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="MOCE.OCTO.....CEPH.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="MOGA.HETE.....HETERO.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="MOGA.PTER.....CLIO.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="MOGA.PTER.....CLIO.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="MOGA.PTER.....CLIO.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="MOGA.PTER.....GYMNO.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="MOGA.PTER.....GYMNO.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="MOGA.PTER.....GYMNO.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="MOGA.PTER.....LIM...s0")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="MOGA.PTER.....LIM...s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="MOGA.PTER.....LIM...s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="MOGA.PTER.....LIM...s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="MOLL......MOLL.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="PROT......PROTO.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="PROT......PROTO.s2")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="UROC.LARV.....LARVAC.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="UROC.LARV.....LARVAC.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="UROC.LARV.....LARVAC.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="UROC.THAL.....DOLIO.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="UROC.THAL.....DOLIO.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="UROC.THAL.....DOLIO.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="UROC.THAL.....SALP.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="UROC.THAL.....SALP.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="UROC.THAL.....SALP.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="VERT.PISC.....PISCES.egg.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="VERT.PISC.....PISCES.s1")] <- "<5"
lengthdata$Group[which(lengthdata$Group=="VERT.PISC.....PISCES.s2")] <- "5-10"
lengthdata$Group[which(lengthdata$Group=="VERT.PISC.....PISCES.s3")] <- ">10"
lengthdata$Group[which(lengthdata$Group=="XXXX....Remainder")] <- "5-10"
#get rid of everything else because it has no size specified? or assume ave medium

lengthsum <- lengthdata %>%
  group_by(Station, year, region_name, Group) %>%
  summarise(Biomass=sum(Biomass))

lengthsum$Group <- as.factor(lengthsum$Group)

lengthsum$Group <- factor(lengthsum$Group, levels(lengthsum$Group)[c(1, 3, 2)])

ggplot(lengthsum, aes(Group, Biomass))+
  geom_bar(aes(fill=region_name), stat = "identity", position = "dodge")

lengthsum$year <- as.factor(lengthsum$year)

ggplot(lengthsum, aes(Group, Biomass))+
  geom_bar(aes(), stat = "identity", position = "dodge")+
  facet_wrap(~year)

##### BAR PLOTS #####

simpledata$Group[which(simpledata$Group=="ANNE.POLY.....POLY.larvae.s1")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="ANNE.POLY.....POLY.s1")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="ANNE.POLY.....POLY.s2")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="ANNE.POLY.....POLY.s3")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="ARCR......CIRRI.s1")] <- "Crustacean Larvae"
simpledata$Group[which(simpledata$Group=="ARCR......CRUST.larvae.s1")] <- "Crustacean Larvae"
simpledata$Group[which(simpledata$Group=="ARCR.AMPH.....AMPHI.s1")] <- "Amphipod"
simpledata$Group[which(simpledata$Group=="ARCR.AMPH.....AMPHI.s2")] <- "Amphipod"
simpledata$Group[which(simpledata$Group=="ARCR.AMPH.....AMPHI.s3")] <- "Amphipod"
simpledata$Group[which(simpledata$Group=="ARCR.AMPH.GAMM....GAMM.s1")] <- "Amphipod"
simpledata$Group[which(simpledata$Group=="ARCR.AMPH.GAMM....GAMM.s2")] <- "Amphipod"
simpledata$Group[which(simpledata$Group=="ARCR.AMPH.GAMM....GAMM.s3")] <- "Amphipod"
simpledata$Group[which(simpledata$Group=="ARCR.AMPH.HYPE....HYPER.s1")] <- "Amphipod"
simpledata$Group[which(simpledata$Group=="ARCR.AMPH.HYPE....HYPER.s2")] <- "Amphipod"
simpledata$Group[which(simpledata$Group=="ARCR.AMPH.HYPE....HYPER.s3")] <- "Amphipod"
simpledata$Group[which(simpledata$Group=="ARCR.CLAD.....CLADO.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.COPE.....COPEN.s1")] <- "Copepod"
simpledata$Group[which(simpledata$Group=="ARCR.COPE.....COPE.other..2mm")] <- "Copepod"
simpledata$Group[which(simpledata$Group=="ARCR.COPE.....COPE.other..2mm.1")] <- "Copepod"
simpledata$Group[which(simpledata$Group=="ARCR.COPE.CALA....CALA..1mm")] <- "Copepod"
simpledata$Group[which(simpledata$Group=="ARCR.COPE.CALA....CALA..3mm.1mm")] <- "Copepod"
simpledata$Group[which(simpledata$Group=="ARCR.COPE.CALA....CALA..5mm.3mm")] <- "Copepod"
simpledata$Group[which(simpledata$Group=="ARCR.COPE.CALA....CALA..5mm")] <- "Copepod"
simpledata$Group[which(simpledata$Group=="ARCR.CUMA.....CUMAC.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.CUMA.....CUMAC.s2")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.CUMA.....CUMAC.s3")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.DECA.....DECA.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.DECA.....DECA.s2")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.DECA.....DECA.s3")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.DECA.CARI....CARID.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.DECA.CARI....CARID.s2")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.DECA.CARI....CARID.s3")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.DECA.MYSI....MYSID.s2")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.DECA.MYSI....MYSID.s3")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.EUPH.....EUPH.eggs")] <- "Euphausiid"
simpledata$Group[which(simpledata$Group=="ARCR.EUPH.....EUPH.s1")] <- "Euphausiid"
simpledata$Group[which(simpledata$Group=="ARCR.EUPH.....EUPH.s2")] <- "Euphausiid"
simpledata$Group[which(simpledata$Group=="ARCR.EUPH.....EUPH.s3")] <- "Euphausiid"
simpledata$Group[which(simpledata$Group=="ARCR.ISOP.....ISOPOD.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.ISOP.....ISOPOD.s2")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.ISOP.....ISOPOD.s3")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.OSTR.....OSTRA..3mm")] <- "Other"
simpledata$Group[which(simpledata$Group=="ARCR.OSTR.....OSTRA..3mm.1")] <- "Other"
simpledata$Group[which(simpledata$Group=="CHAE......CHAET.s1")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="CHAE......CHAET.s2")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="CHAE......CHAET.s3")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="CHAE......EUKHA.s2")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="CHAE......EUKHA.s3")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="CHAE......PATEL.s2")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="CHAE......PATEL.s3")] <- "Chaeto/Poly"
simpledata$Group[which(simpledata$Group=="CNID.ANTH.....ANTH.s1")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.ANTH.....ANTH.s2")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.HYDR.....MEDUS.s1")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.HYDR.....MEDUS.s2")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.HYDR.....MEDUS.s3")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.HYDR.SIPH....CALYC.s1")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.HYDR.SIPH....CALYC.s2")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.HYDR.SIPH....CALYC.s3")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.HYDR.SIPH....PHYS.float..5mm")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.HYDR.SIPH....PHYS.s1")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.HYDR.SIPH....PHYS.s2")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.SCYF.....SCYPH.s1")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.SCYF.....SCYPH.s2")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CNID.SCYF.....SCYPH.s3")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CTEN......CTENO.s1")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CTEN......CTENO.s2")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="CTEN......CTENO.s3")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="ECHI......ECHIN.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="ECTO......BRYOC.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOCE.OCTO.....CEPH.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOCE.OCTO.....CEPH.s2")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOCE.OCTO.....CEPH.s3")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.HETE.....HETERO.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.PTER.....CLIO.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.PTER.....CLIO.s2")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.PTER.....CLIO.s3")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.PTER.....GYMNO.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.PTER.....GYMNO.s2")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.PTER.....GYMNO.s3")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.PTER.....LIM...s0")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.PTER.....LIM...s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.PTER.....LIM...s2")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOGA.PTER.....LIM...s3")] <- "Other"
simpledata$Group[which(simpledata$Group=="MOLL......MOLL.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="PROT......PROTO.s1")] <- "Other"
simpledata$Group[which(simpledata$Group=="PROT......PROTO.s2")] <- "Other"
simpledata$Group[which(simpledata$Group=="UROC.LARV.....LARVAC.s1")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="UROC.LARV.....LARVAC.s2")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="UROC.LARV.....LARVAC.s3")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="UROC.THAL.....DOLIO.s1")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="UROC.THAL.....DOLIO.s2")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="UROC.THAL.....DOLIO.s3")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="UROC.THAL.....SALP.s1")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="UROC.THAL.....SALP.s2")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="UROC.THAL.....SALP.s3")] <- "Gelatinous"
simpledata$Group[which(simpledata$Group=="VERT.PISC.....PISCES.egg.s1")] <- "Fish"
simpledata$Group[which(simpledata$Group=="VERT.PISC.....PISCES.s1")] <- "Fish"
simpledata$Group[which(simpledata$Group=="VERT.PISC.....PISCES.s2")] <- "Fish"
simpledata$Group[which(simpledata$Group=="VERT.PISC.....PISCES.s3")] <- "Fish"
simpledata$Group[which(simpledata$Group=="XXXX....Remainder")] <- "Other"

summed <- simpledata %>%
  group_by(Station, year, region_name, Group) %>%
  summarize(Biomass=sum(Biomass))

grouped <- summed %>%
  spread(Group, Biomass, fill=0)

ggplot(summed, aes(region_name, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

ggplot(summed, aes(year, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

unique(grouped$region_name)

##### SIMPER #####

decwide <- mutate(simplewide, decade = ifelse(year %in% 1970:1979, "1970s",
                                              ifelse(year %in% 1980:1989, "1980s",
                                                     ifelse(year %in% 1990:1999, "1990s",
                                                            ifelse(year %in% 2000:2009, "2000s",
                                                                   ifelse(year %in% 2010:2017, "2010s"))))))

decinfo <- decwide$decade

decmat <- decwide %>%
  ungroup() %>%
  select(-c(Station, year, region_name, decade))

decsimper <- simper(decmat, decinfo)

yearinfo <- simplewide$year

yearmat <- simplewide %>%
  ungroup() %>%
  select(-c(Station, year, region_name))

yearsimper <- simper(yearmat, yearinfo)

sitesimper <- simper(sitemat, siteinfo)

##### ANOSIM #####

siteanosim <- anosim(sitemat, siteinfo)

summary(siteanosim)

plot(siteanosim)

##### NMDS - REGION #####

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

##### NMDS - YEAR #####

howmanyyrs <- simplewide %>%
  ungroup() %>%
  count(year)
#need to eliminate 1975 (copepods only) and 1989 (only two sampling events)

lessyears <- filter(simplewide, !year %in% c(1975, 1989))

yearinfo <- as.data.frame(lessyears$year)

yearmat <- lessyears %>%
  ungroup() %>% 
  select(-c(year, Station, region_name))

yearmat <- as.matrix(yearmat)

rownames(yearmat) <- yearinfo[, 1]

class(yearmat) <- "numeric"

rankindex(yearinfo, yearmat, indices = c("euc", "man", "gow", "bra", "kul"))

lessyears$year <- as.factor(lessyears$year)

yearinfo <- lessyears$year

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(yearmat,distance="bray",labels=yearinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(yearmat ~ yearinfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=yearinfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,yearinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  #scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                  name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  #scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                    guide=FALSE) +
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

##### NMDS - DECADE #####

decades <- c("1980s", "1980s", "1980s", "1980s", "1990s", "1990s", "1990s", "1990s", "1990s", "1990s", "1990s", "1990s", "1990s", "1990s", "2000s", "2000s", "2000s", "2000s", "2000s", "2000s", "2000s", "2000s", "2000s", "2000s", "2010s", "2010s", "2010s", "2010s", "2010s", "2010s", "2010s", "2010s")

no70s <- filter(yearwide, year != 1975)

yearinfo <- as.data.frame(no70s$year)

yearmat <- no70s %>%
  ungroup() %>% 
  select(-c(year))

yearmat <- as.matrix(yearmat)

decades <- as.data.frame(decades)

rownames(yearmat) <- decades[, 1]

class(yearmat) <- "numeric"

rankindex(decades, yearmat, indices = c("euc", "man", "gow", "bra", "kul"))

decades$decades <- as.factor(decades$decades)

Decades <- decades$decades

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(yearmat,distance="bray",labels=Decades, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(yearmat ~ Decades, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=Decades)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,Decades,display="sites",kind="sd", conf = 0.95, label=T)
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
  #scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                  name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  #scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                    guide=FALSE) +
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

##### NMDS - EARLY AND LATE DECADES #####

eras <- c("Late 1980s", "Late 1980s", "Late 1980s", "Early 1990s", "Early 1990s", "Early 1990s", "Early 1990s", "Early 1990s", "Late 1990s", "Late 1990s", "Late 1990s", "Late 1990s", "Late 1990s", "Early 2000s", "Early 2000s", "Early 2000s", "Early 2000s", "Early 2000s", "Late 2000s", "Late 2000s", "Late 2000s", "Late 2000s", "Late 2000s", "Early 2010s", "Early 2010s", "Early 2010s", "Early 2010s", "Early 2010s", "Late 2010s", "Late 2010s", "Late 2010s")

late80splus <- filter(yearwide, !year %in% c(1975, 1983))

yearinfo <- as.data.frame(late80splus$year)

yearmat <- late80splus %>%
  ungroup() %>% 
  select(-c(year))

yearmat <- as.matrix(yearmat)

eras <- as.data.frame(eras)

rownames(yearmat) <- eras[, 1]

class(yearmat) <- "numeric"

rankindex(eras, yearmat, indices = c("euc", "man", "gow", "bra", "kul"))

eras$eras <- as.factor(eras$eras)

Eras <- eras$eras

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(yearmat,distance="gower",labels=Eras, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(yearmat ~ Eras, permutations = 999, method="gower")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=Eras)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,Eras,display="sites",kind="sd", conf = 0.95, label=T)
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
  #scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                  name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  #scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                    guide=FALSE) +
  #scale_y_continuous(limits=c(-2,2.5),breaks=seq(-2,2.5,by=.5),name = "NMDS2, Proportion-based dissimilarity")+
  #scale_x_continuous(limits=c(-3,2.5),breaks=seq(-3,2.5,by=.5),name = "NMDS1, Proportion-based dissimilarity")+
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

##### PCA #####

autoplot(prcomp(clustmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=sitewide$Region), size=2.5, shape=21, color = "black")+
  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow"),
                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

autoplot(prcomp(yearmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=yearwide$year), size=2.5, shape=21, color = "black")+
  #scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow"),
  #                  name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

#possible things to add later: eliminate 1975 copepods only sampling; group together some of the zoops for more clarity; explore %s of PC's

##### NVI SHELF - BAR PLOTS #####

class(decwide$year) <- "character"

summed %>%
  filter(region_name=="Northern Vancouver Island Shelf") %>%
  ggplot(aes(Station, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

summed %>%
  filter(region_name=="Northern Vancouver Island Shelf") %>%
  ggplot(aes(year, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

nvis <- decwide %>%
  filter(region_name=="Northern Vancouver Island Shelf") %>%
  ungroup()

##### NVI SHELF - CLUSTERS #####

nvisyear <- nvis %>%
  group_by(year, decade) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

nvissite <- nvis %>%
  group_by(Station) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

nvisymat <- select(nvisyear, ANNE.POLY.....POLY.s1:XXXX....Remainder)

nvissmat <- select(nvissite, ANNE.POLY.....POLY.s1:XXXX....Remainder)

nvisymat <- as.matrix(nvisymat)

nvissmat <- as.matrix(nvissmat)

yearinfo <- as.data.frame(nvisyear$year)

siteinfo <- as.data.frame(nvissite$Station)

rownames(nvisymat) <- yearinfo[, 1]

rownames(nvissmat) <- siteinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(nvisymat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

nvisdec <- nvisyear %>%
  select(year, decade)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "year")

#class(labs$year) <- "character"

lab <- full_join(labs, nvisdec, by = "year")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$decade), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

Bray_Curtis_Dissimilarity <- vegdist(nvissmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")
ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "none")

##### NVI SHELF - NMDS/PCA #####

class(nvis$year) <- "integer"

nvismat <- select(nvis, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

nvismat <- as.matrix(nvismat)

autoplot(prcomp(nvismat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=nvis$year), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
# change fill = nvis $ Station in geom_point if want to see stations instead of years colored

filtyear <- nvis %>%
  filter(!year %in% c("1989", "2017"))

yearmat <- select(filtyear, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

class(filtyear$year) <- "character"

yearinfo <- filtyear$decade
#change this back to filtyear $ year if you want to see the year by year breakdown instead

filtsite <- nvis %>%
  add_count(Station) %>%
  filter(n > 2)

sitemat <- select(filtsite, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

siteinfo <- filtsite$Station

eco.nmds.bc<- metaMDS(yearmat,distance="bray",labels=yearinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
permanova_eco.bc<-adonis(yearmat ~ yearinfo, permutations = 999, method="bray")
permanova_eco.bc
NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=yearinfo)
ord.bc<-ordiellipse(eco.nmds.bc,yearinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  #scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                  name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  #scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                    guide=FALSE) +
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()
  ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
a

eco.nmds.bc<- metaMDS(sitemat,distance="bray",labels=siteinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
permanova_eco.bc<-adonis(sitemat ~ siteinfo, permutations = 999, method="bray")
permanova_eco.bc
NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=siteinfo)
ord.bc<-ordiellipse(eco.nmds.bc,siteinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  #scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                  name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  #scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                    guide=FALSE) +
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()
  ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
a

##### VI INLETS - BAR PLOTS #####

summed %>%
  filter(region_name=="Vancouver Island Inlets") %>%
  ggplot(aes(Station, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

summed %>%
  filter(region_name=="Vancouver Island Inlets") %>%
  ggplot(aes(year, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

ivi <- decwide %>%
  filter(region_name=="Vancouver Island Inlets") %>%
  ungroup()

##### VI INLETS - CLUSTERS #####

iviyear <- ivi %>%
  group_by(year, decade) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

ivisite <- ivi %>%
  group_by(Station) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

iviymat <- select(iviyear, ANNE.POLY.....POLY.s1:XXXX....Remainder)

ivismat <- select(ivisite, ANNE.POLY.....POLY.s1:XXXX....Remainder)

iviymat <- as.matrix(iviymat)

ivismat <- as.matrix(ivismat)

yearinfo <- as.data.frame(iviyear$year)

siteinfo <- as.data.frame(ivisite$Station)

rownames(iviymat) <- yearinfo[, 1]

rownames(ivismat) <- siteinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(iviymat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

ividec <- iviyear %>%
  select(year, decade)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "year")

#class(labs$year) <- "character"

lab <- full_join(labs, ividec, by = "year")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$decade), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

Bray_Curtis_Dissimilarity <- vegdist(ivismat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")
ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "none")

##### VI INLETS - NMDS/PCA #####

#class(ivi$year) <- "integer"

ivimat <- select(ivi, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

ivimat <- as.matrix(ivimat)

autoplot(prcomp(ivimat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=ivi$year), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

autoplot(prcomp(ivimat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=ivi$Station), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

filtyear <- ivi %>%
  filter(year != "2004")

yearmat <- select(filtyear, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

class(filtyear$year) <- "character"

yearinfo <- filtyear$year
#change this back to filtyear $ year if you want to see the year by year breakdown instead

eco.nmds.bc<- metaMDS(yearmat,distance="bray",labels=yearinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
permanova_eco.bc<-adonis(yearmat ~ yearinfo, permutations = 999, method="bray")
permanova_eco.bc
NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=yearinfo)
ord.bc<-ordiellipse(eco.nmds.bc,yearinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  #scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                  name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  #scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                    guide=FALSE) +
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()
  ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
a

##### HECATE STRAIT - BAR PLOTS #####

summed %>%
  filter(region_name %in% c("Hecate Strait", "Haida Basin")) %>%
  ggplot(aes(Station, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

summed %>%
  filter(region_name %in% c("Hecate Strait", "Haida Basin")) %>%
  ggplot(aes(year, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

hs <- decwide %>%
  filter(region_name %in% c("Hecate Strait", "Haida Basin")) %>%
  ungroup()

##### HECATE STRAIT - CLUSTERS #####

hsyear <- hs %>%
  group_by(year, decade) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

hssite <- hs %>%
  group_by(Station) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

hsymat <- select(hsyear, ANNE.POLY.....POLY.s1:XXXX....Remainder)

hssmat <- select(hssite, ANNE.POLY.....POLY.s1:XXXX....Remainder)

hsymat <- as.matrix(hsymat)

hssmat <- as.matrix(hssmat)

yearinfo <- as.data.frame(hsyear$year)

siteinfo <- as.data.frame(hssite$Station)

rownames(hsymat) <- yearinfo[, 1]

rownames(hssmat) <- siteinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(hsymat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

hsdec <- hsyear %>%
  select(year, decade)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "year")

#class(labs$year) <- "character"

lab <- full_join(labs, hsdec, by = "year")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$decade), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

Bray_Curtis_Dissimilarity <- vegdist(hssmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")
ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "none")

##### HECATE STRAIT - NMDS/PCA #####

class(hs$year) <- "integer"

hsmat <- select(hs, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

hsmat <- as.matrix(hsmat)

autoplot(prcomp(hsmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=hs$year), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

autoplot(prcomp(hsmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=hs$Station), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

#only GI02, SS2, SS4, SS5
#1983, 1991, 1998, 2006, 2015, 2016

filtyear <- hs %>%
  filter(year %in% c("1983", "1991", "1998", "2006", "2015", "2016"))

yearmat <- select(filtyear, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

class(filtyear$year) <- "character"

yearinfo <- filtyear$year

filtsite <- hs %>%
  filter(Station %in% c("GI02", "SS2", "SS4", "SS5"))

sitemat <- select(filtsite, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

siteinfo <- filtsite$Station

eco.nmds.bc<- metaMDS(yearmat,distance="bray",labels=yearinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
permanova_eco.bc<-adonis(yearmat ~ yearinfo, permutations = 999, method="bray")
permanova_eco.bc
NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=yearinfo)
ord.bc<-ordiellipse(eco.nmds.bc,yearinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  #scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                  name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  #scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                    guide=FALSE) +
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()
  ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
a
#very cool NMDS that shows six years across the decades with only some overlap, some very diff!!
#actually it was just a fluke... reran it and it looks messy like all the others do... blech

eco.nmds.bc<- metaMDS(sitemat,distance="bray",labels=siteinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
permanova_eco.bc<-adonis(sitemat ~ siteinfo, permutations = 999, method="bray")
permanova_eco.bc
NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=siteinfo)
ord.bc<-ordiellipse(eco.nmds.bc,siteinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  #scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                  name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  #scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                    guide=FALSE) +
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()
  ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
a
#not significant as can be seen from GI02 overlapping all the SS sites, no diffs here

##### QC STRAIT - BAR PLOTS #####

summed %>%
  filter(region_name %in% c("Queen Charlotte Strait", "Johnstone Discovery")) %>%
  ggplot(aes(Station, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

summed %>%
  filter(region_name %in% c("Queen Charlotte Strait", "Johnstone Discovery")) %>%
  ggplot(aes(year, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

qcs <- decwide %>%
  filter(region_name %in% c("Queen Charlotte Strait", "Johnstone Discovery")) %>%
  ungroup()

##### QC STRAIT - CLUSTERS #####

qcsyear <- qcs %>%
  group_by(year, decade) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

qcssite <- qcs %>%
  group_by(Station) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

qcsymat <- select(qcsyear, ANNE.POLY.....POLY.s1:XXXX....Remainder)

qcssmat <- select(qcssite, ANNE.POLY.....POLY.s1:XXXX....Remainder)

qcsymat <- as.matrix(qcsymat)

qcssmat <- as.matrix(qcssmat)

class(qcsyear$year)

yearinfo <- as.data.frame(qcsyear$year)

siteinfo <- as.data.frame(qcssite$Station)

rownames(qcsymat) <- yearinfo[, 1]

rownames(qcssmat) <- siteinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(qcsymat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

Bray_Curtis_Dissimilarity <- vegdist(qcssmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")
ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "none")

##### QC STRAIT - NMDS/PCA #####

class(qcs$year) <- "integer"

qcsmat <- select(qcs, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

qcsmat <- as.matrix(qcsmat)

autoplot(prcomp(qcsmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=qcs$year), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

autoplot(prcomp(qcsmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=qcs$Station), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#too few points for any NMDS plots - only PCA here!

##### NVI OFFSHORE - BAR PLOTS #####

summed %>%
  filter(region_name=="Northern Vancouver Island Offshore") %>%
  ggplot(aes(Station, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

summed %>%
  filter(region_name=="Northern Vancouver Island Offshore") %>%
  ggplot(aes(year, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

nvio <- decwide %>%
  filter(region_name=="Northern Vancouver Island Offshore") %>%
  ungroup()

##### NVI OFFSHORE - CLUSTERS #####

nvioyear <- nvio %>%
  group_by(year, decade) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

nviosite <- nvio %>%
  group_by(Station) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

nvioymat <- select(nvioyear, ANNE.POLY.....POLY.s1:XXXX....Remainder)

nviosmat <- select(nviosite, ANNE.POLY.....POLY.s1:XXXX....Remainder)

nvioymat <- as.matrix(nvioymat)

nviosmat <- as.matrix(nviosmat)

class(nvioyear$year)

yearinfo <- as.data.frame(nvioyear$year)

siteinfo <- as.data.frame(nviosite$Station)

rownames(nvioymat) <- yearinfo[, 1]

rownames(nviosmat) <- siteinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(nvioymat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

nviodec <- nvioyear %>%
  select(year, decade)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "year")

lab <- full_join(labs, nviodec, by = "year")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$decade), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

Bray_Curtis_Dissimilarity <- vegdist(nviosmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")
ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "none")

##### NVI OFFSHORE - NMDS/PCA #####

class(nvio$year) <- "integer"

nviomat <- select(nvio, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

nviomat <- as.matrix(nviomat)

autoplot(prcomp(nviomat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=nvio$year), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

autoplot(prcomp(nviomat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=nvio$Station), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

filtyear <- nvio %>%
  filter(year %in% c("1998", "2001", "2002"))

yearmat <- select(filtyear, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

class(filtyear$year) <- "character"

yearinfo <- filtyear$year

sitemat <- select(nvio, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

siteinfo <- nvio$Station

eco.nmds.bc<- metaMDS(yearmat,distance="bray",labels=yearinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
permanova_eco.bc<-adonis(yearmat ~ yearinfo, permutations = 999, method="bray")
permanova_eco.bc
NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=yearinfo)
ord.bc<-ordiellipse(eco.nmds.bc,yearinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  guides(fill= guide_legend(override.aes = list(size=4)))+
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()
  ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
a
# NOTE REALLY COOL GRAPH HERE WITH CLEAR DISTINCTION BETWEEN YEARS FOR NVI OFFSHORE!

eco.nmds.bc<- metaMDS(sitemat,distance="bray",labels=siteinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
permanova_eco.bc<-adonis(sitemat ~ siteinfo, permutations = 999, method="bray")
permanova_eco.bc
NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=siteinfo)
ord.bc<-ordiellipse(eco.nmds.bc,siteinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  #scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                  name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  #scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                    guide=FALSE) +
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()
  ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
a
#not significant, big old mess with all the stations just overlapping each other

##### BROUGHTON ARCH - BAR PLOTS #####

summed %>%
  filter(region_name=="Broughton") %>%
  ggplot(aes(Station, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

summed %>%
  filter(region_name=="Broughton") %>%
  ggplot(aes(year, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

ba <- decwide %>%
  filter(region_name=="Broughton") %>%
  ungroup()

##### BROUGHTON ARCH - CLUSTERS #####

bayear <- ba %>%
  group_by(year, decade) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

basite <- ba %>%
  group_by(Station) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

baymat <- select(bayear, ANNE.POLY.....POLY.s1:XXXX....Remainder)

basmat <- select(basite, ANNE.POLY.....POLY.s1:XXXX....Remainder)

baymat <- as.matrix(baymat)

basmat <- as.matrix(basmat)

class(bayear$year)

yearinfo <- as.data.frame(bayear$year)

siteinfo <- as.data.frame(basite$Station)

rownames(baymat) <- yearinfo[, 1]

rownames(basmat) <- siteinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(baymat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

badec <- bayear %>%
  select(year, decade)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "year")

lab <- full_join(labs, badec, by = "year")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$decade), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

Bray_Curtis_Dissimilarity <- vegdist(basmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")
ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "none")

##### BROUGHTON ARCH - NMDS/PCA #####

class(ba$year) <- "integer"

bamat <- select(ba, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

bamat <- as.matrix(bamat)

autoplot(prcomp(bamat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=ba$year), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

autoplot(prcomp(bamat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=ba$Station), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

yearmat <- select(ba, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

class(ba$year) <- "character"

yearinfo <- ba$year

eco.nmds.bc<- metaMDS(yearmat,distance="bray",labels=yearinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
permanova_eco.bc<-adonis(yearmat ~ yearinfo, permutations = 999, method="bray")
permanova_eco.bc
NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=yearinfo)
ord.bc<-ordiellipse(eco.nmds.bc,yearinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  guides(fill= guide_legend(override.aes = list(size=4)))+
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()
  ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
a
#oooooh 2006 and 2010 are different. PCA says there be more jellies in da more recent year

##### BC INLETS - BAR PLOTS #####

summed %>%
  filter(region_name=="BC Inlets") %>%
  ggplot(aes(Station, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

summed %>%
  filter(region_name=="BC Inlets") %>%
  ggplot(aes(year, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

bci <- decwide %>%
  filter(region_name=="BC Inlets") %>%
  ungroup()

##### BC INLETS - CLUSTERS #####

bciyear <- bci %>%
  group_by(year, decade) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

bcisite <- bci %>%
  group_by(Station) %>% 
  summarise(ANNE.POLY.....POLY.s1=mean(ANNE.POLY.....POLY.s1), ANNE.POLY.....POLY.s2=mean(ANNE.POLY.....POLY.s2), ANNE.POLY.....POLY.s3=mean(ANNE.POLY.....POLY.s3), ANNE.POLY.....POLY.larvae.s1=mean(ANNE.POLY.....POLY.larvae.s1), ARCR......CIRRI.s1=mean(ARCR......CIRRI.s1), ARCR.AMPH.....AMPHI.s1=mean(ARCR.AMPH.....AMPHI.s1), ARCR.AMPH.....AMPHI.s2=mean(ARCR.AMPH.....AMPHI.s2), ARCR.AMPH.....AMPHI.s3=mean(ARCR.AMPH.....AMPHI.s3), ARCR.AMPH.GAMM....GAMM.s1=mean(ARCR.AMPH.GAMM....GAMM.s1), ARCR.AMPH.GAMM....GAMM.s2=mean(ARCR.AMPH.GAMM....GAMM.s2), ARCR......CRUST.larvae.s1=mean(ARCR......CRUST.larvae.s1), ARCR.AMPH.GAMM....GAMM.s3=mean(ARCR.AMPH.GAMM....GAMM.s3), ARCR.AMPH.HYPE....HYPER.s1=mean(ARCR.AMPH.HYPE....HYPER.s1), ARCR.AMPH.HYPE....HYPER.s2=mean(ARCR.AMPH.HYPE....HYPER.s2), ARCR.AMPH.HYPE....HYPER.s3=mean(ARCR.AMPH.HYPE....HYPER.s3), ARCR.CLAD.....CLADO.s1=mean(ARCR.CLAD.....CLADO.s1), ARCR.COPE.....COPEN.s1=mean(ARCR.COPE.....COPEN.s1), ARCR.COPE.....COPE.other..2mm=mean(ARCR.COPE.....COPE.other..2mm), ARCR.COPE.....COPE.other..2mm.1=mean(ARCR.COPE.....COPE.other..2mm.1), ARCR.COPE.CALA....CALA..1mm=mean(ARCR.COPE.CALA....CALA..1mm), ARCR.COPE.CALA....CALA..3mm.1mm=mean(ARCR.COPE.CALA....CALA..3mm.1mm), ARCR.COPE.CALA....CALA..5mm.3mm=mean(ARCR.COPE.CALA....CALA..5mm.3mm), ARCR.COPE.CALA....CALA..5mm=mean(ARCR.COPE.CALA....CALA..5mm), ARCR.CUMA.....CUMAC.s1=mean(ARCR.CUMA.....CUMAC.s1), ARCR.CUMA.....CUMAC.s2=mean(ARCR.CUMA.....CUMAC.s2), ARCR.CUMA.....CUMAC.s3=mean(ARCR.CUMA.....CUMAC.s3), ARCR.DECA.....DECA.s1=mean(ARCR.DECA.....DECA.s1), ARCR.DECA.....DECA.s2=mean(ARCR.DECA.....DECA.s2),
            ARCR.DECA.....DECA.s3=mean(ARCR.DECA.....DECA.s3), ARCR.DECA.CARI....CARID.s1=mean(ARCR.DECA.CARI....CARID.s1), ARCR.DECA.CARI....CARID.s2=mean(ARCR.DECA.CARI....CARID.s2), ARCR.DECA.CARI....CARID.s3=mean(ARCR.DECA.CARI....CARID.s3), ARCR.DECA.MYSI....MYSID.s2=mean(ARCR.DECA.MYSI....MYSID.s2), ARCR.DECA.MYSI....MYSID.s3=mean(ARCR.DECA.MYSI....MYSID.s3), ARCR.EUPH.....EUPH.eggs=mean(ARCR.EUPH.....EUPH.eggs), ARCR.EUPH.....EUPH.s1=mean(ARCR.EUPH.....EUPH.s1), ARCR.EUPH.....EUPH.s2=mean(ARCR.EUPH.....EUPH.s2), ARCR.EUPH.....EUPH.s3=mean(ARCR.EUPH.....EUPH.s3), ARCR.ISOP.....ISOPOD.s1=mean(ARCR.ISOP.....ISOPOD.s1), ARCR.ISOP.....ISOPOD.s2=mean(ARCR.ISOP.....ISOPOD.s2), ARCR.ISOP.....ISOPOD.s3=mean(ARCR.ISOP.....ISOPOD.s3), ARCR.OSTR.....OSTRA..3mm=mean(ARCR.OSTR.....OSTRA..3mm), ARCR.OSTR.....OSTRA..3mm.1=mean(ARCR.OSTR.....OSTRA..3mm.1), CHAE......CHAET.s1=mean(CHAE......CHAET.s1), CHAE......CHAET.s2=mean(CHAE......CHAET.s2), CHAE......CHAET.s3=mean(CHAE......CHAET.s3), CHAE......EUKHA.s2=mean(CHAE......EUKHA.s2), CHAE......EUKHA.s3=mean(CHAE......EUKHA.s3), CHAE......PATEL.s2=mean(CHAE......PATEL.s2), CHAE......PATEL.s3=mean(CHAE......PATEL.s3), CNID.ANTH.....ANTH.s1=mean(CNID.ANTH.....ANTH.s1), CNID.ANTH.....ANTH.s2=mean(CNID.ANTH.....ANTH.s2), CNID.HYDR.....MEDUS.s1=mean(CNID.HYDR.....MEDUS.s1), CNID.HYDR.....MEDUS.s2=mean(CNID.HYDR.....MEDUS.s2), CNID.HYDR.....MEDUS.s3=mean(CNID.HYDR.....MEDUS.s3), CNID.HYDR.SIPH....CALYC.s1=mean(CNID.HYDR.SIPH....CALYC.s1), CNID.HYDR.SIPH....CALYC.s2=mean(CNID.HYDR.SIPH....CALYC.s2), CNID.HYDR.SIPH....CALYC.s3=mean(CNID.HYDR.SIPH....CALYC.s3), CNID.HYDR.SIPH....PHYS.float..5mm=mean(CNID.HYDR.SIPH....PHYS.float..5mm), CNID.HYDR.SIPH....PHYS.s1=mean(CNID.HYDR.SIPH....PHYS.s1), CNID.HYDR.SIPH....PHYS.s2=mean(CNID.HYDR.SIPH....PHYS.s2), CNID.SCYF.....SCYPH.s1=mean(CNID.SCYF.....SCYPH.s1), CNID.SCYF.....SCYPH.s2=mean(CNID.SCYF.....SCYPH.s2), CNID.SCYF.....SCYPH.s3=mean(CNID.SCYF.....SCYPH.s3), CTEN......CTENO.s1=mean(CTEN......CTENO.s1), CTEN......CTENO.s2=mean(CTEN......CTENO.s2), CTEN......CTENO.s3=mean(CTEN......CTENO.s3), ECHI......ECHIN.s1=mean(ECHI......ECHIN.s1), ECTO......BRYOC.s1=mean(ECTO......BRYOC.s1), MOCE.OCTO.....CEPH.s1=mean(MOCE.OCTO.....CEPH.s1), MOCE.OCTO.....CEPH.s2=mean(MOCE.OCTO.....CEPH.s2), MOCE.OCTO.....CEPH.s3=mean(MOCE.OCTO.....CEPH.s3), MOGA.HETE.....HETERO.s1=mean(MOGA.HETE.....HETERO.s1), MOGA.PTER.....CLIO.s1=mean(MOGA.PTER.....CLIO.s1), MOGA.PTER.....CLIO.s2=mean(MOGA.PTER.....CLIO.s2), MOGA.PTER.....CLIO.s3=mean(MOGA.PTER.....CLIO.s3), MOGA.PTER.....GYMNO.s1=mean(MOGA.PTER.....GYMNO.s1), MOGA.PTER.....GYMNO.s2=mean(MOGA.PTER.....GYMNO.s2), MOGA.PTER.....GYMNO.s3=mean(MOGA.PTER.....GYMNO.s3), MOGA.PTER.....LIM...s0=mean(MOGA.PTER.....LIM...s0), MOGA.PTER.....LIM...s1=mean(MOGA.PTER.....LIM...s1), MOGA.PTER.....LIM...s2=mean(MOGA.PTER.....LIM...s2), MOGA.PTER.....LIM...s3=mean(MOGA.PTER.....LIM...s3), MOLL......MOLL.s1=mean(MOLL......MOLL.s1), PROT......PROTO.s1=mean(PROT......PROTO.s1), PROT......PROTO.s2=mean(PROT......PROTO.s2), UROC.LARV.....LARVAC.s1=mean(UROC.LARV.....LARVAC.s1), UROC.LARV.....LARVAC.s2=mean(UROC.LARV.....LARVAC.s2), UROC.LARV.....LARVAC.s3=mean(UROC.LARV.....LARVAC.s3), UROC.THAL.....DOLIO.s1=mean(UROC.THAL.....DOLIO.s1), UROC.THAL.....DOLIO.s2=mean(UROC.THAL.....DOLIO.s2), UROC.THAL.....DOLIO.s3=mean(UROC.THAL.....DOLIO.s3),
            UROC.THAL.....SALP.s1=mean(UROC.THAL.....SALP.s1), UROC.THAL.....SALP.s2=mean(UROC.THAL.....SALP.s2),  UROC.THAL.....SALP.s3=mean(UROC.THAL.....SALP.s3), VERT.PISC.....PISCES.egg.s1=mean(VERT.PISC.....PISCES.egg.s1),  VERT.PISC.....PISCES.s1=mean(VERT.PISC.....PISCES.s1), VERT.PISC.....PISCES.s2=mean(VERT.PISC.....PISCES.s2),  VERT.PISC.....PISCES.s3=mean(VERT.PISC.....PISCES.s3), XXXX....Remainder=mean(XXXX....Remainder)) %>%
  ungroup()

bciymat <- select(bciyear, ANNE.POLY.....POLY.s1:XXXX....Remainder)

bcismat <- select(bcisite, ANNE.POLY.....POLY.s1:XXXX....Remainder)

bciymat <- as.matrix(bciymat)

bcismat <- as.matrix(bcismat)

class(bciyear$year)

yearinfo <- as.data.frame(bciyear$year)

siteinfo <- as.data.frame(bcisite$Station)

rownames(bciymat) <- yearinfo[, 1]

rownames(bcismat) <- siteinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(bciymat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

bcidec <- bciyear %>%
  select(year, decade)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "year")

lab <- full_join(labs, bcidec, by = "year")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$decade), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

Bray_Curtis_Dissimilarity <- vegdist(bcismat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")
ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "none")

##### BC INLETS - NMDS/PCA #####

class(bci$year) <- "integer"

bcimat <- select(bci, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

bcimat <- as.matrix(bcimat)

autoplot(prcomp(bcimat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=bci$year), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

autoplot(prcomp(bcimat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=bci$Station), size=2.5, shape=21, color = "black")+
  labs(x="PC1", y="PC2")+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

filtyear <- bci %>%
  filter(year %in% c("2001", "2006", "2008", "2009", "2010", "2014", "2015", "2016"))

yearmat <- select(filtyear, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

class(filtyear$year) <- "character"

yearinfo <- filtyear$year

#PRUTH, RI1, RI2, RI3, RI4, RI5, RI6

filtsite <- bci %>%
  filter(Station %in% c("PRUTH", "RI1", "RI2", "RI3", "RI4", "RI5", "RI6"))

sitemat <- select(filtsite, ANNE.POLY.....POLY.larvae.s1:XXXX....Remainder)

siteinfo <- filtsite$Station

eco.nmds.bc<- metaMDS(yearmat,distance="bray",labels=yearinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
permanova_eco.bc<-adonis(yearmat ~ yearinfo, permutations = 999, method="bray")
permanova_eco.bc
NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=yearinfo)
ord.bc<-ordiellipse(eco.nmds.bc,yearinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  guides(fill= guide_legend(override.aes = list(size=4)))+
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()
  ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
a

eco.nmds.bc<- metaMDS(sitemat,distance="bray",labels=siteinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
permanova_eco.bc<-adonis(sitemat ~ siteinfo, permutations = 999, method="bray")
permanova_eco.bc
NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=siteinfo)
ord.bc<-ordiellipse(eco.nmds.bc,siteinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  #scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                  name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  #scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow"),
  #                    guide=FALSE) +
  theme_bw()+
  theme(axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(angle=90,size=12),
        axis.text.y=element_text(size=12),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank()
  ) + coord_fixed() +
  annotate("text",x=-0.85,y=1,label="(stress = 0.07)",size=4, hjust = -0.1)
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
