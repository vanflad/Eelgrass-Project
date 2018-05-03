# Forage fish diets #

##### Initial Set-up #####

rm(list=ls())
#remove other R stuff

library(readr)
#read in files

setwd("/Users/Vanessa/Desktop/2017-2018/EOSC 575")
#set working directory

dietdata <- read_csv("forfish.csv")
#read in diet data file

zoops <- read_csv("biomass.csv")
#read in zooplankton file

regions <- select(zoops, region_name, Station)
#only select site and region columns

reg <- unique(regions)
#get rid of duplicates

colnames(reg) <- c("Region", "Site")
#rename columns to match diet dataset

dietdata <- as.data.frame(dietdata)
#make sure it's the right format

unique(dietdata$Sp)
#list the 11 study sp.

lessdetail <- select(dietdata, ID, FID, Sp, Site, Date, Latitude, Longitude, Group, Abundance, Biomass)
#get rid of columns that aren't needed

abund <- select(lessdetail, ID, FID, Sp, Site, Date, Latitude, Longitude, Group, Abundance)
#get rid of biomass column for abundance data

biomasses <- select(lessdetail, ID, FID, Sp, Site, Date, Latitude, Longitude, Group, Biomass)
#get rid of abundance column for biomass data (will combine some zoop groups)

morebio <- left_join(biomasses, reg, by="Site")
#copy of biomasses dataset with added region names (for some - many are NA, not in zoop data)

morebio$Region[which(morebio$Site=="CS11")] <- "Vancouver Island Inlets"
morebio$Region[which(morebio$Site=="CS19")] <- "Vancouver Island Inlets"
morebio$Region[which(morebio$Site=="CS12")] <- "Vancouver Island Inlets"
morebio$Region[which(morebio$Site=="CS10")] <- "Vancouver Island Inlets" #changed from NVIS
morebio$Region[which(morebio$Site=="QLST19")] <- "Queen Charlotte Strait"
morebio$Region[which(morebio$Site=="QLST13")] <- "Queen Charlotte Strait"
morebio$Region[which(morebio$Site=="QLST09")] <- "Queen Charlotte Strait"
morebio$Region[which(morebio$Site=="QLST06")] <- "Queen Charlotte Strait"
morebio$Region[which(morebio$Site=="EP02")] <- "West Coast Vancouver Island"
morebio$Region[which(morebio$Site=="IVI07")] <- "Vancouver Island Inlets"
morebio$Region[which(morebio$Site=="CS13")] <- "Vancouver Island Inlets"
morebio$Region[which(morebio$Site=="KI01")] <- "BC Inlets" #up by JS/QCS tho not RI/Calvert
morebio$Region[which(morebio$Site=="VI26")] <- "Northern Vancouver Island Shelf"
morebio$Region[which(morebio$Site=="VI25")] <- "Northern Vancouver Island Shelf"
morebio$Region[which(morebio$Site=="VI06")] <- "West Coast Vancouver Island"
morebio$Region[which(morebio$Site=="QLSD01")] <- "Northern Vancouver Island Shelf"
morebio$Region[which(morebio$Site=="VI03")] <- "West Coast Vancouver Island"
morebio$Region[which(morebio$Site=="VI22")] <- "West Coast Vancouver Island"
morebio$Region[which(morebio$Site=="IBC01")] <- "BC Inlets" #by Calvert
morebio$Region[which(morebio$Site=="EP04")] <- "West Coast Vancouver Island"
morebio$Region[which(morebio$Site=="VI04")] <- "West Coast Vancouver Island"
morebio$Region[which(morebio$Site=="IBC02")] <- "BC Inlets" #by Calvert
morebio$Region[which(morebio$Site=="VI19")] <- "West Coast Vancouver Island"
morebio$Region[which(morebio$Site=="VI05")] <- "West Coast Vancouver Island"
morebio$Region[which(morebio$Site=="WCVI")] <- "West Coast Vancouver Island"
morebio$Region[which(morebio$Site=="VI16")] <- "West Coast Vancouver Island" #changed from NVIS
morebio$Region[which(morebio$Site=="VI15")] <- "West Coast Vancouver Island" #changed from NVIS
morebio$Region[which(morebio$Site=="EP06")] <- "West Coast Vancouver Island" #changed from NVIS
morebio$Region[which(morebio$Site=="VI14")] <- "West Coast Vancouver Island" #changed from NVIS
morebio$Region[which(morebio$Site=="VI18")] <- "West Coast Vancouver Island" #changed from NVIS
#rename all the regions (except the inlets) because shit does not match up between data sets!

nosardines <- filter(morebio, Sp != "Sardines")

unique(nosardines$Site)

unique(nosardines$Region)

m <- leaflet(nosardines) %>%
  addTiles() %>% 
  addCircles(data=nosardines, lng = nosardines$Longitude, lat = nosardines$Latitude, popup = nosardines$Region)
#make map of datapoints

m
#print map of datapoints

##### Abundance Data Set-up #####

abdlong <- abund %>%
  group_by(FID, Sp, Site, Group) %>%
  summarise(Abundance = sum(Abundance)) %>%
  ungroup()

fishinfo <- unique(abdlong$FID)

fishinfo <- as.data.frame(fishinfo, stringsAsFactors = FALSE)

abdwide <- abdlong %>%
  group_by(FID, Sp, Site) %>% 
  spread(key=Group, value=Abundance, fill=0)

awinfo <- cbind.data.frame(fishinfo, abdwide)

##### Biomass Data Set-up #####

biomasses$Group[which(biomasses$Group=="Arthropod")] <- "Digested food"
biomasses$Group[which(biomasses$Group=="Crustacean")] <- "Digested food"
biomasses$Group[which(biomasses$Group=="Malacostracan")] <- "Digested food"
biomasses$Group[which(biomasses$Group=="Cumacean")] <- "Other"
biomasses$Group[which(biomasses$Group=="Isopod")] <- "Other"
biomasses$Group[which(biomasses$Group=="Ostracod")] <- "Other"
biomasses$Group[which(biomasses$Group=="Cnidarian")] <- "Gelatinous"
biomasses$Group[which(biomasses$Group=="Ctenophore")] <- "Gelatinous"
biomasses$Group[which(biomasses$Group=="Larvacean")] <- "Gelatinous"
biomasses$Group[which(biomasses$Group=="Pteropod")] <- "Gelatinous"
biomasses$Group[which(biomasses$Group=="Detritus")] <- "Other"
biomasses$Group[which(biomasses$Group=="Diatom")] <- "Other"
biomasses$Group[which(biomasses$Group=="Parasite")] <- "Other"
biomasses$Group[which(biomasses$Group=="Insect")] <- "Other"
biomasses$Group[which(biomasses$Group=="Empty")] <- "Other"
biomasses$Group[which(biomasses$Group=="Bivalve")] <- "Other"
biomasses$Group[which(biomasses$Group=="Barnacle")] <- "Other"
biomasses$Group[which(biomasses$Group=="Decapod")] <- "Other"
biomasses$Group[which(biomasses$Group=="Gammarid")] <- "Amphipod"
biomasses$Group[which(biomasses$Group=="Hyperiid")] <- "Amphipod"
biomasses$Group[which(biomasses$Group=="Calanoid")] <- "Copepod"
biomasses$Group[which(biomasses$Group=="Cyclopoid/Poecilostomatoid")] <- "Copepod"
biomasses$Group[which(biomasses$Group=="Harpacticoid")] <- "Copepod"
#combine these zoop groups for graphing

biolong <- biomasses %>%
  group_by(FID, Sp, Site, Group) %>%
  summarise(Biomass = sum(Biomass)) %>%
  ungroup()
#summarize biomass for repeated groups in each stomach

detaillong <- morebio %>%
  group_by(FID, Sp, Site, Region, Group) %>%
  summarise(Biomass = sum(Biomass)) %>%
  ungroup()
#summarize biomass (same as above but more groups)

filtered <- filter(detaillong, Biomass!=0)
#get rid of empty stomachs

empty <- filter(detaillong, Biomass==0)
#see which stomachs are empty

biowide <- filtered %>%
  group_by(FID, Sp, Site) %>% 
  spread(key=Group, value=Biomass, fill=0)
#make a long dataframe (all groups)

biolong %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")
#plot biomass comparisons for each site

biolong %>%
  ggplot(aes(Sp, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")
#plot biomass comparisons for each species of fish

fishinfo <- as.data.frame(biowide$FID)
#make a 1 column data frame of fish ID's

spinfo <- as.data.frame(biowide$Sp)
#make a 1 columna data frame of fish species

sitesinfo <- as.data.frame(biowide$Site)
#make a 1 column data frame of sites sampled

reginfo <- as.data.frame(biowide$Region)
#make a 1 column data frame of regions included (NA's if not in zoop data)

dropped <- biowide %>%
  ungroup() %>% 
  select(-c(Sp, Site, FID, Region))
#make a data frame with only numerical diet data

biomat <- as.matrix(dropped)
#make it into a numerical matrix

class(biomat) <- "numeric"
#make sure it's numeric to prevent errors

row.names(biomat) <- fishinfo[, 1]
#identity the diet matrix rows with fish ID's

rankindex(spinfo, biomat, indices = c("euc", "man", "gow", "bra", "kul"))
#see which is best

Bray_Curtis_Dissimilarity <- vegdist(biomat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
#make dendrogram data

dendr <- dendro_data(bcclust, type = "rectangle")
#put it in ggdendro form

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1), size=2, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())
#plot the dendrogram data for the different fish ID's

# repeating the above for sites instead:

summarized <- biowide %>%
  group_by(Site, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod))

sitemat <- select(biowide, Amphipod:Pteropod)

sitematinfo <- select(biowide, Site)

sitematreg <- select(biowide, Region)

# resume working towards fish diet site clustering here #

row.names(biomat) <- sitesinfo[, 1]
#identity the diet matrix rows with sites

rankindex(reginfo, biomat, indices = c("euc", "man", "gow", "bra", "kul"))
#see which is best

Bray_Curtis_Dissimilarity <- vegdist(biomat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
#make dendrogram data

dendr <- dendro_data(bcclust, type = "rectangle")
#put it in ggdendro form

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1), size=2, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())
#plot the dendrogram data for the different sites

#pick up here later: try to color each of the different species (so hard to read text)

#also try doing the same for site locations and regions and see what's up with those

#and try NMDS for both diets and zoops and maybe rethink zoop stations to include more (<-done)

##### NMDS #####

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(biomat,distance="bray",labels=reginfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

#region, proportion based dissimilarity - euclidean
eco.nmds.eu<- metaMDS(biomat,distance="kulczynski",labels=reginfo, trymax = 100, autotransform = FALSE)
eco.nmds.eu
plot(eco.nmds.eu)
#found convergence easily (<20), stress is rel. low = 0.05774715

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(biomat ~ reginfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

permanova_eco.eu<-adonis(biomat ~ reginfo, permutations = 999, method="euclidean")
permanova_eco.eu #significant! p = 0.001 so plot it (has lower mean sqs and sum of sqs tho)

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=reginfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

NMDS.eu<-data.frame(NMDS1.eu=eco.nmds.eu$points[,1],NMDS2.eu=eco.nmds.eu$points[,2],group=reginfo)
#plot NMDS, only once (picking Gower because bray can't converge), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,reginfo,display="sites",kind="sd", conf = 0.95, label=T)
#Ellipses are standard deviation, no scaling of data (can use standard error, scaling, and confidence limit options)

ord.eu<-ordiellipse(eco.nmds.eu,reginfo,display="sites",kind="sd", conf = 0.95, label=T)

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

# then repeat this process for regions rather than species! #

##### Species Data Set-up #####

her <- filter(biolong, Sp == "Herring")

pom <- filter(biolong, Sp == "Pompano")

stick<- filter(biolong, Sp == "Stickleback")

poll <- filter(biolong, Sp == "Pollock")

eul <- filter(biolong, Sp == "Eulachon")

cap <- filter(biolong, Sp == "Capelin")

mack <- filter(biolong, Sp == "Jack Mackerel" | Sp == "Chub Mackerel")

anc <- filter(biolong, Sp == "Anchovy")

sar <- filter(biolong, Sp == "Sardines")

chum <- filter(biolong, Sp == "Chum")

her %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

pom %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

stick %>%
  ggplot(aes(FID, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

poll %>%
  ggplot(aes(FID, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

eul %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

cap %>%
  ggplot(aes(FID, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

mack %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

mack %>%
  ggplot(aes(Sp, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

anc %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

sar %>%
  ggplot(aes(FID, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

chum %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")