# Forage fish diets #

##### Initial Set-up #####

# Note to self: NMDS was attempted but with no convergence so it's no bueno.

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

lessdetail <- select(dietdata, ID, FID, Sp, Site, Date, Latitude, Longitude, MaxLength, Group, Abundance, Biomass, PreyW)
#get rid of columns that aren't needed

morebio <- left_join(lessdetail, reg, by="Site")
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

lessbio <- morebio
#to make a copy to group zoops together!!

write_csv(morebio, "fish_edited.csv")

nosardines <- filter(morebio, Sp != "Sardines")

unique(nosardines$Site)

unique(nosardines$Region)

m <- leaflet(nosardines) %>%
  addTiles() %>% 
  addCircles(data=nosardines, lng = nosardines$Longitude, lat = nosardines$Latitude, popup = nosardines$Sp)
#make map of datapoints

m
#print map of datapoints

##### Biomass Data Set-up #####

lessbio$Group[which(lessbio$Group=="Arthropod")] <- "Digested food"
lessbio$Group[which(lessbio$Group=="Crustacean")] <- "Digested food"
lessbio$Group[which(lessbio$Group=="Malacostracan")] <- "Digested food"
lessbio$Group[which(lessbio$Group=="Cumacean")] <- "Other"
lessbio$Group[which(lessbio$Group=="Isopod")] <- "Other"
lessbio$Group[which(lessbio$Group=="Ostracod")] <- "Other"
lessbio$Group[which(lessbio$Group=="Cnidarian")] <- "Gelatinous"
lessbio$Group[which(lessbio$Group=="Ctenophore")] <- "Gelatinous"
lessbio$Group[which(lessbio$Group=="Larvacean")] <- "Gelatinous"
lessbio$Group[which(lessbio$Group=="Pteropod")] <- "Gelatinous"
lessbio$Group[which(lessbio$Group=="Detritus")] <- "Other"
lessbio$Group[which(lessbio$Group=="Diatom")] <- "Other"
lessbio$Group[which(lessbio$Group=="Parasite")] <- "Other"
lessbio$Group[which(lessbio$Group=="Insect")] <- "Other"
lessbio$Group[which(lessbio$Group=="Empty")] <- "Other"
lessbio$Group[which(lessbio$Group=="Bivalve")] <- "Other"
lessbio$Group[which(lessbio$Group=="Barnacle")] <- "Other"
lessbio$Group[which(lessbio$Group=="Decapod")] <- "Other"
lessbio$Group[which(lessbio$Group=="Gammarid")] <- "Amphipod"
lessbio$Group[which(lessbio$Group=="Hyperiid")] <- "Amphipod"
lessbio$Group[which(lessbio$Group=="Calanoid")] <- "Copepod"
lessbio$Group[which(lessbio$Group=="Cyclopoid/Poecilostomatoid")] <- "Copepod"
lessbio$Group[which(lessbio$Group=="Harpacticoid")] <- "Copepod"
#combine these zoop groups for graphing

abund <- select(morebio, -Biomass)
#get rid of biomass column for abundance data

biomasses <- select(morebio, -Abundance)
#get rid of abundance column for biomass data (will combine some zoop groups)

abundless <- select(lessbio, -Biomass)
#get rid of biomass column for abundance data

biomassless <- select(lessbio, -Abundance)
#get rid of abundance column for biomass data (will combine some zoop groups)

biolong <- biomassless %>%
  group_by(FID, Sp, Site, Group, Region) %>%
  summarise(Biomass = sum(Biomass)) %>%
  ungroup()
#summarize biomass for repeated groups in each stomach

detaillong <- biomasses %>%
  group_by(FID, Sp, Site, Region, Group) %>%
  summarise(Biomass = sum(Biomass)) %>%
  ungroup()
#summarize biomass (same as above but more groups)

filtered <- filter(detaillong, Biomass!=0)
#get rid of empty stomachs

empty <- filter(detaillong, Biomass==0)
#see which stomachs are empty

filterless <- filter(biolong, Biomass != 0)
#get rid of empty stomachs for grouped together dataset as well

lesswide <- filterless %>%
  group_by(FID, Sp, Site) %>%
  spread(key=Group, value=Biomass, fill=0)
#make a wide dataframe (less zoop groups i.e. "copepods")

biowide <- filtered %>%
  group_by(FID, Sp, Site) %>% 
  spread(key=Group, value=Biomass, fill=0)
#make a wide dataframe (all groups)

emptywide <- detaillong %>%
  group_by(FID, Sp, Site) %>% 
  spread(key=Group, value=Biomass, fill=0)
#make wide dataframe with all groups and empty stomachs too

biolong %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")+
  theme(axis.text.x = element_text(angle = 90))
#plot biomass comparisons for each site

biolong %>%
  ggplot(aes(Sp, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")
#plot biomass comparisons for each species of fish

biolong %>%
  ggplot(aes(Region, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")
#plot biomass comparisons for each region

biolong %>%
  ggplot(aes(Region, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")+
  facet_wrap(~Sp)+
  theme(axis.text.x = element_text(angle = 90))

biolong %>%
  ggplot(aes(Sp, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")+
  facet_wrap(~Region)+
  theme(axis.text.x = element_text(angle = 90))

biolong %>%
  filter(Sp %in% c("Anchovy", "Chub Mackerel", "Herring", "Jack Mackerel")) %>%
  ggplot(aes(Region, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")+
  facet_wrap(~Sp)+
  theme(axis.text.x = element_text(angle = 90))

biolong %>%
  filter(Sp %in% c("Anchovy", "Chub Mackerel", "Herring", "Jack Mackerel")) %>%
  ggplot(aes(Sp, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")+
  facet_wrap(~Region)+
  theme(axis.text.x = element_text(angle = 90))

##### Abundance Data Set-up #####

abdlong <- abundless %>%
  group_by(FID, Sp, Site, Group, Region) %>%
  summarise(Abundance = sum(Abundance)) %>%
  ungroup()

fishinfo <- unique(abdlong$FID)

fishinfo <- as.data.frame(fishinfo, stringsAsFactors = FALSE)

abdwide <- abdlong %>%
  group_by(FID, Sp, Site, Region) %>% 
  spread(key=Group, value=Abundance, fill=0)

awinfo <- cbind.data.frame(fishinfo, abdwide)

# add in here - Region column by joining and renaming groups to less detail

abdlong %>%
  ggplot(aes(Site, Abundance))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")
#plot biomass comparisons for each site

abdlong %>%
  ggplot(aes(Sp, Abundance))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")
#plot biomass comparisons for each species of fish

abdlong %>%
  ggplot(aes(Region, Abundance))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")
#plot biomass comparisons for each region

##### CLUSTER ANALYSIS - INDIVID FISH #####

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

fishsp <- biowide %>%
  ungroup() %>%
  select(FID, Sp)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "FID")

lab <- left_join(labs, fishsp, by = "FID")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Sp), size=2, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank(), legend.position = "bottom")
#plot the dendrogram data for the different fish ID's

##### CLUSTER ANALYSIS - SPECIES #####

species <- biowide %>%
  group_by(Sp) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod)) %>%
  ungroup()

spmat <- species %>%
  ungroup() %>%
  select(Amphipod:Pteropod)

spinfo <- as.data.frame(species$Sp)

spmat <- as.matrix(spmat)

class(spmat) <- "numeric"

row.names(spmat) <- spinfo[, 1]

rankindex(spinfo, spmat, indices = c("euc", "man", "gow", "bra", "kul"))
#whatever

Bray_Curtis_Dissimilarity <- vegdist(spmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
#make dendrogram data

dendr <- dendro_data(bcclust, type = "rectangle")
#put it in ggdendro form

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=3) +
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_color_manual(values = c("red", "green", "blue", "yellow", "purple", "pink"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank(), legend.position = "bottom")
#plot the dendrogram data for the different sites

##### CLUSTER ANALYSIS - SITES #####

summarized <- biowide %>%
  group_by(Site, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod)) %>%
  ungroup()

sitemat <- summarized %>%
  ungroup() %>%
  select(Amphipod:Pteropod)

sitematinfo <- as.data.frame(summarized$Site)

sitematreg <- as.data.frame(summarized$Region)

sitemat <- as.matrix(sitemat)
#make it into a numerical matrix

class(sitemat) <- "numeric"
#make sure it's numeric to prevent errors

row.names(sitemat) <- sitematinfo[, 1]
#identity the diet matrix rows with fish ID's

rankindex(sitematreg, sitemat, indices = c("euc", "man", "gow", "bra", "kul"))
#see which is best

Bray_Curtis_Dissimilarity <- vegdist(sitemat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
#make dendrogram data

dendr <- dendro_data(bcclust, type = "rectangle")
#put it in ggdendro form

regsite <- summarized %>%
  ungroup() %>%
  select(Site, Region)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "Site")

lab <- left_join(labs, regsite, by = "Site")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Region), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  scale_color_manual(values = c("red", "green", "blue", "yellow", "purple", "pink"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")
#plot the dendrogram data for the different sites

##### CLUSTER ANALYSIS - REGIONS #####

sumreg <- biowide %>%
  group_by(Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod)) %>%
  ungroup()

rmat <- sumreg %>%
  ungroup() %>%
  select(Amphipod:Pteropod)

rinfo <- as.data.frame(sumreg$Region)

rmat <- as.matrix(rmat)

class(rmat) <- "numeric"

row.names(rmat) <- rinfo[, 1]

rankindex(rinfo, rmat, indices = c("euc", "man", "gow", "bra", "kul"))
#whatever

Bray_Curtis_Dissimilarity <- vegdist(rmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
#make dendrogram data

dendr <- dendro_data(bcclust, type = "rectangle")
#put it in ggdendro form

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=3) +
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_color_manual(values = c("red", "green", "blue", "yellow", "purple", "pink"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank(), legend.position = "bottom")
#plot the dendrogram data for the different sites

##### CLUSTER ANALYSIS - REGION + SPECIES #####

regionsp <- biowide %>%
  group_by(Sp, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod)) %>%
  ungroup()

rsmat <- regionsp %>%
  ungroup() %>%
  select(Amphipod:Pteropod)

rs <- mutate(regionsp, RS=paste(Region, Sp, sep=" "))

rsinfo <- rs$RS

rsinfo <- as.data.frame(rsinfo)

spinfo <- as.data.frame(regionsp$Sp)

reginfo <- as.data.frame(regionsp$Region)

rsmat <- as.matrix(rsmat)
#make it into a numerical matrix

class(rsmat) <- "numeric"
#make sure it's numeric to prevent errors

row.names(rsmat) <- rsinfo[, 1]
#identity the diet matrix rows with fish ID's

rankindex(spinfo, rsmat, indices = c("euc", "man", "gow", "bra", "kul"))
#see which is best

Bray_Curtis_Dissimilarity <- vegdist(rsmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
#make dendrogram data

dendr <- dendro_data(bcclust, type = "rectangle")
#put it in ggdendro form

#regsp <- summarized %>%
#  ungroup() %>%
#  select(Site, Region)

#labs <- label(dendr)

#colnames(labs) <- c("x", "y", "Site")

#lab <- left_join(labs, regsite, by = "Site")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=3) +
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  scale_color_manual(values = c("red", "green", "blue", "yellow", "purple", "pink"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank(), legend.position = "bottom")
#plot the dendrogram data for the different sites

##### LENGTH DATA #####

hist(morebio$MaxLength, breaks = 50, col = "purple")

ggplot(morebio, aes(MaxLength))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~Sp)

ggplot(morebio, aes(MaxLength))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~Region)

#try to figure out a statistical way to compare these? idfk

##### PCA #####

# This one is for all zooplankton groups separated and colored by fish species
autoplot(prcomp(biomat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=biowide$Sp), size=2.5, shape=21, color = "black")+
  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

# This one is for all zooplankton groups separated and colored by region
autoplot(prcomp(biomat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=biowide$Region), size=2.5, shape=21, color = "black")+
  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

lessmat <- lesswide %>%
  ungroup() %>% 
  select(Amphipod:Other)

# This one is for most zooplankton groups conglomerated together and colored by fish species
autoplot(prcomp(lessmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=lesswide$Sp), size=2.5, shape=21, color = "black")+
  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

# This one is for most zooplankton groups conglomerated together and colored by Region
autoplot(prcomp(lessmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=lesswide$Region), size=2.5, shape=21, color = "black")+
  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

##### SIMPER #####

spinfo <- biowide$Sp
siteinfo <- biowide$Site
reginfo <- biowide$Region

simperdata <- simper(biomat, reginfo)

##### ANOSIM #####

reganosim <- anosim(biomat, reginfo)

summary(reganosim)

plot(reganosim)

spanosim <- anosim(biomat, spinfo)

summary(spanosim)

plot(spanosim)

##### Species Bar Plots #####

biolong %>%
  filter(Sp=="Herring") %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp=="Pompano") %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp=="Stickleback") %>%
  ggplot(aes(FID, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp=="Pollock") %>%
  ggplot(aes(FID, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp=="Eulachon") %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp=="Capelin") %>%
  ggplot(aes(FID, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp %in% c("Chub Mackerel", "Jack Mackerel")) %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp %in% c("Chub Mackerel", "Jack Mackerel")) %>%
  ggplot(aes(Sp, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp %in% c("Chub Mackerel", "Jack Mackerel")) %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")+
  facet_wrap(~Sp)

biolong %>%
  filter(Sp == "Chub Mackerel") %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp == "Jack Mackerel") %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp=="Anchovy") %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp=="Sardines") %>%
  ggplot(aes(FID, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

biolong %>%
  filter(Sp=="Chum") %>%
  ggplot(aes(Site, Biomass))+
  geom_bar(aes(fill=Group), stat = "identity", position = "fill")

##### HERRING #####

her <- filter(biowide, Sp=="Herring")
hermat <- select(ungroup(her), Amphipod:Pteropod)
hermat <- as.matrix(hermat)
herinfo <- as.data.frame(her$Site)
rownames(hermat) <- herinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(hermat, method = "bray")
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

hersum <- her %>%
  group_by(Site, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod))

hersmat <- select(ungroup(hersum), Amphipod:Pteropod)
hersmat <- as.matrix(hersmat)
hersinfo <- as.data.frame(hersum$Site)
rownames(hersmat) <- hersinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(hersmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

hersite <- hersum %>%
  ungroup() %>%
  select(Site, Region)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "Site")

lab <- left_join(labs, hersite, by = "Site")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Region), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

lessher <- filter(her, !Site %in% c("QLST19", "VI16"))
herlmat <- select(ungroup(lessher), Amphipod:Pteropod)
herlmat <- as.matrix(herlmat)
herlinfo <- as.data.frame(lessher$Site)
rownames(herlmat) <- herlinfo[, 1]
herlinfo <- lessher$Site

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(herlmat,distance="bray",labels=herlinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(herlmat ~ herlinfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=herlinfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,herlinfo,display="sites",kind="sd", conf = 0.95, label=T)
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

rownames(hermat) <- her$FID

autoplot(prcomp(hermat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=her$Site), size=2.5, shape=21, color = "black")+
  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

amph <- her %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(her)*100

arth <- her %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(her)*100

barn <- her %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(her)*100

biv <- her %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(her)*100

cala <- her %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(her)*100

chae <- her %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(her)*100

cnid <- her %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(her)*100

cope <- her %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(her)*100

crus <- her %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(her)*100

cten <- her %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(her)*100

cuma <- her %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(her)*100

cycl <- her %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(her)*100

deca <- her %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(her)*100

detr <- her %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(her)*100

diat <- her %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(her)*100

digf <- her %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(her)*100

euph <- her %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(her)*100

fish <- her %>%
  ungroup() %>%
  tally(Fish>0)/nrow(her)*100

gamm <- her %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(her)*100

harp <- her %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(her)*100

hyp <- her %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(her)*100

ins <- her %>%
  ungroup() %>%
  tally(Insect>0)/nrow(her)*100

iso <- her %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(her)*100

larv <- her %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(her)*100

mala <- her %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(her)*100

ostr <- her %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(her)*100

para <- her %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(her)*100

pter <- her %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(her)*100

herring <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(herring) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

##### POMPANO #####

pom <- filter(biowide, Sp=="Pompano")
pommat <- select(ungroup(pom), Amphipod:Pteropod)
pommat <- as.matrix(pommat)
pominfo <- as.data.frame(pom$FID)
rownames(pommat) <- pominfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(pommat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

pomsite <- pom %>%
  ungroup() %>%
  select(FID, Site)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "FID")

lab <- left_join(labs, pomsite, by = "FID")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Site), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

autoplot(prcomp(pommat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=pom$Site), size=2.5, shape=21, color = "black")+
  #scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
  #                  name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

amph <- pom %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(pom)*100

arth <- pom %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(pom)*100

barn <- pom %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(pom)*100

biv <- pom %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(pom)*100

cala <- pom %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(pom)*100

chae <- pom %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(pom)*100

cnid <- pom %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(pom)*100

cope <- pom %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(pom)*100

crus <- pom %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(pom)*100

cten <- pom %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(pom)*100

cuma <- pom %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(pom)*100

cycl <- pom %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(pom)*100

deca <- pom %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(pom)*100

detr <- pom %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(pom)*100

diat <- pom %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(pom)*100

digf <- pom %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(pom)*100

euph <- pom %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(pom)*100

fish <- pom %>%
  ungroup() %>%
  tally(Fish>0)/nrow(pom)*100

gamm <- pom %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(pom)*100

harp <- pom %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(pom)*100

hyp <- pom %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(pom)*100

ins <- pom %>%
  ungroup() %>%
  tally(Insect>0)/nrow(pom)*100

iso <- pom %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(pom)*100

larv <- pom %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(pom)*100

mala <- pom %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(pom)*100

ostr <- pom %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(pom)*100

para <- pom %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(pom)*100

pter <- pom %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(pom)*100

pompano <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(pompano) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

##### STICKLEBACK #####

stick <- filter(biowide, Sp=="Stickleback")
stickmat <- select(ungroup(stick), Amphipod:Pteropod)
stickmat <- as.matrix(stickmat)
stickinfo <- as.data.frame(stick$FID)
rownames(stickmat) <- stickinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(stickmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

autoplot(prcomp(stickmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=stick$FID), size=2.5, shape=21, color = "black")+
  #scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
  #                  name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))

amph <- stick %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(stick)*100

arth <- stick %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(stick)*100

barn <- stick %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(stick)*100

biv <- stick %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(stick)*100

cala <- stick %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(stick)*100

chae <- stick %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(stick)*100

cnid <- stick %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(stick)*100

cope <- stick %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(stick)*100

crus <- stick %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(stick)*100

cten <- stick %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(stick)*100

cuma <- stick %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(stick)*100

cycl <- stick %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(stick)*100

deca <- stick %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(stick)*100

detr <- stick %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(stick)*100

diat <- stick %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(stick)*100

digf <- stick %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(stick)*100

euph <- stick %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(stick)*100

fish <- stick %>%
  ungroup() %>%
  tally(Fish>0)/nrow(stick)*100

gamm <- stick %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(stick)*100

harp <- stick %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(stick)*100

hyp <- stick %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(stick)*100

ins <- stick %>%
  ungroup() %>%
  tally(Insect>0)/nrow(stick)*100

iso <- stick %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(stick)*100

larv <- stick %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(stick)*100

mala <- stick %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(stick)*100

ostr <- stick %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(stick)*100

para <- stick %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(stick)*100

pter <- stick %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(stick)*100

stickleback <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(stickleback) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

##### POLLOCK #####

poll <- filter(biowide, Sp=="Pollock")
pollmat <- select(ungroup(poll), Amphipod:Pteropod)
pollmat <- as.matrix(pollmat)
pollinfo <- as.data.frame(poll$FID)
rownames(pollmat) <- pollinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(pollmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

#this species is not interesting to plot! POLL1 is 99% EUPH and 1 % CUMA and POLL2 is 100% EUPH

amph <- poll %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(poll)*100

arth <- poll %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(poll)*100

barn <- poll %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(poll)*100

biv <- poll %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(poll)*100

cala <- poll %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(poll)*100

chae <- poll %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(poll)*100

cnid <- poll %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(poll)*100

cope <- poll %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(poll)*100

crus <- poll %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(poll)*100

cten <- poll %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(poll)*100

cuma <- poll %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(poll)*100

cycl <- poll %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(poll)*100

deca <- poll %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(poll)*100

detr <- poll %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(poll)*100

diat <- poll %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(poll)*100

digf <- poll %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(poll)*100

euph <- poll %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(poll)*100

fish <- poll %>%
  ungroup() %>%
  tally(Fish>0)/nrow(poll)*100

gamm <- poll %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(poll)*100

harp <- poll %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(poll)*100

hyp <- poll %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(poll)*100

ins <- poll %>%
  ungroup() %>%
  tally(Insect>0)/nrow(poll)*100

iso <- poll %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(poll)*100

larv <- poll %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(poll)*100

mala <- poll %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(poll)*100

ostr <- poll %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(poll)*100

para <- poll %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(poll)*100

pter <- poll %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(poll)*100

pollock <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(pollock) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

##### EULACHON #####

eul <- filter(biowide, Sp=="Eulachon")
eulmat <- select(ungroup(eul), Amphipod:Pteropod)
eulmat <- as.matrix(eulmat)
eulinfo <- as.data.frame(eul$FID)
rownames(eulmat) <- eulinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(eulmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

eulsite <- eul %>%
  ungroup() %>%
  select(Site, FID)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "FID")

lab <- left_join(labs, eulsite, by = "FID")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Site), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

eulsum <- eul %>%
  group_by(Site, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod))

eulsmat <- select(ungroup(eulsum), Amphipod:Pteropod)
eulsmat <- as.matrix(eulsmat)
eulsinfo <- as.data.frame(eulsum$Site)
rownames(eulsmat) <- eulsinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(eulsmat, method = "bray")
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

lesseul <- filter(eul, Site != "CS12")
eullmat <- select(ungroup(lesseul), Amphipod:Pteropod)
eullmat <- as.matrix(eullmat)
eullinfo <- as.data.frame(lesseul$Site)
rownames(eullmat) <- eullinfo[, 1]
eullinfo <- lesseul$Site

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(eullmat,distance="bray",labels=eullinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(eullmat ~ eullinfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=eullinfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,eullinfo,display="sites",kind="sd", conf = 0.95, label=T)
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

rownames(eulmat) <- eul$FID

autoplot(prcomp(eulmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=eul$Site), size=2.5, shape=21, color = "black")+
#  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
#                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

amph <- eul %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(eul)*100

arth <- eul %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(eul)*100

barn <- eul %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(eul)*100

biv <- eul %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(eul)*100

cala <- eul %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(eul)*100

chae <- eul %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(eul)*100

cnid <- eul %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(eul)*100

cope <- eul %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(eul)*100

crus <- eul %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(eul)*100

cten <- eul %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(eul)*100

cuma <- eul %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(eul)*100

cycl <- eul %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(eul)*100

deca <- eul %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(eul)*100

detr <- eul %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(eul)*100

diat <- eul %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(eul)*100

digf <- eul %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(eul)*100

euph <- eul %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(eul)*100

fish <- eul %>%
  ungroup() %>%
  tally(Fish>0)/nrow(eul)*100

gamm <- eul %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(eul)*100

harp <- eul %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(eul)*100

hyp <- eul %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(eul)*100

ins <- eul %>%
  ungroup() %>%
  tally(Insect>0)/nrow(eul)*100

iso <- eul %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(eul)*100

larv <- eul %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(eul)*100

mala <- eul %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(eul)*100

ostr <- eul %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(eul)*100

para <- eul %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(eul)*100

pter <- eul %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(eul)*100

eulachon <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(eulachon) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

##### CAPELIN #####

cap <- filter(biowide, Sp=="Capelin")
capmat <- select(ungroup(cap), Amphipod:Pteropod)
capmat <- as.matrix(capmat)
capinfo <- as.data.frame(cap$FID)
rownames(capmat) <- capinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(capmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

#not interesting or enough samples for more analysis. one had calanoid, one had euphausiid, one had "crustacean"

amph <- cap %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(cap)*100

arth <- cap %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(cap)*100

barn <- cap %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(cap)*100

biv <- cap %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(cap)*100

cala <- cap %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(cap)*100

chae <- cap %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(cap)*100

cnid <- cap %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(cap)*100

cope <- cap %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(cap)*100

crus <- cap %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(cap)*100

cten <- cap %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(cap)*100

cuma <- cap %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(cap)*100

cycl <- cap %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(cap)*100

deca <- cap %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(cap)*100

detr <- cap %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(cap)*100

diat <- cap %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(cap)*100

digf <- cap %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(cap)*100

euph <- cap %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(cap)*100

fish <- cap %>%
  ungroup() %>%
  tally(Fish>0)/nrow(cap)*100

gamm <- cap %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(cap)*100

harp <- cap %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(cap)*100

hyp <- cap %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(cap)*100

ins <- cap %>%
  ungroup() %>%
  tally(Insect>0)/nrow(cap)*100

iso <- cap %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(cap)*100

larv <- cap %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(cap)*100

mala <- cap %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(cap)*100

ostr <- cap %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(cap)*100

para <- cap %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(cap)*100

pter <- cap %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(cap)*100

capelin <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(capelin) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

##### MACKEREL - SITES #####

mack <- filter(biowide, Sp %in% c("Chub Mackerel", "Jack Mackerel"))
mackmat <- select(ungroup(mack), Amphipod:Pteropod)
mackmat <- as.matrix(mackmat)
mackinfo <- as.data.frame(mack$FID)
macksiteinfo <- as.data.frame(mack$Site)
mackreginfo <- as.data.frame(mack$Region)
rownames(mackmat) <- mackinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(mackmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

macksite <- mack %>%
  ungroup() %>%
  select(Region, FID)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "FID")

lab <- left_join(labs, macksite, by = "FID")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Region), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

macksum <- mack %>%
  group_by(Site, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod))

macksmat <- select(ungroup(macksum), Amphipod:Pteropod)
macksmat <- as.matrix(macksmat)
macksinfo <- as.data.frame(macksum$Site)
rownames(macksmat) <- macksinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(macksmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

macksite <- macksum %>%
  ungroup() %>%
  select(Region, Site)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "Site")

lab <- left_join(labs, macksite, by = "Site")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Region), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

lessmack <- filter(mack, !Site %in% c("CS12", "CS11", "CS10", "EP02", "IBC01", "IBC02", "IVI01", "VI14", "VI19", "VI22"))
macklmat <- select(ungroup(lessmack), Amphipod:Pteropod)
macklmat <- as.matrix(macklmat)
macklinfo <- as.data.frame(lessmack$Site)
rownames(macklmat) <- macklinfo[, 1]
macklinfo <- lessmack$Site

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(macklmat,distance="bray",labels=macklinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(macklmat ~ macklinfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=macklinfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,macklinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow", "pink", "grey"),
                    name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow", "pink", "grey"),
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
#Just note that there was no convergence in the NMDS calculation *

a

macklmat <- select(ungroup(mack), Amphipod:Pteropod)
macklmat <- as.matrix(macklmat)
macklinfo <- as.data.frame(mack$Region)
rownames(macklmat) <- macklinfo[, 1]
macklinfo <- mack$Region

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(macklmat,distance="bray",labels=macklinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(macklmat ~ macklinfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=macklinfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,macklinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow", "pink", "grey"),
                    name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow", "pink", "grey"),
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
#Just note that there was no convergence in the NMDS calculation *

a

rownames(mackmat) <- mack$FID

autoplot(prcomp(mackmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=mack$Region), size=2.5, shape=21, color = "black")+
  #  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
  #                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

#cluster 1: use all fish and color by region
#cluster 3: group together sites and color by region
#NMDS: color by site
#NMDS: color by region
#PCA: color by region

amph <- mack %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(mack)*100

arth <- mack %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(mack)*100

barn <- mack %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(mack)*100

biv <- mack %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(mack)*100

cala <- mack %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(mack)*100

chae <- mack %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(mack)*100

cnid <- mack %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(mack)*100

cope <- mack %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(mack)*100

crus <- mack %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(mack)*100

cten <- mack %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(mack)*100

cuma <- mack %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(mack)*100

cycl <- mack %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(mack)*100

deca <- mack %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(mack)*100

detr <- mack %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(mack)*100

diat <- mack %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(mack)*100

digf <- mack %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(mack)*100

euph <- mack %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(mack)*100

fish <- mack %>%
  ungroup() %>%
  tally(Fish>0)/nrow(mack)*100

gamm <- mack %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(mack)*100

harp <- mack %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(mack)*100

hyp <- mack %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(mack)*100

ins <- mack %>%
  ungroup() %>%
  tally(Insect>0)/nrow(mack)*100

iso <- mack %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(mack)*100

larv <- mack %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(mack)*100

mala <- mack %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(mack)*100

ostr <- mack %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(mack)*100

para <- mack %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(mack)*100

pter <- mack %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(mack)*100

mackerel <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(mackerel) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

##### MACKEREL - SPECIES #####

mack <- filter(biowide, Sp %in% c("Chub Mackerel", "Jack Mackerel"))
mackmat <- select(ungroup(mack), Amphipod:Pteropod)
mackmat <- as.matrix(mackmat)
mackinfo <- as.data.frame(mack$FID)
mackspinfo <- as.data.frame(mack$Sp)
macksiteinfo <- as.data.frame(mack$Site)
mackreginfo <- as.data.frame(mack$Region)
rownames(mackmat) <- mackinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(mackmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

macksite <- mack %>%
  ungroup() %>%
  select(Sp, FID)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "FID")

lab <- left_join(labs, macksite, by = "FID")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Sp), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

macksum <- mack %>%
  group_by(Sp, Site, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod))

macksmat <- select(ungroup(macksum), Amphipod:Pteropod)
macksmat <- as.matrix(macksmat)
macksinfo <- as.data.frame(macksum$Site)
rownames(macksmat) <- macksinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(macksmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

macksite <- macksum %>%
  ungroup() %>%
  select(Sp, Site)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "Site")

lab <- left_join(labs, macksite, by = "Site")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Sp), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

mackspinfo <- mack$Sp

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(mackmat,distance="bray",labels=mackspinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(mackmat ~ mackspinfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=mackspinfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,mackspinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
  scale_fill_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow", "pink", "grey"),
                    name="Region", guide="legend") +
  guides(fill= guide_legend(override.aes = list(size=4)))+
  scale_colour_manual(values=c("purple", "#4daf4a", "blue", "#ff7f00", "red", "green", "yellow", "pink", "grey"),
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
#Just note that there was no convergence in the NMDS calculation *

a

rownames(mackmat) <- mack$FID

autoplot(prcomp(mackmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=mack$Sp), size=2.5, shape=21, color = "black")+
  #  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
  #                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

#cluster 2: use all fish and color by species
#cluster 4: group together sites and color by species
#NMDS: color by species
#PCA: color by species

##### MACKEREL - CHUB #####

chub <- filter(biowide, Sp == "Chub Mackerel")
chubmat <- select(ungroup(chub), Amphipod:Pteropod)
chubmat <- as.matrix(chubmat)
chubinfo <- as.data.frame(chub$FID)
rownames(chubmat) <- chubinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(chubmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

chubsite <- chub %>%
  ungroup() %>%
  select(Site, FID)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "FID")

lab <- left_join(labs, chubsite, by = "FID")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Site), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

chubsum <- chub %>%
  group_by(Site, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod))

chubsmat <- select(ungroup(chubsum), Amphipod:Pteropod)
chubsmat <- as.matrix(chubsmat)
chubsinfo <- as.data.frame(chubsum$Site)
rownames(chubsmat) <- chubsinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(chubsmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

chubsite <- chubsum %>%
  ungroup() %>%
  select(Region, Site)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "Site")

lab <- left_join(labs, chubsite, by = "Site")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Region), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

rownames(chubmat) <- chub$FID

autoplot(prcomp(chubmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=chub$Site), size=2.5, shape=21, color = "black")+
  #  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
  #                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

##### MACKEREL - JACK #####

jack <- filter(biowide, Sp == "Jack Mackerel")
jackmat <- select(ungroup(jack), Amphipod:Pteropod)
jackmat <- as.matrix(jackmat)
jackinfo <- as.data.frame(jack$FID)
rownames(jackmat) <- jackinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(jackmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

jacksite <- jack %>%
  ungroup() %>%
  select(Region, FID)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "FID")

lab <- left_join(labs, jacksite, by = "FID")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Region), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

jacksum <- jack %>%
  group_by(Site, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod))

jacksmat <- select(ungroup(jacksum), Amphipod:Pteropod)
jacksmat <- as.matrix(jacksmat)
jacksinfo <- as.data.frame(jacksum$Site)
rownames(jacksmat) <- jacksinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(jacksmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

jacksite <- jacksum %>%
  ungroup() %>%
  select(Region, Site)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "Site")

lab <- left_join(labs, jacksite, by = "Site")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Region), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

rownames(jackmat) <- jack$FID

autoplot(prcomp(jackmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=jack$Region), size=2.5, shape=21, color = "black")+
  #  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
  #                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

#Decided to skip the NMDS for jack mackerel because will look similar to mackerel, which doesn't converge anyway.

##### ANCHOVY #####

anc <- filter(biowide, Sp=="Anchovy")
ancmat <- select(ungroup(anc), Amphipod:Pteropod)
ancmat <- as.matrix(ancmat)
ancinfo <- as.data.frame(anc$FID)
rownames(ancmat) <- ancinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(ancmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

ancsite <- anc %>%
  ungroup() %>%
  select(Site, FID)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "FID")

lab <- left_join(labs, ancsite, by = "FID")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Site), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

ancsum <- anc %>%
  group_by(Site, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod))

ancsmat <- select(ungroup(ancsum), Amphipod:Pteropod)
ancsmat <- as.matrix(ancsmat)
ancsinfo <- as.data.frame(ancsum$Site)
rownames(ancsmat) <- ancsinfo[, 1]
ancsinfo <- ancsum$Site

Bray_Curtis_Dissimilarity <- vegdist(ancsmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

ancsite <- ancsum %>%
  ungroup() %>%
  select(Site, Region)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "Site")

lab <- left_join(labs, ancsite, by = "Site")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Region), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

ancinfo <- anc$Site

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(ancmat,distance="bray",labels=ancinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(ancmat ~ ancinfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=ancinfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,ancinfo,display="sites",kind="sd", conf = 0.95, label=T)
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

rownames(ancmat) <- anc$FID

autoplot(prcomp(ancmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=anc$Site), size=2.5, shape=21, color = "black")+
  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

amph <- anc %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(anc)*100

arth <- anc %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(anc)*100

barn <- anc %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(anc)*100

biv <- anc %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(anc)*100

cala <- anc %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(anc)*100

chae <- anc %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(anc)*100

cnid <- anc %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(anc)*100

cope <- anc %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(anc)*100

crus <- anc %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(anc)*100

cten <- anc %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(anc)*100

cuma <- anc %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(anc)*100

cycl <- anc %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(anc)*100

deca <- anc %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(anc)*100

detr <- anc %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(anc)*100

diat <- anc %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(anc)*100

digf <- anc %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(anc)*100

euph <- anc %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(anc)*100

fish <- anc %>%
  ungroup() %>%
  tally(Fish>0)/nrow(anc)*100

gamm <- anc %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(anc)*100

harp <- anc %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(anc)*100

hyp <- anc %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(anc)*100

ins <- anc %>%
  ungroup() %>%
  tally(Insect>0)/nrow(anc)*100

iso <- anc %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(anc)*100

larv <- anc %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(anc)*100

mala <- anc %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(anc)*100

ostr <- anc %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(anc)*100

para <- anc %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(anc)*100

pter <- anc %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(anc)*100

anchovy <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(anchovy) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

##### SARDINES #####

sar <- filter(biowide, Sp=="Sardines")
sarmat <- select(ungroup(sar), Amphipod:Pteropod)
sarmat <- as.matrix(sarmat)
sarstatinfo <- c("Old", "Old", "Old", "Old", "Old", "Old", "Old", "Old", "Old", "Old", "Old", "Old", "Old",
             "Fresh", "Fresh", "Fresh", "Fresh", "Fresh", "Fresh", "Fresh", "Fresh", "Fresh",
             "Old", "Fresh", "Fresh", "Fresh", "Fresh", "Fresh", "Fresh", "Fresh", "Fresh", "Fresh", "Fresh",
             "Old", "Fresh", "Old", "Old", "Old", "Old", "Old")
sarinfo <- as.data.frame(sar$FID)
sarstatinfo <- as.data.frame(sarstatinfo)
rownames(sarmat) <- sarinfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(sarmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

sarsite <- sar %>%
  ungroup() %>%
  select(FID) %>%
  cbind(sarstatinfo)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "FID")

lab <- left_join(labs, sarsite, by = "FID")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$sarstatinfo), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

sarbind <- cbind.data.frame(sar, sarstatinfo) 

sarsum <- sarbind %>%
  group_by(sarstatinfo) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod))

sarsmat <- select(ungroup(sarsum), Amphipod:Pteropod)
sarsmat <- as.matrix(sarsmat)
sarsinfo <- as.data.frame(sarsum$sarstatinfo)
rownames(sarsmat) <- sarsinfo[, 1]
sarsinfo <- sarsum$sarstatinfo

Bray_Curtis_Dissimilarity <- vegdist(sarsmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=dendr$labels$label), size=3, angle=90) +
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

sarinfo <- sarbind$sarstatinfo

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(sarmat,distance="bray",labels=sarinfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(sarmat ~ sarinfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=sarinfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,sarinfo,display="sites",kind="sd", conf = 0.95, label=T)
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
#Note, the result was not significant, which is reflected in the overlapping plot.
#Which is actually good news, that the degradation factor from the formalin, doesn't affect results!

rownames(sarmat) <- sar$FID

autoplot(prcomp(sarmat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=sarbind$sarstatinfo), size=2.5, shape=21, color = "black")+
  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

amph <- sar %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(sar)*100

arth <- sar %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(sar)*100

barn <- sar %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(sar)*100

biv <- sar %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(sar)*100

cala <- sar %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(sar)*100

chae <- sar %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(sar)*100

cnid <- sar %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(sar)*100

cope <- sar %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(sar)*100

crus <- sar %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(sar)*100

cten <- sar %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(sar)*100

cuma <- sar %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(sar)*100

cycl <- sar %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(sar)*100

deca <- sar %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(sar)*100

detr <- sar %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(sar)*100

diat <- sar %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(sar)*100

digf <- sar %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(sar)*100

euph <- sar %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(sar)*100

fish <- sar %>%
  ungroup() %>%
  tally(Fish>0)/nrow(sar)*100

gamm <- sar %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(sar)*100

harp <- sar %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(sar)*100

hyp <- sar %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(sar)*100

ins <- sar %>%
  ungroup() %>%
  tally(Insect>0)/nrow(sar)*100

iso <- sar %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(sar)*100

larv <- sar %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(sar)*100

mala <- sar %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(sar)*100

ostr <- sar %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(sar)*100

para <- sar %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(sar)*100

pter <- sar %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(sar)*100

sardines <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(sardines) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

##### CHUM #####

chum <- filter(biowide, Sp=="Chum")
chummat <- select(ungroup(chum), Amphipod:Pteropod)
chummat <- as.matrix(chummat)
chuminfo <- as.data.frame(chum$FID)
rownames(chummat) <- chuminfo[, 1]

Bray_Curtis_Dissimilarity <- vegdist(chummat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

chumsite <- chum %>%
  ungroup() %>%
  select(Site, FID)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "FID")

lab <- left_join(labs, chumsite, by = "FID")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Site), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

chumsum <- chum %>%
  group_by(Site, Region) %>%
  summarise(Amphipod=mean(Amphipod), Arthropod=mean(Arthropod), Barnacle=mean(Barnacle), Bivalve=mean(Bivalve), Calanoid=mean(Calanoid), Chaetognath=mean(Chaetognath), Cnidarian=mean(Cnidarian), Copepod=mean(Copepod), Crustacean=mean(Crustacean), Ctenophore=mean(Ctenophore), Cumacean=mean(Cumacean), `Cyclopoid/Poecilostomatoid`=mean(`Cyclopoid/Poecilostomatoid`), Decapod=mean(Decapod), Detritus=mean(Detritus), Diatom=mean(Diatom), `Digested food`=mean(`Digested food`), Euphausiid=mean(Euphausiid), Fish=mean(Fish), Gammarid=mean(Gammarid), Harpacticoid=mean(Harpacticoid), Hyperiid=mean(Hyperiid), Insect=mean(Insect), Isopod=mean(Isopod), Larvacean=mean(Larvacean), Malacostracan=mean(Malacostracan), Ostracod=mean(Ostracod), Parasite=mean(Parasite), Pteropod=mean(Pteropod))

chumsmat <- select(ungroup(chumsum), Amphipod:Pteropod)
chumsmat <- as.matrix(chumsmat)
chumsinfo <- as.data.frame(chumsum$Site)
rownames(chumsmat) <- chumsinfo[, 1]
chumsinfo <- chumsum$Site

Bray_Curtis_Dissimilarity <- vegdist(chumsmat, method = "bray")
bcclust <- hclust(Bray_Curtis_Dissimilarity)
dendr <- dendro_data(bcclust, type = "rectangle")

chumsite <- chumsum %>%
  ungroup() %>%
  select(Site, Region)

labs <- label(dendr)

colnames(labs) <- c("x", "y", "Site")

lab <- left_join(labs, chumsite, by = "Site")

ggplot()+
  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, color="white"))+
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=1, color=lab$Region), size=3, angle=90) +
  #coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  #scale_colour_manual(values = c("purple", "red", "dark green", "blue", "yellow", "dark orange", "black", "dark blue", "green", "brown", "aquamarine"))+
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid=element_blank(), legend.position = "bottom")

lesschum <- filter(chum, Site != "QLST06")
chumlmat <- select(ungroup(lesschum), Amphipod:Pteropod)
chumlmat <- as.matrix(chumlmat)
chumlinfo <- as.data.frame(lesschum$FID)
rownames(chumlmat) <- chumlinfo[, 1]
chumlinfo <- lesschum$FID

chuminfo <- lesschum$Site

#region, proportion based dissimilarity - bray curtis
eco.nmds.bc<- metaMDS(chumlmat,distance="bray",labels=chuminfo, trymax = 100, autotransform = FALSE)
eco.nmds.bc
plot(eco.nmds.bc)
#found convergence easily (<20), stress is rel. low = 0.06764617

##PERMANOVA - provides r2 and p values related to the nmds (are differences between factor levels (e.g. clusters) significant?)
permanova_eco.bc<-adonis(chumlmat ~ chuminfo, permutations = 999, method="bray")
permanova_eco.bc #significant! p = 0.001 so plot it

NMDS.bc<-data.frame(NMDS1.bc=eco.nmds.bc$points[,1],NMDS2.bc=eco.nmds.bc$points[,2],group=chuminfo)
#plot NMDS, only once (picking Bray because all similar), no presence absence

ord.bc<-ordiellipse(eco.nmds.bc,chuminfo,display="sites",kind="sd", conf = 0.95, label=T)
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

rownames(chummat) <- chum$FID

autoplot(prcomp(chummat), geom = "point", loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3, loadings.label.colour="black") +
  geom_point(stat = "identity", aes(fill=chum$Site), size=2.5, shape=21, color = "black")+
  scale_fill_manual(values = c("purple", "#4daf4a", "blue", "#ff7f00", "red", "aquamarine", "yellow", "brown", "grey", "pink", "white"),
                    name="Region", guide="legend")+
  labs(x="PC1", y="PC2")+
  #scale_y_continuous(limits=c(-0.2,0.21),breaks=seq(-0.2,0.21,by=.1))+
  #scale_x_continuous(limits=c(-0.13,0.22),breaks=seq(-0.13,0.22,by=.1))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.background = element_rect(color = "dark grey", fill = NA))
#Main PCA graph we're working with here!

amph <- chum %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(chum)*100

arth <- chum %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(chum)*100

barn <- chum %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(chum)*100

biv <- chum %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(chum)*100

cala <- chum %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(chum)*100

chae <- chum %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(chum)*100

cnid <- chum %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(chum)*100

cope <- chum %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(chum)*100

crus <- chum %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(chum)*100

cten <- chum %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(chum)*100

cuma <- chum %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(chum)*100

cycl <- chum %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(chum)*100

deca <- chum %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(chum)*100

detr <- chum %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(chum)*100

diat <- chum %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(chum)*100

digf <- chum %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(chum)*100

euph <- chum %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(chum)*100

fish <- chum %>%
  ungroup() %>%
  tally(Fish>0)/nrow(chum)*100

gamm <- chum %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(chum)*100

harp <- chum %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(chum)*100

hyp <- chum %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(chum)*100

ins <- chum %>%
  ungroup() %>%
  tally(Insect>0)/nrow(chum)*100

iso <- chum %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(chum)*100

larv <- chum %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(chum)*100

mala <- chum %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(chum)*100

ostr <- chum %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(chum)*100

para <- chum %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(chum)*100

pter <- chum %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(chum)*100

chum <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(chum) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

#things to do tomorrow besides making interpretations and beginning to write and research:
#perform simper analysis for a lot of these different combinations (find out how!)
#do clustering, pca and nmds for certain zoop regions to look closer at 'em
#print out (or doodle out) a map to see where all of the stations are! for interpreting.
#frequency of occurance calculations for fishies? bar plots for zoops over time and space!
#and maybe size histograms of zoops and fish prey; and direct comparisons of zoops and prey
##### FREQUENCY OF OCCURANCE #####

amph <- emptywide %>%
  ungroup() %>%
  tally(Amphipod>0)/nrow(emptywide)*100

arth <- emptywide %>%
  ungroup() %>%
  tally(Arthropod>0)/nrow(emptywide)*100

barn <- emptywide %>%
  ungroup() %>%
  tally(Barnacle>0)/nrow(emptywide)*100

biv <- emptywide %>%
  ungroup() %>%
  tally(Bivalve>0)/nrow(emptywide)*100

cala <- emptywide %>%
  ungroup() %>%
  tally(Calanoid>0)/nrow(emptywide)*100

chae <- emptywide %>%
  ungroup() %>%
  tally(Chaetognath>0)/nrow(emptywide)*100

cnid <- emptywide %>%
  ungroup() %>%
  tally(Cnidarian>0)/nrow(emptywide)*100

cope <- emptywide %>%
  ungroup() %>%
  tally(Copepod>0)/nrow(emptywide)*100

crus <- emptywide %>%
  ungroup() %>%
  tally(Crustacean>0)/nrow(emptywide)*100

cten <- emptywide %>%
  ungroup() %>%
  tally(Ctenophore>0)/nrow(emptywide)*100

cuma <- emptywide %>%
  ungroup() %>%
  tally(Cumacean>0)/nrow(emptywide)*100

cycl <- emptywide %>%
  ungroup() %>%
  tally(`Cyclopoid/Poecilostomatoid`>0)/nrow(emptywide)*100

deca <- emptywide %>%
  ungroup() %>%
  tally(Decapod>0)/nrow(emptywide)*100

detr <- emptywide %>%
  ungroup() %>%
  tally(Detritus>0)/nrow(emptywide)*100

diat <- emptywide %>%
  ungroup() %>%
  tally(Diatom>0)/nrow(emptywide)*100

digf <- emptywide %>%
  ungroup() %>%
  tally(`Digested food`>0)/nrow(emptywide)*100

euph <- emptywide %>%
  ungroup() %>%
  tally(Euphausiid>0)/nrow(emptywide)*100

fish <- emptywide %>%
  ungroup() %>%
  tally(Fish>0)/nrow(emptywide)*100

gamm <- emptywide %>%
  ungroup() %>%
  tally(Gammarid>0)/nrow(emptywide)*100

harp <- emptywide %>%
  ungroup() %>%
  tally(Harpacticoid>0)/nrow(emptywide)*100

hyp <- emptywide %>%
  ungroup() %>%
  tally(Hyperiid>0)/nrow(emptywide)*100

ins <- emptywide %>%
  ungroup() %>%
  tally(Insect>0)/nrow(emptywide)*100

iso <- emptywide %>%
  ungroup() %>%
  tally(Isopod>0)/nrow(emptywide)*100

larv <- emptywide %>%
  ungroup() %>%
  tally(Larvacean>0)/nrow(emptywide)*100

mala <- emptywide %>%
  ungroup() %>%
  tally(Malacostracan>0)/nrow(emptywide)*100

ostr <- emptywide %>%
  ungroup() %>%
  tally(Ostracod>0)/nrow(emptywide)*100

para <- emptywide %>%
  ungroup() %>%
  tally(Parasite>0)/nrow(emptywide)*100

pter <- emptywide %>%
  ungroup() %>%
  tally(Pteropod>0)/nrow(emptywide)*100

allfish <- cbind(amph, arth, barn, biv, cala, chae, cnid, cope, crus, cten, cuma, cycl, deca, detr, diat, digf, euph, fish, gamm, harp, hyp, ins, iso, larv, mala, ostr, para, pter)

colnames(allfish) <- c("amph", "arth", "barn", "biv", "cala", "chae", "cnid", "cope", "crus", "cten", "cuma", "cycl", "deca", "detr", "diat", "digf", "euph", "fish", "gamm", "harp", "hyp", "ins", "iso", "larv", "mala", "ostr", "para", "pter")

foo <- rbind(allfish, herring, pompano, stickleback, pollock, eulachon, capelin, mackerel, anchovy, sardines, chum)

rownames(foo) <- c("allfish", "herring", "pompano", "stickleback", "pollock", "eulachon", "capelin", "mackerel", "anchovy", "sardines", "chum")