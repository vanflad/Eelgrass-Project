#EEEEELGRAAAAASS PROOOOOJEEEEECT

eelgrass <- read.csv(file.choose(), stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
#upload a simple version of raw data file

library(tidyverse)
#load library for ggplot and whatnot

eel <- mutate(eelgrass, Eelsite = paste(Site, Eelgrass, sep = " "))
#make a variable with both site and habitat, separated by a space

#first we'll work with the gut fullness aspect ~

eel$GFI[which(eel$GFI == "0-25")] <- "0-25%"
#fixing a typo, oopsie!

eel$GFI[which(eel$GFI == "0%")] <- "0"
eel$GFI[which(eel$GFI == "0-25%")] <- "12.5"
eel$GFI[which(eel$GFI == "25-50%")] <- "37.5"
eel$GFI[which(eel$GFI == "50-75%")] <- "62.5"
eel$GFI[which(eel$GFI == "75-100%")] <- "87.5"
eel$GFI[which(eel$GFI == "100%")] <- "100"
#changing ranges to something measurable

eel$GFI <- as.numeric(eel$GFI)
#changing from character to numeric

eel$Eelgrass <- as.factor(eel$Eelgrass)
#changing to factor to reflect the two different "levels"

eel$Eelgrass <- factor(eel$Eelgrass, levels(eel$Eelgrass)[c(2, 1)])
#rearranging levels for the sake of the ggplot visuals

gut <- eel %>%
  group_by(ID) %>%
  summarize(GFI=first(GFI), Eelgrass=first(Eelgrass),
            Site=first(Site), Eelsite=first(Eelsite)) %>%
  filter(Site != "Bedwell")
#need to organize data by each fish not by each prey item per fish
#also want to leave out Bedwell since it is useless for comparing

library(lattice)
#load library first
bwplot(GFI ~ Site | Eelgrass, data=gut, scales = list(x = list(rot = 45)), ylab = "Gut Fullness Index")
#lattice boxplot separates by eelgrass and non-eelgrass, it's okay.
bwplot(GFI ~ Eelgrass | Site, data=gut, scales = list(x = list(rot=45)),
       ylab = "Gut Fullness Index")
#lattice boxplot separates by region rather than eelgrass, better!
#but neither are as nice as ggplot actually so nevermind

library(ggplot2)
#load the ggplot library

#Gut Fullness Graph!
ggplot(gut, aes(x=Eelgrass, y=GFI, fill=Eelgrass))+ 
  geom_boxplot()+
  facet_wrap(~Site)+
  labs(y="Gut Fullness Index", x=NULL, caption="p-value = 0.025",
       title="Eelgrass vs. Non-eelgrass: Salmon Gut Fullness")+
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        panel.grid.major.x = element_blank())+
  scale_x_discrete(limits=c("Eelgrass", "Noneelgrass")) +
  scale_fill_discrete(breaks=c("Eelgrass", "Noneelgrass"))+
  geom_jitter(width=0.2)
#this is the most I can do to modify this, ask for feedback...

#working on a t-test for GFI values (not including bedwell)
y1 <- filter(gut, Eelgrass == "Eelgrass")
ya <- as.vector(y1[["GFI"]])
#create first vector for eelgrass
y2 <- filter(gut, Eelgrass == "Noneelgrass")
yb <- as.vector(y2[["GFI"]])
#create second vector for noneelgrass
yc <- length(ya)-length(yb)
yd <- rep_len(NA, yc)
#have to have same sample sizes, difference calculated using length
ye <- c(yb, yd)
#our completed noneelgrass vector with the right sample size
t.test(ya, ye)
#results give p-value=0.02529, are able to reject null hypothesis!

#switching it up to isotope work now ~

isotope <- read.csv(file.choose(), stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
#loading/reading isotope data in

iso <- mutate(isotope, Eelsite = paste(Site, Eelgrass, sep = " "))
#add variable w/eelgrass site just other dataframe (necessary tho?)

iso$Eelgrass <- as.factor(iso$Eelgrass)
#changing to factor to reflect the two different "levels"

iso$Eelgrass <- factor(iso$Eelgrass, levels(iso$Eelgrass)[c(2, 1)])
#rearranging levels for the sake of the ggplot visuals

#Isotope Graph!
ggplot(iso, aes(carb, nitro))+
  geom_point(aes(color=Eelgrass)) + facet_wrap(~ Site) +
  labs(x=expression(paste(delta^"13","C")),
       y=expression(paste(delta^"15","N")),
       title= "Stable Isotopes of Salmon Muscle Tissue")+
  theme(axis.ticks=element_blank(), legend.title=element_blank(),
        panel.grid.major = element_blank())+
  guides(color = guide_legend(reverse = TRUE))
#A beautiful scatterplot plot with all the right features (finally)

#working on a t-test for isotope values (not including bedwell)
y1 <- filter(iso, Eelgrass == "Eelgrass" & Site != "Bedwell")
ya <- as.vector(y1[["nitro"]])
#create first vector for eelgrass
y2 <- filter(iso, Eelgrass == "Non-eelgrass" & Site != "Bedwell")
yb <- as.vector(y2[["nitro"]])
#create second vector for noneelgrass
yc <- length(ya)-length(yb)
yd <- rep_len(NA, yc)
#have to have same sample sizes, difference calculated using length
ye <- c(yb, yd)
#our completed noneelgrass vector with the right sample size
t.test(ya, ye)
#C results give p-value=0.2745, not able to reject null hypothesis
#R results give p-value=0.2966, not able to reject again, just FYI

#working on diet data now, to graph abundance and whatnot

eel$Group <- as.factor(eel$Group)
#changing to factor to reflect the different "levels" of prey items

#replace groups since there's too many
eel$Group[which(eel$Group=="Arachnid")] <- "Insect"
eel$Group[which(eel$Group=="Crustacean")] <- "Arthropod"
eel$Group[which(eel$Group=="Caprellid")] <- "Amphipod"
eel$Group[which(eel$Group=="Corophiid")] <- "Amphipod"
eel$Group[which(eel$Group=="Hyperiid")] <- "Amphipod"
eel$Group[which(eel$Group=="Gammarid")] <- "Amphipod"
eel$Group[which(eel$Group=="Calanoid")] <- "Copepod"
eel$Group[which(eel$Group=="Harpacticoid")] <- "Copepod"
eel$Group[which(eel$Group=="Cyclopoid/Poecilostomatoid")] <- "Copepod"
eel$Group[which(eel$Group=="Malacostracan")] <- "Arthropod"
eel$Group <- as.character(eel$Group) #change to create new "level"
eel$Group[which(eel$Group=="Detritus")] <- "Other"
eel$Group[which(eel$Group=="Diatom")] <- "Other"
eel$Group[which(eel$Group=="Parasite")] <- "Other"
eel$Group[which(eel$Group=="Octopus")] <- "Other"
eel$Group[which(eel$Group=="Cyphonaut")] <- "Other"
eel$Group[which(eel$Group=="Bivalve")] <- "Other"
eel$Group[which(eel$Group=="Chaetognath")] <- "Other"
eel$Group[which(eel$Group=="Mysid")] <- "Arthropod"
eel$Group[which(eel$Group=="Ostracod")] <- "Arthropod"
eel$Group[which(eel$Group=="Cladoceran")] <- "Arthropod"
eel$Group[which(eel$Group=="Cumacean")] <- "Arthropod"
eel$Group[which(eel$Group=="Euphausiid")] <- "Arthropod"
eel$Group[which(eel$Group=="Isopod")] <- "Arthropod"
eel$Group[which(eel$Group=="Larvacean")] <- "Other"
#switching the class back and forth gets rid of empty levels
eel$Group <- as.factor(eel$Group)
eel$Group <- as.character(eel$Group)
eel$Group <- as.factor(eel$Group)
levels(eel$Group)
#there, it's more simple to work with now!

#group together prey so there's no repeated rows for multiple prey
filt <- filter(eel, Group=="Amphipod")
amph <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(amph))
#HOLY GUACAMOLE I THINK I GOT IT TO FINALLY WOOOOORK! FOR REAL THO!

filt <- filter(eel, Group=="Arthropod")
arth <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(arth))
#first amphipod then arthropod then repeat manually for each prey

filt <- filter(eel, Group=="Barnacle")
barn <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(barn))

filt <- filter(eel, Group=="Cnidarian")
cnid <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(cnid))

filt <- filter(eel, Group=="Copepod")
cope <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(cope))

filt <- filter(eel, Group=="Decapod")
deca <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(deca))

filt <- filter(eel, Group=="Digested food")
digf <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(digf))

filt <- filter(eel, Group=="Fish")
fish <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(fish))

filt <- filter(eel, Group=="Insect")
inse <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(inse))

filt <- filter(eel, Group=="Other")
othr <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(othr))

filt <- filter(eel, Group=="Polychaete")
polyc <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(polyc))

filt <- filter(eel, Group=="Pteropod")
pter <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(pter))

neweel <- rbind(amph, arth, barn, cnid, cope, deca, digf, fish, inse, othr, polyc, pter)
#my fabulous new dataframe with prey abd and biom grouped together!

?data.frame
str(neweel)
head(eelg)
n_distinct(neweel$Group)
sum(neweel$abd)
eel$Abundance <- as.numeric(eel$Abundance)
sum(eel$Abundance)
which(is.na(eel$Abundance)==TRUE)
#just double checking that the data is as it should be

#Now... Have to take the ave. abundances for each region/habitat!
filt <- filter(neweel, Group=="Amphipod")
Amph <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Amph))
#copying the layout of the last transformation but for diff purpose

filt <- filter(neweel, Group=="Arthropod")
Arth <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Arth))
#first amphipod then arthropod then repeat manually for each prey

filt <- filter(neweel, Group=="Barnacle")
Barn <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Barn))

filt <- filter(neweel, Group=="Cnidarian")
Cnid <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Cnid))

filt <- filter(neweel, Group=="Copepod")
Cope <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Cope))

filt <- filter(neweel, Group=="Decapod")
Deca <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Deca))

filt <- filter(neweel, Group=="Digested food")
Digf <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Digf))

filt <- filter(neweel, Group=="Fish")
Fish <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Fish))

filt <- filter(neweel, Group=="Insect")
Inse <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Inse))

filt <- filter(neweel, Group=="Other")
Othr <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Othr))

filt <- filter(neweel, Group=="Polychaete")
Polyc <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Polyc))

filt <- filter(neweel, Group=="Pteropod")
Pter <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site) %>%
  print(head(Pter))

eelgr <- rbind(Amph, Arth, Barn, Cnid, Cope, Deca, Digf, Fish, Inse, Othr, Polyc, Pter)
#my fabulous new dataframe with prey Abd and Biom averaged together

str(eelgr)
#making sure it all looks okay and it does! So now, graphing!

eelgr <- mutate(eelgr, ABD=log10(Abd+1))
#log transform the abundances so it's easier to see in the graph
eelg <- mutate(eelgr, ABD=log(Abd+1))
#can alternatively use natural log - does it matter??

eelg$Eelgrass <- factor(eelg$Eelgrass, levels(eelg$Eelgrass)[c(2, 1)])
#putting eelgrass before noneelgrass for this particular plot

#Log Abundance Graph
eelgr %>%
  ggplot(aes(Eelsite, ABD))+
  geom_bar(aes(fill=Group), stat="identity")+
  theme(axis.text.x = element_text(angle=45, hjust=1))
#raw Abd gives QE>QNe but log(Abd) gives QNe>QE, not ideal...
#i think i might need to log transform data BEFORE averaging...?

ggsave("LogAbd.pdf") #saves last plot to a PDF

#Raw Abundance Graph
eelgr %>%
  ggplot(aes(Eelsite, Abd))+
  geom_bar(aes(fill=Group), stat="identity")+
  theme(axis.text.x = element_text(angle=45, hjust=1))

#if want to have sites graphed separately:
#add "+facet_wrap(~Site)" and change "Eelsite" to "Eelgrass"

eelgr <- mutate(eelgr, BIO=log10(Bio+1))
#log transforming biomass data too

#Log Biomass Graph
eelgr %>%
  ggplot(aes(Eelsite, BIO))+
  geom_bar(aes(fill=Group), stat="identity")+
  theme(axis.text.x = element_text(angle=45, hjust=1))

#Raw Biomass Graph
eelgr %>%
  ggplot(aes(Eelsite, Bio))+
  geom_bar(aes(fill=Group), stat="identity")+
  theme(axis.text.x = element_text(angle=45, hjust=1))

#next step is prey selectivity... need to import invert datafile.
#y axis continuous

eelg %>%
  filter(ID) %>%
  summarise(ID=first(ID), Eelsite=first(Eelsite), relabd=)

scale_y_continuous() #figure out how to utilize this function!

