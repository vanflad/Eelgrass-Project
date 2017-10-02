#EEEEELGRAAAAASS PROOOOOJEEEEECT

setwd("~/Desktop")
#so it knows where to find my gosh darn files! :)

eelgrass <- read.csv(file="eel.csv", stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
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

#Gut Fullness Graph!
ggplot(gut, aes(x=Eelgrass, y=GFI, fill=Eelgrass))+ 
  geom_boxplot()+
  facet_wrap(~Site)+
  labs(y="Gut Fullness Index", x=NULL, caption="p-value = 0.025",
       title="Eelgrass vs. Non-eelgrass: Salmon Gut Fullness")+
  theme_bw()+
  theme(axis.text.x=element_blank(), axis.ticks=element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.text = element_text(size=15),
        title = element_text(size=15),
        strip.text = element_text(size=15, face="bold"))+
  scale_x_discrete(limits=c("Eelgrass", "Noneelgrass")) +
  scale_fill_discrete(breaks=c("Eelgrass", "Noneelgrass"))+
  geom_jitter(width=0.2)
#NEED TO CHANGE PANEL TITLES SOMEHOW! *****

#working on a t-test for GFI values (not including bedwell)
#y1 <- filter(gut, Eelgrass == "Eelgrass")
#ya <- as.vector(y1[["GFI"]])
#create first vector for eelgrass
#y2 <- filter(gut, Eelgrass == "Noneelgrass")
#yb <- as.vector(y2[["GFI"]])
#create second vector for noneelgrass
#yc <- length(ya)-length(yb)
#yd <- rep_len(NA, yc)
#have to have same sample sizes, difference calculated using length
#ye <- c(yb, yd)
#our completed noneelgrass vector with the right sample size
#t.test(ya, ye)
#results give p-value=0.02529, are able to reject null hypothesis!
#hash-tagged out because I don't need to keep running this code.

#switching it up to isotope work now ~

isotope <- read.csv(file="iso.csv", stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
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
  theme_bw()+
  theme(axis.ticks=element_blank(), legend.title=element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.text = element_text(size=15),
        title = element_text(size=15),
        strip.text = element_text(size=15, face="bold"))+
  guides(color = guide_legend(reverse = TRUE))
#Still need to change panel titles for this graph too! *****

#working on diet data now, to graph abundance and whatnot! ~

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
eel$Group[which(eel$Group=="Digested food")] <- "Digested/Other"
eel$Group[which(eel$Group=="Detritus")] <- "Digested/Other"
eel$Group[which(eel$Group=="Diatom")] <- "Digested/Other"
eel$Group[which(eel$Group=="Parasite")] <- "Digested/Other"
eel$Group[which(eel$Group=="Octopus")] <- "Digested/Other"
eel$Group[which(eel$Group=="Cyphonaut")] <- "Digested/Other"
eel$Group[which(eel$Group=="Bivalve")] <- "Digested/Other"
eel$Group[which(eel$Group=="Chaetognath")] <- "Digested/Other"
eel$Group[which(eel$Group=="Polychaete")] <- "Digested/Other"
eel$Group[which(eel$Group=="Pteropod")] <- "Digested/Other"
eel$Group[which(eel$Group=="Jellyfish")] <- "Digested/Other"
eel$Group[which(eel$Group=="Cumacean")] <- "Arthropod"
eel$Group[which(eel$Group=="Euphausiid")] <- "Arthropod"
eel$Group[which(eel$Group=="Isopod")] <- "Arthropod"
eel$Group[which(eel$Group=="Larvacean")] <- "Digested/Other"
eel$Group[which(eel$Group=="Mysid")] <- "Arthropod"
eel$Group[which(eel$Group=="Cladoceran")] <- "Arthropod"
eel$Group[which(eel$Group=="Ostracod")] <- "Arthropod"
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
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site)
#HOLY GUACAMOLE I THINK I GOT IT TO FINALLY WOOOOORK! FOR REAL THO!

filt <- filter(eel, Group=="Arthropod")
arth <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site)
#first amphipod then arthropod then repeat manually for each prey

filt <- filter(eel, Group=="Barnacle")
barn <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site)

filt <- filter(eel, Group=="Copepod")
cope <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site)

filt <- filter(eel, Group=="Decapod")
deca <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site) %>%
  print(head(deca))

filt <- filter(eel, Group=="Digested/Other")
digf <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site)

filt <- filter(eel, Group=="Fish")
fish <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site)

filt <- filter(eel, Group=="Insect")
inse <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), bio=sum(Biomass), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, bio, Group, Eelsite, Eelgrass, Site)

neweel <- rbind(amph, arth, barn, cope, deca, digf, fish, inse)
#my fabulous new dataframe with prey abd and biom grouped together!

str(neweel)
#just double checking that the data is as it should be

#Now... Have to take the ave. abundances for each region/habitat!
filt <- filter(neweel, Group=="Amphipod")
Amph <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site)
#copying the layout of the last transformation but for diff purpose

filt <- filter(neweel, Group=="Arthropod")
Arth <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site)
#first amphipod then arthropod then repeat manually for each prey

filt <- filter(neweel, Group=="Barnacle")
Barn <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site)

filt <- filter(neweel, Group=="Copepod")
Cope <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site)

filt <- filter(neweel, Group=="Decapod")
Deca <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site)

filt <- filter(neweel, Group=="Digested/Other")
Digf <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site)

filt <- filter(neweel, Group=="Fish")
Fish <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site)

filt <- filter(neweel, Group=="Insect")
Inse <- filt %>%
  group_by(filt$Eelsite) %>%
  summarize(Eelsite=first(Eelsite), Abd=mean(abd), Bio=mean(bio),
            Group=first(Group),Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Abd, Bio, Group, Eelgrass, Site)

eelgr <- rbind(Amph, Arth, Barn, Cope, Deca, Digf, Fish, Inse)
#my fabulous new dataframe with prey Abd and Biom averaged together

str(eelgr)
#making sure it all looks okay and it does! So now, graphing!

eelgr$Eelgrass <- factor(eelgr$Eelgrass, levels(eelgr$Eelgrass)[c(2, 1)])
#putting eelgrass before noneelgrass for this particular plot

eelgr$Eelsite <- as.factor(eelgr$Eelsite)
#rearranging levels for site and eelgrass

eelgr$Eelsite <- factor(eelgr$Eelsite, levels(eelgr$Eelsite)[c(5, 4, 3, 2, 1, 7, 6)])
#re-ordering levels to make it look nice for graphs!

#Raw Abundance Graph
eelgr %>%
  ggplot(aes(Eelsite, Abd))+
  geom_bar(aes(fill=Group), stat="identity")+
  theme_minimal()+
  theme(axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.text = element_text(size=12))+
  coord_flip() +
  labs(title="Average Prey Abundance", x=NULL,
       y="Ave. Abundance (# of organisms)")+
  guides(fill=guide_legend(title="Prey Group"))
#looks amaze now

#Relative Abundance Graph (100% y scale)
ggplot(eelgr, aes(Eelsite, Abd)) +
  geom_bar(aes(fill=Group), position="fill", stat="identity")+
  theme_minimal()+
  theme(axis.ticks=element_blank(),
        panel.grid = element_blank(),
        title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.text = element_text(size=12))+
  coord_flip() +
  labs(title="Relative Prey Abundance", x=NULL,
       y="Relative Abundance (%)")+
  guides(fill=guide_legend(title="Prey Group"))+
  scale_y_continuous(labels=scales::unit_format("", 100))

#Raw Biomass Graph
eelgr %>%
  ggplot(aes(Eelsite, Bio))+
  geom_bar(aes(fill=Group), stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.text = element_text(size=12))+
  coord_flip() +
  labs(title="Average Prey Biomass", x=NULL,
       y="Ave. Biomass (Wet weight, mg)")+
  guides(fill=guide_legend(title="Prey Group"))

#Relative Biomass Graph
eelgr %>%
  ggplot(aes(Eelsite, Bio))+
  geom_bar(aes(fill=Group), position = "fill", stat = "identity")+
  theme_minimal()+
  theme(axis.ticks=element_blank(),
        panel.grid = element_blank(),
        title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.text = element_text(size=12))+
  coord_flip() +
  labs(title="Relative Prey Biomass", x=NULL,
       y="Relative Biomass (%)")+
  guides(fill=guide_legend(title="Prey Group"))+
  scale_y_continuous(labels=scales::unit_format("", 100))
ggsave("RELB.jpeg")
setwd("~/Eelgrass Project")
#in order to commit to Github and track saves and changes and etc!

#next step is prey selectivity! then nmds analysis and stuff? idk!

setwd("~/Desktop")
#reset working directory to find new file

invert <- read.csv(file="invertebrates.csv", stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
#read in invertebrate file

invert $Group <- as.factor(invert$Group)
#change to factor

levels(invert$Group)
#check the groups

eelgrass <- mutate(eelgrass, Eelsite = paste(Site, Eelgrass, sep = " "))
#mutate this dataframe with eelsite as well

eelgrass$Group <- as.factor(eelgrass$Group)
#turn into factor

#change groups to match inverts
eelgrass$Group[which(eelgrass$Group=="Arachnid")] <- "Insect"
eelgrass$Group <- as.character(eelgrass$Group) #change to create new "level"
eelgrass$Group[which(eelgrass$Group=="Crustacean")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Corophiid")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Malacostracan")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Digested food")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Detritus")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Diatom")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Parasite")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Octopus")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Cyphonaut")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Chaetognath")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Isopod")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Arthropod")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Amphipod")] <- "Digested/Other"
eelgrass$Group[which(eelgrass$Group=="Copepod")] <- "Digested/Other"
#switching the class back and forth gets rid of empty levels
eelgrass$Group <- as.factor(eelgrass$Group)
eelgrass$Group <- as.character(eelgrass$Group)
eelgrass$Group <- as.factor(eelgrass$Group)
levels(eelgrass$Group)
#there, inverts and stom data have matching groups now

#group prey together so there's no repeating rows
filt <- filter(eelgrass, Group=="Barnacle")
barn <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)
#manually repeating each step is the best way I know how to do it.
filt <- filter(eelgrass, Group=="Bivalve")
biv <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Calanoid")
cal <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Caprellid")
capr <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Cladoceran")
clad <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Cumacean")
cuma <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Cyclopoid/Poecilostomatoid")
cycl <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Decapod")
deca <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Digested/Other")
digf <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Euphausiid")
euph <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Fish")
fish <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Gammarid")
gam <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Harpacticoid")
harp <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Hyperiid")
hyp <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Insect")
ins <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Jellyfish")
jell <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Larvacean")
larv <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Mysid")
mys <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Ostracod")
ostr <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Polychaete")
poly <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

filt <- filter(eelgrass, Group=="Pteropod")
pter <- filt %>%
  group_by(filt$ID) %>%
  summarize(ID=first(ID), abd=sum(Abundance), Group=first(Group),
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(ID, abd, Group, Eelsite, Eelgrass, Site)

eelgrass2 <- rbind(barn, biv, cal, capr, clad, cuma, cycl, deca,
                   digf, euph, fish, gam, harp, hyp, ins, jell,
                   larv, mys, ostr, poly, pter) #new dataframe!

str(eelgrass2)
#check that it all worked out okay

#now we need the proportions for each site to compare w inverts!
filt <- filter(eelgrass2, Eelsite == "Qualicum Eelgrass")
QE <- filt %>%
  group_by(Group) %>%
  summarize(Abd=mean(abd), Total=sum(abd), Proportion=Abd/Total*100,
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Proportion, Group, Eelgrass, Site)
#one for each site
filt <- filter(eelgrass2, Eelsite == "Qualicum Noneelgrass")
QN <- filt %>%
  group_by(Group) %>%
  summarize(Abd=mean(abd), Total=sum(abd), Proportion=Abd/Total*100,
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Proportion, Group, Eelgrass, Site)

filt <- filter(eelgrass2, Eelsite == "Fraser Eelgrass")
FE <- filt %>%
  group_by(Group) %>%
  summarize(Abd=mean(abd), Total=sum(abd), Proportion=Abd/Total*100,
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Proportion, Group, Eelgrass, Site)

filt <- filter(eelgrass2, Eelsite == "Fraser Noneelgrass")
FN <- filt %>%
  group_by(Group) %>%
  summarize(Abd=mean(abd), Total=sum(abd), Proportion=Abd/Total*100,
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Proportion, Group, Eelgrass, Site)

filt <- filter(eelgrass2, Eelsite == "Koeye Eelgrass")
KE <- filt %>%
  group_by(Group) %>%
  summarize(Abd=mean(abd), Total=sum(abd), Proportion=Abd/Total*100,
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Proportion, Group, Eelgrass, Site)

filt <- filter(eelgrass2, Eelsite == "Koeye Noneelgrass")
KN <- filt %>%
  group_by(Group) %>%
  summarize(Abd=mean(abd), Total=sum(abd), Proportion=Abd/Total*100,
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Proportion, Group, Eelgrass, Site)

filt <- filter(eelgrass2, Eelsite == "Bedwell noneelgrass")
BN <- filt %>%
  group_by(Group) %>%
  summarize(Abd=mean(abd), Total=sum(abd), Proportion=Abd/Total*100,
            Eelsite=first(Eelsite), Eelgrass=first(Eelgrass), Site=first(Site)) %>%
  select(Eelsite, Proportion, Group, Eelgrass, Site)

sites <- rbind(QE, QN, FE, FN, KE, KN, BN)
#combine into dataframe
str(sites)
#make sure it worked

#sites$inpro <- invert$Proportion[match(sites$Eelsite, invert$Eelsite)]
#match info from dataframes together, not correct way to do it?

#str(sites$inpro)
#summary(sites$inpro)
#check if it worked, it didn't, I give up for now...