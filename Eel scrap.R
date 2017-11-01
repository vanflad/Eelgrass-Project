#Eelgrass NDMS

eelgrass <- read.csv(file="eel.csv", stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA",""))
#read in file

library(tidyverse)
#library for manipulation and plotting

library(gridExtra)
#for plotting graphs side by side

eelgrass <- mutate(eelgrass, Eelsite = paste(Site, Eelgrass, sep = " "))
#create new column for eelgrass site

eelgrass$GFI[which(eelgrass$GFI == "0-25")] <- "0-25%"
#fixing a typo, oopsie!
eelgrass$GFI[which(eelgrass$GFI == "0%")] <- "0"
eelgrass$GFI[which(eelgrass$GFI == "0-25%")] <- "12.5"
eelgrass$GFI[which(eelgrass$GFI == "25-50%")] <- "37.5"
eelgrass$GFI[which(eelgrass$GFI == "50-75%")] <- "62.5"
eelgrass$GFI[which(eelgrass$GFI == "75-100%")] <- "87.5"
eelgrass$GFI[which(eelgrass$GFI == "100%")] <- "100"
#changing ranges to something measurable
eelgrass$GFI <- as.numeric(eelgrass$GFI)
#changing from character to numeric

View(eelgrass)

#forget this r script, continue off huge main r script file

#eel <- eelgrass %>%
#  select(ID, Detail, Abundance)

eelwide <- eel %>%
  group_by(ID) %>% 
  spread(Detail, Abundance, fill=0)

?spread()

#Ditched code at bottom of eelgrass.R file:

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