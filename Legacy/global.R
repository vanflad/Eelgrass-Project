longdata <- readRDS("longdata.rds")
  
longdata$Latitude <- jitter(longdata$Latitude)
longdata$Longitude <- jitter(longdata$Longitude)
#long data lists sources and materials for bar plot (mulitple rows per cleanup)
  
widedata <- readRDS("widedata.rds")

widedata$Latitude <- jitter(widedata$Latitude)
widedata$Longitude <- jitter(widedata$Longitude)
#wide data lists all categories in one cleanup (so only 1 cleanup per row!)