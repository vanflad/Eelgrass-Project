library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(DT)
library(rgeos)
library(rgdal)
library(sp)
library(tidyr)

#reiterate input variables that are used in this file too
useShinyjs()

region_vars <- sort(unique(widedata$Regions))

district_vars <- sort(unique(widedata$ED_NAME))

widedata$FID <- as.character(widedata$FID)

longdata$FID <- as.character(longdata$FID)

function(input, output, session) {
  
##### Interactive Map #####
  
  ##### ShapefileMap Polygons! #####
  
  # Download Shapefile
  tmp <- tempdir()
  unzip("/Users/Vanessa/Desktop/2017-2018/FISH 507/Legacy/edsre2015.zip", exdir = tmp)
  BCDist <- readOGR(dsn = tmp, layer = "edsre2015", encoding = "UTF-8")
  
  #Project layer in WGS84
  BCDist <- spTransform(BCDist, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
  
  unzip("/Users/Vanessa/Desktop/2017-2018/FISH 507/Legacy/Regions.zip", exdir = tmp)
  BCReg <- readOGR(dsn = tmp, layer = "RP", encoding = "UTF-8")
  
  #Project layer in WGS84
  BCReg <- spTransform(BCReg, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
  BCReg$Regions<-c("North Coast B.C.","West Coast Vancouver Island","Inner Coast Vancouver Island","Southern Strait of Georgia")
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -124, lat = 52, zoom = 6)
  })
  
  
  ##### Reactive dataframes #####
  
  # Reactive expression returns set of cleanups that are in bounds right now (for table)
  trashInBoundsW <- reactive({
    if (is.null(input$map_bounds))
      return(widedata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    if (input$location=="Regions")
      subset(widedata,
             Latitude >= latRng[1] & Latitude <= latRng[2] &
               Longitude >= lngRng[1] & Longitude <= lngRng[2] &
               Year_ >= input$year[1] & Year_ <= input$year[2]
               & Regions %in% input$region)
    
    else
      if(input$edyn==TRUE)
        subset(widedata,
               Latitude >= latRng[1] & Latitude <= latRng[2] &
                 Longitude >= lngRng[1] & Longitude <= lngRng[2] &
                 Year_ >= input$year[1] & Year_ <= input$year[2] & ED_NAME %in% input$district)
    
    else
      subset(widedata,
             Latitude >= latRng[1] & Latitude <= latRng[2] &
               Longitude >= lngRng[1] & Longitude <= lngRng[2] &
               Year_ >= input$year[1] & Year_ <= input$year[2])
    
  }) #data points aren't disappearing/reactive right now for some reason...
  #but if we get polygons to be reactive then we can have static datapoints!
  
  # Reactive expression returns set of cleanups that are in bounds right now (for graphs)
  trashInBoundsL <- reactive({
    if (is.null(input$map_bounds))
      return(longdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    if (input$location=="Regions")
      subset(longdata,
             Latitude >= latRng[1] & Latitude <= latRng[2] &
               Longitude >= lngRng[1] & Longitude <= lngRng[2] &
               Year_ >= input$year[1] & Year_ <= input$year[2] & 
               Regions %in% input$region & !(Litter %in% input$litter))
    
    else
      if(input$edyn==TRUE)
        subset(longdata,
               Latitude >= latRng[1] & Latitude <= latRng[2] &
                 Longitude >= lngRng[1] & Longitude <= lngRng[2] &
                 Year_ >= input$year[1] & Year_ <= input$year[2] & 
                 ED_NAME %in% input$district & !(Litter %in% input$litter))
    
    else
      subset(longdata,
             Latitude >= latRng[1] & Latitude <= latRng[2] &
               Longitude >= lngRng[1] & Longitude <= lngRng[2] &
               Year_ >= input$year[1] & Year_ <= input$year[2] &
               !(Litter %in% input$litter))
    
  })
  
  #Reset district selections when district is unselected (UI stuff)
  #output$conditionalReset <- renderUI({
  #  if (input$location=="ED_NAME"){
  #    if(input$edyn==FALSE)
  #      reset("district")
  #    else
  #      checkboxGroupInput("district", "Choose Districts", district_vars, district_vars)
  #  } else NULL
  #})
  
  ##### Render Plots (for Map) #####
  
  #source profile graph
  output$sourceprofile <- renderPlot({
  
    if (nrow(trashInBoundsL()) == 0)
      return(NULL)
    else
    trashInBoundsL() %>%
      group_by(Source) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Source), stat="identity")+
      labs(x=NULL, y=NULL, title="Source of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size=12), title = element_text(size=12),
            legend.title = element_text(size=14),
            axis.text.x = element_blank())+
      scale_y_continuous(labels=scales::comma_format())+
      scale_fill_brewer(type = "qual", palette = "Set1")
    
  })
  
  #material profile graph
  output$materialprofile <- renderPlot({

    if (nrow(trashInBoundsL()) == 0)
      return(NULL)
    else
    trashInBoundsL() %>%
      group_by(Material) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Material), stat="identity")+
      labs(x=NULL, y=NULL, title="Material of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size=12), title = element_text(size=12),
            legend.title = element_text(size=14),
            axis.text.x = element_blank())+
      scale_y_continuous(labels=scales::comma_format())+
      scale_fill_brewer(type = "qual", palette = "Dark2")
    
  })
  
  ##### Stuff Cassandra added (double check later it's not repeated somewhere) #####
  
  observe({
    if (is.null(input$location)) {
      BCDist <- subset(BCDist,ED_NAME %in% longdata$ED_NAME)
      state_popup <- paste0("District: ", 
                            BCDist$ED_NAME)}
    
    else {
      BCDist <- subset(BCDist,ED_NAME %in% input$district)
      #Add pop up information
      state_popup <- paste0("District: ", 
                            BCDist$ED_NAME)}
    
    
    #Create map
    leafletProxy("map", data = BCDist) %>%
      clearShapes() %>% 
      #changed clearShapes to clear tiles instead
      addTiles() %>%
      addPolygons(color = "gray",
                  weight = 2, 
                  fillOpacity = 0.2,
                  fillColor = "orange",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = state_popup,
                  labelOptions = labelOptions(noHide = FALSE))
  })
  
  observe({
    BCReg <- subset(BCReg,Regions %in% input$region)
    #Add pop up information
    region_popup <- paste0("Region: ", 
                           BCReg$Regions)
    
    #Create map
    leafletProxy("map", data = BCReg) %>%
      clearShapes() %>% 
      #change from clearShapes
      addTiles() %>%
      addPolygons(color = "gray",
                  weight = 2, 
                  fillOpacity = 0.2,
                  fillColor = "orange",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = region_popup,
                  labelOptions = labelOptions(noHide = FALSE))
  })
  
  ##### SIZE/POP UP STUFF #####
  
  #add points
  observe({
    
    leafletProxy("map", data = widedata) %>%
      clearShapes() %>%
      #removeControl("map", layerId = ~FID) %>% 
      #removeShape(layerId = ~FID) %>% 
      #clearMarkers() %>% 
      addCircles(lng = ~Longitude, lat = ~Latitude, layerId=~FID,
                 stroke=TRUE, fillOpacity=0.5, radius = 200, color = "purple")
  }) 
  
  # Show a popup at the given location
  showTrashPopup <- function(FID, lat, lng) {
    selectedFID <- widedata[widedata$FID==FID,]
    
    content <- as.character(tagList(
      tags$h5("Nearest City:", selectedFID$Nearest_Ci),
      tags$strong(HTML(sprintf("District: %s", selectedFID$ED_NAME
      ))), tags$br(),
      tags$strong(HTML(sprintf("Region: %s", selectedFID$Regions
      ))),
      tags$br(),
      sprintf("Total Volunteers: %s", as.integer(selectedFID$Volunteers)), tags$br(),
      sprintf("Kg of trash collected: %s", as.integer(selectedFID$Kilograms)), tags$br(),
      sprintf("Km of shoreline cleaned: %s", as.integer(selectedFID$Kilometers))
    ))
    
    if(is.null(FID)){
      return(NULL)}
    else {
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = FID)}
    
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event)){
      return()}
    else{
    isolate({
      showTrashPopup(event$id, event$lat, event$lng)
    })
      }
  })
  
  ##### Summary Data Table (for Map) #####
  output$trashsummary <- DT::renderDataTable({
    
    viewtable <- reactive({trashInBoundsL() %>%
        group_by(FID) %>%
        summarise(Kilograms=first(Kilograms), Kilometers=first(Kilometers),
                  Volunteers=first(Volunteers)) %>%
        select(FID, Kilograms, Kilometers, Volunteers) %>%
        ungroup() %>% 
      summarise("Total Cleanups"= n(), "Kilograms collected"=sum(Kilograms),
                "Kilometers cleaned"=sum(Kilometers), "Total Volunteers"=sum(Volunteers)) %>%
      gather("Summary", "Results", "Total Cleanups":"Total Volunteers")})
    
    DT::datatable(viewtable(), options = list(dom = 't'), rownames = FALSE) %>%
      formatCurrency("Results",currency = "", interval = 3, mark = ",", digits = 0)
    
  })
  
  ##### ED_ShapefileMap Polygons! (Not in Cassandra's version - double check later) #####
  
  # Load packages
  #library(rgdal)
  
  # Download Shapefile
  
  #tmp <- tempdir()
  #url <- "https://catalogue.data.gov.bc.ca/dataset/9530a41d-6484-41e5-b694-acb76e212a58/resource/34eedf53-c60b-4237-bf6e-81228a51ab12/download/edsre2015.zip"
  #file <- basename(url)
  #download.file(url, file)
  #unzip(file, exdir = tmp)
  #unzip("edsre2015.zip", exdir = "/Users/Vanessa/Desktop/2017-2018/FISH 507/Legacy")
  #BCDist <- readOGR(dsn = tmp, layer = "edsre2015", encoding = "UTF-8")
  
  #Project layer in WGS84
  #BCDist <- spTransform(BCDist, CRS("+init=epsg:4326"))
  
  #BCDist@data[["ED_NAME"]] is how you access this part of the data!!!!! *****
  
  #filter out ED's not used!
  #BCdata <- as.data.frame(BCDist@data) %>%
  #  filter(ED_NAME %in% widedata$ED_NAME)
  # got this working but need to convert it back into a list to read the polygons... * confusing.
  
  #Add pop up information
  #state_popup <- paste0("District: ", 
  #                      BCDist$ED_NAME)
  
  #district_selected <- reactive({
  #  BCDist[substr(BCDist@data$ED_NAME, 1, 1)==substr(widedata$ED_NAME, 1, 1), ]
  #})
  #trying to make reactive polygons work a different way, still not working...
  
  #Create plain polygons of all districts
  #leafletProxy("map", data = BCDist) %>%
  #  addTiles() %>%
  #  addPolygons(color = "gray",
  #              weight = 2, 
  #              fillOpacity = 0.2,
  #              highlightOptions = highlightOptions(color = "black", weight = 2,
  #                                                  bringToFront = FALSE),
  #              label = state_popup,
  #              labelOptions = labelOptions(noHide = FALSE))
  
##### Comparison Tab #####
  
  #trying to get action button to jump to map set up
  observe({
    if (is.null(input$goto)){
      return()}
    else{
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      fid <- input$goto$fid
      lat <- input$goto$lat
      lng <- input$goto$lng
      showTrashPopup(fid, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })}
  })
  
  ##### Graphs #####
  
  #first reactive dataframe for graphs
  trashdataA <- reactive({
    if (input$locationA=="Regions")
      longdata %>%
      filter(Year_ %in% input$yearA, !(Litter %in% input$litterA),
             Regions == input$regionA)
    else
    longdata %>% 
      filter(Year_ %in% input$yearA, !(Litter %in% input$litterA),
             ED_NAME == input$districtA)
  })
  
  #first source profile graph
  output$sourceprofileA <- renderPlot({
    
    if (nrow(trashdataA()) == 0)
      return(NULL)
    else
    trashdataA() %>%
      group_by(Source) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Source), stat="identity")+
      labs(x=NULL, y=NULL, title="Source of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size=12), title = element_text(size=12),
            legend.title = element_text(size=14),
            axis.text.x = element_blank())+
      scale_y_continuous(labels=scales::comma_format())+
      scale_fill_brewer(type = "qual", palette = "Set1")
  })
  
  #first material profile graph
  output$materialprofileA <- renderPlot({
    
    if (nrow(trashdataA()) == 0)
      return(NULL)
    else
    trashdataA() %>%
      group_by(Material) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Material), stat="identity")+
      labs(x=NULL, y=NULL, title="Material of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size=12), title = element_text(size=12),
            legend.title = element_text(size=14),
            axis.text.x = element_blank())+
      scale_y_continuous(labels=scales::comma_format())+
      scale_fill_brewer(type = "qual", palette = "Dark2")
  })
  
  #second reactive data frame for graphs
  trashdataB <- reactive({
    if (input$locationB=="Regions")
    longdata %>%
      filter(Year_ %in% input$yearA, !(Litter %in% input$litterA),
             Regions == input$regionB)
    else
    longdata %>% 
      filter(Year_ %in% input$yearA, !(Litter %in% input$litterA),
             ED_NAME == input$districtB)
  })
  
  #second source profile graph
  output$sourceprofileB <- renderPlot({ 
    if(input$locationA== "Regions" & input$locationB=="Regions" & input$regionA==input$regionB){
      output$regiontext <- renderText("Sorry, please select a different region to compare!")
      reset("districttext")
      return(NULL)}
    else{
      if(input$locationA=="ED_NAME" & input$locationB=="ED_NAME" & input$districtA==input$districtB){
        output$districttext <- renderText("Sorry, please select a different district to compare!")
        reset("regiontext")
        return(NULL)}
      else{
        reset("districttext")
        reset("regiontext")
        trashdataB() %>%
          group_by(Source) %>%
          summarise(num=sum(Number)) %>%
          ggplot(aes(x=factor(""), y=num))+
          geom_bar(aes(fill=Source), stat="identity")+
          labs(x=NULL, y=NULL, title="Source of Litter (by count)")+
          theme(legend.position = "right", axis.title.x = element_blank(),
                axis.text.y = element_text(size = 12),
                legend.text = element_text(size=12), title = element_text(size=12),
                legend.title = element_text(size=14),
                axis.text.x = element_blank())+
          scale_y_continuous(labels=scales::comma_format())+
          scale_fill_brewer(type = "qual", palette = "Set1")}}
  })
  
  #second material profile graph
  output$materialprofileB <- renderPlot({
    if(input$locationA== "Regions" & input$locationB=="Regions" & input$regionA==input$regionB){
      output$regiontext <- renderText("Sorry, please select a different region to compare!")
      reset("districttext")
      return(NULL)}
    else{
      if(input$locationA=="ED_NAME" & input$locationB=="ED_NAME" & input$districtA==input$districtB){
        output$districttext <- renderText("Sorry, please select a different district to compare!")
        reset("regiontext")
        return(NULL)}
      else{
        reset("districttext")
        reset("regiontext")
    trashdataB() %>%
      group_by(Material) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Material), stat="identity")+
      labs(x=NULL, y=NULL, title="Material of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size=12), title = element_text(size=12),
            legend.title = element_text(size=14),
            axis.text.x = element_blank())+
      scale_y_continuous(labels=scales::comma_format())+
      scale_fill_brewer(type = "qual", palette = "Dark2")}}
  })
  
  ##### Tables #####
  
  #first reactive dataframe for summary table
  tabledataA <- reactive({
    
    (if (input$locationA=="Regions"){
      widedata %>%
      filter(Year_ %in% input$yearA,
             Regions == input$regionA) %>%
        summarise("Total Cleanups"= n(), "Kilograms collected"=sum(Kilograms),
                  "Kilometers cleaned"=sum(Kilometers), "Total Volunteers"=sum(Volunteers)) %>%
        gather("Summary", "Results", "Total Cleanups":"Total Volunteers")}
    else{
      widedata %>% 
      filter(Year_ %in% input$yearA,
             ED_NAME == input$districtA) %>%
        summarise("Total Cleanups"= n(), "Kilograms collected"=sum(Kilograms),
                  "Kilometers cleaned"=sum(Kilometers), "Total Volunteers"=sum(Volunteers)) %>%
        gather("Summary", "Results", "Total Cleanups":"Total Volunteers")})
    
  })
  
  #second reactive dataframe for summary table
  tabledataB <- reactive({
    
    (if (input$locationB=="Regions"){
    widedata %>%
      filter(Year_ %in% input$yearA,
             Regions == input$regionB) %>%
        summarise("Total Cleanups"= n(), "Kilograms collected"=sum(Kilograms),
                  "Kilometers cleaned"=sum(Kilometers), "Total Volunteers"=sum(Volunteers)) %>%
        gather("Summary", "Results", "Total Cleanups":"Total Volunteers")}
    else{
      widedata %>% 
        filter(Year_ %in% input$yearA,
               ED_NAME == input$districtB) %>%
        summarise("Total Cleanups"= n(), "Kilograms collected"=sum(Kilograms),
                  "Kilometers cleaned"=sum(Kilometers), "Total Volunteers"=sum(Volunteers)) %>%
        gather("Summary", "Results", "Total Cleanups":"Total Volunteers")})
  })
  
  #first summary table
  output$trashtableA <- DT::renderDataTable({
    #  mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <- DT::dataTableAjax(session, df)
    #trying to get action button to work here
    
    DT::datatable(tabledataA(), options = list(dom = 't'), rownames = FALSE) %>%
      formatCurrency("Results",currency = "", interval = 3, mark = ",", digits = 0)
    #, options = list(ajax = list(url = action)), escape = FALSE)
    #more action button code to implement later!
  })
  
  #second summary table
  output$trashtableB <- DT::renderDataTable({
    
    #top3B <- trashdataB() %>%
    #  top_n(Litter, 3, Number)
    
    #tableB <- 
      
      #need to look up how I did this for that assignment on candy dataset...
    #to add in the top 3 or 5 or whatever number of items for each region/district!
    
    DT::datatable(tabledataB(), options = list(dom = 't'), rownames = FALSE) %>%
      formatCurrency("Results",currency = "", interval = 3, mark = ",", digits = 0)
  })
  }

#next steps:
#region polygons (need Cassandra's shapefile!)
#colored polygons (need Cassandra's help! too confusing!)
#toggle polygons on and off when selected and deselected
#show reg/ed polygons depending on type of location chosen
#add action button to jump to map polygons somehow
#add top litter items to the tables (~ easy thing to fix next *)
#make moveable panels less transparent (find out how!)
#figure out how to make graphs look best (fluid page?)
#finalize what should be in the pop-ups (dates of cleanups?)
#double check nearest city stuff if we want to use it
#add text "litter in cleanups not on shores" or whatev?
#add a link to lead or join a cleanup near you
#possibly add fancy geolocalize me option
#possibly add a search bar (for nearest city stuff?)
#figure out how to get consistent colors when categories change
#make sure the code is streamlined as best as possible to avoid slow responses of app!
#figure out if we want dates of cleanups somewhere in there at all