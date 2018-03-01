library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(DT)

useShinyjs()

region_vars <- sort(unique(widedata$Regions))

district_vars <- sort(unique(widedata$ED_NAME))

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -127.5, lat = 52, zoom = 6)
  })
  
  # Reactive expression returns set of cleanups that are in bounds right now
  trashInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(longdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    if (input$location=="Regions")
      subset(longdata,
           Latitude >= latRng[1] & Latitude <= latRng[2] &
             Longitude >= lngRng[1] & Longitude <= lngRng[2] &
             Year_ %in% input$year & !(Litter %in% input$litter) &
             Regions %in% input$region)
    
    else
      if(input$edyn==TRUE)
        subset(longdata,
             Latitude >= latRng[1] & Latitude <= latRng[2] &
               Longitude >= lngRng[1] & Longitude <= lngRng[2] &
               Year_ %in% input$year & !(Litter %in% input$litter) &
               ED_NAME %in% input$district)
  
      else
        subset(longdata,
               Latitude >= latRng[1] & Latitude <= latRng[2] &
                 Longitude >= lngRng[1] & Longitude <= lngRng[2] &
                 Year_ %in% input$year & !(Litter %in% input$litter))
      
  })
  
  output$conditionalReset <- renderUI({
    if (input$location=="ED_NAME"){
      if(input$edyn==FALSE)
        reset("district")
      else
        checkboxGroupInput("district", "Choose Districts", district_vars, district_vars)
    } else NULL
  })
  
  output$sourceprofile <- renderPlot({
  
    if (nrow(trashInBounds()) == 0)
      return(NULL)
    else
    trashInBounds() %>%
      group_by(Source) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Source), stat="identity")+
      labs(x=NULL, y=NULL, title="Source of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            #axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            #legend.text = element_text(size=16), title = element_text(size=16),
            #legend.title = element_text(size=16),
            axis.text.x = element_blank())+
      scale_y_continuous(labels=scales::comma_format())+
      scale_fill_brewer(type = "qual", palette = "Set1")
    
  })
  
  output$materialprofile <- renderPlot({

    if (nrow(trashInBounds()) == 0)
      return(NULL)
    else
    trashInBounds() %>%
      group_by(Material) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Material), stat="identity")+
      labs(x=NULL, y=NULL, title="Material of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            #axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            #legend.text = element_text(size=16), title = element_text(size=16),
            #legend.title = element_text(size=16),
            axis.text.x = element_blank())+
      scale_y_continuous(labels=scales::comma_format())+
      scale_fill_brewer(type = "qual", palette = "Dark2")
    
  })

  ##### ED_ShapefileMap Polygons! #####
  
  # Load packages
  library(rgdal)
  
  # Download Shapefile
  
  #tmp <- tempdir()
  #url <- "https://catalogue.data.gov.bc.ca/dataset/9530a41d-6484-41e5-b694-acb76e212a58/resource/34eedf53-c60b-4237-bf6e-81228a51ab12/download/edsre2015.zip"
  #file <- basename(url)
  #download.file(url, file)
  #unzip(file, exdir = tmp)
  unzip("edsre2015.zip", exdir = "/Users/Vanessa/Desktop/2017-2018/FISH 507/Legacy")
  BCDist <- readOGR(dsn = tmp, layer = "edsre2015", encoding = "UTF-8")
  
  #Project layer in WGS84
  BCDist <- spTransform(BCDist, CRS("+init=epsg:4326"))
  
  #Add pop up information
  state_popup <- paste0("District: ", 
                        BCDist$ED_NAME)
  #Create map
  leafletProxy("map", data = BCDist) %>%
    addTiles() %>%
    addPolygons(color = "gray",
                weight = 2, 
                fillOpacity = 0.2,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = FALSE),
                label = state_popup,
                labelOptions = labelOptions(noHide = FALSE))
  #just recreate this once we have the region shapefile as well!
  
  ##### SIZE/POP UP STUFF #####
  
  observe({
    
    leafletProxy("map", data = trashInBounds()) %>%
      #clearShapes() %>%
      clearMarkers() %>% 
      addCircles(lng = ~Longitude, lat = ~Latitude, layerId=~FID,
                 stroke=TRUE, fillOpacity=0.5, radius = 200, color = "purple")
  }) 

  # Show a popup at the given location
  showTrashPopup <- function(FID, lat, lng) {
    selectedFID <- widedata[widedata$FID==FID,]
      
      #widedata %>%
      #group_by(Latitude, Longitude) %>%
      #subset(Latitude==lat & Longitude==lng)
    
    content <- as.character(tagList(
      tags$h4("Nearest City:", selectedFID$Nearest_Ci),
      tags$strong(HTML(sprintf("%s, %s", selectedFID$ED_NAME, selectedFID$Regions
      ))), tags$br(),
      sprintf("Number of volunteers: %s", as.integer(selectedFID$Volunteers)), tags$br(),
      sprintf("Kg of trash cleaned: %s", as.integer(selectedFID$Kilograms)), tags$br(),
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
      
      output$basictext <- renderText("Summary of Cleanups in View")
      
      output$barplotinfo <- renderText({ "Current Area:"
        outputArgs = widedata %>%
          filter(FID==event$id) %>%
          print(as.character(Nearest_Ci))
      })
      
      output$sourceclicked <- renderPlot({
        
        longdata %>%
          filter(FID==event$id) %>% 
          group_by(Source) %>%
          summarise(num=sum(Number)) %>%
          ggplot(aes(x=factor(""), y=num))+
          geom_bar(aes(fill=Source), stat="identity")+
          labs(x=NULL, y=NULL, title="Source of Litter (by count)")+
          theme(legend.position = "right", axis.title.x = element_blank(),
                #axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
                #legend.text = element_text(size=16), title = element_text(size=16),
                #legend.title = element_text(size=16),
                axis.text.x = element_blank())+
          scale_y_continuous(labels=scales::comma_format())+
          scale_fill_brewer(type = "qual", palette = "Set1")
        
      })
      
      output$materialclicked <- renderPlot({
        
        longdata %>%
          filter(FID==event$id) %>% 
          group_by(Material) %>%
          summarise(num=sum(Number)) %>%
          ggplot(aes(x=factor(""), y=num))+
          geom_bar(aes(fill=Material), stat="identity")+
          labs(x=NULL, y=NULL, title="Material of Litter (by count)")+
          theme(legend.position = "right", axis.title.x = element_blank(),
                #axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
                #legend.text = element_text(size=16), title = element_text(size=16),
                #legend.title = element_text(size=16),
                axis.text.x = element_blank())+
          scale_y_continuous(labels=scales::comma_format())+
          scale_fill_brewer(type = "qual", palette = "Dark2")
        
      })
      }
  })
  
  ## Data Explorer ###########################################
  
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
            #axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            #legend.text = element_text(size=16), title = element_text(size=16),
            #legend.title = element_text(size=16),
            axis.text.x = element_blank())+
      scale_y_continuous(labels=scales::comma_format())+
      scale_fill_brewer(type = "qual", palette = "Set1")
  })
  
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
            #axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            #legend.text = element_text(size=16), title = element_text(size=16),
            #legend.title = element_text(size=16),
            axis.text.x = element_blank())+
      scale_y_continuous(labels=scales::comma_format())+
      scale_fill_brewer(type = "qual", palette = "Dark2")
  })
  
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
            #axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            #legend.text = element_text(size=16), title = element_text(size=16),
            #legend.title = element_text(size=16),
            axis.text.x = element_blank())+
          scale_y_continuous(labels=scales::comma_format())+
          scale_fill_brewer(type = "qual", palette = "Set1")}}
  })
  
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
            #axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            #legend.text = element_text(size=16), title = element_text(size=16),
            #legend.title = element_text(size=16),
            axis.text.x = element_blank())+
      scale_y_continuous(labels=scales::comma_format())+
      scale_fill_brewer(type = "qual", palette = "Dark2")}}
  })
  
  tabledataA <- reactive({
    
    (if (input$locationA=="Regions"){
      widedata %>%
      filter(Year_ %in% input$yearA,
             Regions == input$regionA) %>%
      summarise("Number of cleanups"= n(), "Number of Kg collected"=sum(Kilograms),
                "Number of Km cleaned"=sum(Kilometers), "Number of volunteers"=sum(Volunteers)) %>%
        gather("Summary Statistics", "Results", "Number of cleanups":"Number of volunteers")}
    else{
      widedata %>% 
      filter(Year_ %in% input$yearA,
             ED_NAME == input$districtA) %>%
      summarise("Number of cleanups"= n(), "Number of Kg collected"=sum(Kilograms),
                "Number of Km cleaned"=sum(Kilometers), "Number of volunteers"=sum(Volunteers)) %>%
        gather("Summary Statistics", "Results", "Number of cleanups":"Number of volunteers")})
    
    #cbind(A, B) #%>%
    #  mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '" data-fid="', FID, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <- DT::dataTableAjax(session, df)
    
  }) # NOTE removed !(Litter %in% input$litterA) because it's the widedata dataframe (no Litter)
  #the litter filtering will be done for the trashdataA and B, which is where we'll get top items from.
  
  tabledataB <- reactive({
    
    (if (input$locationB=="Regions"){
    widedata %>%
      filter(Year_ %in% input$yearA,
             Regions == input$regionB) %>%
      summarise("Number of cleanups"= n(), "Number of Kg collected"=sum(Kilograms),
                "Number of Km cleaned"=sum(Kilometers), "Number of volunteers"=sum(Volunteers)) %>%
      gather("Summary Statistics", "Results", "Number of cleanups":"Number of volunteers")}
    else{
      widedata %>% 
        filter(Year_ %in% input$yearA,
               ED_NAME == input$districtB) %>%
        summarise("Number of cleanups"= n(), "Number of Kg collected"=sum(Kilograms),
                  "Number of Km cleaned"=sum(Kilometers), "Number of volunteers"=sum(Volunteers)) %>%
        gather("Summary Statistics", "Results", "Number of cleanups":"Number of volunteers")})
  })
  
  output$trashtableA <- DT::renderDataTable({
    #  mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <- DT::dataTableAjax(session, df)
    
    DT::datatable(tabledataA(), options = list(dom = 't'))#, options = list(ajax = list(url = action)), escape = FALSE)
    #merge two filtered, summarized and reformatted (wide to long) dataframes for table!
    #is it important that we have a date range or some dates listed? also in popups maybe?
    #then merge with the two trashdataA and B dataframes to incorporate common litter items
    #then clean and rearrange dataframes as necessary to make this table nice and pretty!
    #then add the action button to jump to that district and/or region (reg polyg to come!)
  })
  
  output$trashtableB <- DT::renderDataTable({
    DT::datatable(tabledataB(), options = list(dom = 't'))    
  })
  
  } #to do next: seperate tables! done! Now: add a comma to the numbers!