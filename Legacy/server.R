library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -127, lat = 51.5, zoom = 7)
  })#try diff lat longs if not quite right!
  
  # A reactive expression that returns the set of cleanups that are
  # in bounds right now
  trashInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(longdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    #reactive to cleanups in bounds
    
      subset(longdata,
           Latitude >= latRng[1] & Latitude <= latRng[2] &
             Longitude >= lngRng[1] & Longitude <= lngRng[2] &
             Year_ %in% input$year & Litter %in% input$litter &
             Regions %in% input$region & ED_NAME %in% input$district)
  }) #see if any issues arise here, reactive expressions are tricky!!
  #changed from region | ED_NAME to region & ED_NAME to see if that helps it work better?
  
  output$sourceprofile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(trashInBounds()) == 0)
      return(NULL)
    
    trashInBounds() %>%
      group_by(Source) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Source), stat="identity")+
      labs(x=NULL, y=NULL, title="Source of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            legend.text = element_text(size=16), title = element_text(size=16),
            legend.title = element_text(size=16), axis.text.x = element_blank())+
      scale_fill_brewer(type = "qual", palette = "Set1")
    
  })
  
  output$materialprofile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(trashInBounds()) == 0)
      return(NULL)
    
    trashInBounds() %>%
      group_by(Material) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Material), stat="identity")+
      labs(x=NULL, y=NULL, title="Material of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            legend.text = element_text(size=16), title = element_text(size=16),
            legend.title = element_text(size=16), axis.text.x = element_blank())+
      scale_fill_brewer(type = "qual", palette = "Dark2")
    
  })
  #also include the trash profile bar plot for clicked on cleanup (figure out how later!*)

  ##### IRRELEVANT COLOR AND SIZE STUFF????? #####
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    leafletProxy("map", data = widedata) %>%
      clearShapes() %>%
      addCircles(~Longitude, ~Latitude, layerId=~FID,
                 stroke=FALSE, fillOpacity=0.7, radius = 500, color = "dark purple")
  }) # I don't know, work out later how we might incorporate this shit here...

  # Show a popup at the given location
  showTrashPopup <- function(FID, Latitude, Longitude) {
    selectedFID <- widedata[widedata$FID == FID,]
    #figure out how to select individual cleanups or group together by city???
    
    content <- as.character(tagList(
      tags$h4("Nearest City:", as.character(widedata$Nearest_Ci)),
      #change to region name or ED name or city or something here instead?????
      tags$strong(HTML(sprintf("%s, %s",
                               selectedFID$ED_NAME, selectedFID$Region
      #change to region name or ED name or city or something here instead?????
      ))), tags$br(),
      sprintf("Number of volunteers: %s", as.integer(selectedFID$Volunteers)), tags$br(),
      sprintf("Kg of trash cleaned: %s", as.integer(selectedFID$Kilograms)), tags$br(),
      sprintf("Km of shoreline cleaned: %s", as.integer(selectedFID$Kilometers))
    )) #change to # volunteers, # cleanups(?), # km, # kg, top 3 or 5 items?
    leafletProxy("map") %>% addPopups(Longitude, Latitude, content, layerId = FID)
  } #change layerId to whatever is relevant here, region? ED? whatever it is.
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showTrashPopup(event$id, event$Latitude, event$Longitude)
    })
  })
  #figure out how to include a bar chart for the selected popup in sidebar *
  
  ## Data Explorer ###########################################
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      FID <- input$goto$FID
      Latitude <- input$goto$Latitude
      Longitude <- input$goto$Longitude
      showTrashPopup(FID, Latitude, Longitude)
      map %>% fitBounds(Longitude - dist, Latitude - dist, Longitude + dist, Latitude + dist)
    })
  }) # this may cause trouble with the cleanup ID vs Region/district business
  #it does cause trouble! hash out for now to implement rough draft then find out how to fix!
  
  trashdataA <- reactive({
    if (input$locationA=="Region")
      longdata %>%
      filter(Year_ %in% input$yearA, Litter %in% input$litterA,
             Region == input$regionA)
    
    longdata %>% 
      filter(Year_ %in% input$yearA, Litter %in% input$litterA,
             ED_NAME == input$districtA)
  })
  #reactive dataframe here for filtered and selected option for A
  #if it doesn't work try getting rid of the "if" statement and switch to "Region == input$regionA | ED_NAME == input$districtA"?????
  
  output$sourceprofileA <- renderPlot({
    # If no cleanups are in chosen dataset, don't plot!
    if (nrow(trashdataA()) == 0)
      return(NULL)
    
    trashdataA() %>%
      group_by(Source) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Source), stat="identity")+
      labs(x=NULL, y=NULL, title="Source of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            legend.text = element_text(size=16), title = element_text(size=16),
            legend.title = element_text(size=16), axis.text.x = element_blank())+
      scale_fill_brewer(type = "qual", palette = "Set1")
  }) #resume here, try to figure out how to have a reactive expression and bar plots and action items and descriptions (table?)
  
  output$materialprofileA <- renderPlot({
    # If no cleanups are in chosen dataset, don't plot!
    if (nrow(trashdataA()) == 0)
      return(NULL)
    
    trashdataA() %>%
      group_by(Material) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Material), stat="identity")+
      labs(x=NULL, y=NULL, title="Material of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            legend.text = element_text(size=16), title = element_text(size=16),
            legend.title = element_text(size=16), axis.text.x = element_blank())+
      scale_fill_brewer(type = "qual", palette = "Dark2")
  })
  
  trashdataB <- reactive({
    if (input$locationB=="Region")
    longdata %>%
      filter(Year_ %in% input$yearA, Litter %in% input$litterA,
             Region == input$regionB)
    
    longdata %>% 
      filter(Year_ %in% input$yearA, Litter %in% input$litterA,
             ED_NAME == input$districtB)
  })
  #reactive dataframe here for filtered and selected option for B
  #if it doesn't work try getting rid of the "if" statement and switch to "Region == input$regionB | ED_NAME == input$districtB"?????
  
  output$sourceprofileB <- renderPlot({ 
    # If no cleanups are in chosen dataset, don't plot!
    if (nrow(trashdataB()) == 0)
      return(NULL)
    
    trashdataB() %>%
      group_by(Source) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Source), stat="identity")+
      labs(x=NULL, y=NULL, title="Source of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            legend.text = element_text(size=16), title = element_text(size=16),
            legend.title = element_text(size=16), axis.text.x = element_blank())+
      scale_fill_brewer(type = "qual", palette = "Set1")
  }) #resume here, try to figure out how to have a reactive expression and bar plots and action items and descriptions (table?)
  
  output$materialprofileB <- renderPlot({
    # If no cleanups are in chosen dataset, don't plot!
    if (nrow(trashdataB()) == 0)
      return(NULL)
    
    trashdataB() %>%
      group_by(Material) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(x=factor(""), y=num))+
      geom_bar(aes(fill=Material), stat="identity")+
      labs(x=NULL, y=NULL, title="Material of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_blank(),
            axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            legend.text = element_text(size=16), title = element_text(size=16),
            legend.title = element_text(size=16), axis.text.x = element_blank())+
      scale_fill_brewer(type = "qual", palette = "Dark2")
  })

  }