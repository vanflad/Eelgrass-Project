library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)

# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
widedata <- widedata[order(widedata$Region, decreasing = TRUE),]
# in this case, we'll order by region so SSOG is first, NCBC is drawn last!

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -123, lat = 49, zoom = 4)
  })#try diff lat longs if not quite right!
  
  # A reactive expression that returns the set of zips that are
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
             Longitude >= lngRng[1] & Longitude <= lngRng[2])
  })
  
  output$sourceprofile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(trashInBounds()) == 0)
      return(NULL)
    
    gar <- trash %>%
      group_by(Nearest_Ci, Region, Litter, Source, Material) %>%
      summarise(num=sum(Number))
    
    trashInBounds() %>%
      group_by(Nearest_Ci, Region, Litter, Source, Material) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(Region, num))+
      geom_bar(aes(fill=Source), position = "fill", stat="identity")+
      labs(x="Region", y=NULL, title="Source of Litter (by count)")+
      theme(legend.position = "right", axis.title.x = element_text(size=14),
            axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            legend.text = element_text(size=16), title = element_text(size=18),
            legend.title = element_text(size=16), axis.text.x = element_text(size=14))+
      scale_y_continuous(labels=scales::unit_format("%", 100))+
      scale_fill_manual(values = brewer.pal(n=4, "Set1"))
    
  })
  
  output$materialprofile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(trashInBounds()) == 0)
      return(NULL)
    
    trashInBounds() %>%
      group_by(Nearest_Ci, Region, Litter, Source, Material) %>%
      summarise(num=sum(Number)) %>%
      ggplot(aes(Region, num))+
      geom_bar(aes(fill=Material), position = "fill", stat="identity")+
      labs(x="Region", y=NULL, title="Material of Litter (by count)")+
      scale_y_continuous(labels=scales::unit_format("%", 100))+
      scale_fill_manual(values = brewer.pal(n=2, "Dark2"))+
      theme(legend.position = "right", axis.title.x = element_text(size=14),
            axis.text.y = element_text(size = 16), axis.title = element_text(size=14),
            legend.text = element_text(size=16), title = element_text(size=18),
            legend.title = element_text(size=16), axis.text.x = element_text(size=14))
    
  }) #change this to the trash profile bar plot!
  #also include the trash profile bar plot for selected (clicked) cleanup
  #And/or (?) change it to a line graph showing changes over time?????
  
  ##### IRRELEVANT COLOR AND SIZE STUFF????? #####
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    colorData <- widedata[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    
      radius <- widedata[[sizeBy]] / max(widedata[[sizeBy]]) * 30000
      #maybe use the size stuff to make big circles for many cleanups?????
    
    leafletProxy("map", data = widedata) %>%
      clearShapes() %>%
      addCircles(~Longitude, ~Latitude, radius=radius, layerId=~ED_NAME,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  }) # I don't know, work out later how we might incorporate this shit here...
  
  # Show a popup at the given location
  showTrashPopup <- function(ED_NAME, Latitude, Longitude) {
    selectedED <- widedata[widedata$ED_NAME == ED_NAME,]
    #figure out how to select individual cleanups or group together by city???
    
    content <- as.character(tagList(
      tags$h4("Nearest City:", as.character(widedata$Nearest_Ci)),
      #change to region name or ED name or city or something here instead?????
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedED$ED_NAME, selectedED$Region
      #change to region name or ED name or city or something here instead?????
      ))), tags$br(),
      sprintf("Number of volunteers: %s", as.integer(selectedED$Volunteers)), tags$br(),
      sprintf("Kg of trash cleaned: %s%%", as.integer(selectedED$Kilograms)), tags$br(),
      sprintf("Km of shoreline cleaned: %s", as.integer(selectedED$Kilometers))
    )) #change to # volunteers, # cleanups(?), # km, # kg, top 3 or 5 items?
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = ED_NAME)
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
    ED_NAME <- if (is.null(input$Region)) character(0) else {
      filter(widedata, Region %in% input$Region) %>%
        `$`('ED_NAME') %>%
        unique() %>%
        sort()
    }
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      Region <- input$goto$Region
      ED_NAME <- input$goto$ED_NAME
      Latitude <- input$goto$Latitude
      Longitude <- input$goto$Longitude
      showTrashPopup(ED_NAME, Latitude, Longitude)
      map %>% fitBounds(Longitude - dist, Latitude - dist, Longitude + dist, Latitude + dist)
    })
  })
  
  output$trashtable <- DT::renderDataTable({
    df <- widedata %>%
      filter(
        is.null(input$Region) | Region %in% input$Region,
        is.null(input$ED_NAME) | ED_NAME %in% input$ED_NAME
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
} #change this part to the comparison idea with two bar charts and tables *
#might need to sort out lat/long namings and ED_NAME/Region namings (follow superzip?)