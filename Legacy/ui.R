library(leaflet)
library(shinyjs)
library(DT)
#
##### Variables for Inputs #####

location_vars <- c(
  "Region" = "Regions",
  "District" = "ED_NAME"
  )

litter_vars <- c(
  "Appliances" = "Appliances",
  "Paper Bags" = "Bags__Pape",
  "Plastic Bags" = "Bags__Plas",
  "Balloons" = "Balloons",
  "Batteries" = "Batteries",
  "Plastic Bottles (2L or less)" = "Beverage_B",
  "Beverage Cans" = "Beverage_C",
  "Metal Bottle Caps" = "Bottle_C_1",
  "Plastic Bottle Caps" = "Bottle_Cap",
  "Building Materials" = "Building_M",
  "Buoys and Floats" = "Buoys_Floa",
  "Cigar Tips" = "Cigar_Tips",
  "Cigarette Lighters" = "Cigarette_",
  "Cigarettes and Filters" = "Cigarettes",
  "Clothing" = "Clothing__",
  "Condoms" = "Condoms",
  "Foam Cups and Plates" = "Cups_and_1",
  "Plastic Cups and Plates" = "Cups_and_2",
  "Paper Cups and Plates" = "Cups_and_P",
  "Diapers" = "Diapers",
  "6-Pack Holders" = "F6_Pack_Ho",
  "Fireworks" = "Fireworks",
  "Fishing Line" = "Fishing_Li",
  "Fishing Lures and Light Sticks" = "Fishing_Lu",
  "Fishing Nets" = "Fishing_Ne",
  "Foam Pieces" = "Foam_Piece",
  "Food Wrappers and Containers" = "Food_Wrapp",
  "Forks, Knives and Spoons" = "Forks__Kni",
  "Glass Beverage Bottles" = "Glass_Beve",
  "Glass Pieces" = "Glass_Piec",
  "Plastic Lids" = "Lids__Plas",
  "Other Plastic/Foam Packaging" = "Other_Pl_1",
  "Other Plastic Bottles (oil, bleach, etc.)" = "Other_Pl_2",
  "Other Plastic Bags" = "Other_plas",
  "Plastic Pieces" = "Plastic_Pi",
  "Rope" = "Rope",
  "Strapping" = "Strapping",
  "Straws and Stirrers" = "Straws__St",
  "Syringes" = "Syringes",
  "Foam Takeout Containers" = "Takeout_1",
  "Plastic Takeout Containers" = "Takeout_Co",
  "Tampons and Applicators" = "Tampons_Ta",
  "Tires" = "Tires",
  "Tobacco Packaging" = "Tobacco_Pa",
  "Toys" = "Toys"
)

region_vars <- sort(unique(widedata$Regions))
  
district_vars <- sort(unique(widedata$ED_NAME))

#year_vars <- c("2013", "2014", "2015", "2016")

year_vars <- sort(unique(widedata$Year_))

##### Map Display #####

useShinyjs()

navbarPage("Towards Cleaner Shores", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Litter Explorer
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 320, bottom = "auto",
                                      width = 320, height = "auto", style = "overflow-y:scroll; max-height: 600px",
                          h2("Litter Explorer"),
                          selectInput("location", "Region or District?", choices = location_vars, selected = "Regions"),

                          sliderInput("year", "Select Years", min = 2013, max = 2016, value = range(year_vars), step = 1, sep = "", dragRange = FALSE),
         
                          selectInput("litter", "Remove Litter Items to Simulate Prevention", choices = litter_vars, multiple = TRUE),
             
                          conditionalPanel("input.location=='Regions'",
                                           checkboxGroupInput("region", "Choose Regions", choices = region_vars, selected = region_vars)),
          
                          conditionalPanel("input.location=='ED_NAME'",
                                           checkboxGroupInput("district", "Choose Districts", choices = district_vars, selected = district_vars)),
                                           #radioButtons("edyn", "Choose certain districts?", choices = list("Yes"=TRUE, "No"=FALSE), selected = FALSE)),
                          uiOutput("conditionalReset")
              
                        ),
                        
                        # Litter Results
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 300, height = "auto",
                                      
                                      h2("Litter Results"),
                                      
                                      plotOutput("sourceprofile", height = 200),
                                      plotOutput("materialprofile", height = 200),
                                      DT::dataTableOutput("trashsummary")
                        ),
                        
                        tags$div(id="cite",
                                 'Data provided by ', tags$em('The Great Canadian Shoreline Cleanup;'), ' App created by Fladmark, V., Konecny, C. and De la Puente, S.'
                        )
                    )
           ),
           
##### Comparison Tab #####
           
           tabPanel("Comparisons",
                    
                    # Filtering options
                    fluidRow(
                      column(3,
                             sliderInput("yearA", "Select Years", 2013, 2016, c(2013, 2016), 1, sep = "")),
                           
                      column(8,
                             selectInput("litterA", "Remove Litter Items to Simulate Prevention", litter_vars, multiple = TRUE)
                             
                      )), 
                    
                    # left hand side graphs and summary table
                    fluidRow(  
                      column(4,
                             selectInput("locationA", "Region or District?", location_vars, "Regions"),
                           
                             conditionalPanel("input.locationA=='Regions'",
                                              selectInput("regionA", "Choose Region", region_vars, "Southern Strait of Georgia")),
                          
                             conditionalPanel("input.locationA=='ED_NAME'",
                                              selectInput("districtA", "Choose District", district_vars, "Vancouver-West End")),
                  
                             plotOutput("sourceprofileA", height = 200),
                             plotOutput("materialprofileA", height = 200),
                             DT::dataTableOutput("trashtableA")
                      ),
                      
                      # right hand side graphs and summary table
                      column(4,
                             selectInput("locationB", "Region or District?", location_vars, "Regions"),
                          
                             conditionalPanel("input.locationB=='Regions'",
                                              selectInput("regionB", "Choose Region", region_vars, "North Coast B.C.")),
                         
                             conditionalPanel("input.locationB=='ED_NAME'",
                                              selectInput("districtB", "Choose District", district_vars, "North Island")),
                             conditionalPanel("input.locationA=='Regions' & input.locationB=='Regions' & input.regionA==input.regionB",
                                              textOutput("regiontext")),
                             conditionalPanel("input.locationA=='ED_NAME' & input.locationB=='ED_NAME' & input.districtA==input.districtB",
                                              textOutput("districttext")),
                       
                             plotOutput("sourceprofileB", height = 200),
                             plotOutput("materialprofileB", height = 200),
                             DT::dataTableOutput("trashtableB"),
                             offset = 1)
                      )
           ), 
           
           conditionalPanel("false", icon("crosshair"))
)
