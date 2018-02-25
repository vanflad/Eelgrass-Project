library(leaflet)

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
  "Takeout Containers" = "Takeout_1",
  "Takeout Containers" = "Takeout_Co",
  "Tampons and Applicators" = "Tampons_Ta",
  "Tires" = "Tires",
  "Tobacco Packaging" = "Tobacco_Pa",
  "Toys" = "Toys"
)
#I need team help selecting which categories to include *
#and which to group together to decrease the large number!
#maybe merge any categories separated by material type?
#Also: figure out how to rearrange by dominant items not alphabetical? *
#Even if it means rearranging manually, it shouldn't be alphabetical.
#and definitely need Cassandra's help to include polygons in here...
#also ask team if we should simplify our material categories?????

region_vars <- sort(unique(widedata$Regions))
  
district_vars <- sort(unique(widedata$ED_NAME))
#Update with relevant datafile name!

year_vars <- c("2013", "2014", "2015", "2016")

navbarPage("Towards Cleaner Shores", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                          h2("Litter Explorer"),
                          selectInput("location", "Region or District?", location_vars, selected = "Regions"),
                          #view polygons and breakdowns of data and potential filtering by 4 regions or 34 districts
                          sliderInput("year", "Select Years", 2013, 2016, c(2013, 2016), 1, sep = ""),
                          #give slider input for choosing 2013-2016, 2014-2015, single year only, 3 years, etc.
                          conditionalPanel("input.location=='Regions'",
                                           checkboxGroupInput("region", "Choose Regions", region_vars, region_vars)),
                          #if region is selected give user an option to select/deselect from 4 options
                          conditionalPanel("input.location=='ED_NAME'",
                                           radioButtons("edyn", "Choose certain districts?", c("Yes", "No"), "No")),
                          conditionalPanel("input.location=='ED_NAME' & input.edyn=='Yes'",
                                           checkboxGroupInput("district", "Choose Districts", district_vars, district_vars)),
                          #if district is selected give user an option to select/deselect from 34 options
                          selectInput("litter", "Remove Litter Items to Simulate Prevention", litter_vars, multiple = TRUE)
                          #give prompt to see effect of changes in policy/behaviour by "removing" litter
                        ),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 350, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Litter Results"),
                                      
                                      plotOutput("sourceprofile", height = 200),
                                      plotOutput("materialprofile", height = 200)
                                      #change these to those trash profile bar charts!
                        ),
                        
                        tags$div(id="cite",
                                 'Data provided by ', tags$em('The Great Canadian Shoreline Cleanup;'), ' App created by Fladmark, V., Konecny, C. and De la Puente, S.'
                        ) #change this to say data provided by GCSC, app by us
                    )
           ),
           
           tabPanel("Comparisons",
                    fluidRow(
                      column(2,
                             sliderInput("yearA", "Select Years", 2013, 2016, c(2013, 2016), 1, sep = ""),
                             #give slider input for choosing 2013-2016, 2014-2015, single year only, 3 years, etc.
                             selectInput("litter", "Remove Litter Items to Simulate Prevention", litter_vars, multiple = TRUE)
                             #give prompt to see effect of changes in policy/behaviour by "removing" litter
                      ), 
                      #first column have filtering options! and text and whatever else here.
                      column(4,
                             selectInput("locationA", "Region or District?", location_vars, "Regions"),
                             #choose whether to view a large region or relatively smaller district for comparison
                             conditionalPanel("input.locationA=='Regions'",
                                              selectInput("regionA", "Choose Region", region_vars, "Southern Strait of Georgia")),
                             #if region is selected give user an option to choose from 4 options
                             conditionalPanel("input.locationA=='ED_NAME'",
                                              selectInput("districtA", "Choose District", district_vars, "Vancouver-West End")),
                             #if district is selected give user an option to choose from 34 options
                             plotOutput("sourceprofileA", height = 200),
                             plotOutput("materialprofileA", height = 200)
                      ),
                      #second column is the selection and display of the first chosen region/district
                      column(4,
                             selectInput("locationB", "Region or District?", location_vars, "Regions"),
                             #choose whether to view a large region or relatively smaller district for comparison
                             conditionalPanel("input.locationB=='Regions'",
                                              selectInput("regionB", "Choose Region", region_vars, "North Coast B.C.")),
                             #if region is selected give user an option to choose from 4 options
                             conditionalPanel("input.locationB=='ED_NAME'",
                                              selectInput("districtB", "Choose District", district_vars, "North Island")),
                             #if district is selected give user an option to choose from 34 options
                             plotOutput("sourceprofileB", height = 200),
                             plotOutput("materialprofileB", height = 200))
                      )
                      #third column is the selection and display of the second chosen region/district
           ), #maybe include list of nearest cities so people know where these districts are without the map?
           #be sure to double check any nearest city data because data isn't 100% consistent in this column.
           
           conditionalPanel("false", icon("crosshair"))
) #when this is working, make sure to add visit a cleanup near you stuff
#and maybe the note that says this is cleaned litter not everything on shores
#maybe cite our inspiration from Superzip somewhere if possible and appropriate
#give GCSC a bigger shoutout? Like they're a big organization with WWF & Van Aqua
#then try to add those tricky polygons/e-mail rough draft and set up Monday meeting