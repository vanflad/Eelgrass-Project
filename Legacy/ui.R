library(leaflet)

#random notes to self: figure out how to deal with 0's/NA's, "Data not avail."
#maybe write out a short term game plan for how to tackle all this shit?????
#today get basic thing running, sat troubleshoot improvements, sun rough draft
#monday do edits/troubleshooting/talk to group, and then tues/wed final touches

# Choices for drop-downs
vars <- c(
  "Volunteers" = "Volunteers",
  "Kilograms" = "Kilograms",
  "Kilometers" = "Kilometers"
) #add number of cleanups (?) or exclude this size/color business altogether
#maybe try to get simple copycat version of superzip working then modify?????

location_vars <- c(
  "Region" = "Region",
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

widedata$Region <- as.character(widedata$Region)
#update with relevant datafile name! to change the names of regions.

region_vars <- c(
  "North Coast B.C." = "1",
  "Inner Coast Vancouver Island" = "2",
  "West Coast Vancouver Island" = "3",
  "Southern Strait of Georgia" = "4"
)
  
district_vars <- unique(widedata$ED_NAME)
#Update with relevant datafile name!

# Need to figure out how to include Region polygons when region is chosen
# And how to include ED polygons when ED is chosen (region by default for BC)

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
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Litter explorer"),
                                      #put in inputs for filtering here instead:
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "Volunteers"),
                                      
                                      plotOutput("sourceprofile", height = 200),
                                      plotOutput("materialprofile", height = 200)
                                      #change these to those trash profile bar charts!
                        ),
                        
                        tags$div(id="cite",
                                 'Data provided by ', tags$em('The Great Canadian Shoreline Cleanup'), ' App created by Fladmark, V., Konecny, C. and De la Puente, S.'
                        ) #change this to say data provided by GCSC, app by us
                    )
           ),
           
           tabPanel("Comparisons", #change inputs to relevant region/ed choice
                    fluidRow(
                      column(3,
                             selectInput("Region", "Regions", c("All regions"="")
                      ),
                      column(3,
                             conditionalPanel("input.Region",
                                              selectInput("ED_NAME", "Electoral District", c("All districts"=""))
                             )
                      ),
                    hr(),
                    DT::dataTableOutput("trashtable")
           ), #change to print out bar chart and relevant info here instead!
           
           conditionalPanel("false", icon("crosshair"))
           ))) #might need to fix this fucked up comma and bracket situation
           