if (!require(devtools))
  install.packages("devtools")
devtools::install_github("rstudio/leaflet")
library(viridisLite)
shiny::runGitHub("rstudio/shiny-examples", subdir="063-superzip-example")
shiny::runApp(display.mode="showcase")
#try to compare two areas (in 2nd tab rather than data table?) and use proportions

#try line graphs for source and materials as a % of total items (over time?)

#be able to filter out what it looks like when you remove a certain item!

#be able to filter out in certain areas 