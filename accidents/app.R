# app.R
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(DT)
library(bslib)


# Source UI and Server
# local = TRUE ensures environment inheritance
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# Run the application
shinyApp(ui = ui, server = server)
