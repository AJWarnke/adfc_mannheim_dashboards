# app.R
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(DT)
library(bslib)
library(dbscan)


# Source UI and Server
# local = TRUE ensures environment inheritance
source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# Run the application
shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = as.numeric(Sys.getenv("PORT", 8080))))
