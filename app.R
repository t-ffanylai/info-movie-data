# Setup
library(shiny)
source('ui.R')
source('server.R')

# Defines the ui and server for the movie revenue data ap
shinyApp(ui = ui, server = server)
