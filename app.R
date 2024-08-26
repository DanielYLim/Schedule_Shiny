library(shiny)
library(ggplot2)
library(plotly)
library(DT)

# Source UI and server files

source("server.R")
source("ui.R")


# Run the Shiny App
shinyApp(ui = ui, server = server)

