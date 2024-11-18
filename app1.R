# This application just shows the basic structure of a shiny application,
# which is the basic structure for all speech experiment interfaces
# created with speechcollectr. 

# Load packages----
library(shiny)
library(imola)

# build the appearance of the page in the ui object----
# right now this is just a blank page! 
ui <- gridPage()

# Make things on the page do things with the server function----
# It will always have these three arguments
# (the current blank page does nothing)
server <- function(input, output, session) {}

# This line of code runs the application----
shinyApp(ui = ui, server = server)