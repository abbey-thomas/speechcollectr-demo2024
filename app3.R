# This application shows different ways to show/hide/disable/enable elements
# These functions are accomplished with shinyjs

library(shiny)
library(imola)
library(shinyjs)

# ui----
ui <- gridPage(
  
  # Use this line to center content in all div tags 
  # Or remove it to leave things left-aligned.
  tags$head(tags$style(HTML("div { text-align:center; }"))),
  
  ## to use shinyjs----
  # This line must be placed in the ui code of ANY application that requires shinyjs
  # (This will probably be all experiments)
  useShinyjs(),
  
  # make a simple layout with a large centered content area
  # learn more about how to create a gridPanel layout in app2.R
  gridPanel(areas = list(default = c("left main right")), 
            
            # the main area will occupy 80% of the vertical space
            # and the left and right gaps will occupy 10% of the screen each
            columns = c("10% 80% 10%"), 
            
            # add content to the main area
            main = div(id = "mainDiv", 
                       
                       ## Interface with 2 buttons----
                       div(id = "view1", 
                           actionButton(inputId = "button1", 
                                        label = "Click to enable Button 2"),
                           
                           disabled(actionButton(inputId = "button2", 
                                                 label = "Button 2 (Click me!)"))), 
                       
                       ## Interface with more buttons and elements that will be defined dynamically---- 
                       hidden(div(id = "view2", 
                                  actionButton(inputId = "button3", 
                                               label = "Click here to see what's next"), 
                                  hidden(actionButton(inputId = "button4", 
                                                      label = "Button Four (Click me!)")), 
                                  
                                  # Use uiOutput to create a placeholder for a portion of the ui that will change throughout the experiment
                                  # uiOutput() in the ui object will always be paired with renderUI({}) in the server code
                                  uiOutput(outputId = "dynamic_ui")))))
)

# server----
server <- function(input, output, session) {
  
  ## Actions applied to individual buttons----
  observeEvent(input$button1, {
    enable("button2")
    hide("button1")
    
    # NOTE: Both enable/disable and hide/showElement functions take 
    # the inputId or id of the element they are to act upon in quotes
    # as their first (usually only) argument. 
  })
  
  ## Showing/hiding whole interfaces----
  observeEvent(input$button2, {
    hide("view1")
    showElement("view2")
  })
  
  observeEvent(input$button3, {
    disable("button3")
    showElement("button4")
    
    ## Defining a dynamic ui----
    # (The text will change each time button four is clicked)
    # uiOutput() in the ui object will always be paired with renderUI({}) in the server code
    output$dynamic_ui <- renderUI({
      div(id = "view2", 
          p("Please click Button Four at least 3 times."), 
          p(paste0("You clicked Button Four ", input$button4, " time(s). 
                   (This text will update as soon as you click Button 4.)")))
    })
  })
  
  ## Responding to user inputs with a popup/dialog box----
  observeEvent(input$button4, {
    
    # Show this text after the participant clicks on button four 3 times
    if (input$button4 == 3) {
      showModal(modalDialog(title = "Here's another way to do dynamic text", 
                            p("This is a modal dialog box. 
                            It appeared because you told it to do so when input$button4 == 3."), 
                            
                            # You can also add other inputs to a modal! 
                            footer = actionButton(inputId = "button5", 
                                                  label = "Okay, got it!")))
    }
  })
  
  ## Close the dialog box----
  # (when the participant clicks on the button in the dialog box)
  observeEvent(input$button5, {
    removeModal()
  })
}

# run the application----
shinyApp(ui = ui, server = server)