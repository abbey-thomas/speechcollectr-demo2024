# This app shows a basic layouts that can be used for many experiments.
# Layout 1: centered content and a progress bar at the top of the screen. 
# Layout 2: A header with multiple sections & a progress bar at the bottom.
# Layout 3: Four clickable quadrants equally dividing the full screen.
# Each layout is constructed with a separate call to gridPanel 
# within a single call to gridPage

library(shiny)
library(imola)
library(shinyjs)
library(shinyWidgets)

# Note throughout the ui: sister elements (those at the same level in the hierarchy,
# like the calls to gridPanel) are separated by commas. 
# ui----
ui <- gridPage(
  
  # Use this line to center content in all div tags 
  # Or remove it to leave things left-aligned.
  tags$head(tags$style(HTML("div { text-align:center; }"))),
  
  # This is necessary for showing/hiding different screens
  useShinyjs(),
  
  ## Layout 1----
  gridPanel(id = "layout1",
            
            # This will be the "default" layout for the app, used on all devices
            # unless you also add values 
            # for "sm =" (small screens, e.g. cell phones) or other screen sizes 
            # to this list. More info in imola documentation. 
            areas = list(
              default = c(
                "header header header",
                ". main .")), 
            # each row MUST have the same number of columns 
            # i.e., each element in the vector above must have the same number 
            # of space separated character strings
            # two rows, three columns
            # the header area spans all three columns
            # the main area spans one column and has two empty areas beside it
            
            
            # Put gaps between areas. Increasing/decreasing this number will 
            # increase/decrease the size of the gap. 
            #gap = "10px",
            
            # some different ways to set row widths: 
            # Here we use a combination of auto 
            # (make the width of this row equal to the width of content in it)
            # and "fr" (short for fractionary unit) 
            # which will simply take up whatever space is left on the screen
            # note that these width values are just separated by spaces in a single text vector
            rows = c("auto 1fr"),
            
            # set the column widths
            # options used for the rows are also available here
            # in addition to the percentages (of screen width) used below
            # we also define different column widths for regular screens and 
            # narrow screens (e.g. smartphones, or a narrow browser window)
            columns = list(
              default = c("10% 80% 10%"),
              sm = c("5% 90% 5%")),
            
            #align_items = "center", 
            
            # Here we put stuff (i.e., a progress bar) in the header
            header =  div(id = "headerDiv",
                          
                          # we also give it a background color
                          style = "background-color:lightgray;",
                          
                          # And add a progress bar
                          progressBar(id = "progress",
                                      value = 0, # starting value
                                      total = 6, # total number of trials in the experiment
                                      title = "Trials Completed:")),
            
            # Here we add stuff to the main content area
            main = div(id = "mainDiv",
                       
                       #setting margin to auto will vertically/horizontally center everything enclosed in this div tag
                       #setting width and height to 100% makes the div take up all available space...
                       # (This part isn't normally important but here allows the outline of this grid area to be visible)
                       style = "margin:auto;width:100%;height:100%",
                       
                       # the content: a button that can be clicked to show the next layout
                       actionButton(inputId = "view_layout2", 
                                    label = "View Layout 2", 
                                    style = "width:100%;height:100%"))
  ), 
  
  ## Layout 2----
  hidden(gridPanel(id = "layout2", 
                   areas = list(
                     default = c("header1 header1 header1 header2 header2", 
                                 ". main main main .", 
                                 "footer footer footer footer footer")
                   ), 
                   gap = "10px", 
                   
                   # We can hard code the width of the rows and 
                   # use the relative sizing with fr and auto as above. 
                   rows = c("100px 1fr auto"), 
                   columns = c("5% 30% 30% 30% 5%"), 
                   header1 = div(id = "header1Div", 
                                 style = "width:100%;height:100%;background-color:teal;margin:auto;",
                                 h1("Header 1")), 
                   header2 = div(id = "header2Div", 
                                 style = "width:100%;height:100%;background-color:coral;margin:auto;",
                                 h1("Header 2")), 
                   main = div(id = "mainDiv2",
                              
                              #this will vertically/horizontally center everything enclosed in this div tag
                              style = "margin:auto;",
                              
                              # the content: a button that can be clicked to show the next layout
                              actionButton(inputId = "view_layout3", 
                                           label = "View Layout 3")), 
                   footer = div(id = "footerDiv",
                                
                                # we also give it a background color
                                style = "background-color:lightgray;",
                                
                                # And add a progress bar
                                progressBar(id = "progress",
                                            value = 0, # starting value
                                            total = 6, # total number of trials in the experiment
                                            title = "Trials Completed:"))
                   )),
  
  ## Layout 3----
  hidden(gridPanel(id = "layout3", 
                   
                   # create four quadrants (2 rows, 2 columns)
                   areas = list(
                     default = c("quad1 quad2",
                                 "quad3 quad4")),
                   
                   # Note that the row and column widths are not explicitly defined here
                   # because everything is equal by default! 
                   
                   # We can label the buttons one at a time, 
                   # or batch process with lapply
                   lapply(c("one", "two", "three", "four"), function(i){
                     actionButton(inputId = i,
                                  label = i,
                                  
                                  # make the buttons take up all available space
                                  # give them a larger font
                                  # make the corners of the buttons not rounded
                                  style = "width:100%;height:100%;
                                       margin:0px;font-size:30px;
                                       border-radius:0px;")
                   })
    
  ))
  
)

# server----
# Make the buttons do things
server <- function(input, output, session) {
  
  ## Layout 1 to Layout 2----
  # with a click on the button in layout 1, 
  # hide layout 1, show layout 2
  observeEvent(input$view_layout2, {
    hide("layout1")
    showElement("layout2")
  })
  
  ## Layout 2 to Layout 3----
  # with a click on the button in layout 2, 
  # hide layout 2, show layout 3
  observeEvent(input$view_layout3, {
    hide("layout2")
    showElement("layout3")
  })
  
  ## Layout 3 to Layout 1----
  # with a click on any of the buttons in layout 3, 
  # go back to layout 1 after a 1 second delay
  lapply(c("one", "two", "three", "four"), function(i){
    observeEvent(input[[i]], {
      delay(1000, hide("layout3"))
      delay(1000, showElement("layout1"))
    })
  })
  
  
}

# run the app----
shinyApp(ui = ui, server = server)