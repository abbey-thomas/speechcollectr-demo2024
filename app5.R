library(shiny)
library(shinyjs)
library(speechcollectr)

ui <- fluidPage(
  
  useRecorder(), 
  useShinyjs(), 
  
  actionButton(inputId = "begin", label = "START RECORDING"), 
  disabled(actionButton(inputId = "stop", label = "STOP"))
)

server <- function(input, output, session) {
  observeEvent(input$begin, {
    disable("begin")
    startRec(readyId = "recording")
  })
  
  observeEvent(input$recording, {
    enable("stop")
  })
  
  observeEvent(input$stop, {
    disable("stop")
    stopRec(filename = "test.wav", 
            finishedId = "file_saved")
  })
  
  observeEvent(input$file_saved, {
    evalWavServer(wave = input$file_saved)
  })
  
  observeEvent(input[["evalWav-result"]], {
    if (input[["evalWav-result"]] == "pass") {
      enable("begin")
    }
  })
}

shinyApp(ui = ui, server = server)