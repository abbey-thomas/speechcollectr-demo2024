# Load necessary libraries----
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(imola)
library(speechcollectr)

# Build the UI----
ui <- gridPage(
  
  # Center all the text that will be displayed
  tags$head(tags$style(HTML("div { text-align:center; }"))), 
  
  # Make shinyjs accessible to show/hide things at the right times
  useShinyjs(),
  
  # Make the speechcollectr audio recording scripts available
  useRecorder(),
  
  ## Format the display grid----
  gridPanel(id = "main",
            
            # Set the areas in the grid. 
            # The header should span across the whole screen
            # But the content should be centered below the header
            # with a blank space on either side. 
            areas = list(
              default = c(
                "header header header",
                ". content .")),
            
            # Let the first row (header area) be as big as it needs to be 
            # and let the content row take up all the remaining space (vertically). 
            rows = c("auto 1fr"),
            
            # The blank space on either side of the content area should be 10% of the screen
            # the content area should take up 80% of the horizontal space
            columns = c("10% 80% 10%"),
            
            # Put a progress bar in the header
            header =  div(id = "progressDiv",
                          
                          # Give it a background color to separate it from the rest of the screen
                          style = "background-color:lightgray;",
                          
                          progressBar(id = "progress",
                                      value = 0, # starting value
                                      total = 6, # total number of trials
                                      title = "Trials Completed:")),
            
            ## Format the main content area----
            content = div(id = "contentDiv",
                          style = "margin:auto;",
                          
                          ### Welcome screen----
                          # Important for web-based experiments: Create an initial landing page 
                          # Participants will see this when they visit your site, 
                          # so it should let them know they are in the right place. 
                          div(id = "welcomeDiv", 
                              
                              h1("Welcome to the ASA Tutorial Experiment!"),
                              
                              actionButton(inputId = "enter",
                                           label = "Enter Experiment")
                          ), 
                          # Note: commas separate ui elements that are siblings to each other! 
                          
                          ### Instructions----
                          hidden(div(id = "instructionsDiv", 
                                     h4("During this experiment, you will be listening and repeating words you hear. 
                                         You'll also be asked to pay attention to the talker's tone of voice."), 
                                     h4("You will complete 6 trials. Each trial consistents of 3 steps. 
                                        Further instructions will be given as you progress through the steps."), 
                                     
                                     # Add a white space between the instructions and the button.
                                     br(), 
                                     
                                     # Button to begin the trials
                                     actionButton(inputId = "begin_trial", 
                                                  label = "Click here to begin the first trial.")
                          )
                        ), 
                          
                          ### Listening interface----
                          # This will be hidden until the participant clicks the "begin trials" button
                          hidden(
                            div(id = "listenDiv", 
                                h2(id = "play_instructions", 
                                   "Click the play button that appears below to listen to the recording."), 
                                
                                # Add a blank space here
                                br(), 
                                
                                # Create a placeholder for the audio playback button 
                                # which will be created and dynamically updated in the server function
                                uiOutput("playInterface")
                            )
                          ), 
                          
                          ### Recording interface----
                          hidden(
                            div(id = "recordDiv", 
                                
                                # Give participant some instructions
                                h4(id = "record_instructions", 
                                   "Click the microphone to record yourself saying
                                   the word the talker asked you to say. 
                                   Try to match the talker's tone of voice."), 
                                
                                # Add a blank line
                                br(),
                                
                                # Create the buttons for audio recording
                                actionButton(inputId = "start_rec", 
                                             label = "Start Recording"), 
                                
                                # Note that the stop button is initially disabled
                                disabled(actionButton(inputId = "stop_rec", 
                                                      label = "Stop", 
                                                      inline = TRUE)) 
                                # use the "inline" argument to put it on the same horizontal line as the start button
                                
                            )
                          ), 
                        
                        ### Forced choice interface----
                        hidden(
                          div(id = "choiceDiv", 
                              
                              radioGroupButtons(inputId = "emotion_bttns", 
                                                label = "Which word best describes the emotion the talker used when speaking?", 
                                                choices = sample(c("angry", "happy", "sad", "disgusted", "fearful", "surprised")), 
                                                selected = character()),
                              
                              disabled(actionButton(inputId = "submit_choice", 
                                                    label = "SUBMIT ANSWER"))
                            
                          )
                        ), 
                        
                        # Slider scale interface----
                        hidden(
                          div(id = "ratingDiv",
                            rateUI(id = "emo_rating", 
                                   type = "slider"), 
                            
                            br(), 
                            
                            disabled(actionButton(inputId = "submit_rating", 
                                                  label = "SUBMIT ANSWER"))
                          )
                        ), 
                        
                        # End of experiment interface----
                        hidden(
                          div(id = "endDiv",
                            h1("Experiment complete! You may now close this window.")
                          )
                        )
                
            )
  )
)

# Server code----
server <- function(session, input, output) {
  
  # Object to hold reactive values----
  rvs <- reactiveValues(trial_n = 0)
  
  # When the participant clicks "enter"----
  observeEvent(input$enter, {
    
    # Hide the welcome screen
    hide("welcomeDiv")
    
    # Show the task instructions
    showElement("instructionsDiv")
    
    ## Generate PIN----
    # Create a code for the participant
    # Add it to the rvs object to make it available throughout the entire session
    rvs$pin <- pinGen(reactive = FALSE)
    
    ## Randomize stimuli----
    # Randomize the stimuli for the participant
    # Add stimulus list to rvs object
    rvs$stimuli <- list.files("www", pattern = ".wav")
    
  })
  
  # Participant clicks "begin trial"----
  observeEvent(input$begin_trial, {
    
    # Hide instructions
    hide("instructionsDiv")
    
    # Advance the trial number
    rvs$trial_n <- rvs$trial_n+1
    
    # Show the listening interface
    showElement("listenDiv")
    
    
    ## Set/reset empty values for response interfaces----
    rvs$emotion <- NULL
    rvs$slider <- NULL
  }, priority = 1)
  
  observeEvent(input$begin_trial, {
    ## Generate audio playback interface----
    # Attach the correct audio file to the play button
    # Note the use of ({}) after the render function!
    output$playInterface <- renderUI({
      playBttn(inputId = "play_stim", 
               src = rvs$stimuli[[rvs$trial_n]], 
               audioId = paste0("audio", rvs$trial_n), 
               label = "Play Recording")
    })
    
  })
  
  # Play button reaction----
  observeEvent(input$play_stim, {
    
    # Hide the listening interface
    hide("listenDiv")
    
    # Wait for 2500 ms (while the recording plays)
    # Then show the recording interface
    delay(2500, showElement("recordDiv"))
  })
  
  # Reaction to "Start Recording"----
  observeEvent(input$start_rec, {
    
    # Disable the start recording button
    disable("start_rec")
    
    # Start recording
    # readyId will be the input ID assigned to the event
    # of the participant giving the site access to their microphone
    startRec(readyId = "recording")
  })
  
  # Once we are recording----
  observeEvent(input$recording, {
    
    # Enable the stop recording button
    enable("stop_rec")
  })
  
  # "Stop Recording" reaction----
  observeEvent(input$stop_rec, {
    
    ## Stop the recording and save it to a wav file
    stopRec(filename = paste0("outputs/rec", rvs$pin, "_", rvs$trial_n, ".wav"), 
            finishedId = "rec_done")
    
    ## Get ready for the next recording by disabling stop/enabling start
    disable("stop_rec")
    enable("start_rec")
  })
  
  observeEvent(input$rec_done, {
    evalWavServer(wave = input$rec_done)
  })
  
  observeEvent(input[["evalWav-result"]], {
    if (input[["evalWav-result"]] == "pass") {
      
      ## Hide the recording interface
      hide("recordDiv")
      
      showElement("choiceDiv")
    }
  })
  
  observeEvent(input$emotion_bttns, {
    
    rvs$emotion <- isolate(input$emotion_bttns)
    
    enable("submit_choice")
  })
  
  observeEvent(input$submit_choice, {
    
    # Get ready for the next trial by hiding the choice interface
    # and clearing the response
    hide("choiceDiv")
    updateRadioGroupButtons(session = session, 
                            inputId = "emotion_bttns", 
                            label = "Which word best describes the emotion the talker used when speaking?", 
                            choices = sample(c("angry", "happy", "sad", "disgusted", "fearful", "surprised")), 
                            selected = character())
    
    # Show the rating interface
    showElement("ratingDiv")
    
    # Program the slider based on the participant's answer to the forced choice question
    rvs$slider <- rateServer(id = "emo_rating", 
                         type = "slider",
                         pretext = paste0("How ", rvs$emotion, " did the talker sound?"), 
                         choices = list(c(paste0("Not very ", rvs$emotion), 
                                          paste0("Extremely ", rvs$emotion))))
    
  })
  
  observeEvent(rvs$slider$ratings, {
    if (isTruthy(rvs$slider$ratings)) {
      enable("submit_rating")
    } else {
      disable("submit_rating")
    }
  })
  observeEvent(input$submit_rating, {
    
    saveRDS(data.frame(stimulus = paste0(rvs$stimuli[[rvs$trial_n]]), 
                       trial = rvs$trial_n, 
                       emo_selected = rvs$emotion, 
                       emo_strength = rvs$slider$ratings), 
            file = paste0("outputs/response", rvs$pin, "_", rvs$trial_n, ".rds"))
    
    hide("ratingDiv")
    
    updateProgressBar(session = session, 
                      id = "progress", 
                      title = "Trials Completed:", 
                      value = rvs$trial_n, 
                      total = 6)
    
    if (rvs$trial_n < 6) {
      delay(500, click("begin_trial"))
    } else {
      showElement("endDiv")
    }
  })
  
}

# Run the application----
shinyApp(ui = ui, server = server)