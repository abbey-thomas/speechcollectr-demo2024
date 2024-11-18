# This application demonstrates basic tasks required for many speech experiments, 
# like advancing a trial number, creating an id number for the participant,
# loading and randomizing stimuli for each participant, timing trials,
# saving data, and closing the application when the experiment is complete. 

library(shiny)
library(imola)
library(shinyjs)
library(shinyWidgets)
library(speechcollectr)

# The audio files in the www folder will serve as the stimuli. 
# In this case there are 6 files by two different talkers, 
# the talker id is the first part of each filename
# so the "block" column assigns files to blocks by talker. 
stimuli <- data.frame(filename = sort(list.files(path = "www", pattern = ".wav")), 
                      block = c(1,1,1,2,2,2)) 

# sometimes this line is necessary to get reaction times on the order of milliseconds rather than seconds
#my_options <- options(digits.secs = 3)

# ui----
ui <- gridPage(
  
  # Use this line to center content in all div tags 
  # Or remove it to leave things left-aligned.
  tags$head(tags$style(HTML("div { text-align:center; }"))),
  
  # This is necessary for showing/hiding different screens
  useShinyjs(),
  
  ## Set up the interface layout (more details about the gridPanel arguments available in 'app2.R')
  gridPanel(id = "layout1", 
            
            # set up the page layout
            areas = list(
              default = c(
                "header header header",
                ". main .")), 
            
            gap = "10px",
            rows = c("auto 1fr"),
            columns = list(
              default = c("10% 80% 10%")),
            
            ## progress bar----
            header =  div(id = "headerDiv",
                          style = "background-color:lightgray;",
                          progressBar(id = "progress",
                                      value = 0, # starting value
                                      total = 6, # total number of trials in the experiment
                                      title = "Trials Completed:")),
            
            ## Main interface----
            main = div(id = "mainDiv",
                       
                       #this will vertically/horizontally center everything enclosed in this div tag
                       style = "margin:auto;",
                       
                       # initially the only thing visible is the "begin" button
                       actionButton(inputId = "begin", 
                                    label = "ENTER EXPERIMENT"), 
                       
                       # The remaining elements will be shown/hidden at appropriate times using the server code
                       hidden(radioGroupButtons(inputId = "trial_buttons", 
                                                label = "What is the current trial number?", 
                                                choices = c(1:6), 
                                                selected = character(0))), 
                       hidden(actionButton(inputId = "submit", 
                                           label = "SUBMIT ANSWER")),
                       hidden(actionButton(inputId = "next_trial", 
                                           label = "NEXT TRIAL")))
  )
)

# server----
server <- function(input, output, session) {
  
  ## Reactive values----
  # Make an object to hold the trial number and other reactive values
  # that will be created later
  # this must be at the top level of the server code! 
  rvs <- reactiveValues(trial_n = 0)
  
  ## When the participant clicks begin
  observeEvent(input$begin, {
    
    ## Generate an id number----
    # The first time you run this it will generate a file called "pinlist.rds"
    # in the application's www folder, where it will record the PINs already used
    rvs$pin <- pinGen(reactive = FALSE, # set this to FALSE, because we're adding it to an object that's already reactive
                      random = TRUE, 
                      digits = 4)
    
    # hide the begin button once it is clicked
    hide("begin")
  })
  
  # Once the participant has an ID number....
  observeEvent(rvs$pin, {
    
    # Randomize stimuli
    # This will randomize the "stimuli" dataframe created at the top level of this script
    rvs$stimuli <- randomStim(dataFile = stimuli, 
                              what = "both", # randomize the order of blocks and the order of stimuli within blocks
                              blockCol = "block", # the name of the block column
                              n_perBlock = 3, 
                              outFile = paste0("outputs/stim", rvs$pin, ".csv"))
    # the randomized stimulus data frame for this participant is saved to the file name provided as the outFile argument
  })
  
  # Once the stimuli are randomized and added to the reactive values
  observeEvent(rvs$stimuli, {
    
    # programmatically click on the (still hidden) "next trial" button
    click("next_trial")
  }, 
  
  # This isn't strictly necessary here, but can be helpful
  # This argument to observeEvent says that the responses contained in the 
  # curly braces in this call to observeEvent shouldn't happen every time 
  # the stimuli are updated during the session, but only the *first* time 
  # rvs$stimuli is given a value
  once = TRUE)
  
  ## Prioritizing events----
  # Both calls to observeEvent below describe things that happen in response to 
  # a click on the next trial button. 
  # The observeEvent statement with the *highest* priority will always
  # be executed BEFORE things with lower priority values.
  observeEvent(input$next_trial, {
    
    # Advance the trial number
    rvs$trial_n <- rvs$trial_n + 1
  }, priority = 1)
  
  observeEvent(input$next_trial, {
    
    # Update buttons with information specific to this trial from the 
    # randomized stimuli data frame. 
    updateRadioGroupButtons(session = session, 
                            inputId = "trial_buttons", 
                            label = paste0("The current trial number is ", rvs$trial_n,
                                          ". This trial is from block #",
                                          rvs$stimuli$block_num[rvs$trial_n], 
                                          ", and the audio associated with this trial is ", 
                                          rvs$stimuli$filename[rvs$trial_n], 
                                          ". Click on the current trial number."), 
                            choices = c(1:6), 
                            selected = character(0))
    
    # Show the response buttons
    showElement("trial_buttons")
    
    # Hide the next trial button (it will be already hidden on the first trial)
    hide("next_trial")
    
    ## Record trial start time----
    rvs$start <- Sys.time()
    
  }, priority = 0)
  
  # When the participant gives an answer
  observeEvent(input$trial_buttons, {
    
    ## Record trial end time----
    rvs$end <- Sys.time()
    
    # show the submit button only if the participant gave the correct answer!
    if (input$trial_buttons == rvs$trial_n) {
      showElement("submit")
    }
  })
  
  # When the participant clicks submit
  observeEvent(input$submit, {
    
    # If this isn't the last trial....
    if (rvs$trial_n < 6) {
      
      # hide submit and trial buttons
      hide("submit")
      hide("trial_buttons")
      
      # show the next trial button
      showElement("next_trial")
      
      # If this IS the last trial...
    } else {
      
      # Hide the main interface 
      hide("mainDiv")
      
      # Tell the participant in a popup that the experiment is over
      showModal(modalDialog(title = "Experiment Complete!", 
                            p("You may now close your browser window. 
                              If you do nothing, this window will close
                              in 5 seconds.")))
      
      # Close the app automatically after 5 s if the participant doesn't do it manually. 
      delay(5000, stopApp())
    }
    
    # No matter the trial number
    # Save the trial data
    saveRDS(data.frame(participant = rvs$pin, 
                       trial = rvs$trial_n, 
                       stimulus = rvs$stimuli$filename[rvs$trial_n], 
                       time = as.numeric(difftime(rvs$end, rvs$start, units = "secs"))), # the difference between the start and end time of the trial
            paste0("outputs/response", rvs$pin, "_", rvs$trial_n, ".rds"))
    
    print(data.frame(participant = rvs$pin, 
                     trial = rvs$trial_n, 
                     stimulus = rvs$stimuli$filename[rvs$trial_n], 
                     time = as.numeric(difftime(rvs$end, rvs$start, units = "secs"))))
    
    # update the progress bar to the current trial number
    updateProgressBar(session = session, 
                      id = "progress", 
                      value = rvs$trial_n, 
                      total = 6, # total number of trials in the experiment
                      title = "Trials Completed:")
    
  })
  
}

# This line of code runs the application----
shinyApp(ui = ui, server = server)