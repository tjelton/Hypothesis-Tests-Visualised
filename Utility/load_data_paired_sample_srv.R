# Originally, we were using the PairedData package to use real paired datasets. However, this is not compatabile with shiny live...
# Now, we will use sleep (which is paired data included in base R, and synthetic data).

# Sleep paired dataset that is inbuilt into R.
SleepStudy <- reshape(sleep,
                      timevar = "group",
                      idvar = "ID",
                      direction = "wide")
names(SleepStudy)[names(SleepStudy) == "extra.1"] <- "group 1"
names(SleepStudy)[names(SleepStudy) == "extra.2"] <- "group 2"

# ChatGPT generated synthetic datasets
set.seed(1)

# BloodPressureStudy: before/after treatment with two drugs
BloodPressureStudy <- data.frame(
  PatientID = 1:30,
  Before_Treatment = round(rnorm(30, 140, 15), 1),
  After_DrugA = round(rnorm(30, 135, 14), 1),
  After_DrugB = round(rnorm(30, 133, 13), 1)
)

# CognitiveScores: paired cognitive test scores at baseline and 6 months
CognitiveScores <- data.frame(
  SubjectID = 101:130,
  Baseline_Memory = round(rnorm(30, 75, 8), 0),
  SixMonths_Memory = round(rnorm(30, 80, 7), 0),
  Baseline_Attention = round(rnorm(30, 70, 10), 0),
  SixMonths_Attention = round(rnorm(30, 72, 9), 0)
)

# FitnessTestResults: paired measurements before and after 8 weeks training
FitnessTestResults <- data.frame(
  AthleteID = 201:240,
  VO2Max_Before = round(rnorm(40, 45, 5), 1),
  VO2Max_After = round(rnorm(40, 49, 5), 1),
  SprintTime_Before = round(rnorm(40, 12.0, 0.8), 2),
  SprintTime_After = round(rnorm(40, 11.5, 0.7), 2)
)

# DietImpactStudy: paired weight and cholesterol before and after diet
DietImpactStudy <- data.frame(
  ParticipantID = 301:325,
  Weight_Before = round(rnorm(25, 85, 12), 1),      # kg
  Weight_After = round(rnorm(25, 80, 11), 1),       # kg
  Cholesterol_Before = round(rnorm(25, 200, 25), 0), # mg/dL
  Cholesterol_After = round(rnorm(25, 185, 20), 0)   # mg/dL
)

load_data_paired_sample_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Store the sample data.
      data_condition_1 <- reactiveVal(NULL)
      data_condition_2 <- reactiveVal(NULL)
      data_paired_difference <- reactiveVal(NULL)
      
      # To trigger the pre_uploaded functions to re-run so data is re-updated.
      re_run_flag <- reactiveVal(FALSE)
      
      # Mechanism to allow the user to specify the data sample they will be using.
      output$data_upload <- renderUI({
        
        # Ensure some radio button option has been selected.
        req(input$data_upload_choice)
        
        # Reset data to be null.
        data_condition_1(NULL)
        data_condition_2(NULL)
        data_paired_difference(NULL)
        
        # Pre-uploaded data
        # The pre-uploaded data set comes from the 'PairedData' package: https://cran.r-project.org/web/packages/PairedData/PairedData.pdf
        if (input$data_upload_choice == "pre_uploaded") {
          
          isolate({
            re_run_flag(!re_run_flag())
          })
          
          return(
            tagList(
              selectInput( 
                ns("data_set_pre_uploaded"), 
                HTML("<p><b>Which data set would you like to analyse?</b></p>"),
                list("SleepStudy","BloodPressureStudy","CognitiveScores","FitnessTestResults","DietImpactStudy")
              ),
              HTML("<p><i>Warning: Other than the 'SleepStudy' data set, the other data sets are synthetic (made up) data.</i></p>"),
              HTML("<p>Now we need to select what data we want to be in condition 1, and what to be in condition 2. The paired difference will be condition 2 - condition 1.</p><br>")
            )
          )
        }
        
        # Manual upload data.
        if (input$data_upload_choice == "manually_specified") {
          return(
            tagList(
              HTML("<br>"),
              HTML("<p><b>Enter data into the text box below for each sample.</b>
            <ul>
              <li>All values must be numeric, and numbers should contain no spaces or any characters other than a '.' for a decimal place.</li>
              <li>Each value must be on it's own line, or comma seperated.</li>
              <li>As this is paired data, there must be the same number of values in each textbox.</li>
              <li>Don't forget to press 'Upload' once you are finished!</li>
            </ul></p><br>"),
              
              # Textbox -> condition 1 data entry.
              HTML("<p><b>Condition 1:</b></p>"),
              textAreaInput( 
                ns("manual_data_upload_textbox_condition_1"), 
                NULL, 
                value = "",
                width = "100%"
              ),
              
              # Textbox -> condition 2 data entry.
              HTML("<p><b>Condition 2:</b></p>"),
              textAreaInput( 
                ns("manual_data_upload_textbox_condition_2"), 
                NULL, 
                value = "",
                width = "100%"
              ),
              
              # Upload action button. Wrapped in a fluid row to make it right-aligned.
              fluidRow(
                column(8),
                column(4,
                       actionButton(
                         inputId = ns("load_manual_data"),
                         label = "Upload",
                         class = "btn-success text-white",
                         style = "color: #fff;",
                         width = "100%"
                       ),
                )
              )
              
            )
          )
        }
        
      })
      
      # When the user chooses a pre uploaded data set, prompt them to choose the numeric column for condition 1 and 2.
      output$numeric_column_selection_pre_uploaded_data <- renderUI({
        req(input$data_set_pre_uploaded)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "pre_uploaded") {
          return()
        }
        
        # Get numeric column names.
        data = get(input$data_set_pre_uploaded) 
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        
        return(
          tagList(
            fluidRow(
              column(6,
                     selectInput(
                       ns("condition_1_pre_uploaded"),
                       HTML("<p><b>Condition 1:</b></p>"),
                       numeric_cols,
                       selected = numeric_cols[1]
                     )
              ),
              column(6,
                     selectInput(
                       ns("condition_2_pre_uploaded"),
                       HTML("<p><b>Condition 2:</b></p>"),
                       numeric_cols,
                       selected = numeric_cols[2]
                     )
              )
            ),
            HTML("<br><br>")
          )
        )
      })
      
      # Warning message when for the pre-uploaded option, conditions 1 and 2 are the same.
      output$pre_uploaded_warning_and_save_data <- renderUI({
        req(input$data_set_pre_uploaded)
        req(input$condition_1_pre_uploaded)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "pre_uploaded") {
          return()
        }
        
        # Warning message when the conditions are the same.
        if (input$condition_1_pre_uploaded == input$condition_2_pre_uploaded) {
          data_condition_1(NULL)
          data_condition_2(NULL)
          data_paired_difference(NULL)
          return(
            tagList(
              HTML("<span style='color: red;'><p>Warning: Condition 1 and condition 2 should be different.</p></span>"),
            )
          )
          
          # Otherwise, set the data.
        } else {
          data_condition_1(get(input$data_set_pre_uploaded)[[input$condition_1_pre_uploaded]])
          data_condition_2(get(input$data_set_pre_uploaded)[[input$condition_2_pre_uploaded]])
          data_paired_difference(data_condition_2() - data_condition_1())
          return()
        }
      })
      
      # Reactive variables to keep track of the number of values entered for each condition.
      num_values_cond_1 = reactiveVal(NULL)
      num_values_cond_2 = reactiveVal(NULL)
      error_message_unequal_paired_samples = reactiveVal(FALSE)
      error_message_insufficient_unique_values_manual_upload = reactiveVal(FALSE)
      
      # Observe if the "Upload" button is clicked. This will involve checking the data, and triggering a warning if the data is not in the correct form.
      observeEvent(input$load_manual_data, {
        
        req(input$manual_data_upload_textbox_condition_1)
        req(input$manual_data_upload_textbox_condition_2)
        
        # Re-set warning messages
        error_message_unequal_paired_samples(FALSE)
        error_message_insufficient_unique_values_manual_upload(FALSE)
        
        data_condition_1(NULL)
        data_condition_2(NULL)
        data_paired_difference(NULL)
        
        condition_1 = input$manual_data_upload_textbox_condition_1
        condition_2 = input$manual_data_upload_textbox_condition_2
        
        # Process condition 1 data.
        splits_cond_1 <- strsplit(condition_1, "[,\n]")[[1]]
        splits_cond_1 <- trimws(splits_cond_1)
        splits_cond_1 = splits_cond_1[splits_cond_1 != ""]
        numeric_cond_1 = as.numeric(splits_cond_1)
        numeric_cond_1 = numeric_cond_1[!is.na(numeric_cond_1)]
        
        # Process condition 2 data.
        splits_cond_2 <- strsplit(condition_2, "[,\n]")[[1]]
        splits_cond_2 <- trimws(splits_cond_2)
        splits_cond_2 = splits_cond_2[splits_cond_2 != ""]
        numeric_cond_2 = as.numeric(splits_cond_2)
        numeric_cond_2 = numeric_cond_2[!is.na(numeric_cond_2)]
        
        # Check if the data is really paired -> otherwise, trigger a warning message.
        if (length(numeric_cond_1) != length(numeric_cond_2)) {
          num_values_cond_1(length(numeric_cond_1))
          num_values_cond_2(length(numeric_cond_2))
          error_message_unequal_paired_samples(TRUE)
          return()
        }
        
        # Find paired difference between the groups.
        paired_difference = numeric_cond_2 - numeric_cond_1
        
        # Check that there are more than one unique value (table length will be greater than 1). Otherwise, sd = 0, and the test will break.
        adequate_unique <- length(table(paired_difference)) > 1
        
        # In the case that there are not unique values, raise an error with the user.
        if (adequate_unique == FALSE) {
          error_message_insufficient_unique_values_manual_upload(TRUE)
          return()
        }
        
        data_condition_1(numeric_cond_1)
        data_condition_2(numeric_cond_2)
        data_paired_difference(paired_difference)
        
      })
      
      # Error message for when not enogh unique values.
      output$manual_entry_unequal_samples <- renderUI({
        req(input$data_upload_choice)
        
        # Only display if the manually_specified radio button option is selected.
        if (input$data_upload_choice != "manually_specified") {
          return()
        }
        
        if (error_message_unequal_paired_samples()) {
          string = paste("<span style='color: red;'><p>Warning: The number of values in each condition is unequal. Condition 1 has ", 
                         as.character(num_values_cond_1()), " values, and condition 2 has ", as.character(num_values_cond_2()), " values.</p></span>", sep = "")
          return(
            tagList(
              HTML("<br>"),
              HTML(string)
            )
          )
        } else {
          return()
        }
      })
      
      # Error message for when not enogh unique values.
      output$manual_entry_insufficient_unique_values <- renderUI({
        req(input$data_upload_choice)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "manually_specified") {
          return()
        }
        
        if (error_message_insufficient_unique_values_manual_upload()) {
          string = "<span style='color: red;'><p>Warning: You must have at least two unique values in your manually specified data.</p></span>"
          return(
            tagList(
              HTML("<br>"),
              HTML(string)
            )
          )
        } else {
          return()
        }
      })
      
      # Radio button allowing user to choose which data they want to plot.
      output$radio_buttons_data_to_plot <- renderUI({
        
        # If data has not been uploaded, don't display anything.
        if (is.null(data_paired_difference())) {
          return()
        }
        
        return(
            radioButtons(ns("data_to_plot"), NULL, c(
                                                 "Paired Difference" = "Paired_Difference",
                                                 "Condition 1" = "Condition_1", 
                                                 "Condition 2" = "Condition_2"
                                                  ), inline=T)
        )
      })
      
      # Plot UI or warning message output.
      output$data_upload_plot_section_output <- renderUI({
        
        # Text to render alerting the user that they can't proceed until they make a data choice.
        # This is when no data has been set.
        if (is.null(data_paired_difference())) {
          string = "<span style='color: blue;'><p>In order to proceed, you must select some data to act as your sample.</p></span>"
          return(
            tagList(
                HTML(string)
            )
          )
        }
        
        # If data has been set, display the plots.
        return(
          tagList(
            plotOutput(ns("initial_data_plots"),  width = "100%")
          )
        )
      })
      
      # Display input data.
      output$initial_data_plots <- renderPlot({
        
        req(input$data_to_plot)
        
        # If data has not been uploaded, don't display anything.
        if (is.null(data_paired_difference())) {
          return()
        }
        
        data_plotting = data_condition_1()
        if (input$data_to_plot == "Condition_2") {
          data_plotting = data_condition_2()
        } else if (input$data_to_plot == "Paired_Difference") {
          data_plotting = data_paired_difference()
        }
        
        data_plotting = data.frame(data_plotting)
        
        # Extract the column (assuming only one column)
        x <- data_plotting[[1]]
        
        # Check that the data is actually loaded in.
        if (length(x) == 0) {
          return()
        }
        
        # Save original plotting settings
        old_par <- par(no.readonly = TRUE)
        
        # Stack plots: 2 rows, 1 column
        par(mfrow = c(2, 1))
        
        # Boxplot (default vertical)
        boxplot(x,
                horizontal = TRUE,
                col = "blue",
                main = "Boxplot",
                ylab = "Values")
        
        # Histogram
        hist(x,
             breaks = 30,
             col = "blue",
             border = "black",
             main = "Histogram",
             xlab = "Values",
             ylab = "Frequency")
        
        # Restore settings
        par(old_par)
        
      })
      
      # To make the data accessible outside of the module.
      list(data_condition_1 = data_condition_1, 
           data_condition_2 = data_condition_2, 
           data_paired_difference = data_paired_difference)
      
    }
    
  )
}
