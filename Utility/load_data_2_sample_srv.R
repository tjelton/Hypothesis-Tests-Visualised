# GPT generate syntehtic blood_pressure data.
set.seed(1)  # For reproducibility

# Number of samples per group
n <- 50

# Create data
blood_pressure <- data.frame(
  drug = rep(c("Drug_A", "Drug_B"), each = n),
  blood_pressure = c(
    rnorm(n, mean = 120, sd = 10),  # Drug_A values
    rnorm(n, mean = 115, sd = 10)   # Drug_B values
  )
)


load_data_2_sample_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      data_sample_1 <- reactiveVal(NULL)
      data_sample_2 <- reactiveVal(NULL)
      
      # To trigger the pre_uploaded functions to re-run so data is re-updated.
      re_run_flag <- reactiveVal(FALSE)
      
      # Mechanism to allow the user to specify the data sample they will be using.
      output$data_upload <- renderUI({
        
        # Ensure some radio button option has been selected.
        req(input$data_upload_choice)
        
        # Reset data to be null.
        data_sample_1(NULL)
        data_sample_2(NULL)
        
        # Pre-uploaded data
        if (input$data_upload_choice == "pre_uploaded") {
          
          isolate({
            re_run_flag(!re_run_flag())
          })
          
          return(
            tagList(
              selectInput( 
                ns("data_set_pre_uploaded"), 
                HTML("<p>Which <b>data set</b> would you like to analyse?</p>"),
                list("blood_pressure", "iris", "InsectSprays", "CO2", "ToothGrowth", "PlantGrowth")
              ),
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
              <li>Don't forget to press 'Upload' once you are finished!</li>
            </ul></p><br>"),
              
              # Textbox -> Sample 1
              HTML("<p><b>Sample 1:</b></p>"),
              textAreaInput( 
                ns("manual_data_upload_sample_1"), 
                NULL, 
                value = "",
                width = "100%"
              ),
              
              # Textbox -> Sample 2
              HTML("<p><b>Sample 2:</b></p>"),
              textAreaInput( 
                ns("manual_data_upload_sample_2"), 
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
      
      error_message_insufficient_unique_values_manual_upload = reactiveVal(FALSE)
      
      # Observe if the "Upload" button is clicked for the manual data. This will involve checking the data, and triggering a warning if the data is not in the correct form.
      observeEvent(input$load_manual_data, {
        
        req(input$manual_data_upload_sample_1)
        req(input$manual_data_upload_sample_2)
        
        # Re-set warning messages
        error_message_insufficient_unique_values_manual_upload(FALSE)
        
        data_sample_1(NULL)
        data_sample_2(NULL)
        
        sample_1 = input$manual_data_upload_sample_1
        sample_2 = input$manual_data_upload_sample_2
        
        # Process condition 1 data.
        splits_samp_1 = strsplit(sample_1, "[,\n]")[[1]]
        splits_samp_1 = trimws(splits_samp_1)
        splits_samp_1 = splits_samp_1[splits_samp_1 != ""]
        numeric_samp_1 = as.numeric(splits_samp_1)
        numeric_samp_1 = numeric_samp_1[!is.na(numeric_samp_1)]
        
        # Process condition 2 data.
        splits_samp_2 = strsplit(sample_2, "[,\n]")[[1]]
        splits_samp_2 = trimws(splits_samp_2)
        splits_samp_2 = splits_samp_2[splits_samp_2 != ""]
        numeric_samp_2 = as.numeric(splits_samp_2)
        numeric_samp_2 = numeric_samp_2[!is.na(numeric_samp_2)]
        
        # Check that there are more than one unique value (table length will be greater than 1). Otherwise, sd = 0, and the test will break.
        adequate_unique = length(table(numeric_samp_1)) > 1 &&  length(table(numeric_samp_2)) > 1
        
        # In the case that there are not unique values, raise an error with the user.
        if (adequate_unique == FALSE) {
          error_message_insufficient_unique_values_manual_upload(TRUE)
          return()
        }
        
        data_sample_1(numeric_samp_1)
        data_sample_2(numeric_samp_2)
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
      
      
      # Categorical Variable Selection
      output$categorical_variable_selection_pre_uploaded <- renderUI({
        req(input$data_set_pre_uploaded)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "pre_uploaded") {
          return()
        }
        
        # Get character or factor column names.
        data = get(input$data_set_pre_uploaded)
        cols_char_or_factor <- names(data)[sapply(data, function(col) {
          (is.character(col) || is.factor(col)) && length(unique(col)) >= 2
        })]
        
        return(
          tagList(
            fluidRow(
              HTML("<p>The second step in choosing our samples involves selecting a <b>categorical variable</b> that we will <b>split the samples</b> from:</p>"),
              column(6,
                     selectInput(
                       ns("categorical_pre_uploaded"),
                       NULL,
                       cols_char_or_factor,
                       selected = cols_char_or_factor[1]
                     )
              )
            ),
          )
        )
      })
      
      # Select the samples.
      output$sample_selection_pre_uploaded <- renderUI({
        req(input$data_set_pre_uploaded)
        req(input$categorical_pre_uploaded)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "pre_uploaded") {
          return()
        }
        
        # Get numeric column names.
        df <- get(input$data_set_pre_uploaded)
        unique_values <- unique(df[[input$categorical_pre_uploaded]])
        
        return(
          tagList(
            fluidRow(
              HTML("<p>With the categorical variable set, we now <b>choose the 2 samples</b> we would like to analyse:</p>"),
              column(6,
                     selectInput(
                       ns("sample_1_pre_uploaded"),
                       HTML("<p><b>Sample 1:</b></p>"),
                       unique_values,
                       selected = unique_values[1]
                     )
              ),
              column(6,
                     selectInput(
                       ns("sample_2_pre_uploaded"),
                       HTML("<p><b>Sample 2:</b></p>"),
                       unique_values,
                       selected = unique_values[2]
                     )
              )
            )
          )
        )
      })
      
      # Dependent Variable Selection
      output$dependent_variable_selection_pre_uploaded_data <- renderUI({
        req(input$data_set_pre_uploaded)
        req(input$categorical_pre_uploaded)
        req(input$sample_1_pre_uploaded)
        req(input$sample_2_pre_uploaded)
        
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
              HTML("<p>Finally, we select the <b>dependent variable</b>. This is the variable that each obervation in the samples are being measured against.</p>"),
              column(6,
                     selectInput(
                       ns("dependent_variable"),
                       NULL,
                       numeric_cols,
                       selected = numeric_cols[1]
                     )
              ),
              HTML("<br><br><br><br>")
            ),
          )
        )
      })
      
      # Warning message when for the pre-uploaded option, samples 1 and 2 are the same.
      # Also, is the function where the data is saved into the reactive variables.
      output$pre_uploaded_warning_and_save_data <- renderUI({
        req(input$data_set_pre_uploaded)
        req(input$categorical_pre_uploaded)
        req(input$sample_1_pre_uploaded)
        req(input$sample_2_pre_uploaded)
        req(input$dependent_variable)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "pre_uploaded") {
          return()
        }
        
        # Warning message when the conditions are the same.
        if (input$sample_1_pre_uploaded == input$sample_2_pre_uploaded) {
          data_sample_1(NULL)
          data_sample_2(NULL)
          return(
            tagList(
              HTML("<span style='color: red;'><p>Warning: Sample 1 and Sample 2 should be different.</p></span>"),
            )
          )
          
          # Otherwise, set the data.
        } else {
          
          df = get(input$data_set_pre_uploaded)
          temp_1 = df[as.character(df[[input$categorical_pre_uploaded]]) == input$sample_1_pre_uploaded, input$dependent_variable]
          data_sample_1(temp_1)
          temp_2 = df[as.character(df[[input$categorical_pre_uploaded]]) == input$sample_2_pre_uploaded, input$dependent_variable]
          data_sample_2(temp_2)
          return()
        }
      })
      
      # Radio button allowing user to choose which data they want to plot.
      output$radio_button_plot_type <- renderUI({
        
        # If data has not been uploaded, don't display anything.
        if (is.null(data_sample_1()) || is.null(data_sample_2())) {
          return()
        }
        
        return(
          radioButtons(ns("plot_type"), NULL, c(
            "Box plot" = "Box_plot",
            "Histogram" = "Histogram" 
          ), inline=T)
        )
      })
      
      # Plot UI or warning message output.
      output$data_upload_plot_section_output <- renderUI({
        
        # Text to render alerting the user that they can't proceed until they make a data choice.
        # This is when no data has been set.
        if (is.null(data_sample_1())) {
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
        
        req(input$plot_type)
        
        # If data has not been uploaded, don't display anything.
        if (is.null(data_sample_1()) || is.null(data_sample_2())) {
          return()
        }
        
        if (input$plot_type == "Box_plot") {
          
          boxplot(list(
            `Sample 1` = data_sample_1(), 
            `Sample 2` = data_sample_2()
          ),
          horizontal = TRUE,
          col = c("blue", "red"),
          main = "Side-by-side Boxplots",
          ylab = "Samples")
          
          # Histograms
        } else {
          
          # Save original plotting settings
          old_par <- par(no.readonly = TRUE)
          
          # Stack plots: 2 rows, 1 column
          par(mfrow = c(2, 1))
          
          # Histogram - Sample 1
          hist(data_sample_1(),
               breaks = 30,
               col = "blue",
               border = "black",
               main = "Sample 1 Histogram",
               xlab = "Values",
               ylab = "Frequency")
          
          # Histogram - Sample 2
          hist(data_sample_2(),
               breaks = 30,
               col = "red",
               border = "black",
               main = "Sample 2 Histogram",
               xlab = "Values",
               ylab = "Frequency")
          
          # Restore settings
          par(old_par)
        }
      })
      
      # To make the data accessible outside of the module.
      list(data_sample_1 = data_sample_1, 
           data_sample_2 = data_sample_2)
      
      
    }
    
  )
}
