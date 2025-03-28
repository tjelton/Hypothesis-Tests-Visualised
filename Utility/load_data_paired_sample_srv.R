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
              HTML("<br>"),
              selectInput( 
                ns("data_set_pre_uploaded"), 
                "Which data set would you like to analyse?", 
                list("Blink", "BloodLead", "GDO", "GrapeFruit", "HorseBeginners", "Iron", "Meat", "PrisonStress") 
              ),
              HTML("<p><i>Warning: Some of these data sets are not truly paired. However, they are still included here to have more data for experimentation.</i></p>"),
              HTML("<p>Now we need to select what data we want to be in condition 1, and what to be in condition 2. The paired difference will be condition 2 - condition 1.</p>")
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
            </ul></p>"),
              
              # Textbox -> condition 1 data entry.
              HTML("<b>Condition 1:</b>"),
              textAreaInput( 
                ns("manual_data_upload_textbox_condition_1"), 
                NULL, 
                value = ""
              ),
              
              # Textbox -> condition 2 data entry.
              HTML("<b>Condition 2:</b>"),
              textAreaInput( 
                ns("manual_data_upload_textbox_condition_2"), 
                NULL, 
                value = ""
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
                       "Condition 1",
                       numeric_cols,
                       selected = numeric_cols[1]
                     )
              ),
              column(6,
                     selectInput(
                       ns("condition_2_pre_uploaded"),
                       "Condition 2",
                       numeric_cols,
                       selected = numeric_cols[2]
                     )
              )
            )
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
          box(
            solidHeader = TRUE,
            width = "100%",
            radioButtons(ns("data_to_plot"), NULL, c("Condition 1" = "Condition_1", 
                                                 "Condition 2" = "Condition_2", 
                                                 "Paired Difference" = "Paired_Difference"), inline=T)
          )
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
              box(
                solidHeader = TRUE,
                width = "100%",
                HTML(string)
              )
            )
          )
        }
        
        # If data has been set, display the plots.
        return(
          tagList(
            plotOutput(ns("initial_data_plots"),  width = "80%")
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
        
        # Boxplot of data
        boxplot = data_plotting %>%
          ggplot() +
          aes(x = data_plotting) +
          geom_boxplot(fill = "blue") +
          labs(title = "Boxplot", x = "Values") +
          theme_minimal() + 
          theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
          )
        
        # Histogram of data
        histogram = data_plotting %>%
          ggplot() +
          aes(x = data_plotting) +
          geom_histogram(bins = 30, fill = "blue", color = "black") +
          labs(title = "Histogram", x = "Values", y = "Frequency") +
          theme_minimal()
        
        # Place plots on top of each other using cowplot.
        combined = plot_grid(
          boxplot,
          histogram,
          ncol = 1,
          rel_heights = c(1,2)
        )
        
        return(combined)
        
      })
      
      # To make the data accessible outside of the module.
      list(data_condition_1 = data_condition_1, 
           data_condition_2 = data_condition_2, 
           data_paired_difference = data_paired_difference)
      
    }
    
  )
}
