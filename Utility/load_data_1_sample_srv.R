load_1_sample_data_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Store the sample data.
      data <- reactiveVal(NULL)
      
      # To trigger the pre_uploaded functions to re-run so data is re-updated.
      re_run_flag <- reactiveVal(FALSE)
      
      # Mechanism to allow the user to specify the data sample they will be using.
      output$data_upload <- renderUI({
        
        # Ensure some radio button option has been selected.
        req(input$data_upload_choice)
        
        data(NULL)
        
        # Pre-uploaded data
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
                list("ChickWeight", "Orange", "PlantGrowth", "ToothGrowth", "chickwts", "iris") 
              ),
              HTML("<p><i>Note: These are common data sets. If you want to learn more about them, feel free to look on Google!<i></p>")
            )
          )
        }
        
        # Manual upload data.
        if (input$data_upload_choice == "manually_specified") {
          return(
            tagList(
              HTML("<br>"),
              HTML("<p><b>Enter data into the text box below.</b>
            <ul>
              <li>All values must be numeric, and numbers should contain no spaces or any characters other than a '.' for a decimal place.</li>
              <li>Each value must be on it's own line, or comma seperated.</li>
              <li>Don't forget to press 'Upload' once you are finished!</li>
            </ul></p>"),
              
              # Textbox
              textAreaInput( 
                ns("manual_data_upload_textbox"), 
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
      
      # When the user chooses a pre uploaded data set, prompt them to choose the numeric column they wish to analyse.
      output$numeric_column_seleciton_pre_uploaded_data <- renderUI({
        req(input$data_set_pre_uploaded)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "pre_uploaded") {
          return()
        }
        
        # Get numeric columns.
        data = get(input$data_set_pre_uploaded)
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        
        # Select button for the identified numeric columns.
        return(
          tagList(
            selectInput( 
              ns("column_select_pre_uploaded"), 
              "Which column containing numeric deata would you like to analyse?", 
              numeric_cols
            )
          )
        )
      })
      
      # Update the data store once the column has been selected.
      observeEvent(input$column_select_pre_uploaded, {
        data_to_store = get(input$data_set_pre_uploaded)[[input$column_select_pre_uploaded]]
        data(data_to_store)
      })
      
      # Optional button for if the user want to separate their data by a categorical variable.
      output$separate_categorical_variable_pre_uploaded_data <- renderUI({
        req(input$column_select_pre_uploaded)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "pre_uploaded") {
          return()
        }
        
        # Get factor columns.
        data = get(input$data_set_pre_uploaded)
        factor_cols <- names(data)[sapply(data, is.factor)]
        
        factor_cols = append("(None)", factor_cols)
        
        # Select button for the identified factor columns to optionally separate on.
        return(
          tagList(
            selectInput( 
              ns("factor_filtering_select_pre_uploaded"), 
              "(OPTIONAL) Below, you can optionally indicate if you want to separate your identified numeric variable by a categorical variable.", 
              factor_cols
            )
          )
        )
        
      })
      
      # If the user takes up the option to separate by a categorical variable, get the category they wish to filter with.
      output$category_choice_pre_uploaded_data <- renderUI({
        
        req(input$factor_filtering_select_pre_uploaded)
        
        # Only display if the pre_uploaded radio button option is selected and "(None)" is not selected for the category.
        if (input$data_upload_choice != "pre_uploaded" || input$factor_filtering_select_pre_uploaded == "(None)") {
          return()
        }
        
        different_categories = levels(get(input$data_set_pre_uploaded)[[input$factor_filtering_select_pre_uploaded]])
        
        # Select button for the different categories of the identified column.
        return(
          tagList(
            selectInput( 
              ns("specific_category_select_pre_uploaded"), 
              "(OPTIONAL) Select which column to filter on.", 
              different_categories
            )
          )
        )
      })
      
      # Re-update the data store if a filter is selected.
      observeEvent(input$specific_category_select_pre_uploaded, {
        if (input$factor_filtering_select_pre_uploaded == "(None)") {
          return()
        }
        data_to_store = get(input$data_set_pre_uploaded)
        data_to_store = data_to_store %>%
          filter(data_to_store[[input$factor_filtering_select_pre_uploaded]] == input$specific_category_select_pre_uploaded)
        data(data_to_store[[input$column_select_pre_uploaded]])
      })
      
      # Used to update the data when returning to the "pre-uploaded data" state.
      observe({
        re_run_flag()
        req(input$data_set_pre_uploaded)
        req(input$column_select_pre_uploaded)
        data_to_store = get(input$data_set_pre_uploaded)[[input$column_select_pre_uploaded]]
        data(data_to_store)
      })
      
      num_missing_values_manual_upload <- reactiveVal(0)
      error_message_insufficient_unique_values_manual_upload <- reactiveVal(FALSE)
      
      # Observe if the "Upload" button is clicked. This will involve checking the data, and triggering a warning if the data is not in the correct form.
      observeEvent(input$load_manual_data, {
        
        req(input$manual_data_upload_textbox)
        
        input_string = input$manual_data_upload_textbox
        
        # Split string at commas and newline characters.
        splits <- strsplit(input_string, "[,\n]")[[1]]
        
        # Remove any instances of empty strings.
        splits <- trimws(splits)
        splits = splits[splits != ""]
        
        # Convert the values to numbers.
        numeric_vec <- as.numeric(splits)
        
        # Check that there are more than one unique value (table length will be greater than 1). Otherwise, sd = 0, and the test will break.
        adequate_unique <- length(table(numeric_vec)) > 1
        
        # In the case that there are not unique values, raise an error with the user.
        if (adequate_unique == FALSE) {
          num_missing_values_manual_upload(0)
          error_message_insufficient_unique_values_manual_upload(TRUE)
          data(NULL)
          return()
        }
        
        error_message_insufficient_unique_values_manual_upload(FALSE)
        
        # Count number of NA values (NA values mean that there was an issue with the data upload).
        na_count <- sum(sapply(numeric_vec, function(x) is.na(x)))
        num_missing_values_manual_upload(na_count)
        
        # Updata the data store with manual data
        data(numeric_vec)
      })
      
      # Warning message notifying that there were some NA values found when manually updating the values.
      output$manual_entry_missing_values_warning <- renderUI({
        req(input$data_upload_choice)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "manually_specified") {
          return()
        }
        
        if (num_missing_values_manual_upload() > 0) {
          string = paste("<span style='color: red;'><p>Warning: From the data that you uploaded, ", as.character(num_missing_values_manual_upload()), " of the values
                     could not be interpreted. This could be becuase these values were not numeric, or because you did not specify the data into the required format.</p></span>")
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
      
      # Plot of data
      output$initial_data_plots = renderPlot({
        
        # If data has not been uploaded, don't display anything.
        if (is.null(data())) {
          return()
        }
        
        data_plotting = data.frame(data())
        
        # Boxplot of data
        boxplot = data_plotting %>%
          ggplot() +
          aes(x = `data..`) +
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
          aes(x = `data..`) +
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
      
      # Plot UI or warning message output.
      output$data_upload_plot_section_output <- renderUI({
        
        # Text to render alerting the user that they can't proceed until they make a data choice.
        # This is when no data has been set.
        if (is.null(data())) {
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
      
      # To make the data accessible outside of the module.
      list(data = data)
      
    }

  )
}
