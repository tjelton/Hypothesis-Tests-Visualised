load_data_regression_2_variable_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Store the sample data.
      data_x_axis <- reactiveVal(NULL)
      data_y_axis <- reactiveVal(NULL)
      
      # To trigger the pre_uploaded functions to re-run so data is re-updated.
      re_run_flag <- reactiveVal(FALSE)
      
      # Mechanism to allow the user to specify the data sample they will be using.
      output$data_upload <- renderUI({
        
        # Ensure some radio button option has been selected.
        req(input$data_upload_choice)
        
        data_x_axis(NULL)
        data_y_axis(NULL)
        
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
                list("iris", "mtcars", "trees", "airquality", "pressure") 
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
              HTML("<p><b>Enter data into the text box below for each sample.</b>
                <ul>
                  <li>All values must be numeric, and numbers should contain no spaces or any characters other than a '.' for a decimal place.</li>
                  <li>Each value must be on it's own line, or comma seperated.</li>
                  <li>There must be the same number of values in each textbox.</li>
                  <li>Don't forget to press 'Upload' once you are finished!</li>
                </ul></p><br>"),
              
              # Textbox -> condition 1 data entry.
              HTML("<p><b>x-axis:</b></p>"),
              textAreaInput( 
                ns("manual_data_upload_textbox_x_axis"), 
                NULL, 
                value = "",
                width = "100%"
              ),
              
              # Textbox -> condition 2 data entry.
              HTML("<p><b>y-axis:</b></p>"),
              textAreaInput( 
                ns("manual_data_upload_textbox_y_axis"), 
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
      
      # When the user chooses a pre uploaded data set, prompt them to choose the numeric column they wish to analyse.
      output$numeric_column_seleciton_pre_uploaded_data <- renderUI({
        req(input$data_set_pre_uploaded)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "pre_uploaded") {
          return()
        }
        
        # Get numeric columns
        data = get(input$data_set_pre_uploaded)
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        
        # Select button for the identified numeric columns.
        return(
          tagList(
            HTML("<br>"),
            fluidRow(
              column(6,
                     selectInput( 
                       ns("x_axis_column_select_pre_uploaded"), 
                       HTML("<p><b>Select x-axis data:</b></p>"), 
                       numeric_cols,
                       selected = numeric_cols[1]
                     )
              ),
              column(6,
                     selectInput( 
                       ns("y_axis_column_select_pre_uploaded"), 
                       HTML("<p><b>Select y-axis data:</b></p>"), 
                       numeric_cols,
                       selected = numeric_cols[2]
                     )
              )
            )
            
          )
        )
      })
      
      # Update the data store once the column has been selected.
      observeEvent(input$x_axis_column_select_pre_uploaded, {
        data_to_store = get(input$data_set_pre_uploaded)[[input$x_axis_column_select_pre_uploaded]]
        data_x_axis(data_to_store)
      })
      observeEvent(input$y_axis_column_select_pre_uploaded, {
        data_to_store = get(input$data_set_pre_uploaded)[[input$y_axis_column_select_pre_uploaded]]
        data_y_axis(data_to_store)
      })
      
      # Optional button for if the user want to separate their data by a categorical variable.
      output$separate_categorical_variable_pre_uploaded_data <- renderUI({
        req(input$x_axis_column_select_pre_uploaded)
        req(input$y_axis_column_select_pre_uploaded)
        
        # Only display if the pre_uploaded radio button option is selected.
        if (input$data_upload_choice != "pre_uploaded") {
          return()
        }
        
        # Get factor columns.
        data = get(input$data_set_pre_uploaded)
        factor_cols <- names(data)[sapply(data, is.factor)]
        
        # If no factor columns, return nothing.
        if (length(factor_cols) == 0) {
          return(NULL)
        }
        
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
        
        # Filter the data.
        data_to_store <- data_to_store[data_to_store[[input$factor_filtering_select_pre_uploaded]] == input$specific_category_select_pre_uploaded, ]
        
        # Extract the selected column.
        data_x_axis(data_to_store[[input$x_axis_column_select_pre_uploaded]])
        data_y_axis(data_to_store[[input$y_axis_column_select_pre_uploaded]])
      })
      
      # Used to update the data when returning to the "pre-uploaded data" state.
      observe({
        re_run_flag()
        req(input$data_set_pre_uploaded)
        req(input$column_select_pre_uploaded)
        
        if (input$data_set_pre_uploaded == "Mr. Han's Math Class") {
          data_to_store = Han_math_numbers[[input$column_select_pre_uploaded]]
          
          #   Otherwise, user has selected data prebuilt into R.
        } else {
          data_to_store = get(input$data_set_pre_uploaded)[[input$column_select_pre_uploaded]]
        }
        data(data_to_store)
      })
      
      # Reactive variables to keep track of the number of values entered for each condition.
      num_values_x_axis = reactiveVal(NULL)
      num_values_y_axis = reactiveVal(NULL)
      error_message_unequal_samples = reactiveVal(FALSE)
      error_message_insufficient_unique_values_manual_upload = reactiveVal(FALSE)
      
      # Observe if the "Upload" button is clicked. This will involve checking the data, and triggering a warning if the data is not in the correct form.
      observeEvent(input$load_manual_data, {
        
        req(input$manual_data_upload_textbox_x_axis)
        req(input$manual_data_upload_textbox_y_axis)
        
        # Re-set warning messages
        error_message_unequal_samples(FALSE)
        error_message_insufficient_unique_values_manual_upload(FALSE)
        
        data_x_axis(NULL)
        data_y_axis(NULL)
        
        x_axis = input$manual_data_upload_textbox_x_axis
        y_axis = input$manual_data_upload_textbox_y_axis
        
        # Process condition 1 data.
        splits_x_axis <- strsplit(x_axis, "[,\n]")[[1]]
        splits_x_axis <- trimws(splits_x_axis)
        splits_x_axis = splits_x_axis[splits_x_axis != ""]
        numeric_x_axis = as.numeric(splits_x_axis)
        numeric_x_axis = numeric_x_axis[!is.na(numeric_x_axis)]
        
        # Process condition 2 data.
        splits_y_axis <- strsplit(y_axis, "[,\n]")[[1]]
        splits_y_axis <- trimws(splits_y_axis)
        splits_y_axis = splits_y_axis[splits_y_axis != ""]
        numeric_y_axis = as.numeric(splits_y_axis)
        numeric_y_axis = numeric_y_axis[!is.na(numeric_y_axis)]
        
        # Check if the data is 1-1, otherwise, trigger an error message.
        if (length(numeric_x_axis) != length(numeric_y_axis)) {
          num_values_x_axis(length(numeric_x_axis))
          num_values_y_axis(length(numeric_y_axis))
          error_message_unequal_samples(TRUE)
          return()
        }
        
        # Check that there are more than one unique value (table length will be greater than 1). Otherwise, sd = 0, and the test will break.
        adequate_unique <- length(table(numeric_x_axis)) > 1 || length(table(numeric_y_axis)) > 1
        
        # In the case that there are not unique values, raise an error with the user.
        if (adequate_unique == FALSE) {
          error_message_insufficient_unique_values_manual_upload(TRUE)
          return()
        }
        
        data_x_axis(numeric_x_axis)
        data_y_axis(numeric_y_axis)
      })
      
      
      # Error message for when not enogh unique values.
      output$manual_entry_unequal_samples <- renderUI({
        req(input$data_upload_choice)
        
        # Only display if the manually_specified radio button option is selected.
        if (input$data_upload_choice != "manually_specified") {
          return()
        }
        
        if (error_message_unequal_samples()) {
          string = paste("<span style='color: red;'><p>Warning: The number of values in each condition is unequal. Condition 1 has ", 
                         as.character(num_values_x_axis()), " values, and condition 2 has ", as.character(num_values_y_axis()), " values.</p></span>", sep = "")
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
        if (is.null(data_x_axis()) || is.null(data_y_axis())) {
          return()
        }
        
        plot(data_x_axis(), data_y_axis(), xlab = "x-axis", ylab = "y-axis")
        
      })
      
      # Plot UI or warning message output.
      output$data_upload_plot_section_output <- renderUI({
        
        # Text to render alerting the user that they can't proceed until they make a data choice.
        # This is when no data has been set.
        if (is.null(data_x_axis()) || is.null(data_y_axis())) {
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
      
      # To make the data accessible outside of the module.
      list(data_x_axis = data_x_axis, 
           data_y_axis = data_y_axis)

    }
    
  )
}
