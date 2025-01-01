source("libraries.R")
source("Box-Model/box_model_main_srv.R")
source("Box-Model/box_model_main_ui.R")
source("Proportion-Test/proportion_test_srv.R")
source("Proportion-Test/proportion_test_ui.R")

options(shiny.autoreload = TRUE)

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Hypothesis Tests Visualised"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      
      menuItem("Fundamentals" , tabname = "Fundamentals", startExpanded = FALSE,
               menuSubItem("The Box Model", tabName = "box_playground")),
      
      menuItem("z-test" , tabname = "z_test", startExpanded = FALSE,
                menuSubItem("1-Sample z-test", tabName = "1_sample_z_test"),
                menuSubItem("Proportion (z-test)", tabName = "proportion_z_test")
               )
    )
  ),
  
  dashboardBody(
    
    # Make the browser zoomed in.
    tags$style(HTML("
      body {
        zoom: 1.25;
      }
    ")),
    
    # Remove top margin of the dashboard body.
    tags$style(HTML("
      .content-wrapper {
        margin-top: -10px !important;
      }
    ")),
    
    tags$head(tags$style(HTML("
      .content {
        padding-left: 25px;
        padding-right: 25px;
      }
    "))),
    
    # Make the modal wider.
    tags$style(HTML("
        .modal-dialog {
          max-width: 1100px !important;
          width: 100% !important;
        }
      ")),
    
    # Make value box title smaller
    tags$style(HTML("
        .small-box h3 {
          font-size: 25px
        }
      ")),

    tabItems(
      
      # Home page.
      tabItem(tabName = "home",
              
              HTML("<br>"),
              
              fluidRow(
                column(6,
                       infoBox("Welcome!", 
                               "To get started, click on \"≡\" to navigate to your page of interest!",
                               icon = icon("arrow-left"), 
                               color = "light-blue", 
                               fill = TRUE,
                               width = "100%"),
                ),
                column(6,
                       tags$a(
                         href = "https://tjelton.com/2024/11/14/the-hypothesis-tests-visualised-project/", 
                         target = "_blank",
                         valueBox("About", "Click to find out more about the project",
                                  icon = icon("question"), color = "red", width = "100%")
                       ),
                       tags$a(
                         href = "https://docs.google.com/forms/d/e/1FAIpQLSdmyGBjQWRDg-ksPGqSsMuLNsuvnwwVgD0GW1bKvKVu6Rnplg/viewform?usp=sf_link", 
                         target = "_blank",
                         valueBox("Feedback", "Click to report errors or provide suggestions/feedback!",
                                  icon = icon("comment"), color = "olive", width = "100%")
                       )
                )
              ),
      ),
      
      # Box plot playground page.
      tabItem(tabName = "box_playground",
              boxModelMainUI("box_model"),
      ),
      
      tabItem(
        tabName = "1_sample_z_test",
        
        # Title Strip
        fluidRow(
          column(8,
                 HTML("<h1>1-Sample z-test</h1><br>"),
          ),
          column(4,
                 HTML("<br>"),
                 tags$style(HTML(paste0("
                  [id='learning_text'] {
                    font-size: 20px;
                    padding: 10px 20px;
                  }
                  "))),
                 actionButton("learning_text", "What is a 1-sample z-test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
          ),
        ),
        HTML("<br>"),
        
        ############ SECTION: Input Data ############
        fluidRow(
          column(7,
                 box(
                   title = HTML("<u><b>Input Sample Data</b></u>"),
                   status = "primary",
                   width = "100%",
                   solidHeader = FALSE,
                   HTML("<p>In order to do a 1-sample z-test, we first need to proivde our ONE sample that we will be analysing. Below you can
                        choose pre-uploaded data to analysis, or you can manually enter data.</p>"),
                   HTML("<br>"),
                   
                   # User specifies whether they will use inbuilt data or manually specify data.
                   radioButtons( 
                     inputId = "data_upload_choice", 
                     label = HTML("<p><b>What data source will you be using?</b></p>"), 
                     choices = list( 
                       "Pre-uploaded Data" = "pre_uploaded", 
                       "Manually Specified Data" = "manually_specified" 
                     ),
                     selected = NA
                   ),
                   
                   # Mechanism for user to specify data depending on radio button option.
                   uiOutput("data_upload"),
                   
                   # These will output only for the case that the "pre_uploaded" option is selected.
                   uiOutput("numeric_column_seleciton_pre_uploaded_data"),
                   uiOutput("separate_categorical_variable_pre_uploaded_data"),
                   uiOutput("category_choice_pre_uploaded_data"),
                   
                   # These will output only for the case that the "manually_specified" option is selected.
                   uiOutput("manual_entry_insufficient_unique_values"),
                   uiOutput("manual_entry_missing_values_warning")

                   
                 )
          ),
          column(5,
                 box(
                   solidHeader = TRUE,
                   width = "100%",
                   HTML("<p><b>TEST</b></p>"),
                 )
          )
        ),
        
        HTML("<br><br><br>"),
        
        h1("TEST"),
        radioButtons( 
          inputId = "radio", 
          label = "Radio buttons", 
          choices = list( 
            "Option 1" = 1, 
            "Option 2" = 2, 
            "Option 3" = 3 
          ) 
        ),
        conditionalPanel(
          
          condition = 'output.panelStatus',
          h1("IT WORKED")
        )
      ),
      
      # Box plot playground page.
      tabItem(tabName = "proportion_z_test",
              proportionTestMainUI("proportion_test"),
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  
  ############################ Uploading Data Mechanism ############################# 
  
  # Store the sample data.
  data <- reactiveVal(NULL)
  
  # Mechanism to allow the user to specify the data sample they will be using.
  output$data_upload <- renderUI({
    
    # Ensure some radio button option has been selected.
    req(input$data_upload_choice)
    
    # Pre-uploaded data
    if (input$data_upload_choice == "pre_uploaded") {
      return(
        tagList(
          HTML("<br>"),
          selectInput( 
            "data_set_pre_uploaded", 
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
            "manual_data_upload_textbox", 
            NULL, 
            value = ""
          ),
          
          # Upload action button. Wrapped in a fluid row to make it right-aligned.
          fluidRow(
            column(8),
            column(4,
               actionButton(
                 inputId = "load_manual_data",
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
          "column_select_pre_uploaded", 
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
          "factor_filtering_select_pre_uploaded", 
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
          "specific_category_select_pre_uploaded", 
          "(OPTIONAL) Select which column to filter on.", 
          different_categories
        )
      )
    )
  })
  
  # Re-update the data store if a filter is selected.
  observeEvent(input$specific_category_select_pre_uploaded, {
    data_to_store = get(input$data_set_pre_uploaded)
    data_to_store = data_to_store %>%
      filter(data_to_store[[input$factor_filtering_select_pre_uploaded]] == input$specific_category_select_pre_uploaded)
    data(data_to_store[[input$column_select_pre_uploaded]])
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
      string = paste("<span style='color: red;'><p>Warning: You must have at least two unique values in your manually specified data.</p></span>")
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
  
  
  ################################################################
  

  output$panelStatus <- reactive({
    input$radio==2
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  
  
  
  
  boxModelMainServer(id = "box_model")
  
  proportionTestMainServer(id = "proportion_test")
  
}

# Run the application
shinyApp(ui, server, enableBookmarking = "url")