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