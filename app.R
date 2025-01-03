source("libraries.R")
source("Box-Model/box_model_main_srv.R")
source("Box-Model/box_model_main_ui.R")
source("Proportion-Test/proportion_test_srv.R")
source("Proportion-Test/proportion_test_ui.R")
source("Utility/load_data_1_sample_srv.R")
source("Utility/load_data_1_sample_ui.R")

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
        load_1_sample_data_UI("loading_data"),
 
        HTML("<br><br><br>"),
        
        
        conditionalPanel(
          condition = 'output.render_rest_of_exercise',
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

  sample_data_session <- load_1_sample_data_Server(id = "loading_data")
  
  # Variable that is true when the data has been specified, meaning the rest of the exercise can commence
  # This variable can be accessed by the ui conditional panel.
  output$render_rest_of_exercise <- reactive({
    !is.null(sample_data_session$data())
  })
  outputOptions(output, "render_rest_of_exercise", suspendWhenHidden = FALSE)
  
  ################################################################
  
  boxModelMainServer(id = "box_model")
  
  proportionTestMainServer(id = "proportion_test")
  
  
}

# Run the application
shinyApp(ui, server, enableBookmarking = "url")