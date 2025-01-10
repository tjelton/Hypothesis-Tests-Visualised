source("libraries.R")
source("Box-Model/box_model_main_srv.R")
source("Box-Model/box_model_main_ui.R")
source("z-test/proportion_test_srv.R")
source("z-test/proportion_test_ui.R")
source("z-test/1_sample_z_test_ui.R")
source("z-test/1_sample_z_test_srv.R")
source("t-test/t_curve_motivation_ui.R")
source("t-test/t_curve_motivation_srv.R")
source("Utility/load_data_1_sample_srv.R")
source("Utility/load_data_1_sample_ui.R")
source("Utility/generic_plotting_functions.R")

options(shiny.autoreload = TRUE)
options(shiny.reactlog = TRUE)


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
      
      menuItem("Z-Test" , tabname = "z_test", startExpanded = FALSE,
                menuSubItem("1-Sample Z-Test", tabName = "1_sample_z_test"),
                menuSubItem("Proportion (Z-Test)", tabName = "proportion_z_test")
               ),
      
      menuItem("T-Test", tabname = "t_test", startExpanded = FALSE,
                menuSubItem("T-Curve Motivation", tabName = "t_curve_motivation")
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
      
      
      tabItem(tabName = "box_playground",
              boxModelMainUI("box_model"),
      ),
      
      tabItem(tabName = "1_sample_z_test",
              oneSampleZTestUI("1_sample_z_test"),
      ),
      
      tabItem(tabName = "proportion_z_test",
              proportionTestMainUI("proportion_test"),
      ),
      
      tabItem(tabName = "t_curve_motivation",
              tCurveMotivationUI("t_curve_motivation")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  boxModelMainServer(id = "box_model")
  
  proportionTestMainServer(id = "proportion_test")
  
  oneSampleZTestServer(id = "1_sample_z_test")
  
  tCurveMotivationServer(id = "t_curve_motivation")
  
}

# Run the application
shinyApp(ui, server, enableBookmarking = "url")