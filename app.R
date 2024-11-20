source("libraries.R")
source("Box-Model/box_model_main_srv.R")
source("Box-Model/box_model_main_ui.R")
source("Proportion-Test/proportion_test_srv.R")
source("Proportion-Test/proportion_test_ui.R")

library(gmp)

options(shiny.autoreload = TRUE)

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Hypothesis Tests Visualised"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Box Model", tabName = "box_playground", icon = icon("box")),
      menuItem("Proportion (z-test)", tabName = "proportion_z_test", icon = icon("percent"))
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
          max-width: 1100px !important; /* Adjust this value for desired width */
          width: 100% !important;       /* Adjust this value for desired width */
        }
      ")),
    
    tabItems(
      
      # Home page.
      tabItem(tabName = "home",
              HTML("<h1>Home</h1><br>"),
              HTML("<p><b>Welcome! To get started, click on \"≡\" to navigate to your page of interest!</b></p>"),
              HTML("<p>More coming to the home page and this application soon...</p>"),
              
      ),
      
      # Box plot playground page.
      tabItem(tabName = "box_playground",
              boxModelMainUI("box_model"),
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
  
  boxModelMainServer(id = "box_model")
  
  proportionTestMainServer(id = "proportion_test")
  
}

# Run the application
shinyApp(ui, server, enableBookmarking = "url")