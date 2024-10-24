source("libraries.R")
source("Box-Model/box_model_main_srv.R")
source("Box-Model/box_model_main_ui.R")

library(DiagrammeR)
library(rafalib)



options(shiny.autoreload = TRUE)


ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Hypothesis Tests Visualised"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Box Model", tabName = "box_playground", icon = icon("box"))
    )
  ),
  
  dashboardBody(
    
    # Make the browser zoomed in.
    tags$style(HTML("
      body {
        zoom: 1.1;
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
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  boxModelMainServer(id = "box_model")
  
}

# Run the application
shinyApp(ui, server)