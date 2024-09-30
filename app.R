source("Box-Model/template_srv.R")
source("Box-Model/template_ui.R")
source("libraries.R")

options(shiny.autoreload = TRUE) 

ui  <- tagList(
  
  navbarPage(
    
    theme = bs_theme(version = 5, bootswatch = "zephyr"),
    
    # Dashboard title.
    "SAP Report Generator",
    
    # tabPanel(
    #   # Tab Title
    #   HTML('<h7 style="padding-left: 10px; padding-right: 10px; display: inline;">Page 1</h5>'),
    # 
    #   "Home page"
    # ),
    # 
    # tabPanel(
    #   
    #   # Tab Title
    #   HTML('<h7 style="padding-left: 10px; padding-right: 10px; display: inline;">Analysis Components</h5>'),
    #   
    #   analysisUI("analysis")
    # )
  )
)


# ui <- fluidPage(
#   chartUI(id = "chart1"),
#   test_ui(id = "test1")
# )


server <- function(input, output, session) {
  
  # Uncomment to see the bs_themer()
  #bs_themer() #Flatly, #Spacelab, #Zephyr
  
  # analysisServer(id = "analysis")
  
  
  
  # chartServer(
  #   id = "chart1",
  #   x = c("Q1", "Q2", "Q3", "Q4"),
  #   y = c(505.21, 397.18, 591.44, 674.90),
  #   title = "Sales in 000 for 2023"
  # )
  # 
  # test_server(
  #   id = "test1",
  #   data = data
  # )
  
}


shinyApp(ui = ui, server = server)