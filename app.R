source("t-test/t_curve_motivation_ui.R")
source("t-test/t_curve_motivation_srv.R")
source("Utility/helper_functions.R")
source("Utility/generic_plotting_functions.R")

library(shiny)
library(bslib)
library(DiagrammeR)


ui <- page_navbar(
  title = "Hypothesis Tests Visualised",
  theme = bs_theme(version = 5, bootswatch = "lumen"),  # You can switch to "lumen", "materia", "sketchy", "united", "yeti" etc.
  
  nav(
    title = "Home",
    fluidPage(
      h2("Home Page"),
      p("This is the home page. I will place some inspirational text here...")
    )
  ),
  
  nav_menu("T-Tests",
           
           nav("T-Curve Motivation", 
               tCurveMotivationUI("t_curve_motivation")
           ),
           
           
           nav("1-Sample T-Test", "This is the details tab."),
           
           
           
           nav("Paired T-Test", "This is another sub-tab.")
  ),
  
)


server <- function(input, output, session) {
  
  
  tCurveMotivationServer(id = "t_curve_motivation")
  

}

shinyApp(ui, server)