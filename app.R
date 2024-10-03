source("libraries.R")
source("Box-Model/template_srv.R")
source("Box-Model/template_ui.R")

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
    
    # Change the margin of all text.
    tags$style(HTML("
      .content-wrapper p,
      .content-wrapper h1,
      .content-wrapper h2,
      .content-wrapper h3,
      .content-wrapper h4,
      .content-wrapper h5,
      .content-wrapper h6 {
        padding-left: 15px !important;
        padding-right: 15px !important;
      }
    ")),
    
    tabItems(
      
      # Home page.
      tabItem(tabName = "home",
              fluidRow(
                HTML("<h1>Home</h1><br>"),
                HTML("<p><b>Welcome! To get started, click on \"≡\" to navigate to your page of interest!</b></p>"),
                HTML("<p>More coming to the home page and this application soon...</p>")
              )
      ),

      # Box plot playground page.
      tabItem(tabName = "box_playground",
              
              
              # Tab Title
              HTML("<h1>Box Model</h1><br>"),
              

              templateUI("template")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # Uncomment to see the bs_themer()
  #bs_themer() #Flatly, #Spacelab, #Zephyr

  templateServer(id = "template")

  

}

# Run the application
shinyApp(ui, server)


# ui  <- tagList(
#   
#   fluidPage(
#     
#     theme = bs_theme(version = 5, bootswatch = "zephyr"),
#     pushbar_deps(),
#     
#     pushbar(
#       HTML("<p><center>Menu</center></p>"),
#       id = "myPushbar", # add id to get event
#       
#       navlistPanel(
#         id = "navlist",
#         well = TRUE,
#         
#         # First pill
#         tabPanel("Pill 1",
#                  h4("Content for Pill 1"),
#                  p("This is some content for the first pill.")
#         ),
#         
#         # Second pill
#         tabPanel("Pill 2",
#                  h4("Content for Pill 2"),
#                  p("This is some content for the second pill.")
#         ),
#         
#         # Third pill
#         tabPanel("Pill 3",
#                  h4("Content for Pill 3"),
#                  p("This is some content for the third pill.")
#         )
#       ),
#       
#       actionButton("close", "Close pushbar")
#     ),
#   
#     fixedPanel(
#       actionButton("open", label = "←", width = 45, height = 45),
#       left = 10,
#       top = 10,
#     ),
#     
#     # tabPanel(
#     #   # Tab Title
#     #   HTML('<h7 style="padding-left: 10px; padding-right: 10px; display: inline;">Page 1</h5>'),
#     # 
#     #   "Home page"
#     # ),
#     # 
#     # tabPanel(
#     #   
#     #   # Tab Title
#     #   HTML('<h7 style="padding-left: 10px; padding-right: 10px; display: inline;">Analysis Components</h5>'),
#     #   
#     #   analysisUI("analysis")
#     # )
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   # Uncomment to see the bs_themer()
#   #bs_themer() #Flatly, #Spacelab, #Zephyr
#   
#   # analysisServer(id = "analysis")
#   
#   setup_pushbar() # setup
#   
#   observeEvent(input$open, {
#     pushbar_open(id = "myPushbar")
#   })  
#   
#   observeEvent(input$close, {
#     pushbar_close()
#   })
#   
# }
# 
# 
# shinyApp(ui = ui, server = server)