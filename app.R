source("libraries.R")
source("linking_source_files.R")

options(shiny.autoreload = TRUE)
#options(shiny.reactlog = TRUE)

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
                menuSubItem("T-Curve Motivation", tabName = "t_curve_motivation"),
                menuSubItem("1-Sample T-Test", tabName = "1_sample_t_test"),
                menuSubItem("Paired T-Test", tabName = "paired_t_test")
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
      ),
      
      tabItem(tabName = "1_sample_t_test",
              oneSampleTTestUI("1_sample_t_test"),
      ),
      
      tabItem(tabName = "paired_t_test",
              
              # Title Strip
              fluidRow(
                column(8,
                       HTML("<h1>Paired t-Test (Work In Progress)</h1><br>"),
                ),
                column(4,
                       HTML("<br>"),
                       tags$style(HTML(paste0("
                          [id='learning_text'] {
                            font-size: 20px;
                            padding: 10px 20px;
                          }
                          "))),
                       actionButton("learning_text", "What is a paired t-test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
                ),
              ),
              HTML("<br>"),
              
              ############ SECTION: Input Data ############
              load_data_paired_sample_UI("loading_data")
              
      )
      
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  ############################ Uploading Data Mechanism ############################# 
  
  sample_data_session <- load_data_paired_sample_Server(id = "loading_data")
  
  # Store the sample data.
  # sample_data <- reactiveVal(NULL)
  
  # Variable that is true when the data has been specified, meaning the rest of the exercise can commence
  # This variable can be accessed by the ui conditional panel.
  # output$render_rest_of_exercise <- reactive({
  #   !is.null(sample_data_session$data())
  #   sample_data(sample_data_session$data())
  # })
  # outputOptions(output, "render_rest_of_exercise", suspendWhenHidden = FALSE)
  
  ################################################################
  
  

  boxModelMainServer(id = "box_model")
  
  proportionTestMainServer(id = "proportion_test")
  
  oneSampleZTestServer(id = "1_sample_z_test")
  
  tCurveMotivationServer(id = "t_curve_motivation")
  
  oneSampleTTestServer(id = "1_sample_t_test")
  
}

enableBookmarking(store = "url")


# Run the application
shinyApp(ui, server)