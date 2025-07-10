source("libraries.R")
source("linking_source_files.R")

ui <- page_navbar(
  title = "Hypothesis Tests Visualised",
  theme = bs_theme(version = 5, bootswatch = "lumen"),  # You can switch to "lumen", "materia", "sketchy", "united", "yeti" etc.
  
  # Make the modal wider.
    tags$style(HTML("
        .modal-dialog {
          max-width: 1100px !important;
          width: 100% !important;
        }
      ")),
  
  nav_panel(
    title = "Home",
    fluidPage(
      h2("Home Page"),
      p("This is the home page. I will place some inspirational text here...")
    )
  ),
  
  nav_menu("Fundamentals",
           
           nav_panel("The Box Model", 
                boxModelMainUI("box_model"),
           ),
  ),
  
  nav_menu("Z-Tests",
           
           nav_panel("1-Sample Z-Test",
                     oneSampleZTestUI("1_sample_z_test"),
           ),
           nav_panel("Proportion (Z-test)", 
                     proportionTestMainUI("proportion_z_test"),
           ),
  ),
  
  nav_menu("T-Tests",
           nav_panel("T-Curve Motivation", 
               tCurveMotivationUI("t_curve_motivation")
           ),
           nav_panel("1-Sample T-Test",
               oneSampleTTestUI("1_sample_t_test")
           ),
           nav_panel("Paired T-Test",
               pairedTTestUI("paired_t_test") 
           ),
           nav_panel("2-Sample T-Test",
               twoSampleTTestUI("2_sample_t_test")
           ),
           nav_panel("Regression T-Test",
               load_data_regression_2_variable_UI("loading_data"),
           )
  )
)


server <- function(input, output, session) {
  
  ############################ Uploading Data Mechanism ############################# 
  
  sample_data_session = load_data_regression_2_variable_Server(id = "loading_data")
  
  # Store the sample data.
  data_x_axis = reactiveVal(NULL)
  data_y_axis = reactiveVal(NULL)
  
  # Variable that is true when the data has been specified, meaning the rest of the exercise can commence
  # This variable can be accessed by the ui conditional panel.
  output$render_rest_of_exercise = reactive({
    !is.null(sample_data_session$data_x_axis())
    data_x_axis(sample_data_session$data_x_axis())
    data_y_axis(sample_data_session$data_y_axis())
  })
  outputOptions(output, "render_rest_of_exercise", suspendWhenHidden = FALSE)
  
  
  
  
  
  boxModelMainServer(id = "box_model")
  oneSampleZTestServer(id = "1_sample_z_test")
  proportionTestMainServer(id = "proportion_z_test")
  tCurveMotivationServer(id = "t_curve_motivation")
  oneSampleTTestServer(id = "1_sample_t_test")
  pairedTTestServer(id = "paired_t_test")
  twoSampleTTestServer(id = "2_sample_t_test")
  
  
}

shinyApp(ui, server)