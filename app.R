source("libraries.R")
source("Box-Model/box_model_main_srv.R")
source("Box-Model/box_model_main_ui.R")
source("Proportion-Test/proportion_test_srv.R")
source("Proportion-Test/proportion_test_ui.R")
source("Utility/load_data_1_sample_srv.R")
source("Utility/load_data_1_sample_ui.R")

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
          
              ############ SECTION: The NULL Hypothesis - Setting up the Box ############
              fluidRow(
                column(7,
                       box(
                         title = HTML("<u><b>The 'NULL' Hypothesis - Setting up the Box</b></u>"),
                         status = "primary",
                         width = "100%",
                         solidHeader = FALSE,
                         
                         p("We start by using the box model to represent our null hypothesis."),
                         
                         # Step 1: Specify Population Standard Deviation
                         box(
                           title = "Step 1) Specify Population Standard Deviation (σ)",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = FALSE,
                           status = "info",
                           solidHeader = FALSE,
                           withMathJax(HTML("<p>An assumption for the 1-sample z-test is that the standard deviation of the population (denoted \\(\\sigma\\)) in which our sample is drawn is known. For example,
                                a prior research paper may have estimated the population standard deviation, and you could use the value they discovered.<br><br>
                                
                                For this exercise, you can manually set the population standard deviation. However, for the most part, it is likely that the population standard
                                deviation is unknown. Hence, for the purposes of this exercise, you can set the population standard deviation to be equal to the sample standard
                                deviation (this has been automatically done below, but you are free to change the value). In practice, this is far from ideal, and in a future exercise, we will use the 1-sample t-test when the population standard deviation
                                is unknown.</p>")),
                           HTML("<br>"),
                           fluidRow(
                             column(7,
                                    actionButton(
                                      inputId = "set_pop_sd_to_sample",
                                      label = "Set Population SD to be Sample SD",
                                      class = "btn-primary",
                                      style = "color: #fff;",
                                      width = "100%"
                                    )
                             ),
                             column(2,
                                    withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\(\\sigma = \\)</p>"))
                             ),
                             column(3,
                                    uiOutput("pop_sd_numeric_input") 
                             ),
                           ),
                         ),
                         
                         # Step 2: Specify NULL Hypothesis
                         box(
                           title = "Step 2) Specify NULL Hypothesis",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>Next we need to specify the 'null' hypothesis. In this test we are focusing on the mean, so the null hypothesis is that the population mean
                                  is equal to some value which we set below.</p>"),
                           
                           withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>Given \\(\\mu \\), where \\(\\mu \\) is the mean of some variable:</p>")),
                           fluidRow(
                             column(7),
                             column(2,
                                    withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( H_0: \\) \\(\\mu = \\)</p>"))
                             ),
                             column(3,
                                    numericInput(
                                      "null_mu",
                                      NULL,
                                      value = 0
                                    ),
                             ),
                           ),
                         ),
                         
                         
                         # Other
                         box(
                           title = "Other",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>
                           <ul>
                              <li>The value for <b>n</b> comes from the number of values in the sample chosen above.</li>
                              <li>The <b>observed value (OV)</b> comes from the mean of the sample chosen above.</li>
                           </ul></p>")
                         )
                       )
                ),
                column(5,
                       # Box model output.
                       box(
                         solidHeader = TRUE,
                         width = "100%",
                         HTML("<center>"),
                         grVizOutput("box_model", width = "70%", height = "70%"),
                         HTML("</center>")
                       ),
                )
              ),
          
          
              HTML("<br><br><br>"),

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
  
  # Store the sample data.
  sample_data <- reactiveVal(NULL)
  
  # Variable that is true when the data has been specified, meaning the rest of the exercise can commence
  # This variable can be accessed by the ui conditional panel.
  output$render_rest_of_exercise <- reactive({
    !is.null(sample_data_session$data())
    sample_data(sample_data_session$data())
  })
  outputOptions(output, "render_rest_of_exercise", suspendWhenHidden = FALSE)
  
  ################################################################
  
  # Numeric text field to enter the population sd.
  output$pop_sd_numeric_input <- renderUI({
    sample_sd = sd(sample_data() ,na.rm = TRUE)
    numericInput(
      "population_standard_deviation_numeric",
      NULL,
      value = sample_sd,
      min = 0
    )
  })
  
  # Button logic to set the pop sd to the sample sd.
  observeEvent(input$set_pop_sd_to_sample, {
    req(input$population_standard_deviation_numeric)
    sample_sd = sd(sample_data() ,na.rm = TRUE)
    updateNumericInput(
      session,
      inputId = "population_standard_deviation_numeric",
      value = sample_sd
    )
  })
  
  # Override the sd value to be equal to the sample sd if it is less than or equal to 0.
  observeEvent(input$population_standard_deviation_numeric, {
    if (is.na(input$population_standard_deviation_numeric) || input$population_standard_deviation_numeric <= 0) {
      sample_sd = sd(sample_data() ,na.rm = TRUE)
      updateNumericInput(
        session,
        inputId = "population_standard_deviation_numeric",
        value = sample_sd
      )
    }
  })
  
  # Box model plot
  output$box_model <- renderGrViz({
    
    # String with mu and sigma.
    pop_details = paste("&mu; = ", as.character(round(input$null_mu, digits = 3)), "; &sigma; = ", as.character(round(input$population_standard_deviation_numeric, digits = 3)))
    
    # Set up graph and box
    diagram = "digraph diagram { graph [layout = dot, rankdir = TB] node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5] box [label = '"
    diagram = paste(diagram, pop_details, "']", sep = "")
    
    # Set up sample circle.
    diagram = paste(diagram, " node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12] sample [label = '", "OV = ",  
                    as.character(round(mean(sample_data(), na.rm = TRUE), digits = 3)), "']", sep = "")
    
    # Create edge between box and circle.
    # Annotate edge with n value.
    n = length(sample_data())
    diagram = paste(diagram, " edge [minlen = 2] box->sample [label = '  n = ", n, "', fontsize = 12, labeldistance = 5]}", sep = "")
    
    return (grViz(diagram))
    
  })
  
  
  
  
  
  
  boxModelMainServer(id = "box_model")
  
  proportionTestMainServer(id = "proportion_test")
  
  
}

# Run the application
shinyApp(ui, server, enableBookmarking = "url")