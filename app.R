source("libraries.R")
source("Box-Model/box_model_main_srv.R")
source("Box-Model/box_model_main_ui.R")

library(gmp)

options(shiny.autoreload = TRUE)


ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Hypothesis Tests Visualised"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Proportion (Z-test)", tabName = "proportion_z_test", icon = icon("percent")),
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Box Model", tabName = "box_playground", icon = icon("box"))
    )
  ),
  
  dashboardBody(
    
    # Make the browser zoomed in.
    tags$style(HTML("
      body {
        zoom: 1.25;
      }
    ")),
    
    # tags$head(
    #   tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.8")
    # ),
    
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
              
              
              fluidRow(
                column(8,
                       HTML("<h1>Proportion Test (z-test)</h1><br>"),
                ),
                column(4,
                       HTML("<br>"),
                       tags$style(HTML(paste0("
                          [id='", "learning_text", "'] {
                            font-size: 20px; /* Adjust the font size as needed */
                            padding: 10px 20px; /* Optional: adjust padding for a larger button */
                          }
                       "))),
                       actionButton("learning_text", "What is the proportion test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
                ),
              ),
              HTML("<br>"),
              
              ############ SECTION: The NULL Hypotheis - Setting up the Box ############ 
              fluidRow(
                column(7,
                       box(
                           title = HTML("<u><b>The 'NULL' Hypothesis - Setting up the Box</b></u>"),
                           status = "primary", 
                           width = "100%",
                           solidHeader = FALSE,
                           
                           # Step 1: Specify NULL Hypothesis
                           box(
                             title = "Step 1) Specify NULL Hypotheis (Box Tickets)",
                             width = "100%",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             status = "info",
                             solidHeader = FALSE,
                             HTML("<p>
                              <ul>
                                <li>First, we need to specify the 'null' hypothesis for our proportion test. As it is a proportion, the value must be between <b>0</b> and 
                                  <b>1</b>.</li>
                                <li>This is the proportion we are assuming our null hypothesis is equal to.</li>
                                <li>The null hypothesis changes the tickets in our box that we are drawing from.</li>
                              </ul>
                             </p>"),
                             
                             withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>Given \\( p \\), where \\( p \\) is the proportion of some event:</p>")),
                             fluidRow(
                               column(7),
                               column(2,
                                    withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( H_0: \\) \\( p = \\)</p>"))
                               ),
                               column(3,
                                    numericInput( 
                                      "null_porportion", 
                                      NULL, 
                                      value = 0.5, 
                                      min = 0, 
                                      max = 1, 
                                    ),
                               ),
                             ),
                             uiOutput('null_prop_warning')
                           ),
                           
                           # Step 2: Sample Size
                           box(
                             title = "Step 2) Sample Size (Number of Draws)",
                             width = "100%",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             status = "info",
                             solidHeader = FALSE,
                             HTML("<p>
                              <ul>
                                <li>Specify how large your sample will be in the text box below.</li>
                                <li>You can think of this as how many tickets we are drawing from the box with replacement.</li>
                                <li>You will enter the observed value (i.e. the proportion you saw from the sample) later.</li>
                              </ul>
                             </p>"),
                             fluidRow(
                               column(7),
                               column(2,
                                      withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( n =  \\)</p>"))
                               ),
                               column(3,
                                      numericInput( 
                                        "number_of_draws",
                                        label = NULL,
                                        value = 25, 
                                        min = 1
                                      ),
                               ),
                             ),
                             uiOutput('n_warning_message')
                           ),
                           
                           # Step 3: Model Type
                           box(
                             title = "Step 3) Model Using Sum or Sample",
                             width = "100%",
                             collapsible = TRUE,
                             collapsed = TRUE,
                             status = "info",
                             solidHeader = FALSE,
                             HTML("<p>Third, we need to specify whether we will be modelling the sample using the mean or sum of the draws:<br><br></p>"),
                             radioButtons( 
                               "box_sum_or_mean",
                               label = NULL,
                               choices = list( 
                                 "Sum" = 1, 
                                 "Mean" = 2
                               ) 
                             ),
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
              
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  boxModelMainServer(id = "box_model")
  
  # Process the null hypothesis proportion input.
  null_prop = reactiveVal(0.25)
  null_proportion_warning = reactiveVal(FALSE)
  observeEvent(input$null_porportion, {
    if (is.na(input$null_porportion) || input$null_porportion < 0 || input$null_porportion > 1) {
      null_prop(0.25)
      null_proportion_warning(TRUE)
    } else {
      null_prop(input$null_porportion)
      null_proportion_warning(FALSE)
    }
  })
  
  # Error message for when the value for n is invalid.
  output$null_prop_warning <- renderUI({
    if (null_proportion_warning()) {
      return(
        HTML("<span style='color: red;'><p>Error: The value must be between 0 and 1.</p></span>")
      )
    }
  })
  
  # Process the number of draws text input.
  sample_size = reactiveVal(5)
  number_of_draws_warning = reactiveVal(FALSE)
  observeEvent(input$number_of_draws, {
    if (is.na(input$number_of_draws) || input$number_of_draws < 0) {
      sample_size(25)
      number_of_draws_warning(TRUE)
    } else {
      sample_size(ceiling(input$number_of_draws))
      number_of_draws_warning(FALSE)
    }
  })
  
  # Error message for when the value for n is invalid.
  output$n_warning_message <- renderUI({
    if (number_of_draws_warning()) {
      return(
        HTML("<span style='color: red;'><p>Error: The value for n must be a whole number greater than 0.</p></span>")
      )
    }
  })
  
  output$box_model <- renderGrViz({
    
    tickets_string = ""
   
    prop_temp = null_prop() * 100

    # Find simplified proportion of 1's and 0's so that they are equivalent to the proportion provided in the null hypothesis.
    gcd_value = gcd(prop_temp, 100 - prop_temp)
    proportion_of_1s = prop_temp / gcd_value
    proportion_of_0s = (100 - prop_temp) / gcd_value
    print(c(proportion_of_1s, proportion_of_0s))
    
    is_float_1s = is.numeric(proportion_of_1s) && floor(proportion_of_1s) != proportion_of_1s
    is_float_0s = is.numeric(proportion_of_0s) && floor(proportion_of_0s) != proportion_of_0s
    
    # Case where there are too many digits. Write using percentages.
    if ((proportion_of_1s + proportion_of_0s) > 10) {
      is_float_1s = FALSE
    }
    
    # When the simplified proportions are floats, it is too hard to write down as integer. Write as percentages.
    if (is_float_1s || is_float_0s) {
      prop_temp_complement = 100 - prop_temp
      tickets_string = paste("1 x ", as.character(round(proportion_of_1s, digits = 2)), "%, 0 x ", as.character(round(proportion_of_0s, digits = 2)), "%", sep = "")
    
    } else {
      
      # Add 1's to the string
      for (i in 1:proportion_of_1s) {
        if (proportion_of_1s == 0) {
          break
        }
        tickets_string = paste(tickets_string, "1,")
      } 
      
      # Add 0's to the string.
      for (i in 1:proportion_of_0s) {
        if (proportion_of_0s == 0) {
          break
        }
        tickets_string = paste(tickets_string, "0,")
      } 
      tickets_string <- sub(",$", "", tickets_string)

    }

    # Get other elements for the box model
    n = sample_size()
    sample = "Sample Sum"
    if (input$box_sum_or_mean == 2) {
      sample = "Sample Mean"
    }
    
    # Set up graph and box
    diagram = "digraph diagram { graph [layout = dot, rankdir = TB] node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5] box [label = '"
    diagram = paste(diagram, tickets_string, "']", sep = "")
    
    # Set up sample circle.
    diagram = paste(diagram, " node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]sample [label = '", sample, "']", sep = "")
    
    # Create edge between box and circle.
    # Annotate edge with n value.
    diagram = paste(diagram, " edge [minlen = 2] box->sample [label = '  n = ", n, "', fontsize = 12, labeldistance = 5]}", sep = "")
    
    return (grViz(diagram))
  })
  
  
}

# Run the application
shinyApp(ui, server, enableBookmarking = "url")