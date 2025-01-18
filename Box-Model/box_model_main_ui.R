boxModelMainUI <- function(id) {
  ns <- NS(id)
  tagList(

    ############ TITLE ############ 
    fluidRow(
      column(8,
             HTML("<h1>Box Model</h1><br>"),
      ),
      column(4,
             HTML("<br>"),
             tags$style(HTML(paste0("
                [id='", ns("learning_text"), "'] {
                  font-size: 20px;
                  padding: 10px 20px;
                }
             "))),
             actionButton(ns("learning_text"), "What is the box model?", class = "btn btn-primary", style="color: #fff;", width = "100%")
      ),
    ),
    HTML("<br>"),
    
    ############ SECTION: Setting up the Box Model ############ 
    fluidRow(
      column(7,
             box(
               title = HTML("<u><b>Box Parameters</b></u>"),
               status = "primary", 
               width = "100%",
               solidHeader = FALSE,
               
               HTML("<p>First we need to create the box. Follow steps 1, 2, and 3 to configure the box to be whatever you wish to model.
                       The box will output on the right according to the settings that you pick.</p><br>"),
               
               # Step 1: Enter Tickets
               box(
                 title = "Step 1) Tickets",
                 width = "100%",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 status = "info",
                 solidHeader = FALSE,
                 HTML("<p>First, we need to specify the tickets that we will be adding to the box.</p>"),
                 HTML("<p>In the text box below, enter the tickets that you wish to place into the box. Only <i>numbers</i> can be
                               added, and <i>each ticket should be seperated by a comma</i>. For example, if you want to have 1x1 ticket
                               and 3x0 tickets in the box, enter <i>1,0,0,0</i>."),
                 textAreaInput( 
                   ns("box_tickets_text_entry"),
                   NULL,
                   value = "1,0,0,0",
                   width = "100%"
                 ),
                 fluidRow(
                   column(8),
                   column(4,
                     actionButton(ns("submit_tickets"), "Set Tickets", class = "btn btn-success", style="color: #fff;", width = "100%")
                   )
                 ),
                 uiOutput(ns("tickets_text_error_message"))
               ),
               
               # Step 2: Set n
               box(
                 title = "Step 2) Number of Draws",
                 width = "100%",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 status = "info",
                 solidHeader = FALSE,
                 HTML("<p>Second, we need to specify the number of draws (with replacement) that we will be taking from the box:</p>"),
                 numericInput( 
                   ns("number_of_draws"),
                   label = NULL,
                   value = 25, 
                   min = 1
                 ),
                 uiOutput(ns("number_of_draws_error_message"))
               ),
               
               # Step 3: Model Using Sum or Mean
               box(
                 title = "Step 3) Model Using Sum or Mean",
                 width = "100%",
                 collapsible = TRUE,
                 collapsed = TRUE,
                 status = "info",
                 solidHeader = FALSE,
                 HTML("<p>Third, we need to specify whether we will be modelling the sample using the mean or sum of the draws:<br><br></p>"),
                 radioButtons( 
                   ns("box_sum_or_mean"),
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
               grVizOutput(ns("box_model"), width = "70%", height = "70%"),
               HTML("</center>")
             ),
             
             # Mean and SD of the box output.
             box(
               solidHeader = TRUE,
               width = "100%",
               uiOutput(outputId = ns("box_statistics"))
             )
      )
    ),
    
    HTML("<br><br><br>"),
    
    ############ SECTION: Checking Central Limit Theorem ############ 
    fluidRow(
      column(5,
             box(
               title = HTML("<u><b>Central Limit Theorem</b></u>"),
               status = "primary",
               solidHeader = FALSE,
               width = "100%",
               uiOutput(outputId = ns("CLT_text_instructions_output")),
               HTML("<br>"),
               fluidRow(
                 column(1),
                 
                 # Button for repeating adding the mean or sample sum to the histogram.
                 column(5,
                        actionButton(
                          inputId = ns("repeat_1"), label = HTML('<i class="fa fa-plus"></i> Repeat 1'),
                          class = "btn btn-primary", style="color: #fff;", width = "100%"
                        ),
                        HTML("<br><br>"),
                        actionButton(
                          inputId = ns("repeat_25"), label = HTML('<i class="fa fa-plus"></i> Repeat 25'),
                          class = "btn btn-primary", style="color: #fff;", width = "100%"
                        ),
                 ),
                 column(5,
                        actionButton(
                          inputId = ns("repeat_10"), label = HTML('<i class="fa fa-plus"></i> Repeat 10'),
                          class = "btn btn-primary", style="color: #fff;", width = "100%"
                        ),
                        HTML("<br><br>"),
                        actionButton(
                          inputId = ns("repeat_100"), label = HTML('<i class="fa fa-plus"></i> Repeat 100'),
                          class = "btn btn-primary", style="color: #fff;", width = "100%"
                        ),
                 ),
               ),
               HTML("<br><center>"),
               
               # On click, resets the histogram
               actionButton(
                 inputId = ns("reset_button"), label = HTML('<i class="fa fa-redo"></i> Reset'),
                 class = "btn btn-danger", style="color: #fff;"
               ),
               HTML("</center><br>"),
             ),
             
      ),
      
      # Histogram Distribution
      column(7,
             plotOutput(ns("histogram_frequencies"), width = "80%", height = "300px"),
             HTML("<br><br><br><br>"),
             box(
               solidHeader = TRUE,
               width = "100%",
               uiOutput(outputId = ns("CLT_satisfied_text")),
             ),
      )
      
    ),
    
    HTML("<br><br><br>"),
    
    ############ SECTION: Modelling Using the Normal Curve ############
    fluidRow(
      column(5,
             box(title = HTML("<u><b>Modelling Using a Normal Distribution</b></u>"),
                 status = "primary",
                 solidHeader = FALSE,
                 width = "100%",
                 uiOutput(ns("normal_distribution_text"))
              )
      ),
      column(7,
             plotOutput(ns("normal_curve_model"), width = "80%"),
      )
    ),
    
    HTML("<br><br><br>"),
    
    # ############ SECTION: Finding Probabilities ############ 
    fluidRow(
      
      # Only display this section if continue button is pressed.
      column(6,
             box(title = HTML("<u><b>Finding Probabilities</b></u>"),
                 status = "primary",
                 solidHeader = FALSE,
                 width = "100%",
                 uiOutput(ns("finding_probabilities_text")),
                 fluidRow(
                   column(1),
                   column(4,
                      HTML("<center><h5><b>Lower Boundary</b></h5></center>"),
                      checkboxInput(ns("lower_boundary_infinity"), HTML(paste("<p>", withMathJax("\\(-\\infty\\)"),"</p>")), FALSE),
                      conditionalPanel(
                        condition = paste0('input[\'', ns('lower_boundary_infinity'), "\'] == false"),
                        #condition = "input.lower_boundary_infinity == false",
                        numericInput(
                          ns("lower_boundary_numeric"),
                          label = NULL,
                          value = 0
                        ),
                      ),
                   ),
                   column(2),
                   column(4,
                      HTML("<center><h5><b>Upper Boundary</b></h5></center>"),
                      checkboxInput(ns("upper_boundary_infinity"), HTML(paste("<p>", withMathJax("\\(\\infty\\)"),"</p>")), FALSE),
                      conditionalPanel(
                        condition = paste0('input[\'', ns('upper_boundary_infinity'), "\'] == false"),
                        
                        #condition = "input.upper_boundary_infinity == false",
                        numericInput(
                          ns("upper_boundary_numeric"),
                          label = NULL,
                          value = 1
                        ),
                      ),
                   ),
                   column(1)
                 ),
                 uiOutput(ns('interval_error_message'))
             )
      ),
      column(6,
             plotOutput(ns("shaded_normal_curve"), width = "80%", height = "250px"),
             HTML("<br><br>"),
             box(
               solidHeader = TRUE,
               width = "100%",
               uiOutput(ns("probability_answer_text"))
             ),
             
      )

    ),



    
    
  )
}