proportionTestMainUI <- function(id) {
  ns <- NS(id)
  tagList(
    

    fluidRow(
      column(8,
             HTML("<h1>Proportion Test (z-test)</h1>"),
      ),
      column(4,
             tags$style(HTML(paste0("
                [id='", ns("learning_text"), "'] {
                  font-size: 20px;
                  padding: 10px 20px;
                }
             "))),
             actionButton(ns("learning_text"), "What is the proportion test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
      ),
    ),
    HTML("<br>"),
    
    
    ############ SECTION: The NULL Hypothesis - Setting up the Box ############
    fluidRow(
      column(7,
             tight_card(
               "The 'NULL' Hypothesis - Setting up the Box",
               p("We start by using the box model to represent our null hypothesis."),
               
               accordion(
                 
                 # Step 1: Specify NULL Hypothesis
                 accordion_panel(
                   HTML("<b>Step 1) Specify NULL Hypotheis (Box Tickets)</b>"),
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
                              ns("null_porportion"),
                              NULL,
                              value = 0.7,
                              min = 0,
                              max = 1,
                            ),
                     ),
                   ),
                   uiOutput(ns('null_prop_warning'))
                 ),
                 
                 # Step 2: Sample Size
                 accordion_panel(
                   HTML("<b>Step 2) Sample Size (Number of Draws)</b>"),
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
                              ns("number_of_draws"),
                              label = NULL,
                              value = 30,
                              min = 1
                            ),
                     ),
                   ),
                   uiOutput(ns('n_warning_message'))
                 ),
                 
                 # Step 3: Model Type
                 accordion_panel(
                   HTML("<b>Step 3) Model Using Sum or Sample</b>"),
                   HTML("<p>Third, we need to specify whether we will be modelling the sample using the mean or sum of the draws:<br><br></p>"),
                   radioButtons(
                     ns("box_sum_or_mean"),
                     label = NULL,
                     choices = list(
                       "Mean" = 2,
                       "Sum" = 1
                     )
                   )
                 )
               ),
               header_colour = "#3179ae"
             )
      ),
      column(5,
             # Box model output.
             tight_card(
               NULL,
               HTML("<center>"),
               grVizOutput(ns("box_model"), width = "70%", height = "70%"),
               HTML("</center>")
             ),
      )
    ),
    
    HTML("<br><br><br>"),
    
    ############ SECTION: The Alternate Hypothesis ############
    fluidRow(
      column(7,
             tight_card(
               "The Alternate Hypothesis",
               HTML("<p>Specify what type of alternate hypothesis you will be using below:</p>"),
               HTML("<br>"),
               radioButtons(
                 inputId = ns("alternate_hypothesis_choice"),
                 label = NULL,
                 choices = list(
                   "Two Sided" = 1,
                   "One Sided (greater than)" = 2,
                   "One Sided (less than)" = 3
                 )
               ),
               header_colour = "#3179ae"
             )
      ),
      column(5,
             tight_card(
               NULL,
               HTML("<p><b>Null Hypothesis</b></p>"),
               uiOutput(ns('null_hypothesis_output')),
               HTML("<p><b>Alternate Hypothesis</b></p>"),
               uiOutput(ns('alternate_hypothesis_output')),
             )
      )
    ),

    HTML("<br><br><br>"),

    ############ SECTION: Assumptions ############
    fluidRow(
      column(12,
             tight_card(
               "Assumptions",
               HTML("<p>For the hypothesis test to be valid, we need to check the following assumptions:</p>"),

               accordion(
                 open = FALSE,
                 
                 # Assumption 1: Independent Samples
                 accordion_panel(
                   HTML("<b>Assumption 1: Independent Sample</b>"),
                   HTML("<p>The first assumption is that our sample is <b>independent and randomly chosen</b>.</p>"),
                   HTML("<p><span style='color: blue;'><b>How do we check?</b></span> <i>We check by investigating the experimental setup.</i><br><br>
                                For example, consider we were investigating data for a proportion test involving human participants. We could read the accompanying scientific
                                publication to understand the methodology they used to gather the people in the sample.</p>")
                 ),
                 
                 # Assumption 2: Independent Samples
                 accordion_panel(
                   HTML("<b>Assumption 2: Constant Proportion</b>"),
                   HTML("<p>The second assumption is that our sample is the <b>constant proportion</b> assumption. This assumption states that the probability that each sample
                      element falls within the event of interest (stipulated in our hypotheses) is unchanging.</p>"),
                   HTML("<p><span style='color: blue;'><b>How do we check?</b></span> <i>We check by investigating the experimental setup.</i><br><br></p>")
                 ),
                 
                 # Assumption 3: Normal Approximation
                 accordion_panel(
                   HTML("<b>Assumption 3: Normal Approximation</b>"),
                   fluidRow(
                     column(6,
                            uiOutput(ns("assumption2_text_output")),
                            HTML("<br><br>")
                     ),
                     column(6,
                            plotOutput(ns("empirical_draws_hist"), width = "100%", height = "300px"),
                     )
                   )
                 )
               ),
               header_colour = "#3179ae"
             )
      )
    ),

    HTML("<br><br><br>"),
    
    ############ SECTION: Test Statistics ############
    fluidRow(
      column(12,
             tight_card(
               "Test Statistic",
               fluidRow(
                 column(6,
                        HTML("<p><b>Step 1) Observed Value (OV)</b></p>"),
                        uiOutput(ns("observed_value_output")),
                        fluidRow(
                          column(2),
                          column(2,
                                 withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( OV = \\)</p>"))
                          ),
                          column(3,
                                 numericInput(
                                   ns("observed_value"),
                                   NULL,
                                   value = 0.73,
                                   min = 0,
                                   max = 1,
                                   width = "100%"
                                 ),
                          ),
                        ),
                        uiOutput(ns("observed_val_warning_message")),
                        HTML("<p><b>Step 2) Calculate Expected Value (SE) and Standard Error (SE)</b></p>"),
                        uiOutput(ns("ev_and_se_text"))
                 ),
                 column(6,
                        HTML("<p><b>Step 3) Test Statistic Calculation</b></p>"),
                        uiOutput(ns("test_statistic_calculation"))
                 )
               ),
               header_colour = "#3179ae"
             ),
      ),
    ),

    HTML("<br><br><br>"),

    ############ SECTION: p-value ############
    fluidRow(
      column(6,
             tight_card(
               title = HTML("<u><b>p-value</b></u>"),
               status = "primary",
               width = "100%",
               solidHeader = FALSE,
               uiOutput(ns("p_value_prelude")),
               header_colour = "#3179ae"
             )
      ),
      column(6,
             tight_card(
               NULL,
               HTML("<center>"),
               plotOutput(ns("test_stat_normal_plot"), width = "100%", heigh = "325px"),
               HTML("</center>"),
             )
      )
    ),

    HTML("<br><br><br>"),

    ############ SECTION:Conclusion ############
    fluidRow(
      column(12,
             tight_card(
               "Conclusion",

               fluidRow(
                 # Section to enter significance level.
                 column(6,
                        HTML("<p><b>Step 1) What is your significance level</b>?</p>"),

                        # Space to enter significance value.
                        fluidRow(
                          column(1,
                                 withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( \\alpha = \\)</p>"))
                          ),
                          column(3,
                                 numericInput(
                                   ns("alpha_value"),
                                   NULL,
                                   value = 0.05,
                                   min = 0,
                                   max = 1,
                                   width = "100%"
                                 ),
                          ),
                        ),
                        uiOutput(ns("significance_level_warning")),
                 ),

                 # Section to provide final result.
                 column(6,
                        HTML("<p><b>Step 2) Final Conclusion</b></p>"),
                        uiOutput(ns("conclusion_output")),
                 )
               ),
               header_colour = "#3179ae"
             )
      ),
    ),
    
    HTML("<br><br><br><br><br><br><br>"),
    
  )
}