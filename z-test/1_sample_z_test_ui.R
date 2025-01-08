oneSampleZTestUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Title Strip
    fluidRow(
      column(8,
             HTML("<h1>1-Sample z-test</h1><br>"),
      ),
      column(4,
             HTML("<br>"),
             tags$style(HTML(paste0("
              [id='", ns("learning_text"), "'] {
                font-size: 20px;
                padding: 10px 20px;
              }
              "))),
             actionButton(ns("learning_text"), "What is a 1-sample z-test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
      ),
    ),
    HTML("<br>"),
    
    ############ SECTION: Input Data ############
    load_1_sample_data_UI(ns("loading_data")),

    HTML("<br><br><br>"),

    # Conditional panel ensures that the rest of the exercise is displayed only if the data has been uploaded.
    conditionalPanel(
      condition = sprintf('output["%s"]', ns("render_rest_of_exercise")),

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
                       withMathJax(HTML("<p>
                            An assumption for the 1-sample z-test is that the standard deviation of the population (denoted \\(\\sigma\\)) in which our sample is drawn is
                            known. For example, a prior research paper may have estimated the population standard deviation, and you could use the value they discovered.<br><br>

                            For this exercise, you can manually set the population standard deviation. However, for the most part, it is likely that the population standard
                            deviation is unknown. Hence, for the purposes of this exercise, you can set the population standard deviation to be equal to the sample standard
                            deviation (this has been automatically done below, but you are free to change the value). In practice, this is far from ideal, and in a future
                            exercise, we will use the 1-sample t-test when the population standard deviation is unknown.</p>")),
                       HTML("<br>"),
                       fluidRow(
                         column(7,
                                actionButton(
                                  inputId = ns("set_pop_sd_to_sample"),
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
                                uiOutput(ns("pop_sd_numeric_input"))
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
                                  ns("null_mu"),
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
                     grVizOutput(ns("box_model"), width = "70%", height = "70%"),
                     HTML("</center>")
                   ),
            )
          ),

          HTML("<br><br><br>"),
      
          ########### SECTION: The Alternate Hypothesis ############
          fluidRow(
            column(7,
                   box(
                     title = HTML("<u><b>The Alternate Hypothesis</b></u>"),
                     status = "primary",
                     width = "100%",
                     solidHeader = FALSE,
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

                   )
            ),
            column(5,
                   box(
                     solidHeader = TRUE,
                     width = "100%",
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
                   box(
                     title = HTML("<u><b>Assumptions</b></u>"),
                     status = "primary",
                     width = "100%",
                     solidHeader = FALSE,
                     HTML("<p>For the hypothesis test to be valid, we need to check the following assumptions:</p>"),

                     # Assumption 1: Independent Samples
                     box(
                       title = "Assumption 1: Independent Sample",
                       width = "100%",
                       collapsible = TRUE,
                       collapsed = TRUE,
                       status = "info",
                       solidHeader = FALSE,
                       HTML("<p>The first assumption is that our sample is <b>independent and randomly chosen</b>.</p>"),
                       HTML("<p><span style='color: blue;'><b>How do we check?</b></span> <i>We check by investigating the experimental setup.</i><br><br>
                                For example, consider we were investigating data for a proportion test involving human participants. We could read the accompanying scientific
                                publication to understand the methodology they used to gather the people in the sample.</p>")
                     ),

                     # Assumption 2: Independent Samples
                     box(
                       title = "Assumption 2: Normality",
                       width = "100%",
                       collapsible = TRUE,
                       collapsed = TRUE,
                       status = "info",
                       solidHeader = FALSE,
                       HTML("<p>The second assumption is that the sample means follow a normal distribution."),
                       HTML("<p><span style='color: blue;'><b>How do we check?</b></span><br>
                                <ul>
                                  <li>Recall that the central limit theorem tells us that if we take a sufficiently large number of draws from the box, then the sample
                                  means will approximately follow a normal distribution. If confused, please see the box model exercise.</li>
                                  <li>One way to gauge whether the central limit theorem holds or not is to see how large our sample is (this is indicated by the \"n\" in
                                  the box model above).</li>
                                  <li>Many textbooks will say that you can say that you can use the rule of thumb that the central limit theorem will apply if we have 30
                                  or more draws. BEWARE - this is not always true! If the distribution of the values is very skewed, you will need much more than 30 draws!</li>
                                  <li>You should use a combination of the size of \"n\" and a histogram and boxplot of the sample distributions to help you determine whether
                                  the central limit theorem means we can resonably approximate the sample means using a normal distribution.</li>
                                </ul>
                           </p>")
                     ),

                     # Assumption 3: Normal Approximation
                     box(
                       title = "Assumption 3: Known Population Standard Deviation",
                       width = "100%",
                       collapsible = TRUE,
                       collapsed = TRUE,
                       status = "info",
                       solidHeader = FALSE,
                       HTML("<p>As mentioned previously in \"The NULL Hypothesis - Setting up the Box\" section, it is assumed that the population standard deviation is known.</p>")
                     )
                  )
            )
          ),

          HTML("<br><br><br>"),
      
          ############ SECTION: Test Statistics ############
          fluidRow(
            column(12,
                   box(
                     title = HTML("<u><b>Test Statistic</b></u>"),
                     status = "primary",
                     width = "100%",
                     solidHeader = FALSE,

                     fluidRow(
                       column(6,
                              HTML("<p><b>Step 1) Calculate Expected Value (SE) and Standard Error (SE)</b></p>"),
                              uiOutput(ns("ev_and_se_text"))
                       ),
                       column(6,
                              HTML("<p><b>Step 2) Test Statistic Calculation</b></p>"),
                              uiOutput(ns("test_statistic_calculation"))
                       )
                     ),
                   ),
            ),
          ),

          HTML("<br><br><br>"),

          ############ SECTION: p-value ############
          fluidRow(
            column(6,
                   box(
                     title = HTML("<u><b>p-value</b></u>"),
                     status = "primary",
                     width = "100%",
                     solidHeader = FALSE,
                     uiOutput(ns("p_value_prelude"))
                   )
            ),
            column(6,
                   plotOutput(ns("test_stat_normal_plot"), width = "80%", heigh = "275px"),
            )
          ),

          HTML("<br><br><br><br><br><br><br>"),
      
          ############ SECTION:Conclusion ############
          fluidRow(
            column(12,
                   box(
                     title = HTML("<u><b>Conclusion</b></u>"),
                     status = "primary",
                     width = "100%",
                     solidHeader = FALSE,
                     
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
                     )
                   )
            ),
          )
      
      
    )
    
  )
}