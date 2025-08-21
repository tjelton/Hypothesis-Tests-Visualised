oneSampleTTestUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Title Strip
    fluidRow(
      column(8,
             HTML("<h1>1-Sample t-Test</h1>"),
      ),
      column(4,
             tags$style(HTML(paste0("
              [id='", ns("learning_text"), "'] {
                font-size: 20px;
                padding: 10px 20px;
              }
              "))),
             actionButton(ns("learning_text"), "What is a 1-sample t-test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
      ),
    ),
    HTML("<br>"),
    
    ############ SECTION: Input Data ############
    load_1_sample_data_UI(ns("loading_data"), test_name = "1-sample t-test"),

    HTML("<br><br><br>"),
    
    # Conditional panel ensures that the rest of the exercise is displayed only if the data has been uploaded.
    conditionalPanel(
      condition = sprintf('output["%s"]', ns("render_rest_of_exercise")),

      ############ SECTION: The NULL Hypothesis - Setting up the Box ############
      fluidRow(
        column(7,
               tight_card(
                 "The 'NULL' Hypothesis - Setting up the Box",

                 p("We start by using the box model to represent our null hypothesis."),
                 
                 accordion(
                   
                   # Step 1: Specify NULL Hypothesis
                   accordion_panel(
                     HTML("<b>Step 1) Specify NULL Hypothesis</b>"),
                     HTML("<p>
                            The only thing we need to do to set up the box in this test is specify the sample data (which we have already done) and specify the null
                            hypothesis. In this test we are focusing on the mean, so the null hypothesis is that the population mean is equal to some value which
                            we set below.
                        </p>"),
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
                                value = 140
                              ),
                       ),
                     ),
                   ),
                   
                   # Extra notes
                   accordion_panel(
                     HTML("<b>Extra Points</b>"),
                     withMathJax(HTML("<p>In case you are confused about where all the values in the diagram have come from, these come from the sample data that you previously
                            specified. In particular:
                           <ul>
                              <li>\\(s \\) represents the sample standard deviation. Unlike the 1-sample z-test which requires that the population standard
                                deviation is known, the 1-sample t-test removes this assumption, instead using the standard deviation of the sample and then later
                                the t-distribution to determine the p-value.</li>
                              <li>The value for \\(n \\) comes from the number of values in the sample chosen above.</li>
                              <li>The observed value (\\(\\text{OV} \\)) comes from the mean of the sample chosen above.</li>
                           </ul>
                        </p>"))
                   )
                 ),
                 header_colour = "#3179ae"
               )
        ),
        column(5,
               tight_card(
                 NULL,
                 HTML("<center>"),
                 grVizOutput(ns("box_model"), width = "70%", height = "70%"),
                 HTML("</center>")
               ),
        )
      ),

      HTML("<br><br><br>"),
      
      ########### SECTION: The Alternate Hypothesis ############
      alternate_hypotheses_1_sample_t_test_UI(ns("alternate_hypothesis")),
      HTML("<br><br><br>"),

      ############ SECTION: Assumptions ############
      assumptions_1_sample_t_test_UI(ns("assumptions")),
      HTML("<br><br><br>"),

      ############ SECTION: Test Statistic ############
      test_statistic_1_sample_t_test_UI(ns("test_stat")),
      HTML("<br><br><br>"),
      
      ############ SECTION: p-value ############
      p_value_1_sample_t_test_UI(ns("p_val")),
      HTML("<br><br>"),

      ############ SECTION:Conclusion ############
      conclusion_1_sample_t_test_UI(ns("conclusion")),
      HTML("<br><br><br><br><br>"),
      
      
    )

  )
}