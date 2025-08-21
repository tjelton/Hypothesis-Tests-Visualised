confidence_interval_1_sample_t_test_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    ############ SECTION: Confidence Interval ############
    fluidRow(
      column(12,
             tight_card(
               "Conclusion (Confidence Interval)",
               HTML("<p>
                      A confidence interval in a 1-sample z-test shows the range of population means that are plausible at the chosen confidence level, and if the
                      hypothesized mean falls outside this range, the null hypothesis is rejected.
                      <br><br>

                      We can also use a confidence interval to tell us whether we should accept or reject the null hypothesis. If the expected value DOES NOT lie within the
                      confidence interval, then we reject the null hypothesis.
                      </p>"),
               fluidRow(
                 column(6,
                        HTML("<p><b>Step 1) What is your confidence level</b>?</p>"),

                        fluidRow(
                          column(1,
                                 withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( \\alpha = \\)</p>"))
                          ),
                          column(3,
                                 numericInput(
                                   ns("confidence_level"),
                                   NULL,
                                   value = 0.95,
                                   min = 0,
                                   max = 1,
                                   width = "100%"
                                 ),
                          ),
                        ),
                        uiOutput(ns("confidence_level_warning")),
                 ),
                 column(6,
                        HTML("<p><b>Step 2) Final Conclusion</b></p>"),
                        uiOutput(ns("confidence_level_output")),
                 )
               ),
               header_colour = "#3179ae"
             )
      ),
    )
  
  )
}
