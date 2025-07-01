conclusion_1_sample_t_test_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    ############ SECTION: Conclusion ############
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
                        uiOutput(ns("final_conclusion_output"))
                 )
               ),
               header_colour = "#3179ae"
             )
      ),
    )
  
  )
}
