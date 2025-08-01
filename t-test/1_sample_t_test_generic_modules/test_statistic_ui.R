test_statistic_1_sample_t_test_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    ############ SECTION: Test Statistic ############
    fluidRow(
      column(12,
             tight_card(
               "Test Statistic",
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
               header_colour = "#3179ae"
             )
      )
    )
  )
}
