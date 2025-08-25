alternate_hypotheses_1_sample_t_test_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
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
  
  )
}
