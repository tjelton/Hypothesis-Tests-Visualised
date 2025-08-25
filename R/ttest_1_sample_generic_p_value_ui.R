p_value_1_sample_t_test_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    ############ SECTION: Test p-value ############
    fluidRow(
      column(6,
             tight_card(
               "p-value",
               uiOutput(ns("p_value_prelude")),
               header_colour = "#3179ae"
             )
      ),
      column(6,
             tight_card(
               NULL,
               HTML("<center>"),
               plotOutput(ns("test_stat_t_plot"), width = "100%", heigh = "325px"),
               HTML("</center>"),
             )
      )
    )
  
  )
}
