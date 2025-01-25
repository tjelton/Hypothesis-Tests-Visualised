p_value_1_sample_t_test_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    ############ SECTION: Test p-value ############
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
             plotOutput(ns("test_stat_t_plot"), width = "80%", heigh = "275px"),
      )
    )
  
  )
}
