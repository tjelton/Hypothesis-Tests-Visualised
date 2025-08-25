# Requirements:
#     - p_val (int): P-value.
conclusion_1_sample_t_test_Server <- function(id, p_val) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      alpha = reactiveVal(0.05)
      alpha_warning = reactiveVal(FALSE)
      observeEvent(input$alpha_value, {
        if (is.na(input$alpha_value) || input$alpha_value < 0 || input$alpha_value > 1) {
          alpha(0.05)
          alpha_warning(TRUE)
        } else {
          alpha(input$alpha_value)
          alpha_warning(FALSE)
        }
      })
      
      # Error message for when the value for alpha is invalid.
      output$significance_level_warning <- renderUI({
        if (alpha_warning()) {
          return(
            HTML("<span style='color: red;'><p>Error: The value for α must be between 0 and 1.</p></span>")
          )
        }
      })
      
      # Hypothesis test output
      output$final_conclusion_output <- renderUI({
        
        # Change > or < sign depending on whether the p-value is less than or greater than alpha.
        math_line = withMathJax(
          HTML(paste("$$\\begin{align*} \\alpha &> p \\\\", as.character(alpha()), " &> ", as.character(round(p_val(), 4)), "\\end{align*}$$", sep = ""))
        )
        conclusion_line = HTML("<span style='color: blue;'><p>As the p value is less than our significance level, we <b>reject the null hypothesis</b>.</p></span>")
        if (p_val() > alpha()) {
          math_line = withMathJax(
            HTML(paste("$$\\begin{align*} \\alpha &< p \\\\", as.character(alpha()), " &< ", as.character(round(p_val(), 4)), "\\end{align*}$$", sep = ""))
          )
          conclusion_line = HTML("<span style='color: blue;'><p>As the p value is greater than our significance level, we <b>accept the null hypothesis</b>.</p></span>")
        }
        
        return(
          tagList(
            math_line,
            conclusion_line
          )
        )
      })
    }
  )
}
