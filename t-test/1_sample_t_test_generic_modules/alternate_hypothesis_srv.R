# Requirements:
#     - null_mean_string: the string representation of the null value.
#     - whether mu should have a subscript (NULL is that it should not have one)
alternate_hypotheses_1_sample_t_test_Server <- function(id, null_mean_string, mu_subscript = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Null hypothesis (rendered) output.
      output$null_hypothesis_output <- renderUI({
        
        hypothesis = paste("<p style='font-size: 16px;'>\\( H_0: \\) \\( \\mu", "=", null_mean_string(), "\\)</p>")
        if (!is.null(mu_subscript)) {
          hypothesis = paste("<p style='font-size: 16px;'>\\( H_0: \\) \\( \\mu_", mu_subscript , " = ", null_mean_string(), "\\)</p>", sep ="")
        }
        
        return (
          tagList(
            HTML("<center>"),
            withMathJax(HTML(hypothesis)),
            HTML("</center>")
          )
        )
      })
      
      # Alternate hypothesis (rendered) output.
      output$alternate_hypothesis_output <- renderUI({
        
        hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\(\\mu")
        if (!is.null(mu_subscript)) {
          hypothesis = paste(hypothesis, "_d", sep = "")
        }
      
        # Specify alternate hypothesis in reference to whether the user chooses to do a one-sided or two-sided test.
        if (input$alternate_hypothesis_choice == 1) {
          hypothesis = paste(hypothesis, "\\neq", null_mean_string(), "\\)</p>")
        } else if (input$alternate_hypothesis_choice == 2) {
          hypothesis = paste(hypothesis, ">", null_mean_string(), "\\)</p>")
        } else if (input$alternate_hypothesis_choice == 3) {
          hypothesis = paste(hypothesis, "<", null_mean_string(), "\\)</p>")
        }
          
        return (
          tagList(
            HTML("<center>"),
            withMathJax(HTML(hypothesis)),
            HTML("</center>")
          )
        )
      })
      
      # To make the alternate hypothesis choice accessible outside of the module.
      alternate_hypothesis_choice_reactive = reactive(input$alternate_hypothesis_choice)
      list(alternate_hypothesis_choice = alternate_hypothesis_choice_reactive)
    }
  )
}
