confidence_interval_1_sample_t_test_Server <- function(id, sample_data, null_mu, alternate_hypothesis_choice) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Process confidence level text input.
      confidence_level = reactiveVal(0.95)
      alpha_warning_confidence_lvel = reactiveVal(FALSE)
      observeEvent(input$confidence_level, {
        if (is.na(input$confidence_level) || input$confidence_level <= 0 || input$confidence_level >= 1) {
          confidence_level(0.95)
          alpha_warning_confidence_lvel(TRUE)
        } else {
          confidence_level(input$confidence_level)
          alpha_warning_confidence_lvel(FALSE)
        }
      })

      # Error message for when the value for alpha is invalid.
      output$confidence_level_warning <- renderUI({
        if (alpha_warning_confidence_lvel()) {
          return(
            HTML("<span style='color: red;'><p>Error: The value for confidence level must be between 0 and 1.</p></span>")
          )
        }
      })

      # Confidence interval output
      output$confidence_level_output = renderUI({
        
        sample_size = length(sample_data())
        xbar = mean(sample_data())
        se = sd(sample_data())/sqrt(sample_size)
        conf_level = as.numeric(confidence_level())
        alpha = 1 - conf_level
        mu0 = null_mu()
        df = sample_size - 1
        
        formula_line = substitution_line = answer_line = NULL
        conclusion_line = NULL
        
        # Compute CI and generate lines based on hypothesis type
        if (alternate_hypothesis_choice() == 1) {
          # Two sided
          t_val = qt(1 - alpha/2, df = df)
          lower = xbar - t_val * se
          upper = xbar + t_val * se
          
          formula_line = "$$CI = \\bar{x} \\pm t_{\\alpha/2, df} \\cdot SE$$"
          substitution_line = paste0("$$CI = ", round(xbar,4), 
                                     " \\pm t_{", round(alpha/2,4), ",", df, "} \\times ", 
                                     round(se,4), "$$")
          answer_line = paste0("$$CI = (", round(lower,4), ", ", round(upper,4), ")$$")
          
          if (mu0 < lower || mu0 > upper) {
            conclusion_text = "As the null hypothesis value is outside the confidence interval, we <b>reject the null hypothesis</b>."
          } else {
            conclusion_text = "As the null hypothesis value is inside the confidence interval, we <b>fail to reject the null hypothesis</b>."
          }
          
        } else if (alternate_hypothesis_choice() == 2) {
          # One sided (greater than)
          t_val = qt(1 - alpha, df = df)
          lower = xbar - t_val * se
          
          formula_line = "$$CI = (\\bar{x} - t_{\\alpha, df} \\cdot SE, \\infty)$$"
          substitution_line = paste0("$$CI = (", round(xbar,4), 
                                     " - t_{", round(alpha,4), ",", df, "} \\times ", 
                                     round(se,4), ", \\infty)$$")
          answer_line = paste0("$$CI = (", round(lower,4), ", \\infty)$$")
          
          if (mu0 < lower) {
            conclusion_text = "As the null hypothesis value is below the confidence interval, we <b>reject the null hypothesis</b>."
          } else {
            conclusion_text = "As the null hypothesis value is inside the confidence interval, we <b>fail to reject the null hypothesis</b>."
          }
          
        } else if (alternate_hypothesis_choice() == 3) {
          # One sided (less than)
          t_val = qt(1 - alpha, df = df)
          upper = xbar + t_val * se
          
          formula_line = "$$CI = (-\\infty, \\bar{x} + t_{\\alpha, df} \\cdot SE)$$"
          substitution_line = paste0("$$CI = (-\\infty, ", round(xbar,4), 
                                     " + t_{", round(alpha,4), ",", df, "} \\times ", 
                                     round(se,4), ")$$")
          answer_line = paste0("$$CI = (-\\infty, ", round(upper,4), ")$$")
          
          if (mu0 > upper) {
            conclusion_text = "As the null hypothesis value is above the confidence interval, we <b>reject the null hypothesis</b>."
          } else {
            conclusion_text = "As the null hypothesis value is inside the confidence interval, we <b>fail to reject the null hypothesis</b>."
          }
        }
        
        # Build UI
        tagList(
          withMathJax(HTML(formula_line)),
          withMathJax(HTML(substitution_line)),
          withMathJax(HTML(answer_line)),
          HTML(paste0("<span style='color: blue;'><p>", conclusion_text, "</p></span>"))
        )
      })

    }
  )
}
