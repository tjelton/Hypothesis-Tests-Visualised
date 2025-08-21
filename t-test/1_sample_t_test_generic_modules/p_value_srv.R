# Requirements:
#     - test_stat (str): test statistic stored as string.
#     - sample_data (df): data frame for the sample data.
#     - alternate_hypothesis_choice (int): integer representation of which alternate hypothesis we are using.
p_value_1_sample_t_test_Server <- function(id, test_stat, sample_data, alternate_hypothesis_choice) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      p_val = reactiveVal(0)
      
      # P-value text output and calculation.
      output$p_value_prelude <- renderUI({
        # General prelude text about what the p-value is.
        first_string = HTML(paste("<p>The p-value is the probability of observing a test-statistic <b>more extreme that our test statistic of ",
                                  test_stat(), 
                                  ".</b></p>", 
                                  sep = ""))
        
        second_string = withMathJax(HTML(paste("<p>
                            Unlike in a z-test where the test statistics fall on a standard normal curve, in a t-test, the test statistics fall on a t-curve/distribution. 
                            If you recall from the \"T-Curve Motivation\" exercise, to specify a t-distribution, you need to specify the degree of freedom, which adjusts
                            the 'fatness' of the t-curve's tails.</p>
                            
                            <p><b>For a 1-sample t-test, we set the degrees of freedom equal to the sample size - 1 (that is, \\(n - 1\\)).</b></p>
                            
                            </p>In this case, the degree of freedom is equal to \\(", length(sample_data()), " - 1 = ",length(sample_data()) - 1 ,"\\).</p>")))
        
        # Specifically how to find the p-value (based upon alternate hypothesis).
        third_string = "<p>The test statistics fall on a standard normal curve. "
        if (alternate_hypothesis_choice() == 1) {
          negative_test_stat = as.character(-abs(as.numeric(test_stat())))
          positive_test_stat = as.character(abs(as.numeric(test_stat())))
          third_string = paste(third_string, "As we are doing a two-sided alternate hypothesis, we are interested in finding the <b>area below ", negative_test_stat,
                               " and above ", positive_test_stat, ".</p></b>", sep = "")
        } else if (alternate_hypothesis_choice() == 2){
          third_string = paste(third_string, "As we are doing a one-sided greater than alternate hypothesis, we are interested in finding the <b>area above ", test_stat(),
                               ".</p></b>", sep = "")
        } else if (alternate_hypothesis_choice() == 3){
          third_string = paste(third_string, "As we are doing a one-sided less than alternate hypothesis, we are interested in finding the <b>area below ", test_stat(),
                               ".</p></b>", sep = "")
        }
        third_string = HTML(third_string)
        
        # Calculate p-value.
        n = length(sample_data())
        df = n - 1
        p_val_local = 0
        if (alternate_hypothesis_choice() == 1) {
          p_val_local = 2 * (1 - pt(abs(as.numeric(test_stat())), df))
        } else if (alternate_hypothesis_choice() == 2) {
          p_val_local = 1 - pt(as.numeric(test_stat()), df)
        } else if (alternate_hypothesis_choice() == 3) {
          p_val_local = pt(as.numeric(test_stat()), df)
        }
        p_val(p_val_local)
        
        # String to output the p-value.
        p_value = withMathJax(HTML("<p style='font-size: 16px; text-align: center;'>\\( p =", as.character(round(p_val_local,5)) ,"\\)</p>"))
        
        return(
          tagList(
            first_string,
            second_string,
            third_string,
            p_value
          )
        )
        
      })
      
      # Histogram with t curve to show p-value calculation.
      output$test_stat_t_plot = renderPlot({
        return(curve_shaded_test_stat(dt, list(df = length(sample_data()) - 1), as.numeric(test_stat()), alternate_hypothesis_choice()))
      })
      
      # To make the alternate hypothesis choice accessible outside of the module.
      list(p_val = p_val)
    }
  )
}
