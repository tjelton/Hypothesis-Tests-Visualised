# Requirements:
#     - sample_data: the data frame containing the datat that is being analysed.
#     - null_mean_string: the string representation of the null value.
test_statistic_1_sample_t_test_Server <- function(id, sample_data, null_mean_string) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      EV_string = reactiveVal("")
      SE_string = reactiveVal("")

      # Expected value and standard error output.
      output$ev_and_se_text <- renderUI({

        sample_size = length(sample_data())

        # Find EV and SE.
        mean_ = as.numeric(null_mean_string())
        sd_ = sd(sample_data())
        EV = mean_
        SE = sd_ / sqrt(sample_size)

        EV_string(as.character(round(EV, 5)))
        SE_string(as.character(round(SE, 5)))

        expected_value = withMathJax(
          HTML("<p>Expected Value:</p>"),
          HTML(paste("$$\\begin{align*} \\text{EV} &= \\mu \\\\ &=", EV_string(), "\\end{align*}$$", sep = ""))
        )

        standard_error = withMathJax(
          HTML("<p>Standard Error:</p>"),
          HTML(paste("$$\\begin{align*} \\text{SE} &= \\frac{\\sigma}{\\sqrt{n}} \\\\ &= \\frac{", round(sd_, 5) , "}{\\sqrt{",
                     as.character(sample_size), "}}\\\\ &= ", SE_string(), "\\end{align*}$$", sep = "")),
          HTML("(Note that we use the sample standard deviation [\\(s \\)] rather than the population standard deviation [\\(\\sigma \\)])")
        )

        return(
          tagList(
            expected_value, standard_error
          )
        )
      })

      test_stat = reactiveVal("")

      # Test statistic output.
      output$test_statistic_calculation <- renderUI({

        observed_val = mean(sample_data(), na.rm = TRUE)

        # Calculate test statistic.
        temp = (observed_val - as.numeric(EV_string()))/as.numeric(SE_string())
        temp = as.character(round(temp, 4))
        test_stat(temp)

        t_stat = withMathJax(
          HTML(paste("$$\\begin{align*} \\text{TS} &= \\frac{\\text{OV} - \\text{EV}}{\\text{SE}} \\\\ &= \\frac{", as.character(round(observed_val,5)), " - ",
                     EV_string(), "}{", SE_string(), "} \\\\ &= ", test_stat(), "\\end{align*}$$", sep = ""))
        )
        additional_line = HTML(paste("<p style = 'text-align: left;'><span style='color: blue;'><i>The value for the test-statistic is ", test_stat(), ". </i></span></p>", sep = ""))
        return(
          tagList(
            t_stat,
            additional_line
          )
        )
      })
      
      # To make the alternate hypothesis choice accessible outside of the module.
      #alternate_hypothesis_choice_reactive = reactive(input$alternate_hypothesis_choice)
      list(test_stat = test_stat)
    }
  )
}
