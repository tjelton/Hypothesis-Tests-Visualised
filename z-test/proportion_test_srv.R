proportionTestMainServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Process the null hypothesis proportion input.
    null_prop = reactiveVal(0.7)
    null_proportion_warning = reactiveVal(FALSE)
    observeEvent(input$null_porportion, {
      if (is.na(input$null_porportion) || input$null_porportion < 0 || input$null_porportion > 1) {
        null_prop(0.7)
        null_proportion_warning(TRUE)
      } else {
        null_prop(input$null_porportion)
        null_proportion_warning(FALSE)
      }
    })
    
    # Error message for when the value for n is invalid.
    output$null_prop_warning <- renderUI({
      if (null_proportion_warning()) {
        return(
          HTML("<span style='color: red;'><p>Error: The value must be between 0 and 1.</p></span>")
        )
      }
    })
    
    # Process the number of draws text input.
    sample_size = reactiveVal(5)
    number_of_draws_warning = reactiveVal(FALSE)
    observeEvent(input$number_of_draws, {
      if (is.na(input$number_of_draws) || input$number_of_draws < 0) {
        sample_size(25)
        number_of_draws_warning(TRUE)
      } else {
        sample_size(ceiling(input$number_of_draws))
        number_of_draws_warning(FALSE)
      }
    })
    
    # Error message for when the value for n is invalid.
    output$n_warning_message <- renderUI({
      if (number_of_draws_warning()) {
        return(
          HTML("<span style='color: red;'><p>Error: The value for n must be a whole number greater than 0.</p></span>")
        )
      }
    })
    
    # Alternate hypothesis (rendered) output.
    output$alternate_hypothesis_output <- renderUI({
      
      # Specify alternate hypothesis in reference to whether the user chooses to do a one-sided or two-sided test.
      hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\( p", "\\neq", as.character(null_prop()), "\\)</p>")
      if (input$alternate_hypothesis_choice == 2) {
        hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\( p", ">", as.character(null_prop()), "\\)</p>")
      } else if (input$alternate_hypothesis_choice == 3) {
        hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\( p", "<", as.character(null_prop()), "\\)</p>")
        
      }
      return (
        tagList(
          HTML("<center>"),
          withMathJax(HTML(hypothesis)),
          HTML("</center>")
        )
      )
    })
    
    # Null hypothesis (rendered) output.
    output$null_hypothesis_output <- renderUI({
      hypothesis = paste("<p style='font-size: 16px;'>\\( H_0: \\) \\( p", "=", as.character(null_prop()), "\\)</p>")
      return (
        tagList(
          HTML("<center>"),
          withMathJax(HTML(hypothesis)),
          HTML("</center>")
        )
      )
    })
    
    # Text output for assumption 2 (normal distribution).
    output$assumption2_text_output <- renderUI({
      
      # Get whether we are are modelling the sample using the sum or mean.
      sample = "sum"
      if (input$box_sum_or_mean == 2) {
        sample = "mean"
      }
      
      string_1 = paste("<p>The third assumption is that the sample ", sample, "s follow an approximate normal distribution.</p>", sep = "")
      string_2 = paste("<p><span style='color: blue;'><b>How do we check?</b></span><br>
                     <ul>
                        <li>Recall that the central limit theorem tells us that if we take a sufficiently large number of draws from the box,
                             then the sample ", sample, "s will approximately follow a normal distribution. <i>If confused, please see the box model exercise</i>.</li>
                        <li>One way we can easily tell if the central limit theorem applies is to sample taking many draws from the box, and seeing whether the
                            values appear normally distributed.</li>
                        <li>The plot to the left shows the distribution of 10000 simulated samples.</li>
                        <li>Additionally, if the distribution of tickets is symmetric and/or normally distributed, you will need to take fewer draws from the box for the sample",
                        sample, "s to be normally distributed (i.e., a smaller n is needed for the central limit theorem to apply).</li>
                      <ul>
                      </p>", sep = "")
      return(
        tagList(
          HTML(string_1),
          HTML(string_2)
        )
      )
    })
    
    # Text asking user what their observed value is.
    output$observed_value_output <- renderUI({
      sample = "sum"
      if (input$box_sum_or_mean == 2) {
        sample = "mean"
      }
      string = paste("<p><i>What is the observed ", sample, " that you saw from your sample?</i></p>")
      return(
        tagList(
          HTML(string)
        )
      )
      
    })
    
    # Process the number of draws text input.
    observed_val = reactiveVal(0.73)
    observed_val_warning = reactiveVal(FALSE)
    observeEvent(input$observed_value, {
      if (is.na(input$observed_value) || input$observed_value < 0 || input$observed_value > 1) {
        observed_val(0.73)
        observed_val_warning(TRUE)
      } else {
        observed_val(input$observed_value)
        observed_val_warning(FALSE)
      }
    })
    
    # Error message for when the value for n is invalid.
    output$observed_val_warning_message <- renderUI({
      if (observed_val_warning()) {
        return(
          HTML("<span style='color: red;'><p>Error: The observed value must be between 0 and 1.</p></span>")
        )
      }
    })
    
    EV_string = reactiveVal("")
    SE_string = reactiveVal("")
    
    # Expected value and standard error output.
    output$ev_and_se_text <- renderUI({
      
      # Find EV and SE.
      mean_ = null_prop()
      sd_ = sqrt(null_prop() * (1-null_prop()))
      EV = sample_size() * mean_
      SE = sqrt(sample_size()) *sd_
      if (input$box_sum_or_mean == 2) { 
        EV = mean_
        SE = sd_/sqrt(sample_size())
      }
      
      # EV and SE text (changes based upon whether the sample sum or mean is being used).
      expected_value = ""
      standard_error = ""
      
      EV_string(as.character(round(EV, 5)))
      SE_string(as.character(round(SE, 5)))
      
      # Mean
      if (input$box_sum_or_mean == 2) { 
        expected_value = withMathJax(
          HTML("<p>Expected Value:</p>"),
          HTML(paste("$$\\begin{align*} \\text{EV} &= \\mu \\\\ &=", EV_string(), "\\end{align*}$$", sep = ""))
        )
        
        standard_error = withMathJax(
          HTML("<p>Standard Error:</p>"),
          HTML(paste("$$\\begin{align*} \\text{SE} &= \\frac{\\sigma}{\\sqrt{n}} \\\\ &= \\frac{", round(sd_, 5) , "}{\\sqrt{", 
                     as.character(sample_size()), "}}\\\\ &= ", SE_string(), "\\end{align*}$$", sep = ""))
        )
        
        # Sum
      } else {
        expected_value = withMathJax(
          HTML("<p>Expected Value:</p>"),
          HTML(paste("$$\\begin{align*} \\text{EV} &= n \\times \\mu \\\\ &=", as.character(sample_size()), "\\times", round(mean_, 5),
                     "\\\\ &= ", EV_string(), "\\end{align*}$$", sep = ""))
        )
        
        standard_error = withMathJax(
          HTML("<p>Standard Error:</p>"),
          HTML(paste("$$\\begin{align*} \\text{SE} &= \\sqrt{n} \\times \\sigma \\\\ &= \\sqrt{", as.character(sample_size()), "} \\times", 
                     round(sd_, 5), "\\\\ &= ", SE_string(), "\\end{align*}$$", sep = ""))
        )
      }
      
      return(
        tagList(
          expected_value, standard_error
        )
      )
    })
    
    test_stat = reactiveVal("")
    
    # Test statistic output.
    output$test_statistic_calculation <- renderUI({
      temp = (observed_val() - as.numeric(EV_string()))/as.numeric(SE_string())
      temp = as.character(round(temp, 4))
      test_stat(temp)
      t_stat = withMathJax(
        HTML(paste("$$\\begin{align*} \\text{TS} &= \\frac{\\text{OV} - \\text{EV}}{\\text{SE}} \\\\ &= \\frac{", as.character(round(observed_val(),5)), " - ", 
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
    
    p_val = reactiveVal(0)
    
    # P-value text output and calculation.
    output$p_value_prelude <- renderUI({
      
      # General prelude text about what the p-value is.
      first_string = HTML(paste("<p>The p-value is the probability of observing a test-statistic <b>more extreme that our test statistic of ", test_stat(), ".</b></p>", sep = ""))
      
      # Specifically how to find the p-value (based upon alternate hypothesis).
      second_string = "<p>The test statistics fall on a standard normal curve. "
      if (input$alternate_hypothesis_choice == 1) {
        negative_test_stat = as.character(-abs(as.numeric(test_stat())))
        positive_test_stat = as.character(abs(as.numeric(test_stat())))
        second_string = paste(second_string, "As we are doing a two-sided alternate hypothesis, we are interested in finding the <b>area below ", negative_test_stat, 
                              " and above ", positive_test_stat, ".</p></b>", sep = "")
      } else if (input$alternate_hypothesis_choice == 2){
        second_string = paste(second_string, "As we are doing a one-sided greater than alternate hypothesis, we are interested in finding the <b>area above ", test_stat(), 
                              ".</p></b>", sep = "")
      } else if (input$alternate_hypothesis_choice == 3){
        second_string = paste(second_string, "As we are doing a one-sided less than alternate hypothesis, we are interested in finding the <b>area below ", test_stat(), 
                              ".</p></b>", sep = "")
      }
      second_string = HTML(second_string)
      
      # Calculate p-value.
      p_val_local = 0
      if (input$alternate_hypothesis_choice == 1) {
        p_val_local = 2 * (1 - pnorm(abs(as.numeric(test_stat()))))
      } else if (input$alternate_hypothesis_choice == 2) {
        p_val_local = 1 - pnorm(as.numeric(test_stat()))
      } else if (input$alternate_hypothesis_choice == 3) {
        p_val_local = pnorm(as.numeric(test_stat()))
      }
      p_val(p_val_local)
      
      # String to output the p-value.
      p_value = withMathJax(HTML("<p style='font-size: 16px; text-align: center;'>\\( p =", as.character(round(p_val_local,5)) ,"\\)</p>"))
      
      return(
        tagList(
          first_string,
          second_string,
          p_value
        )
      )
      
    })
    
    # Process significance level text input.
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
    output$conclusion_output <- renderUI({
      
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
    
    output$confidence_level_output = renderUI({
      p_hat = as.numeric(input$observed_value)
      n = as.numeric(input$number_of_draws)
      conf_level = as.numeric(confidence_level())
      alpha = 1 - conf_level
      p0 = as.numeric(input$null_porportion)
      
      formula_line = substitution_line = answer_line = NULL
      conclusion_text = NULL
      
      wilson_ci = function(p_hat, n, z_val) {
        denom = 1 + z_val^2 / n
        margin = z_val * sqrt(p_hat*(1 - p_hat)/n + z_val^2/(4*n^2))
        lower = (p_hat + z_val^2/(2*n) - margin) / denom
        upper = (p_hat + z_val^2/(2*n) + margin) / denom
        return(list(lower=lower, upper=upper))
      }
      
      if (input$alternate_hypothesis_choice == 1) {
        z_val = qnorm(1 - alpha/2)
        ci_vals = wilson_ci(p_hat, n, z_val)
        lower = ci_vals$lower
        upper = ci_vals$upper
        
        formula_line = "$$CI = \\left( \\frac{\\widehat{p} + z^2/(2n) - z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n}, \\frac{\\widehat{p} + z^2/(2n) + z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n} \\right)$$"
        
        substitution_line = paste0(
          "$$CI = \\left( \\frac{", round(p_hat,4), " + ", round(qnorm(1 - alpha/2),4)^2, "/(2*", n, ") - ",
          round(qnorm(1 - alpha/2),4), " \\times \\sqrt{(", round(p_hat,4), "*(1-", round(p_hat,4), ")/", n, ") + (", round(qnorm(1 - alpha/2),4)^2, "/(4*", n, "^2))}}{1 + ", round(qnorm(1 - alpha/2),4)^2, "/", n, "}, ",
          "\\frac{", round(p_hat,4), " + ", round(qnorm(1 - alpha/2),4)^2, "/(2*", n, ") + ", round(qnorm(1 - alpha/2),4), " \\times \\sqrt{(", round(p_hat,4), "*(1-", round(p_hat,4), ")/", n, ") + (", round(qnorm(1 - alpha/2),4)^2, "/(4*", n, "^2))}}{1 + ", round(qnorm(1 - alpha/2),4)^2, "/", n, "} \\right)$$"
        )
        
        answer_line = paste0("$$CI = (", round(lower,4), ", ", round(upper,4), ")$$")
        
        if (p0 < lower || p0 > upper) {
          conclusion_text = "As the null proportion is outside the confidence interval, we <b>reject the null hypothesis</b>."
        } else {
          conclusion_text = "As the null proportion is inside the confidence interval, we <b>fail to reject the null hypothesis</b>."
        }
        
      } else if (input$alternate_hypothesis_choice == 2) {
        z_val = qnorm(1 - alpha)
        ci_vals = wilson_ci(p_hat, n, z_val)
        lower = ci_vals$lower
        
        formula_line = "$$CI = \\left( \\frac{\\widehat{p} + z^2/(2n) - z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n}, \\infty \\right)$$"
        
        substitution_line = paste0(
          "$$CI = \\left( \\frac{", round(p_hat,4), " + ", round(z_val,4)^2, "/(2*", n, ") - ", round(z_val,4),
          " \\times \\sqrt{", round(p_hat,4), "*(1-", round(p_hat,4), ")/", n, " + ", round(z_val,4)^2, "/(4*", n, "^2)}}{1 + ", round(z_val,4)^2, "/", n, "}, \\infty \\right)$$"
        )
        
        answer_line = paste0("$$CI = (", round(lower,4), ", \\infty)$$")
        
        if (p0 < lower) {
          conclusion_text = "As the null proportion is below the confidence interval, we <b>reject the null hypothesis</b>."
        } else {
          conclusion_text = "As the null proportion is inside the confidence interval, we <b>fail to reject the null hypothesis</b>."
        }
        
      } else if (input$alternate_hypothesis_choice == 3) {
        z_val = qnorm(1 - alpha)
        ci_vals = wilson_ci(p_hat, n, z_val)
        upper = ci_vals$upper
        
        formula_line = "$$CI = \\left( -\\infty, \\frac{\\widehat{p} + z^2/(2n) + z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n} \\right)$$"
        
        substitution_line = paste0(
          "$$CI = \\left(-\\infty, \\frac{",  
          round(p_hat,4), " + ", round(z_val,4)^2, "/(2*", n, ") + ",  
          round(z_val,4), " \\times \\sqrt{(", round(p_hat,4), "*(1-", round(p_hat,4), ")/", n, ") + (", round(z_val,4)^2, "/(4*", n, "^2))}}{1 + ",  
          round(z_val,4)^2, "/", n, "}\\right)$$"
        )
        
        answer_line = paste0("$$CI = (-\\infty, ", round(upper,4), ")$$")
        
        if (p0 > upper) {
          conclusion_text = "As the null proportion is above the confidence interval, we <b>reject the null hypothesis</b>."
        } else {
          conclusion_text = "As the null proportion is inside the confidence interval, we <b>fail to reject the null hypothesis</b>."
        }
      }
      
      tagList(
        withMathJax(HTML(formula_line)),
        withMathJax(HTML(substitution_line)),
        withMathJax(HTML(answer_line)),
        HTML(paste0("<span style='color: blue;'><p>", conclusion_text, "</p></span>"))
      )
    })
    
    
    ############################ Modal Intro ############################# 
    
    # Text
    observeEvent(input$learning_text, {
      showModal(modalDialog(
        title = "The 'Proportion Test'",
        
        HTML("<p>
            When you walk into a room, you can be assured that lots of people will be Taylor Swift fans (you could say that people are cray-cray for Tay-Tay).<br><br>
            
            Let's say that we <b>'hypothesise' that 70% of people are Taylor Swift fans</b>. Now, to test our hypothesis, we go to our data science class of <b>30 students</b>
            and ask <b>how many students are Taylor Swift fans</b>. The main goal here is to see if our <b>sample</b> is consistent with our 70% hypothesis.<br><br>
            
            In this regard, we set up a <b>null hypothesis</b> that the <b>proportion of Taylor Swift fans is 0.7</b> (70%). We mathematically write this as:<br>
        </p>"),
        withMathJax(HTML("<center><p style='font-size: 16px'>\\( H_{0} : p = 0.7 \\)</p></center>")),
        HTML("<p><br>
           We set up our alternate hypothesis to be that the proportion of Taylor Swift fans is <b>NOT 0.7</b>. We mathematically write this as:
        </p>"),
        withMathJax(HTML("<center><p style='font-size: 16px'>\\( H_{1} : p \\neq 0.7 \\)</p></center>")),
        HTML("<p><br>
           The main idea is that from our sample of 30 students, we want to see if there is <b>evidence to support or reject</b> the null hypothesis.
        </p>"),
        fluidRow(
          column(8,
                 HTML("<p>
                 <h5><u>How do we go about doing this?</u></h5><br>
              
                  We set up our box assuming that the <b>null hypothesis is true</b>. We add tickets valued <b>\"1\" to represent our target</b> (in this example, that someone 
                  likes Taylor Swift), and <b>\"0\" to represent the complement</b>. The tickets are arranged such that the proportion of \"1\" tickets is the same as the null 
                  hypothesis. As seen in the box model to the right, there are seven \"1\" tickets and three \"0\" tickets, meaning the proportion of \"1\"'s is correctly 70%.<br><br>
                  
                  After setting up our box model, we want to see if what we observe from the class is consistent with the null hypothesis. We call what we observe the <b>observed
                  value (OV)</b>.<br><br>
                  
                  Let's say that in the class, 22 people like Taylor Swift, and 8 do not. We can represent the observed value derived from this observation using the <b>mean</b>
                  or the <b>sum</b>.<br><br>
                  
                  <u>Mean:</u>
                  
                 </p>"),
                 withMathJax(
                   HTML(paste("$$\\begin{align*} \\text{OV} &= \\frac{1 \\times \\text{Number of TS Fans} + 0 \\times \\text{Number of Non-TS Fans}}{\\text{Data Science Class Size}} 
                            \\\\ &= \\frac{1 \\times 22 + 0 \\times 8}{30} \\\\ &= \\frac{22}{30} \\\\ &= 0.73 \\text{ (2 decimal places)} \\end{align*}$$", sep = ""))
                 ),
                 HTML("<p><u>Sum:</u></p>"),
                 withMathJax(
                   HTML(paste("$$\\begin{align*} \\text{OV} &= 1 \\times \\text{Number of TS Fans} + 0 \\times \\text{Number of Non-TS Fans} 
                            \\\\ &= 1 \\times 22 + 0 \\times 8 \\\\ &= 22 \\end{align*}$$", sep = ""))
                 ),
          ),
          column(4,
                 HTML("<br>"),
                 grVizOutput(ns("intro_example_box_model"), width = "80%", height = "70%"),
          )
        ),
        HTML("<p><br>
              Now that we've introduced how to set up the box for this test, as well as what the observed value is, it's your turn to use this app to test whether our
              observed value is consistent with our null hypothesis or not. Good luck :)
              </p>"),
        easyClose = TRUE,
        footer = modalButton("Close"),
      ))
    })
    
    # Example box model
    output$intro_example_box_model <- renderGrViz({
      string = "digraph diagram {
        graph [layout = dot, rankdir = TB]
      
        node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5]
        box [label = '1, 1, 1, 1, 1, 1, 1, 0, 0, 0']
      
        node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]
        sample [label = 'Data Science Class']
      
        edge [minlen = 2]
          box->sample [label = '  n = 30', fontsize = 12, labeldistance = 5]
        }"
      return(grViz(string))
    })
    
    ################################################################
    
    ############################ Plots ############################# 
    
    output$box_model <- renderGrViz({
      
      tickets_string = ""
      
      prop_temp = null_prop() * 100
      
      # Find simplified proportion of 1's and 0's so that they are equivalent to the proportion provided in the null hypothesis.
      gcd_value = gcd(prop_temp, 100 - prop_temp)
      proportion_of_1s = prop_temp / gcd_value
      proportion_of_0s = (100 - prop_temp) / gcd_value
      
      # Case where there are too many digits. Write using percentages.
      if (gcd_value < 5) { # A value smaller than 5 can be chosen. But, much smaller values will lead to too many values in the box.
        prop_temp_complement = 100 - prop_temp
        tickets_string = paste("1 x ", as.character(round(prop_temp, digits = 2)), "%, 0 x ", as.character(round(prop_temp_complement, digits = 2)), "%", sep = "")
        
      } else {
        
        # Add 1's to the string
        for (i in 1:proportion_of_1s) {
          if (proportion_of_1s == 0) {
            break
          }
          tickets_string = paste(tickets_string, "1,")
        } 
        
        # Add 0's to the string.
        for (i in 1:proportion_of_0s) {
          if (proportion_of_0s == 0) {
            break
          }
          tickets_string = paste(tickets_string, "0,")
        } 
        tickets_string <- sub(",$", "", tickets_string)
        
      }
      
      # Get other elements for the box model
      n = sample_size()
      sample = "Sample Sum"
      if (input$box_sum_or_mean == 2) {
        sample = "Sample Mean"
      }
      
      # Set up graph and box
      diagram = "digraph diagram { graph [layout = dot, rankdir = TB] node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5] box [label = '"
      diagram = paste(diagram, tickets_string, "']", sep = "")
      
      # Set up sample circle.
      diagram = paste(diagram, " node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]sample [label = '", sample, "']", sep = "")
      
      # Create edge between box and circle.
      # Annotate edge with n value.
      diagram = paste(diagram, " edge [minlen = 2] box->sample [label = '  n = ", n, "', fontsize = 12, labeldistance = 5]}", sep = "")
      
      return (grViz(diagram))
    })
    
    # Histogram with normal curve to shown normal curve approximation.
    output$empirical_draws_hist <- renderPlot({
      
      x_axis_string <- "Sample Sum Value"
      title_string <- "Empirical Distribution of 10000 Sample Sums with\nOverlaid Normal Curve"
      if (input$box_sum_or_mean == 2) {
        title_string <- "Empirical Distribution of 10000 Sample Means with\nOverlaid Normal Curve"
        x_axis_string <- "Sample Mean Value"
      }
      
      # Generate data based on whether we're summing or taking mean
      data <- if (input$box_sum_or_mean == 1) {
        replicate(10000, sum(sample(c(1, 0), sample_size(), replace = TRUE, prob = c(null_prop(), 1 - null_prop()))))
      } else {
        replicate(10000, mean(sample(c(1, 0), sample_size(), replace = TRUE, prob = c(null_prop(), 1 - null_prop()))))
      }
      
      # Compute normal curve parameters
      mean_ <- null_prop()
      sd_ <- sqrt(null_prop() * (1 - null_prop()))
      EV <- if (input$box_sum_or_mean == 1) sample_size() * mean_ else mean_
      SE <- if (input$box_sum_or_mean == 1) sqrt(sample_size()) * sd_ else sd_ / sqrt(sample_size())
      
      # Choose number of bins
      bins_to_include <- min(length(table(data)), 20)
      breaks_seq <- seq(min(data), max(data), length.out = bins_to_include + 1)
      
      # Plot histogram with density
      hist_out <- hist(data, breaks = breaks_seq, freq = FALSE,
                       col = "lightgreen", border = "black",
                       xlab = x_axis_string, main = title_string)
      
      # Overlay normal density curve
      x_vals <- seq(min(data), max(data), length.out = 300)
      y_vals <- dnorm(x_vals, mean = EV, sd = SE)
      lines(x_vals, y_vals, col = "red", lwd = 2)
    })
    
    
    # Histogram with normal curve to shown normal curve approximation.
    output$test_stat_normal_plot = renderPlot({
      return(curve_shaded_test_stat(dnorm, list(mean = 0, sd = 1), as.numeric(test_stat()), input$alternate_hypothesis_choice))
    })
    
    
    
    ################################################################
    
  })
  
}