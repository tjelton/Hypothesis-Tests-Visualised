twoSampleTTestServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ############################ Uploading Data Mechanism ############################# 
    
    sample_data_session = load_data_2_sample_Server(id = "loading_data")
    
    # Store the sample data.
    data_sample_1 = reactiveVal(NULL)
    data_sample_2 = reactiveVal(NULL)
    
    # Variable that is true when the data has been specified, meaning the rest of the exercise can commence
    # This variable can be accessed by the ui conditional panel.
    output$render_rest_of_exercise = reactive({
      !is.null(sample_data_session$data_sample_1())
      data_sample_1(sample_data_session$data_sample_1())
      data_sample_2(sample_data_session$data_sample_2())
    })
    outputOptions(output, "render_rest_of_exercise", suspendWhenHidden = FALSE)
    
    ############################ Modal Intro ############################# 
    observeEvent(input$learning_text, {
      showModal(modalDialog(
        title = "The 2-sample t-test",
        withMathJax(HTML("<p>
           Let’s consider that we have two different types of medicine: Drug A and Drug B. We are interested in determining whether there is a difference in the average 
           blood pressure when people take these medicines.
           <br><br>
           
           To investigate this, we collect two samples of people. We assign the first sample to try Drug A, and the second sample to try Drug B. This setup leads us to a
           2-sample t-test, a statistical method used to assess whether there is a significant difference between the population means of two independent groups, based on 
           their sample data.
           <br><br>
           
           A key difference between this experimental design and the paired t-test is that in the 2-sample case, participants are only ever assigned to one group 
           (either Drug A or Drug B). In a paired t-test, the same individuals would be assigned to both conditions, allowing us to examine the within-person differences.
           <br><br>
           
           We let the average blood pressure for the people in sample one after taking Drug A be represented by \\(\\mu_1\\) (the 1 because it is sample 1), and the average
           for the second sample (those who took Drug B) be represented by \\(\\mu_2\\).
           <br><br>
           
           The null hypothesis for a 2-sample t-test is that there is no significant difference between the population means of the two samples. This can be written as:
           
           <center><p style='font-size: 16px'>\\( H_{0} : \\mu_1 = \\mu_2 \\)</p></center>
           
           or equivalently:
           
           <center><p style='font-size: 16px'>\\( H_{0} : \\mu_1 - \\mu_2 = 0 \\)</p></center>
           
           If we are interested in testing whether there is a difference between the medicines (i.e., we don't care about the direction of the difference), we are using a 
           2-sided alternative hypothesis. We would hence write the alternate hypothesis as:
           <br><br>

           <center><p style='font-size: 16px'>\\( H_{0} : \\mu_1 \\neq \\mu_2 \\)</p></center>
           
           or equivalently:
           
           <center><p style='font-size: 16px'>\\( H_{0} : \\mu_1 - \\mu_2 \\neq 0 \\)</p></center>
           
           Now that we have set up our hypotheses, let's discuss how you can find the test-statistic for this test.
      </p>")),
        fluidRow(
          column(8,
                 withMathJax(HTML("<p>
                   <h5><u>Investigating the difference in 2 blood pressure medications:</u></h5><br>
                   
                   So far, we have only looked at hypothesis tests that involve a single sample (1-sample z-test, proportion test, 1-sample t-test, paired t-test).
                   The way that we conceptualised these tests was through the box model, which worked well because we only had a single sample.
                   <br><br>
                   
                   However, now we are looking at the 2-sample t-test, which means that we have 2 samples. Not to worry - we will use 2-box models, one to 
                   represent each sample.
                   <br><br>
                   
                   Let's start with the box model for sample 1. We will start with the basics. \\(n_1\\) is equal to 50, because we measured the blood pressure of 50 people 
                   in our Drug A sample. From these 50 people, we calculated the average of their blood pressures, yielding our observed value \\(OV_1\\) of 121.004. 
                   \\(s_1\\) is the sample standard deviation. This is because we are using a t-test, and in a t-test, we approximate the population standard deviation
                   using the sample standard deviation. 
                   <br><br>
                   
                   You may still be wondering what \\(\\mu_1 = \\mu_2\\) stands for. This is based on the fact that in a 2-sample t-test, our null hypothesis is that the 
                   mean of sample 1 equals the mean of sample 2. Hence, \\(\\mu_1 = \\mu_2\\).
                   <br><br>

                   When considering the box model for sample 2, it is the same idea, except using the values that we measured among the people assigned to the sample 2 group. 
                   Note that here, \\(n_2\\) is also equal to 50. However, for a 2-sample t-test, there is no requirement that \\(n_1 = n_2\\)! Similarly, we write 
                   \\(\\mu_2 = \\mu_1\\), as under our null hypothesis, we assume the mean of sample 2 is equal to the mean of sample 1.
                   </p>"))
          ),
          column(4,
                 HTML("<center>"),
                 HTML("<br>"),
                 HTML("<h5><b>Sample 1</b></h5>"),
                 grVizOutput(ns("box_model_modal_sample_1"), width = "60%", height = "40%"),
                 HTML("<br>"),
                 HTML("<h5><b>Sample 2</b></h5>"),
                 grVizOutput(ns("box_model_modal_sample_2"), width = "60%", height = "40%"),
                 HTML("</center>")
          )
        ),
        fluidRow(
          column(12,
                 HTML("<br><p>
               <b><span style='color: blue;'>Now it's off to you! </span></b>Together, we worked through the null and alternate hypotheses, and explored the box models for 
               each sample. Now you need to go through and check the assumptions for a 2-sample t-test, find the test statistic, and finally calculate the p-value. To continue
               with the blood pressure example, in the \"Input Sample Data\" section, select \"Pre-uploaded Data\" and then select the \"blood-press\" data set.
               </p>")
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
      ))
    })
    
    output$box_model_modal_sample_1 <- renderGrViz({
      
      data = blood_pressure$blood_pressure[blood_pressure$drug == "Drug_A"]
      
      # Compose label text with Unicode subscripts and rounded stats
      pop_details <- paste0("μ₁ = μ₂ ; s₁ = ", round(sd(data), 3))
      
      # Build the Graphviz diagram string
      diagram <- paste0(
        "digraph diagram {
      graph [layout = dot, rankdir = TB]
      
      node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12]
      box [label = \"", pop_details, "\"]
      
      node [shape = oval, width = 1.5, fillcolor = \"#f9ffbd\", fontsize = 12]
      sample [label = \"OV₁ = ", round(mean(data, na.rm = TRUE), 3), "\"]
      
      edge [minlen = 2]
      box -> sample [label = \"  n₁ = ", length(data), "\", fontsize = 12, labeldistance = 5]
    }"
      )
      grViz(diagram)
    })
    
    output$box_model_modal_sample_2 <- renderGrViz({
      
      data = blood_pressure$blood_pressure[blood_pressure$drug == "Drug_B"]
      
      # Compose label text with Unicode subscripts and rounded stats
      pop_details <- paste0("μ₁ = μ₂ ; s₁ = ", round(sd(data), 3))
      
      # Build the Graphviz diagram string
      diagram <- paste0(
        "digraph diagram {
      graph [layout = dot, rankdir = TB]
      
      node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12]
      box [label = \"", pop_details, "\"]
      
      node [shape = oval, width = 1.5, fillcolor = \"#f9ffbd\", fontsize = 12]
      sample [label = \"OV₁ = ", round(mean(data, na.rm = TRUE), 3), "\"]
      
      edge [minlen = 2]
      box -> sample [label = \"  n₁ = ", length(data), "\", fontsize = 12, labeldistance = 5]
    }"
      )
      grViz(diagram)
    })
    
    
    
    output$box_model_sample_1 <- renderGrViz({
      
      # Compose label text with Unicode subscripts and rounded stats
      pop_details <- paste0("μ₁ = μ₂ ; s₁ = ", round(sd(data_sample_1()), 3))
      
      # Build the Graphviz diagram string
      diagram <- paste0(
        "digraph diagram {
      graph [layout = dot, rankdir = TB]
      
      node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12]
      box [label = \"", pop_details, "\"]
      
      node [shape = oval, width = 1.5, fillcolor = \"#f9ffbd\", fontsize = 12]
      sample [label = \"OV₁ = ", round(mean(data_sample_1(), na.rm = TRUE), 3), "\"]
      
      edge [minlen = 2]
      box -> sample [label = \"  n₁ = ", length(data_sample_1()), "\", fontsize = 12, labeldistance = 5]
    }"
      )
      grViz(diagram)
    })
    
    output$box_model_sample_2 <- renderGrViz({
      
      # Compose label text with Unicode subscripts and rounded stats
      pop_details <- paste0("μ₂ = μ₁ ; s₂ = ", round(sd(data_sample_2()), 3))
      
      # Build the Graphviz diagram string
      diagram <- paste0(
        "digraph diagram {
      graph [layout = dot, rankdir = TB]
      
      node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12]
      box [label = \"", pop_details, "\"]
      
      node [shape = oval, width = 1.5, fillcolor = \"#f9ffbd\", fontsize = 12]
      sample [label = \"OV₂ = ", round(mean(data_sample_2(), na.rm = TRUE), 3), "\"]
      
      edge [minlen = 2]
      box -> sample [label = \"  n₂ = ", length(data_sample_2()), "\", fontsize = 12, labeldistance = 5]
    }"
      )
      grViz(diagram)
    })
    
    # Alternate hypothesis (rendered) output.
    output$alternate_hypothesis_output <- renderUI({
      hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\(\\mu_1")
      
      # Specify alternate hypothesis in reference to whether the user chooses to do a one-sided or two-sided test.
      if (input$alternate_hypothesis_choice == 1) {
        hypothesis = paste(hypothesis, "\\neq \\mu_2 \\)</p>")
      } else if (input$alternate_hypothesis_choice == 2) {
        hypothesis = paste(hypothesis, ">  \\mu_2 \\)</p>")
      } else if (input$alternate_hypothesis_choice == 3) {
        hypothesis = paste(hypothesis, "<  \\mu_2 \\)</p>")
      }
      
      return (
        tagList(
          HTML("<center>"),
          withMathJax(HTML(hypothesis)),
          HTML("</center>")
        )
      )
    })
    
    output$qqplot_sample_1 <- renderPlot({
      qqnorm(data_sample_1(), main = "Sample 1: QQ Plot")
      qqline(data_sample_1(), col = "red")
    })
    
    output$qqplot_sample_2 <- renderPlot({
      qqnorm(data_sample_2(), main = "Sample 2: QQ Plot")
      qqline(data_sample_2(), col = "red")
    })
    
    output$boxplot_assumption_checking <- renderPlot({
      boxplot(list(
        `Sample 1` = data_sample_1(), 
        `Sample 2` = data_sample_2()
      ),
      horizontal = TRUE,
      col = c("blue", "red"),
      main = "Side-by-side Boxplots",
      ylab = "Samples")
    })
    
    output$histogram_assumption_checking <- renderPlot({
      # Save original plotting settings
      old_par <- par(no.readonly = TRUE)
      
      # Stack plots: 2 rows, 1 column
      par(mfrow = c(1, 2))
      
      # Histogram - Sample 1
      hist(data_sample_1(),
           breaks = 30,
           col = "blue",
           border = "black",
           main = "Sample 1 Histogram",
           xlab = "Values",
           ylab = "Frequency")
      
      # Histogram - Sample 2
      hist(data_sample_2(),
           breaks = 30,
           col = "red",
           border = "black",
           main = "Sample 2 Histogram",
           xlab = "Values",
           ylab = "Frequency")
      
      # Restore settings
      par(old_par)
    })
    
    output$standard_deviation_assumption_checking <- renderUI({
      
      sd_1 = sd(data_sample_1())
      sd_2 = sd(data_sample_2())
      
      return(
        tagList(
          HTML(
            paste0(
              '<div style="text-align: right;">',
              "<p><b>Sample 1's standard deviation = ", round(sd_1, 3), "<br>",
              "Sample 2's standard deviation = ", round(sd_2, 3), "</b></p>",
              "</div>"
            )
          )
        )
      )
    })
    
    output$same_spread_output_decision <- renderUI({
      bool = input$spread_toggle
      string = "<p>You have indicated that the <b>spread</b> of the 2 samples <b>is the same</b>. Hence, below we will do a <b>2-sample t-test with eqaul variance.</b></p>"
      if (bool == FALSE) {
        string = "<p>You have indicated that the <b>spread</b> of the 2 samples <b>is different</b>. Hence, below we will do a <b>Welch 2-sample t-test.</b></p>"
      }
      return(HTML(string))
    })
    
    pooled_sd_store = reactiveVal(1)
    se_store = reactiveVal(1)
    
    # Expected value and standard error output.
    output$ev_and_se_text <- renderUI({
      
      n1 = length(data_sample_1())
      n2 = length(data_sample_2())
      sd1 = sd(data_sample_1())
      sd2 = sd(data_sample_2())
      pooled_sd = sqrt( ((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))
      pooled_sd_store(pooled_sd)
      se = pooled_sd * sqrt((1/n1)+(1/n2))
      se_store(se)
      
      # Find EV and SE.
      # sd_ = sd(sample_data())
      # EV = mean_
      # SE = sd_ / sqrt(sample_size)
      # 
      # SE_string(as.character(round(SE, 5)))
      
      EV_calculation_string = "$$\\begin{align*} \\text{EV} &= \\mu_2 - \\mu_1"
      EV_calculation_string = paste(EV_calculation_string, "\\\\ &=", 0, "\\end{align*}$$", sep = "")
      
      expected_value = withMathJax(
        HTML("<p>Expected Value:</p>"),
        HTML(EV_calculation_string)
      )
      
      pooled_sd_string = withMathJax(
        HTML("<p>Pooled standard deviation:</p>"),
        HTML(paste("$$\\begin{align*} \\widehat{\\sigma_p} &= \\sqrt{\\frac{ (n_1 - 1)\\widehat{\\sigma}_1^2 + (n_2 - 1)\\widehat{\\sigma}_2^2  }{n_1 + n_2 - 2}} \\\\
                                                         &= \\sqrt{\\frac{ (", n1, "- 1)", round(sd1, 3), "^2 + (", n2, "- 1)", round(sd2, 3), "^2 }{", n1, "+", n2, "- 2}} \\\\
                                                         &= \\sqrt{\\frac{ ", round((n1-1)*sd1^2, 3), "+", round((n2-1)*sd2^2,3), "}{", n1 + n2 - 2, "}} \\\\
                                                         &= \\sqrt{", round(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2), 3), "} \\\\
                                                         &= ", round(pooled_sd, 3), "\\end{align*}$$", sep = ""))
      )
      
      if (!input$spread_toggle) {
        se = sqrt( ((sd1^2)/n1) + ((sd2^2)/n2) )
        se_store(se)
        standard_error = withMathJax(
          HTML("<p>Standard Error:</p>"),
          HTML(paste("$$\\begin{align*} \\text{SE} &= \\sqrt{\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}} \\\\
                                                 &= \\sqrt{\\frac{", round(sd1^2,3), "}{", n1, "}+\\frac{", round(sd2^2,3), "}{", n2, "}} \\\\
                                                 &=", round(se, 5),  "\\end{align*}$$", sep = ""))
        )
        pooled_sd_string = ""
      } else {
        standard_error = withMathJax(
          HTML("<p>Standard Error:</p>"),
          HTML(paste("$$\\begin{align*} \\text{SE} &= \\widehat{\\sigma_p}\\sqrt{\\frac{1}{n_1}+\\frac{1}{n_2}} \\\\
                                                 &=", round(pooled_sd, 3), "\\sqrt{\\frac{1}{", n1, "}+\\frac{1}{", n2, "}} \\\\
                                                 &=", round(se, 5),  "\\end{align*}$$", sep = ""))
        )
      }
      
      return(
        tagList(
          expected_value, pooled_sd_string, standard_error
        )
      )
    })
    
    test_stat = reactiveVal("")
    
    # Test statistic output.
    output$test_statistic_calculation <- renderUI({
      
      observed_val = mean(data_sample_1(), na.rm = TRUE) - mean(data_sample_2(), na.rm = TRUE)
      
      # Calculate test statistic.
      temp = observed_val/se_store()
      temp = as.character(round(temp, 4))
      test_stat(temp)
      
      t_stat = withMathJax(
        HTML(paste("$$\\begin{align*} \\text{TS} &= \\frac{\\text{OV} - \\text{EV}}{\\text{SE}} \\\\ 
                                               &= \\frac{\\mu_1 - \\mu_2 - 0}{\\text{SE}} \\\\ 
                                               &= \\frac{", round(mean(data_sample_1(), na.rm = T),3), " - ", round(mean(data_sample_2(), na.rm = T),3), " - 0}{", round(se_store(), 3), "} \\\\
                                               &= ", test_stat(), "\\end{align*}$$", sep = ""))
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
    df = reactiveVal(0)
    
    # P-value text output and calculation.
    output$p_value_prelude <- renderUI({
      
      n1 = length(data_sample_1())
      n2 = length(data_sample_2())
      sd1 = sd(data_sample_1())
      sd2 = sd(data_sample_2())
      
      # General prelude text about what the p-value is.
      first_string = HTML(paste("<p>The p-value is the probability of observing a test-statistic <b>more extreme that our test statistic of ",
                                test_stat(), 
                                ".</b></p>", 
                                sep = ""))
      
      if (!input$spread_toggle) {
        numerator <- ((sd1^2 / n1) + (sd2^2 / n2))^2
        denominator <- ((sd1^2 / n1)^2) / (n1 - 1) + ((sd2^2 / n2)^2) / (n2 - 1)
        df_welch <- round(numerator / denominator, 3)
        df(numerator / denominator)
        second_string <- withMathJax(
          HTML("<p>For a Welch 2-sample t-test, we set the degrees of freedom equal to:</p>"),
          HTML(paste("$$\\begin{align*} \\text{df} &= \\frac{\\left( \\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2} \\right)^2}{\\frac{\\left( \\frac{s_1^2}{n_1} \\right)^2}{n_1 - 1} + \\frac{\\left( \\frac{s_2^2}{n_2} \\right)^2}{n_2 - 1}} \\\\
                                                 &= \\frac{\\left( \\frac{", round(sd1^2, 3), "}{", n1, "} + \\frac{", round(sd2^2, 3), "}{", n2, "} \\right)^2}{\\frac{\\left( \\frac{", round(sd1^2, 3), "}{", n1, "} \\right)^2}{", n1 - 1, "} + \\frac{\\left( \\frac{", round(sd2^2, 3), "}{", n2, "} \\right)^2}{", n2 - 1, "}} \\\\
                                                 &= ", df_welch, "\\end{align*}$$", sep = ""))
        )
      } else {
        df(n1+n2-2)
        second_string = withMathJax(
          HTML("<p>For a 2-sample t-test with equal variance, we set the degree of freedom equal to:</p>"),
          HTML(paste("$$\\begin{align*} \\text{df} &= n_1 + n_2 - 2\\\\
                                                                 &=", n1, "+", n2, " - 2 \\\\
                                                                 &=", n1 + n2 - 2,  "\\end{align*}$$", sep = ""))
        )
      }
      
      # Specifically how to find the p-value (based upon alternate hypothesis).
      third_string = "<p>The test statistics fall on a standard normal curve. "
      if (input$alternate_hypothesis_choice == 1) {
        negative_test_stat = as.character(-abs(as.numeric(test_stat())))
        positive_test_stat = as.character(abs(as.numeric(test_stat())))
        third_string = paste(third_string, "As we are doing a two-sided alternate hypothesis, we are interested in finding the <b>area below ", negative_test_stat,
                             " and above ", positive_test_stat, ".</p></b>", sep = "")
      } else if (input$alternate_hypothesis_choice == 2){
        third_string = paste(third_string, "As we are doing a one-sided greater than alternate hypothesis, we are interested in finding the <b>area above ", test_stat(),
                             ".</p></b>", sep = "")
      } else if (input$alternate_hypothesis_choice == 3){
        third_string = paste(third_string, "As we are doing a one-sided less than alternate hypothesis, we are interested in finding the <b>area below ", test_stat(),
                             ".</p></b>", sep = "")
      }
      third_string = HTML(third_string)
      
      # Calculate p-value.
      p_val_local = 0
      if (input$alternate_hypothesis_choice == 1) {
        p_val_local = 2 * (1 - pt(abs(as.numeric(test_stat())), df()))
      } else if (input$alternate_hypothesis_choice == 2) {
        p_val_local = 1 - pt(as.numeric(test_stat()), df())
      } else if (input$alternate_hypothesis_choice == 3) {
        p_val_local = pt(as.numeric(test_stat()), df())
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
      return(curve_shaded_test_stat(dt, list(df = df() - 1), as.numeric(test_stat()), input$alternate_hypothesis_choice))
    })
    
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
      
      x1 = data_sample_1()
      x2 = data_sample_2()
      
      n1 = length(x1)
      n2 = length(x2)
      mean1 = mean(x1)
      mean2 = mean(x2)
      var1 = var(x1)
      var2 = var(x2)
      
      conf_level = as.numeric(confidence_level())
      alpha = 1 - conf_level
      
      # Mean difference
      diff_means = mean1 - mean2
      
      # --- Choose variance method ---
      if (isTRUE(input$spread_toggle)) {
        # Equal variance (pooled)
        sp = sqrt(((n1 - 1)*var1 + (n2 - 1)*var2) / (n1 + n2 - 2))
        se = sp * sqrt(1/n1 + 1/n2)
        df = n1 + n2 - 2
      } else {
        # Welch
        se = sqrt(var1/n1 + var2/n2)
        df = ( (var1/n1 + var2/n2)^2 ) / 
          ( ((var1/n1)^2)/(n1-1) + ((var2/n2)^2)/(n2-1) )
      }
      
      formula_line = substitution_line = answer_line = conclusion_text = NULL
      
      # --- CI based on alternative hypothesis ---
      if (input$alternate_hypothesis_choice == 1) {
        # Two sided
        t_val = qt(1 - alpha/2, df = df)
        lower = diff_means - t_val * se
        upper = diff_means + t_val * se
        
        formula_line = "$$CI = (\\bar{x}_1 - \\bar{x}_2) \\pm t_{\\alpha/2, df} \\cdot SE$$"
        substitution_line = paste0("$$CI = ", round(diff_means,4),
                                   " \\pm t_{", round(alpha/2,4), ", ", round(df,2), "} \\times ",
                                   round(se,4), "$$")
        answer_line = paste0("$$CI = (", round(lower,4), ", ", round(upper,4), ")$$")
        
        if (0 < lower || 0 > upper) {
          conclusion_text = "As 0 (no difference) is outside the confidence interval, we <b>reject the null hypothesis</b>."
        } else {
          conclusion_text = "As 0 (no difference) is inside the confidence interval, we <b>fail to reject the null hypothesis</b>."
        }
        
      } else if (input$alternate_hypothesis_choice  == 2) {
        # Greater than (diff > 0)
        t_val = qt(1 - alpha, df = df)
        lower = diff_means - t_val * se
        
        formula_line = "$$CI = (\\bar{x}_1 - \\bar{x}_2 - t_{\\alpha, df} \\cdot SE, \\infty)$$"
        substitution_line = paste0("$$CI = (", round(diff_means,4),
                                   " - t_{", round(alpha,4), ", ", round(df,2), "} \\times ",
                                   round(se,4), ", \\infty)$$")
        answer_line = paste0("$$CI = (", round(lower,4), ", \\infty)$$")
        
        if (0 < lower) {
          conclusion_text = "As 0 is below the confidence interval, we <b>reject the null hypothesis</b>."
        } else {
          conclusion_text = "As 0 is inside the confidence interval, we <b>fail to reject the null hypothesis</b>."
        }
        
      } else if (input$alternate_hypothesis_choice  == 3) {
        # Less than (diff < 0)
        t_val = qt(1 - alpha, df = df)
        upper = diff_means + t_val * se
        
        formula_line = "$$CI = (-\\infty, \\bar{x}_1 - \\bar{x}_2 + t_{\\alpha, df} \\cdot SE)$$"
        substitution_line = paste0("$$CI = (-\\infty, ", round(diff_means,4),
                                   " + t_{", round(alpha,4), ", ", round(df,2), "} \\times ",
                                   round(se,4), ")$$")
        answer_line = paste0("$$CI = (-\\infty, ", round(upper,4), ")$$")
        
        if (0 > upper) {
          conclusion_text = "As 0 is above the confidence interval, we <b>reject the null hypothesis</b>."
        } else {
          conclusion_text = "As 0 is inside the confidence interval, we <b>fail to reject the null hypothesis</b>."
        }
      }
      
      # --- Build UI ---
      tagList(
        withMathJax(HTML(formula_line)),
        withMathJax(HTML(substitution_line)),
        withMathJax(HTML(answer_line)),
        HTML(paste0("<span style='color: blue;'><p>", conclusion_text, "</p></span>"))
      )
    })

  })
    
}