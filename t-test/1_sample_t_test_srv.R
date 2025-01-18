oneSampleTTestServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ############################ Modal Intro ############################# 
    
    observeEvent(input$learning_text, {
      showModal(modalDialog(
        title = "The '1-sample s-test'",
        
        withMathJax(HTML("<p>
              <u><i>If you have not read the ‘t-curve motivation’ exercise, it is highly recommended to read it before continuing.</i></u><br><br>

              Recall that we were guided by the following scenario in the ‘1-sample z-test’ exercise.<br><br>
          </p>")),
        fluidRow(
          column(1),
          column(10,
                 withMathJax(HTML("<p>
                  <i>\"Suppose that in a made-up country, there is a statistics exam that all high school students take. If a student received a mark in the exam of 140 or over, 
                  it means that they are demonstrating a satisfactory level of statistical knowledge as deemed by the education board. <span style='color: red;'>Based on data from
                  multiple years, it’s known the population standard deviation in test scores is 7.5.</span><br><br>
                  
                  Mr. Han is a statistics teacher who has been testing some new innovative teaching methods. In particular, he wants to test whether the average exam grade for the
                  25 students in his class is statistically greater than a mark of 140.\"</i><br><br>
              </p>"))
          ),
          column(1)
        ),
        withMathJax(HTML("<p>
              What if we were to remove the information in red, meaning that we no longer know the population standard deviation? The ‘1-sample z-test’ previously discussed
              no longer works, as the box model we have been studying requires that the population standard deviation is known (one of our key assumptions has been violated).<br><br> 
              
              As mentioned in the ‘t-curve motivation’ exercise, we can address this issue by approximating the population standard deviation using the sample standard deviation.
              We can easily get the sample standard deviation by using the sample data!<br><br>
              
              With this in mind, the remainder of this hypothesis test should be familiar! Firstly, Mr. Han's hypotheses are the same as in the ‘1-sample z-test’ exercise. As a 
              reminder, Mr. Han uses the symbol \\(\\mu \\) to represent the average mark of his class. He sets up the null hypothesis that his class’s average grade is 140. 
              Mathematically, this is written as:<br>

              <center><p style='font-size: 16px'>\\( H_{0} : \\mu = 140 \\)</p></center><br>

              When setting up the alternate hypothesis, because Mr. Han is interested in seeing if his innovative teaching approach leads to marks greater than 140, we set up a
              one-sided alternate hypothesis, written mathematically as:<br>

              <center><p style='font-size: 16px'>\\( H_{1} : \\mu > 140 \\)</p></center><br>

              Once again, the main idea is that Mr. Han will use his sample of 25 students to determine whether there is evidence to support or reject the null hypothesis.
          </p>")),
        fluidRow(
          column(8,
                 withMathJax(HTML("<p>
                   <h5><u>How does Mr. Han go about doing this?</u></h5><br>
                   
                   Similar to before, we can use the box model to model the hypothesis test. This time, let’s first turn our attention to the test scores that Mr. Han’s students 
                   achieved in the class. Let’s say that the 25 students had an average score of 142.843 As there were 25 students, we specify \\(n = 25 \\). The average score of 
                   142.843 is called the observed value (OV). Also, using the scores, Mr. Han found that the sample standard deviation equals 4.751 (\\(s = 4.751\\)).<br><br>

                   Using these values and \\(\\mu = 140\\) from the null hypothesis, we have all the information needed to create the box model, which is shown on the right.
                   </p>"))
          ),
          column(4,
                 HTML("<br>"),
                 grVizOutput(ns("intro_example_box_model"), width = "80%", height = "70%"),
          )
        ),
        HTML("<p><br>
                With the box made, Mr. Han can now calculate the test statistic. In the formula below, note that we use the notation SE estimate (the reason for this is 
                explained in the ‘t-curve motivation’ exercise).<br>
                </p>"),
        withMathJax(
          HTML(paste("$$\\begin{align*} \\text{Test-Statistic (TS)} &= \\frac{\\text{OV} - \\text{EV}}{\\hat{\\text{SE}}}
                              \\\\ &= \\frac{\\text{OV} - \\text{EV}}{\\frac{s}{\\sqrt{n}}}
                              \\\\ &= \\frac{142.843 - 140}{\\frac{4.751}{\\sqrt{25}}}
                              \\\\ &= 2.992 \\text{ (3 decimal places)} \\end{align*}$$", sep = ""))
        ),
        HTML("<p><br>
            With the test statistic found, Mr. Han is ready to find the p-value. However, this represents a big difference between the ‘1-sample  
            <span style='color: red;'>z</span>-test’ and the ‘1-sample  <span style='color: red;'>t</span>-test’. These hypothesis tests get the last part of their names from
            the type of curve used to find the p-values. The ‘z-test’ gets its name because the p-value is found from the standard normal distribution (also called the 
             <span style='color: red;'>z</span>-distribution). The ‘t-test’ gets its name because the p-value is found from the  <span style='color: red;'>t</span>-distribution.<br><br>
             
             As we are doing a t-test in this exercise, we will be using the t-distribution. Please continue following the exercise to see how this is done!
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
          box [label = '&mu; = 140; s = 4.751']

          node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]
          sample [label = 'OV = 142.843']

          edge [minlen = 2]
            box->sample [label = '  n = 25', fontsize = 12, labeldistance = 5]
          }"
      return(grViz(string))
    })
    
    ############################ Uploading Data Mechanism ############################# 
    
    sample_data_session <- load_1_sample_data_Server(id = "loading_data")
    
    # Store the sample data.
    sample_data <- reactiveVal(NULL)
    
    # Variable that is true when the data has been specified, meaning the rest of the exercise can commence
    # This variable can be accessed by the ui conditional panel.
    output$render_rest_of_exercise <- reactive({
      !is.null(sample_data_session$data())
      sample_data(sample_data_session$data())
    })
    outputOptions(output, "render_rest_of_exercise", suspendWhenHidden = FALSE)
    
    ################################################################
    
    # Box model plot
    output$box_model <- renderGrViz({

      if (is.null(input$null_mu)) {
        return()
      }

      # String with mu and sigma.
      pop_details = paste("&mu; = ", as.character(round(input$null_mu, digits = 3)), "; s = ", as.character(round(sd(sample_data()), digits = 3)))

      # Set up graph and box
      diagram = "digraph diagram { graph [layout = dot, rankdir = TB] node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5] box [label = '"
      diagram = paste(diagram, pop_details, "']", sep = "")

      # Set up sample circle.
      diagram = paste(diagram, " node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12] sample [label = '", "OV = ",
                      as.character(round(mean(sample_data(), na.rm = TRUE), digits = 3)), "']", sep = "")

      # Create edge between box and circle.
      # Annotate edge with n value.
      n = length(sample_data())
      diagram = paste(diagram, " edge [minlen = 2] box->sample [label = '  n = ", n, "', fontsize = 12, labeldistance = 5]}", sep = "")

      return (grViz(diagram))
    })
    
    # Null hypothesis (rendered) output.
    output$null_hypothesis_output <- renderUI({
      hypothesis = paste("<p style='font-size: 16px;'>\\( H_0: \\) \\( \\mu", "=", as.character(round(input$null_mu, digits = 3)), "\\)</p>")
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
      
      null_mean_string = as.character(round(input$null_mu, digits = 3))
      
      # Specify alternate hypothesis in reference to whether the user chooses to do a one-sided or two-sided test.
      hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\(\\mu", "\\neq", null_mean_string, "\\)</p>")
      if (input$alternate_hypothesis_choice == 2) {
        hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\(\\mu", ">", null_mean_string, "\\)</p>")
      } else if (input$alternate_hypothesis_choice == 3) {
        hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\(\\mu", "<", null_mean_string, "\\)</p>")
        
      }
      return (
        tagList(
          HTML("<center>"),
          withMathJax(HTML(hypothesis)),
          HTML("</center>")
        )
      )
    })
    
    EV_string = reactiveVal("")
    SE_string = reactiveVal("")
    
    # Expected value and standard error output.
    output$ev_and_se_text <- renderUI({
      
      sample_size = length(sample_data())
      
      # Find EV and SE.
      mean_ = input$null_mu
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
                            
                            <p><b>For a 1-sample t-test, we set the degree of freedom equal to the sample size - 1 (that is, \\(n - 1\\)).</b></p>
                            
                            </p>In this case, the degree of freedom is equal to \\(", length(sample_data()), " - 1 = ",length(sample_data()) - 1 ,"\\).</p>")))
      
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
          third_string,
          p_value
        )
      )
      
    })
    
    # Histogram with t curve to show p-value calculation.
    output$test_stat_t_plot = renderPlot({
      return(curve_shaded_test_stat(dt, list(df = length(sample_data()) - 1), as.numeric(test_stat()), input$alternate_hypothesis_choice))
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
    
  })
    
}