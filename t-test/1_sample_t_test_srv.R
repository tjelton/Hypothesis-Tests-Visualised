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
    
    null_mean_string = reactive({as.character(round(input$null_mu, digits = 3))})
    alt_hypothesis_session <- alternate_hypotheses_1_sample_t_test_Server(id = "alternate_hypothesis", null_mean_string)

    test_stat_session <- test_statistic_1_sample_t_test_Server(id = "test_stat", sample_data, null_mean_string)

    p_value_session <- p_value_1_sample_t_test_Server(id = "p_val", test_stat_session$test_stat, sample_data, alt_hypothesis_session$alternate_hypothesis_choice)

    conclusion_1_sample_t_test_Server(id = "conclusion", p_value_session$p_val)
    
  })
    
}