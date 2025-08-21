pairedTTestServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ############################ Modal Intro ############################# 
    observeEvent(input$learning_text, {
      showModal(modalDialog(
        title = "The 'paired t-test'",
        withMathJax(HTML("<p>
           When evaluating the effect of a treatment or intervention, it is often helpful to measure outcomes both <i>before</i> and <i>after</i> the treatment on the same
           individuals. This kind of design is called a <b>paired sample</b> because each person contributes two related measurements: one before and one after treatment.
           <br><br>
           
           To explore this, we are going to be using the example of a new medicine (which we will refer simply to as Drug A), which was developed to alter
           someone's blood pressure. This is important as some people may have elevated blood pressure, meaning they could be at risk of serious health implications.
           <br><br>
           
           Why use a paired sample? A paired design helps control for individual differences. For example, some people naturally have higher or lower blood pressure than
           others. By comparing each person’s blood pressure to their own baseline, we reduce variability and increase the ability to detect a real effect of the drug.
           <br><br>
           
           The paired t-test is used to determine whether the average difference between the paired measurements is significantly different from some hypothesised value. More
           formally, let \\(\\mu_1\\) be the average blood pressure before taking the drug, and let \\(\\mu_1\\) be the average blood pressure after taking the drug. We define
           the difference \\(\\mu_d = \\mu_2 - \\mu_1 \\).
           <br><br>
           
           In this example, we are simply interested in whether the drug causes a change to someone's blood pressure. Hence, the null hypothesis is:
           
           <center><p style='font-size: 16px'>\\( H_{0} : \\mu_d = 0 \\)</p></center><br>

           We set up the alternate hypothesis to be that the drug does cause a change in blood pressure. Hence, the alternate hypothesis is:
           
           <center><p style='font-size: 16px'>\\( H_{1} : \\mu_d \\neq 0 \\)</p></center><br>
           
           <b><span style='color: blue;'>KEY TAKEAWAY:</span></b> Now that we have defined \\(\\mu_d\\) to be \\(\\mu_2 - \\mu_1 \\), we can see that we actually now only
           have a <b>single sample</b>. Hence, a paired t-test is actually just a 1-sample t-test that we learnt previously (you should do that exercise first if you have
           not already completed it).
           
           Regardless, let's continue with this example to work out if we see a blood pressure change when our sample takes the drug.
      </p>")),
        fluidRow(
          column(8,
                 withMathJax(HTML("<p>
                   <h5><u>Investigating the change in blood pressure:</u></h5><br>
                   
                   Identical to the 1-sample t-test exercise, we can use the box model to model the hypothesis test. We first find the sample standard deviation.
                   To do this, we subtract the blood pressure values from before taking the drug from the measured values after taking the drug. We find that the sample standard
                   deviation is equal to 17.346. Additionally, from this data, we know that the observed value (OV) is -4.373. This means that from our sample, on average, blood 
                   pressure decreases by 4.373 mmHg (we still have to investigate whether this is statistically different from 0).
                   <br><br>
                   
                   We also have that \\(n = 30\\) as there are 30 people in the sample, and \\(\\mu = 0\\), which comes from the null hypothesis.
                   </p>"))
          ),
          column(4,
                 HTML("<br><br>"),
                 grVizOutput(ns("intro_example_box_model"), width = "80%", height = "70%"),
          )
        ),
        HTML("<p><br>
                With the box made, we can now calculate the test statistic.<br>
                </p>"),
        withMathJax(
          HTML(paste("$$\\begin{align*} \\text{Test-Statistic (TS)} &= \\frac{\\text{OV} - \\text{EV}}{\\hat{\\text{SE}}}
                              \\\\ &= \\frac{\\text{OV} - \\text{EV}}{\\frac{s}{\\sqrt{n}}}
                              \\\\ &= \\frac{4.373 - 0}{\\frac{17.346}{\\sqrt{30}}}
                              \\\\ &= -1.381 \\text{ (3 decimal places)} \\end{align*}$$", sep = ""))
        ),
        HTML("<p>With the test-statistic found, you can now find the p-value. Scroll down in the lesson to see how this is done!</p>"),
        easyClose = TRUE,
        footer = modalButton("Close"),
      ))
    })
    
    # Example box model
    output$intro_example_box_model <- renderGrViz({
      string = "digraph diagram {
          graph [layout = dot, rankdir = TB]

          node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5]
          box [label = '&mu; = 0; s = 4.751']

          node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]
          sample [label = 'OV = -4.373']

          edge [minlen = 2]
            box->sample [label = '  n = 30', fontsize = 12, labeldistance = 5]
          }"
      return(grViz(string))
    })
    
    ############################ Uploading Data Mechanism ############################# 
    
    sample_data_session <- load_data_paired_sample_Server(id = "loading_data")
    
    # Store the sample data.
    sample_data <- reactiveVal(NULL)
    
    # Variable that is true when the data has been specified, meaning the rest of the exercise can commence
    # This variable can be accessed by the ui conditional panel.
    output$render_rest_of_exercise <- reactive({
      !is.null(sample_data_session$data_paired_difference())
      sample_data(sample_data_session$data_paired_difference())
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
    alt_hypothesis_session <- alternate_hypotheses_1_sample_t_test_Server(id = "alternate_hypothesis", null_mean_string, "d")
    
    assumptions_1_sample_t_test_Server(id = "assumptions", sample_data)
    
    test_stat_session <- test_statistic_1_sample_t_test_Server(id = "test_stat", sample_data, null_mean_string, "d")
    
    p_value_session <- p_value_1_sample_t_test_Server(id = "p_val", test_stat_session$test_stat, sample_data, alt_hypothesis_session$alternate_hypothesis_choice)
    
    conclusion_1_sample_t_test_Server(id = "conclusion", p_value_session$p_val)
    
    confidence_interval_1_sample_t_test_Server(id = "confidence_interval", sample_data, null_mean_string, alt_hypothesis_session$alternate_hypothesis_choice)
    
  })
    
}