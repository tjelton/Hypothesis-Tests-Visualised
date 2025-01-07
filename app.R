source("libraries.R")
source("Box-Model/box_model_main_srv.R")
source("Box-Model/box_model_main_ui.R")
source("Proportion-Test/proportion_test_srv.R")
source("Proportion-Test/proportion_test_ui.R")
source("Utility/load_data_1_sample_srv.R")
source("Utility/load_data_1_sample_ui.R")

source("Utility/generic_plotting_functions.R")

options(shiny.autoreload = TRUE)
options(shiny.reactlog = TRUE)


ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Hypothesis Tests Visualised"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      
      menuItem("Fundamentals" , tabname = "Fundamentals", startExpanded = FALSE,
               menuSubItem("The Box Model", tabName = "box_playground")),
      
      menuItem("z-test" , tabname = "z_test", startExpanded = FALSE,
                menuSubItem("1-Sample z-test", tabName = "1_sample_z_test"),
                menuSubItem("Proportion (z-test)", tabName = "proportion_z_test")
               )
    )
  ),
  
  dashboardBody(
    
    # Make the browser zoomed in.
    tags$style(HTML("
      body {
        zoom: 1.25;
      }
    ")),
    
    # Remove top margin of the dashboard body.
    tags$style(HTML("
      .content-wrapper {
        margin-top: -10px !important;
      }
    ")),
    
    tags$head(tags$style(HTML("
      .content {
        padding-left: 25px;
        padding-right: 25px;
      }
    "))),
    
    # Make the modal wider.
    tags$style(HTML("
        .modal-dialog {
          max-width: 1100px !important;
          width: 100% !important;
        }
      ")),
    
    # Make value box title smaller
    tags$style(HTML("
        .small-box h3 {
          font-size: 25px
        }
      ")),

    tabItems(
      
      # Home page.
      tabItem(tabName = "home",
              
              HTML("<br>"),
              
              fluidRow(
                column(6,
                       infoBox("Welcome!", 
                               "To get started, click on \"≡\" to navigate to your page of interest!",
                               icon = icon("arrow-left"), 
                               color = "light-blue", 
                               fill = TRUE,
                               width = "100%"),
                ),
                column(6,
                       tags$a(
                         href = "https://tjelton.com/2024/11/14/the-hypothesis-tests-visualised-project/", 
                         target = "_blank",
                         valueBox("About", "Click to find out more about the project",
                                  icon = icon("question"), color = "red", width = "100%")
                       ),
                       tags$a(
                         href = "https://docs.google.com/forms/d/e/1FAIpQLSdmyGBjQWRDg-ksPGqSsMuLNsuvnwwVgD0GW1bKvKVu6Rnplg/viewform?usp=sf_link", 
                         target = "_blank",
                         valueBox("Feedback", "Click to report errors or provide suggestions/feedback!",
                                  icon = icon("comment"), color = "olive", width = "100%")
                       )
                )
              ),
      ),
      
      # Box plot playground page.
      tabItem(tabName = "box_playground",
              boxModelMainUI("box_model"),
      ),
      
      tabItem(
        tabName = "1_sample_z_test",
        
        # Title Strip
        fluidRow(
          column(8,
                 HTML("<h1>1-Sample z-test</h1><br>"),
          ),
          column(4,
                 HTML("<br>"),
                 tags$style(HTML(paste0("
                  [id='learning_text'] {
                    font-size: 20px;
                    padding: 10px 20px;
                  }
                  "))),
                 actionButton("learning_text", "What is a 1-sample z-test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
          ),
        ),
        HTML("<br>"),
        
        ############ SECTION: Input Data ############
        load_1_sample_data_UI("loading_data"),
 
        HTML("<br><br><br>"),
        
        
        conditionalPanel(
          condition = 'output.render_rest_of_exercise',
          
              ############ SECTION: The NULL Hypothesis - Setting up the Box ############
              fluidRow(
                column(7,
                       box(
                         title = HTML("<u><b>The 'NULL' Hypothesis - Setting up the Box</b></u>"),
                         status = "primary",
                         width = "100%",
                         solidHeader = FALSE,
                         
                         p("We start by using the box model to represent our null hypothesis."),
                         
                         # Step 1: Specify Population Standard Deviation
                         box(
                           title = "Step 1) Specify Population Standard Deviation (σ)",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = FALSE,
                           status = "info",
                           solidHeader = FALSE,
                           withMathJax(HTML("<p>
                                An assumption for the 1-sample z-test is that the standard deviation of the population (denoted \\(\\sigma\\)) in which our sample is drawn is
                                known. For example, a prior research paper may have estimated the population standard deviation, and you could use the value they discovered.<br><br>
                                
                                For this exercise, you can manually set the population standard deviation. However, for the most part, it is likely that the population standard
                                deviation is unknown. Hence, for the purposes of this exercise, you can set the population standard deviation to be equal to the sample standard
                                deviation (this has been automatically done below, but you are free to change the value). In practice, this is far from ideal, and in a future 
                                exercise, we will use the 1-sample t-test when the population standard deviation is unknown.</p>")),
                           HTML("<br>"),
                           fluidRow(
                             column(7,
                                    actionButton(
                                      inputId = "set_pop_sd_to_sample",
                                      label = "Set Population SD to be Sample SD",
                                      class = "btn-primary",
                                      style = "color: #fff;",
                                      width = "100%"
                                    )
                             ),
                             column(2,
                                    withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\(\\sigma = \\)</p>"))
                             ),
                             column(3,
                                    uiOutput("pop_sd_numeric_input") 
                             ),
                           ),
                         ),
                         
                         # Step 2: Specify NULL Hypothesis
                         box(
                           title = "Step 2) Specify NULL Hypothesis",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>Next we need to specify the 'null' hypothesis. In this test we are focusing on the mean, so the null hypothesis is that the population mean
                                  is equal to some value which we set below.</p>"),
                           
                           withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>Given \\(\\mu \\), where \\(\\mu \\) is the mean of some variable:</p>")),
                           fluidRow(
                             column(7),
                             column(2,
                                    withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( H_0: \\) \\(\\mu = \\)</p>"))
                             ),
                             column(3,
                                    numericInput(
                                      "null_mu",
                                      NULL,
                                      value = 0
                                    ),
                             ),
                           ),
                         ),
                         
                         
                         # Other
                         box(
                           title = "Other",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>
                           <ul>
                              <li>The value for <b>n</b> comes from the number of values in the sample chosen above.</li>
                              <li>The <b>observed value (OV)</b> comes from the mean of the sample chosen above.</li>
                           </ul></p>")
                         )
                       )
                ),
                column(5,
                       # Box model output.
                       box(
                         solidHeader = TRUE,
                         width = "100%",
                         HTML("<center>"),
                         grVizOutput("box_model", width = "70%", height = "70%"),
                         HTML("</center>")
                       ),
                )
              ),
          
              HTML("<br><br><br>"),
          
              ############ SECTION: The Alternate Hypothesis ############
              fluidRow(
                column(7,
                       box(
                         title = HTML("<u><b>The Alternate Hypothesis</b></u>"),
                         status = "primary",
                         width = "100%",
                         solidHeader = FALSE,
                         HTML("<p>Specify what type of alternate hypothesis you will be using below:</p>"),
                         HTML("<br>"),
                         radioButtons(
                           inputId = "alternate_hypothesis_choice",
                           label = NULL,
                           choices = list(
                             "Two Sided" = 1,
                             "One Sided (greater than)" = 2,
                             "One Sided (less than)" = 3
                           )
                         ),
                         
                       )
                ),
                column(5,
                       box(
                         solidHeader = TRUE,
                         width = "100%",
                         HTML("<p><b>Null Hypothesis</b></p>"),
                         uiOutput('null_hypothesis_output'),
                         HTML("<p><b>Alternate Hypothesis</b></p>"),
                         uiOutput('alternate_hypothesis_output'),
                       )
                )
              ),
              
              HTML("<br><br><br>"),
          
              ############ SECTION: Assumptions ############
              fluidRow(
                column(12,
                       
                       box(
                         title = HTML("<u><b>Assumptions</b></u>"),
                         status = "primary",
                         width = "100%",
                         solidHeader = FALSE,
                         HTML("<p>For the hypothesis test to be valid, we need to check the following assumptions:</p>"),
                         
                         # Assumption 1: Independent Samples
                         box(
                           title = "Assumption 1: Independent Sample",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>The first assumption is that our sample is <b>independent and randomly chosen</b>.</p>"),
                           HTML("<p><span style='color: blue;'><b>How do we check?</b></span> <i>We check by investigating the experimental setup.</i><br><br>
                                    For example, consider we were investigating data for a proportion test involving human participants. We could read the accompanying scientific
                                    publication to understand the methodology they used to gather the people in the sample.</p>")
                         ),
                         
                         # Assumption 2: Independent Samples
                         box(
                           title = "Assumption 2: Normality",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>The second assumption is that the sample means follow a normal distribution."),
                           HTML("<p><span style='color: blue;'><b>How do we check?</b></span><br>
                                    <ul>
                                      <li>Recall that the central limit theorem tells us that if we take a sufficiently large number of draws from the box, then the sample 
                                      means will approximately follow a normal distribution. If confused, please see the box model exercise.</li>
                                      <li>One way to gauge whether the central limit theorem holds or not is to see how large our sample is (this is indicated by the \"n\" in
                                      the box model above).</li>
                                      <li>Many textbooks will say that you can say that you can use the rule of thumb that the central limit theorem will apply if we have 30 
                                      or more draws. BEWARE - this is not always true! If the distribution of the values is very skewed, you will need much more than 30 draws!</li>
                                      <li>You should use a combination of the size of \"n\" and a histogram and boxplot of the sample distributions to help you determine whether
                                      the central limit theorem means we can resonably approximate the sample means using a normal distribution.</li>
                                    </ul>
                               </p>")
                         ),
                         
                         # Assumption 3: Normal Approximation
                         box(
                           title = "Assumption 3: Known Population Standard Deviation",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>As mentioned previously in \"The NULL Hypothesis - Setting up the Box\" section, it is assumed that the population standard deviation is known.</p>")
                         )
                      )
                )
              ),
              
              HTML("<br><br><br>"),
          
              ############ SECTION: Test Statistics ############
              fluidRow(
                column(12,
                       box(
                         title = HTML("<u><b>Test Statistic</b></u>"),
                         status = "primary",
                         width = "100%",
                         solidHeader = FALSE,
                         
                         fluidRow(
                           column(6,
                                  HTML("<p><b>Step 1) Calculate Expected Value (SE) and Standard Error (SE)</b></p>"),
                                  uiOutput("ev_and_se_text")
                           ),
                           column(6,
                                  HTML("<p><b>Step 2) Test Statistic Calculation</b></p>"),
                                  uiOutput("test_statistic_calculation")
                           )
                         ),
                       ),
                ),
              ),
              
              HTML("<br><br><br>"),
          
              ############ SECTION: p-value ############
              fluidRow(
                column(6,
                       box(
                         title = HTML("<u><b>p-value</b></u>"),
                         status = "primary",
                         width = "100%",
                         solidHeader = FALSE,
                         uiOutput("p_value_prelude")
                       )
                ),
                column(6,
                       plotOutput("test_stat_normal_plot", width = "80%", heigh = "275px"),
                )
              ),
              
              HTML("<br><br><br><br><br><br><br>"),

        )
        
      ),
      
      # Box plot playground page.
      tabItem(tabName = "proportion_z_test",
              proportionTestMainUI("proportion_test"),
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
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
  
  # Numeric text field to enter the population sd.
  output$pop_sd_numeric_input <- renderUI({
    sample_sd = sd(sample_data() ,na.rm = TRUE)
    numericInput(
      "population_standard_deviation_numeric",
      NULL,
      value = sample_sd,
      min = 0
    )
  })
  
  # Button logic to set the pop sd to the sample sd.
  observeEvent(input$set_pop_sd_to_sample, {
    req(input$population_standard_deviation_numeric)
    sample_sd = sd(sample_data() ,na.rm = TRUE)
    updateNumericInput(
      session,
      inputId = "population_standard_deviation_numeric",
      value = sample_sd
    )
  })
  
  # Override the sd value to be equal to the sample sd if it is less than or equal to 0.
  observeEvent(input$population_standard_deviation_numeric, {
    if (is.na(input$population_standard_deviation_numeric) || input$population_standard_deviation_numeric <= 0) {
      sample_sd = sd(sample_data() ,na.rm = TRUE)
      updateNumericInput(
        session,
        inputId = "population_standard_deviation_numeric",
        value = sample_sd
      )
    }
  })
  
  # Box model plot
  output$box_model <- renderGrViz({
    
    # String with mu and sigma.
    pop_details = paste("&mu; = ", as.character(round(input$null_mu, digits = 3)), "; &sigma; = ", as.character(round(input$population_standard_deviation_numeric, digits = 3)))
    
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
    sd_ = input$population_standard_deviation_numeric
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
                 as.character(sample_size), "}}\\\\ &= ", SE_string(), "\\end{align*}$$", sep = ""))
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
  
  # Histogram with normal curve to shown normal curve approximation.
  output$test_stat_normal_plot = renderPlot({
    return(curve_shaded_test_stat(dnorm, list(mean = 0, sd = 1), as.numeric(test_stat()), input$alternate_hypothesis_choice))
  })
  
  ############################ Modal Intro ############################# 
  
  # Text
  observeEvent(input$learning_text, {
    showModal(modalDialog(
      title = "The '1-Sample z-test'",
      
      withMathJax(HTML("<p>
            Suppose that in a made-up country, there is a statistics exam that all high school students take. If a student received a mark in the exam of 140 or over, 
            it means that they are demonstrating a satisfactory level of statistical knowledge as deemed by the education board. Based on data from multiple years, it’s
            known that the population standard deviation in test scores is 7.5.<br><br>
            
            Mr. Han is a statistics teacher who has been testing some new innovative teaching methods. In particular, he wants to test whether the average exam grade for 
            the 25 students in his class is statistically greater than a mark of 140.<br><br>
            
            To do this, Mr. Han sets up a hypothesis test. He lets the symbol \\(\\mu \\) represent the average of his class. He sets up the null hypothesis that his class’s 
            average grade is 140. Mathematically, this is written as:<br>
            
            <center><p style='font-size: 16px'>\\( H_{0} : \\mu = 140 \\)</p></center><br>
            
            When setting up the alternate hypothesis, because Mr. Han is interested in seeing if his innovative teaching approach leads to marks greater than 140, we set up a 
            one-sided alternate hypothesis, written mathematically as:
            
            <center><p style='font-size: 16px'>\\( H_{1} : \\mu > 140 \\)</p></center><br>
            
            Mr. Han’s main idea is that we will use our sample of 25 students to determine whether there is evidence to support or reject the null hypothesis.
        </p>")),
      fluidRow(
        column(8,
               withMathJax(HTML("<p>
                 <h5><u>How does Mr. Han go about doing this?</u></h5><br>
              
                  We can use the box model to model the hypothesis test. However, there is a caveat: We do not know all the specific tickets to place in the box. Instead, we
                  will specify the mean and standard deviation to summarise the tickets in the box. From the null hypothesis, we are claiming that the mean of the box is 140.
                  We also know that the population standard deviation (denoted by \\(\\mu \\)) is equal to 7.5. Hence, we place these values into the box.<br><br>
                  
                  Next, we want to turn our attention to the test scores that Mr. Han’s students achieved in the class. Let’s say that the 25 students had an average score of 
                  155. As there were 25 students, we specify: \\(n = 25 \\)). The average score of 155 is referred to as the observed value (OV). With these details drawn, we have 
                  completed the box model representation.<br><br>
                 </p>"))
        ),
        column(4,
               HTML("<br>"),
               grVizOutput("intro_example_box_model", width = "80%", height = "70%"),
        )
      ),
      HTML("<p><br>
              With the box made, Mr. Han can now calculate the test statistic. The calculation is as follows:<br>
              </p>"),
      withMathJax(
        HTML(paste("$$\\begin{align*} \\text{Test-Statistic (TS)} &= \\frac{\\text{OV} - \\text{EV}}{\\text{SE}}
                            \\\\ &= \\frac{\\text{OV} - \\text{EV}}{\\frac{\\sigma}{\\sqrt{n}}}
                            \\\\ &= \\frac{155 - 140}{\\frac{7.5}{\\sqrt{25}}}
                            \\\\ &= 0.738 \\text{ (3 decimal places)} \\end{align*}$$", sep = ""))
      ),
      HTML("<p>
          With the test statistic found, Mr. Han is ready to find the p-value. Play around with the remainder of this exercise to help Mr. Han find the p-value, and test whether
          null hypothesis is supported or rejected.
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
        box [label = '&mu; = 140; &sigma; = 7.5']
      
        node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]
        sample [label = 'OV = 155']
      
        edge [minlen = 2]
          box->sample [label = '  n = 25', fontsize = 12, labeldistance = 5]
        }"
    return(grViz(string))
  })
  
  ################################################################
  
  
  boxModelMainServer(id = "box_model")
  
  proportionTestMainServer(id = "proportion_test")
  
  
}

# Run the application
shinyApp(ui, server, enableBookmarking = "url")