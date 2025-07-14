source("libraries.R")
source("linking_source_files.R")

ui <- page_navbar(
  title = "Hypothesis Tests Visualised",
  theme = bs_theme(version = 5, bootswatch = "lumen"),  # You can switch to "lumen", "materia", "sketchy", "united", "yeti" etc.
  
  # Make the modal wider.
  tags$style(HTML("
        .modal-dialog {
          max-width: 1100px !important;
          width: 100% !important;
        }
      ")),
  
  nav_panel(
    title = "Home",
    fluidPage(
      h2("Home Page"),
      p("This is the home page. I will place some inspirational text here...")
    )
  ),
  
  nav_menu("Fundamentals",
           
           nav_panel("The Box Model", 
                     boxModelMainUI("box_model"),
           ),
  ),
  
  nav_menu("Z-Tests",
           
           nav_panel("1-Sample Z-Test",
                     oneSampleZTestUI("1_sample_z_test"),
           ),
           nav_panel("Proportion (Z-test)", 
                     proportionTestMainUI("proportion_z_test"),
           ),
  ),
  
  nav_menu("T-Tests",
           nav_panel("T-Curve Motivation", 
                     tCurveMotivationUI("t_curve_motivation")
           ),
           nav_panel("1-Sample T-Test",
                     oneSampleTTestUI("1_sample_t_test")
           ),
           nav_panel("Paired T-Test",
                     pairedTTestUI("paired_t_test") 
           ),
           nav_panel("2-Sample T-Test",
                     twoSampleTTestUI("2_sample_t_test")
           ),
           nav_panel("Regression T-Test",
                     
                     
                     # Title Strip
                     fluidRow(
                       column(8,
                              HTML("<h1>Regression t-Test</h1>"),
                       ),
                       column(4,
                              tags$style(HTML(paste0("
                            [id='", "learning_text", "'] {
                              font-size: 20px;
                              padding: 10px 20px;
                            }
                            "))),
                              actionButton("learning_text", "What is a Regression t-Test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
                       ),
                     ),
                     HTML("<br>"),
                     
                     load_data_regression_2_variable_UI("loading_data"),
                     
                     HTML("<br><br><br>"),
                     
                     # Conditional panel ensures that the rest of the exercise is displayed only if the data has been uploaded.
                     conditionalPanel(
                       condition = sprintf('output["%s"]', "render_rest_of_exercise"),
                       
                       ############ SECTION: The Null and Alternate Hypotheses ############
                       fluidRow(
                         column(7,
                                tight_card(
                                  "The Null and Alternate Hypotheses",
                                  withMathJax(HTML("<p>
                                 When looking at the other t-tests, we motivated them using the box model. While you could motivate the regression test
                                 using a box model, it is much trickier, and could lead to increased confusion. Instead, we will state the null hypothesis outright.
                                 <br><br>
                                 
                                 A linear regression line, has the form: \\( Y = \\beta_0 + \\beta_1X + \\epsilon \\)
                                 <br>
                                 
                                 Here:
                                 <ul>
                                    <li>\\(\\beta_0\\) is the \\(Y\\)-intercept.</li>
                                    <li>\\(\\beta_1\\) is the gradient (coefficient of \\(X\\)).</li>
                                    <li>\\(\\epsilon\\) is the residual error (error that the model doesn't explain).</li>
                                 </ul>
  
                                 The <b>null hypothesis is \\(\\beta_1 = 0\\)</b>. In other words, that there is <b>no linear relationship</b> between \\(X\\) and \\(Y\\).
                                 
                                 Similar to the other tests, you can choose which type of alternate hypothesis you want:
                                 </p>")),
                                  radioButtons(
                                    inputId = "alternate_hypothesis_choice",
                                    label = NULL,
                                    choices = list(
                                      "Two Sided" = 1,
                                      "One Sided (greater than)" = 2,
                                      "One Sided (less than)" = 3
                                    )
                                  ),
                                  header_colour = "#3179ae"
                                )
                         ),
                         column(5,
                                tight_card(
                                  NULL,
                                  HTML("<p><b>Null Hypothesis</b></p>"),
                                  HTML("<center>"),
                                  withMathJax(HTML("<p style='font-size: 16px;'>\\( H_0: \\) \\( \\beta_1 = 0 \\)</p>")),
                                  HTML("</center>"),
                                  HTML("<p><b>Alternate Hypothesis</b></p>"),
                                  uiOutput('alternate_hypothesis_output')
                                )
                         )
                       ),
                       
                       HTML("<br><br><br>"),
                       
                       ############ SECTION: Assumptions ############
                       fluidRow(
                         column(12,
                                tight_card(
                                  "Assumptions",
                                  HTML("<p>For the hypothesis test to be valid, we need to check the following assumptions:</p>"),
                                  
                                  accordion(
                                    open = FALSE,
                                    
                                    # Assumption 1: Linearity
                                    accordion_panel(
                                      HTML("<b>Assumption 1: Linearity</b>"),
                                      withMathJax(HTML("<p>The <b>relationship</b> between \\(X\\) and \\(Y\\) <b>is linear</b>.</p>")),
                                      withMathJax(HTML("<p><span style='color: blue;'><b>How do we check?</b></span> 
                                
                                      <ul>
                                        <li>Look at a scatterplot of \\(X\\) vs \\(Y\\). There should be a straight-line trend (check graph on the left below).</li>
                                        <li>Look at a residual plot for the linear equation where \\(X\\) is the independent variable and \\(Y\\) is the dependent variable.
                                            It should show homoscedasticity with no pattern (check graph on the right below).</li>
                                      </ul>
                                     </p>")),
                                      fluidRow(
                                        column(6,
                                               plotOutput("assumption_linearity_scatter_plot")
                                        ),
                                        column(6,
                                               plotOutput("assumption_linearity_residual_plot")
                                        ),
                                      )
                                    ),
                                    
                                    # Assumption 2: Homoscedasticity
                                    accordion_panel(
                                      HTML("<b>Assumption 2: Homoscedasticity</b>"),
                                      withMathJax(HTML("<p>The second assumption is that the residuals show <b>homoscedasticity</b>, that is, the residuals have <b>constance varaince</b> across
                                           \\(X\\).</p>")),
                                      HTML("<p><span style='color: blue;'><b>How do we check?</b></span><br></p>"),
                                      HTML("<p><ul>
                                        <li>Residual plot: The spread of the residuals should be equal (check graph below).</li>
                                      </ul></p>"),
                                      HTML("<center>"),
                                      plotOutput("assumption_homoscedasticity_residual_plot", width = "50%"),
                                      HTML("</center>")
                                    ),
                                    
                                    # Assumption 3: Normality
                                    accordion_panel(
                                      HTML("<b>Assumption 3: Normality</b>"),
                                      withMathJax(HTML("<p>The third assumption is that the residuals should be <b>approximately normally distributed</b>.<br><br>
                                          
                                          <span style='color: blue;'><b>How do we check?</b></span><br><br>
                                          
                                          First, a bit of context. The reason why we care about this assumption is because the regression t-test relies on the sample distribution of
                                          the slope \\(\\beta_1\\) being apprxoiately normal.<br><br>
                                          
                                          However, lucky for us, the CLT tell us that the sampling distibution will appoach normality regardless of the residual distribution,
                                          provided the sample size is large enough.<br><br>
                                          
                                          The reason we are interetsed in checking that the residuals are normally distributed is that we may be unsure as to whether the CLT applies.
                                          For example, we may be unsure whether we have enough points for the CLT to apply in our circusmtance. If we see that the residuals are 
                                          normally distributed, we can be more confident that the CLT applies.<br><br>
                                          
                                          There are two main ways to checkthis assumption:
                                          
                                          <ul>
                                            <li>QQ-plot: see if the residuals follow the diagonal QQ-line (see below).</li>
                                            <li>Number of points: You can make reference to the sample size when making your decision about this assumption. For example, if you
                                            see some slight deviations in normality (when looking at the qq-plot), but have a large sample size, you have more confidence that 
                                            the CLT will apply.</li>
                                          </ul>
                                      </p>")),
                                      HTML("<center>"),
                                      plotOutput("qqPlot", width = "50%"),
                                      HTML("</center>")
                                    ),
                                    
                                    # Assumption 4: Independence of Residuals
                                    accordion_panel(
                                      HTML("<b>Assumption 4: Independence of Residuals</b>"),
                                      HTML("<p>The second assumption is the <b>independence of residuals</b>. The assumption is that the residuals are unrealted to eachother.</p>"),
                                      HTML("<p><span style='color: blue;'><b>How do we check?</b></span><br></p>"),
                                      HTML("<p>There are some advanced methods used to detect this. Two methods that you could use:
                                      <ul>
                                        <li>Residual plot against observation order: This is a plot of residuals in the order the data was collected or indexed. If we see a random
                                        scatter of points, this suggests independence. On the other hand, patterns, trends or clusters could suggest dependence (check graph on the 
                                        left below).</li>
                                        <li>Standard residual plot: If the residuals show a systemtic shape or wave, it could suggest dependence, that is the residuals are not
                                        independent (check graph on the right below).</li>
                                      </ul></p>"),
                                      fluidRow(
                                        column(6,
                                               plotOutput("ordered_residuals_plot")
                                        ),
                                        column(6,
                                               plotOutput("assumption_independence_residual_plot")
                                        )
                                      )
                                    )
                                    
                                  ),
                                  header_colour = "#3179ae"
                                )
                         )
                       ),
                       
                       HTML("<br><br><br>"),
                       
                       ############ SECTION: Test Statistic ############
                       fluidRow(
                         column(12,
                                tight_card(
                                  "Test Statistic",
                                  fluidRow(
                                    column(6,
                                           withMathJax(HTML("<p><b>Step 1) Calculate Expected Value (\\(\\mathbb{E}\\)) and Standard Error (\\(\\text{SE}\\))</b></p>")),
                                           uiOutput("ev_and_se_text")
                                    ),
                                    column(6,
                                           HTML("<p><b>Step 2) Test Statistic Calculation</b></p>"),
                                           uiOutput("test_statistic_calculation")
                                    )
                                  ),
                                  header_colour = "#3179ae"
                                )
                         )
                         
                       ),
                       
                       HTML("<br><br><br>"),
                       
                       ############ SECTION: Test p-value ############
                       fluidRow(
                         column(6,
                                tight_card(
                                  "p-value",
                                  uiOutput("p_value_prelude"),
                                  header_colour = "#3179ae"
                                )
                         ),
                         column(6,
                                tight_card(
                                  NULL,
                                  HTML("<center>"),
                                  plotOutput("test_stat_t_plot", width = "100%", heigh = "325px"),
                                  HTML("</center>"),
                                )
                         )
                       ),
                       
                       HTML("<br><br><br>"),
                       
                       ############ SECTION: Conclusion ############
                       fluidRow(
                         column(12,
                                tight_card(
                                  "Conclusion",
                                  
                                  fluidRow(
                                    # Section to enter significance level.
                                    column(6,
                                           HTML("<p><b>Step 1) What is your significance level</b>?</p>"),
                                           
                                           # Space to enter significance value.
                                           fluidRow(
                                             column(1,
                                                    withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( \\alpha = \\)</p>"))
                                             ),
                                             column(3,
                                                    numericInput(
                                                      "alpha_value",
                                                      NULL,
                                                      value = 0.05,
                                                      min = 0,
                                                      max = 1,
                                                      width = "100%"
                                                    ),
                                             ),
                                           ),
                                           uiOutput("significance_level_warning"),
                                    ),
                                    
                                    # Section to provide final result.
                                    column(6,
                                           HTML("<p><b>Step 2) Final Conclusion</b></p>"),
                                           uiOutput("final_conclusion_output")
                                    )
                                  ),
                                  header_colour = "#3179ae"
                                )
                         ),
                       ),
                       
                       HTML("<br><br><br>")
                       
                       
                       
                       
                       
                       
                     )
                     
                     
                     
           )
  )
)


server <- function(input, output, session) {
  
  ############################ Uploading Data Mechanism ############################# 
  
  sample_data_session = load_data_regression_2_variable_Server(id = "loading_data")
  
  # Store the sample data.
  data_x_axis = reactiveVal(NULL)
  data_y_axis = reactiveVal(NULL)
  
  # Variable that is true when the data has been specified, meaning the rest of the exercise can commence
  # This variable can be accessed by the ui conditional panel.
  output$render_rest_of_exercise <- reactive({
    !is.null(sample_data_session$data_x_axis())
  })
  
  outputOptions(output, "render_rest_of_exercise", suspendWhenHidden = FALSE)
  
  observe({
    val_x <- sample_data_session$data_x_axis()
    val_y <- sample_data_session$data_y_axis()

    if (!is.null(val_x)) {
      data_x_axis(val_x)
    }
    if (!is.null(val_y)) {
      data_y_axis(val_y)
    }
  })
  
  ############################ Modal Intro ############################# 
  observeEvent(input$learning_text, {
    showModal(modalDialog(
      title = "The Regression t-Test",
      fluidRow(
        column(7,
              withMathJax(HTML("
                <p>Remember Mr. Han from before? At a math conference, some of his colleagues were discussing that there is a linear relationship between the number
                of hours a student spends studying and their test score. It certainly appears that there is a linear relationship between these variables when considering
                the scatter plot on the right, but Mr. Han wants to formally test this using a hypothesis test.
                <br><br>
    
                To do this, Mr. Han uses a regression t-test. A regression test is used to determine whether there is a significant relationship between two numerical 
                variables. In simple linear regression, we model this relationship using a straight line:
                <br><br>
       
                <center>
                \\(Y = \\beta_0 + \\beta_1X + \\epsilon \\)
                </center>
                <br>
                
                where:
                 <ul>
                    <li>\\(Y\\) is the dependent (response) variable.</li>
                    <li>\\(X\\) is the independent (explanatory) variable.</li>
                    <li>\\(\\beta_0\\) is the \\(Y\\)-intercept.</li>
                    <li>\\(\\beta_1\\) is the slope/gradient (coefficient of \\(X\\)).</li>
                    <li>\\(\\epsilon\\) is the residual error (error that the model doesn't explain).</li>
                 </ul>
               </p>"))
        ),
        column(5,
               plotOutput("intro_scatter_plot")
        
        )
      ),
      fluidRow(
          column(5,
               HTML("<br><br>"),
               plotOutput("intro_horizontal_line_plot")
          ),
          column(7,
              withMathJax(HTML("<p>
                  The regression test specifically tests whether the slope \\(\\beta_1\\) is significantly different from zero. This is because, if \\(\\beta_1\\) were equal to
                  0, then the regression line would be horizontal, indicating that there is no relationship between \\(X\\) and \\(Y\\) (see the example to the left).
                  <br><br>
                  
                  Hence, the null hypothesis is that the slope is equal to zero. This is formulated as:
                  <br><br>
                  
                  <center><p style='font-size: 16px'>\\( H_{0}: \\beta_1 = 0 \\)</p></center>
                  
                  In this example, Mr. Han is interested in whether there is any relationship between study hours and test scores (whether positive or negative), 
                  and so the alternate hypothesis is that the slope is not equal to 0. We write this as:
                  <br><br>
                  
                  <center><p style='font-size: 16px'>\\( H_{1} : \\beta_1  \\neq 0 \\)</p></center>

                  Unlike the other t-tests we have looked at, we will not motivate the regression test using the box model. This is because the box model is conceptually
                  confusing when considering the regression test. That being said, all of the steps from the previous hypothesis tests are the same! We set the hypotheses
                  (we already did this together above), check the test assumptions, find the test-statistic, find the p-value, and then compare this with our 
                  significance level.
              </p>"))
          )
      ),
      fluidRow(
        column(12,
               HTML("<br><p>
               <b><span style='color: blue;'>Now it's off to you! </span></b>Together, we worked through the null and alternate hypotheses. Now you need to go through and 
               check the assumptions for a regression t-test, find the test statistic, and finally calculate the p-value. To continue with the study data example, in 
               the \"Input Sample Data\" section, select \"Pre-uploaded Data\" and then select the \"study_data\" data set.
               </p>")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close"),
    ))
  })
  
  # Plot of data
  output$intro_scatter_plot = renderPlot({
    plot(study_data$Minutes_Studied, study_data$Test_Score, xlab = "Minutes Studied", ylab = "Test Score", main = "Study Data")
  })
  
  # Horizontal line plot
  output$intro_horizontal_line_plot = renderPlot({
    plot(1, type = "n",
         xlim = c(0, 600),
         ylim = c(0, 100),
         xlab = "Minutes Studied",
         ylab = "Test Score",
         main = "Hypothetical Horizontal Regression Line")
    
    # Add a horizontal line at y = 75
    abline(h = 50, col = "red", lwd = 2)
  })
  
  # Alternate hypothesis (rendered) output.
  output$alternate_hypothesis_output <- renderUI({
    hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\(\\beta_1")
    
    # Specify alternate hypothesis in reference to whether the user chooses to do a one-sided or two-sided test.
    if (input$alternate_hypothesis_choice == 1) {
      hypothesis = paste(hypothesis, "\\neq 0 \\)</p>")
    } else if (input$alternate_hypothesis_choice == 2) {
      hypothesis = paste(hypothesis, ">  0 \\)</p>")
    } else if (input$alternate_hypothesis_choice == 3) {
      hypothesis = paste(hypothesis, "<  0 \\)</p>")
    }
    
    return (
      tagList(
        HTML("<center>"),
        withMathJax(HTML(hypothesis)),
        HTML("</center>")
      )
    )
  })
  
  make_scatter_plot <- function() {
    plot(data_x_axis(), data_y_axis(), main = "Scatter Plot", xlab = "X", ylab = "Y")
  }
  output$assumption_linearity_scatter_plot <- renderPlot({make_scatter_plot()})
  
  make_residual_plot <- function() {
    model <- lm(data_y_axis() ~ data_x_axis())
    resids <- residuals(model)
    plot(data_x_axis(), resids,
         main = "Residual Plot",
         xlab = "X",
         ylab = "Residuals")
    abline(h = 0, lty = 2, col = "red")
  }
  output$assumption_linearity_residual_plot <- renderPlot({make_residual_plot()})
  output$assumption_homoscedasticity_residual_plot <- renderPlot({make_residual_plot()})
  output$assumption_independence_residual_plot <- renderPlot({make_residual_plot()})
  
  # Residuals plotted by the data order. E.g., residual one is for the first (x,y) point, residual two for the second (x,y) point, etc.
  output$ordered_residuals_plot <- renderPlot({
    res <- residuals(lm(data_y_axis() ~ data_x_axis()))
    plot(res, type = "b",
         xlab = "Observation Order",
         ylab = "Residuals",
         main = "Residuals vs Observation Order")
    abline(h = 0, col = "red", lty = 2)
  })
  
  # QQ plot of the residuals
  output$qqPlot <- renderPlot({
    model <- lm(data_y_axis() ~ data_x_axis())
    res <- residuals(model)
    qqnorm(res)
    qqline(res, col = "red", lwd = 2)
  })
  
  # Expected value and standard error output.
  output$ev_and_se_text <- renderUI({
  
    expected_value = withMathJax(
      HTML("<p>Expected value under \\( H_0 \\):</p>"),
      HTML("$$\\begin{align*}\\mathbb{E}[\\widehat{\\beta}_1] &= 0 \\end{align*}$$")
    )
    
    # Fit linear model
    model <- lm(data_y_axis() ~ data_x_axis())
    residuals <- residuals(model)
    RSS <- sum(residuals^2)
    n <- length(data_y_axis())
    df <- n - 2
    s <- sqrt(RSS / df)
    
    residual_standard_error = withMathJax(
      HTML("<p>Residual standard deviation:</p>"),
      HTML(paste0(
        "$$\\begin{align*}",
        "s &= \\sqrt{\\frac{1}{n - 2} \\bigl((y_1 - \\widehat{y}_1)^2 + \\cdots + (y_n - \\widehat{y}_n)^2\\bigr)} \\\\",
        "&= \\sqrt{\\frac{1}{", n, " - 2} \\times ", round(RSS, 3), "} \\\\",
        "&= \\sqrt{\\frac{", round(RSS, 3), "}{", df, "}} \\\\",
        "&= \\sqrt{", round(RSS / df, 3), "} \\\\",
        "&= ", round(s, 3),
        "\\end{align*}$$"
      ))
    )

    s <- summary(model)$sigma
    x_dev <- data_x_axis() - mean(data_x_axis())
    SS_x <- sum(x_dev^2)
    SE_b1 <- s / sqrt(SS_x)
    
    standard_error = withMathJax(
      HTML("<p>Standard Error:</p>"),
      HTML(paste0(
        "$$\\begin{align*}",
        "SE(\\widehat{\\beta}_1) &= \\frac{s}{\\sqrt{(x_1 - \\bar{x})^2  + \\cdots + (x_n - \\bar{x})^2}} \\\\",
        "&= \\frac{", round(s, 3), "}{\\sqrt{", round(SS_x, 3), "}} \\\\",
        "&= \\frac{", round(s, 3), "}{", round(sqrt(SS_x), 3), "} \\\\",
        "&= ", round(SE_b1, 3),
        "\\end{align*}$$"
      ))
    )

    return(
      tagList(
        expected_value, residual_standard_error, standard_error
      )
    )
  })
  
  test_stat = reactiveVal("")
  
  # Test statistic output.
  output$test_statistic_calculation <- renderUI({
    
    # Calculate the test statistic.
    x <- data_x_axis()
    y <- data_y_axis()
    model <- lm(y ~ x)
    summary_model <- summary(model)
    beta_hat <- coef(model)["x"]
    SE_beta_hat <- summary_model$coefficients["x", "Std. Error"]
    EV_beta_hat <- 0
    t_stat <- (beta_hat - EV_beta_hat) / SE_beta_hat
    test_stat(round(t_stat, 3))
    
    t_stat = withMathJax(
      HTML(paste0("$$\\begin{align*}",
                 "t &= \\frac{\\widehat{\\beta}_1 - \\mathbb{E}[\\widehat{\\beta}_1]}{SE(\\widehat{\\beta}_1)} \\\\",
                 "&= \\frac{", round(beta_hat, 3), " - ", EV_beta_hat, "}{", round(SE_beta_hat, 3), "} \\\\",
                 "&= ", round(t_stat, 3),
                 "\\end{align*}$$"))
    )
    
    additional_line = HTML(paste("<p style = 'text-align: left;'><span style='color: blue;'><i>The value for the test-statistic is ", round(test_stat(), 3), ". </i></span></p>", sep = ""))
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
    
    
    
    # General prelude text about what the p-value is.
    first_string = HTML(paste("<p>The p-value is the probability of observing a test-statistic <b>more extreme that our test statistic of ",
                              test_stat(), 
                              ".</b></p>", 
                              sep = ""))
    
    df(length(data_x_axis())-2)
    second_string = withMathJax(
      HTML("<p>For a regression t-test, we set the degree of freedom equal to:</p>"),
      HTML(paste("$$\\begin{align*} \\text{df} &= n - 2\\\\
                     &=", length(data_x_axis()), " - 2 \\\\
                     &=", df(),  "\\end{align*}$$", sep = ""))
    )

    
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
  
  # Test statistic on curve to show p-value calculation.
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
  
  
  
  
  
  boxModelMainServer(id = "box_model")
  oneSampleZTestServer(id = "1_sample_z_test")
  proportionTestMainServer(id = "proportion_z_test")
  tCurveMotivationServer(id = "t_curve_motivation")
  oneSampleTTestServer(id = "1_sample_t_test")
  pairedTTestServer(id = "paired_t_test")
  twoSampleTTestServer(id = "2_sample_t_test")
  
  
}

shinyApp(ui, server)