regressionTTestUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Title Strip
    fluidRow(
      column(8,
             HTML("<h1>Regression t-Test</h1>"),
      ),
      column(4,
             tags$style(HTML(paste0("
              [id='", ns("learning_text"), "'] {
                font-size: 20px;
                padding: 10px 20px;
              }
              "))),
             actionButton(ns("learning_text"), "What is a Regression t-Test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
      ),
    ),
    HTML("<br>"),
    
    load_data_regression_2_variable_UI(ns("loading_data")),
    
    HTML("<br><br><br>"),
    
    # Conditional panel ensures that the rest of the exercise is displayed only if the data has been uploaded.
    conditionalPanel(
      condition = sprintf('output["%s"]', ns("render_rest_of_exercise")),
      
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
                   inputId = ns("alternate_hypothesis_choice"),
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
                 uiOutput(ns('alternate_hypothesis_output'))
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
                              plotOutput(ns("assumption_linearity_scatter_plot"))
                       ),
                       column(6,
                              plotOutput(ns("assumption_linearity_residual_plot"))
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
                     plotOutput(ns("assumption_homoscedasticity_residual_plot"), width = "50%"),
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
                     plotOutput(ns("qqPlot"), width = "50%"),
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
                              plotOutput(ns("ordered_residuals_plot"))
                       ),
                       column(6,
                              plotOutput(ns("assumption_independence_residual_plot"))
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
                          uiOutput(ns("ev_and_se_text"))
                   ),
                   column(6,
                          HTML("<p><b>Step 2) Test Statistic Calculation</b></p>"),
                          uiOutput(ns("test_statistic_calculation"))
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
                 uiOutput(ns("p_value_prelude")),
                 header_colour = "#3179ae"
               )
        ),
        column(6,
               tight_card(
                 NULL,
                 HTML("<center>"),
                 plotOutput(ns("test_stat_t_plot"), width = "100%", heigh = "325px"),
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
                                     ns("alpha_value"),
                                     NULL,
                                     value = 0.05,
                                     min = 0,
                                     max = 1,
                                     width = "100%"
                                   ),
                            ),
                          ),
                          uiOutput(ns("significance_level_warning")),
                   ),
                   
                   # Section to provide final result.
                   column(6,
                          HTML("<p><b>Step 2) Final Conclusion</b></p>"),
                          uiOutput(ns("final_conclusion_output"))
                   )
                 ),
                 header_colour = "#3179ae"
               )
        ),
      ),
      
      HTML("<br><br><br>")
      
    )

  )
}