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
                     
               # Title Strip
               fluidRow(
                 column(8,
                        HTML("<h1>2-Sample t-Test</h1>"),
                 ),
                 column(4,
                        tags$style(HTML(paste0("
                          [id='", "learning_text", "'] {
                            font-size: 20px;
                            padding: 10px 20px;
                          }
                          "))),
                        actionButton("learning_text", "What is a 2-sample t-test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
                 ),
               ),
               HTML("<br>"),
                     
               ############ SECTION: Input Data ############
               load_data_2_sample_UI("loading_data"),
               
               HTML("<br><br>"),
               
               # Conditional panel ensures that the rest of the exercise is displayed only if the data has been uploaded.
               conditionalPanel(
                 condition = sprintf('output["%s"]', "render_rest_of_exercise"),
                 
                 ############ SECTION: The NULL Hypothesis - Setting up the Box ############
                 fluidRow(
                   column(6,
                          tight_card(
                            "The 'NULL' Hypothesis - Setting up the Box",
                            
                            HTML("<p>Simimlar to the 1 sample tests, we will be using the box model to represent the null hypothesis. The key difference here however,
                                 is that we don't have to input any values. The null hypothesis is set by default! Take somen time to read the points below to ensure
                                 you understand why the boxes are arranged as they are.</p>"),
                            
                            accordion(
                              
                              open = FALSE,
                              
                              accordion_panel(
                                HTML("<b>Why do we have two boxes?</b>"),
                                withMathJax(HTML("<p>
                                  The first thing you may wonder is why we have two box models for a 2-sample t-test. Unlike the 1-sample t-test which only had ONE sample, 
                                  the 2-sample t-test has TWO samples, and so we need to have two box models to represent each sample.<br><br>
                                  
                                  More concretely, these 2 samples are different, and so they are likely to come from slightly different distributions. Hence, we need to have
                                  2 different box models (note that the standard deviation \\(s\\) for each is different). The samples are not coming from the same distribution.
                                </p>"))
                              ),
                              
                              accordion_panel(
                                HTML("<b>What is going on with the means of the boxes?</b>"),
                                withMathJax(HTML("<p>
                                  For a 2-sample t-test, under the null hypothesis, we claim that the mean of sample 1 (\\(\\mu_1\\)) is equal to the mean of sample 2 
                                  (\\(\\mu_2\\)). More formally, we write this as - \\(H_0: \\mu_1 = \\mu_2\\).<br><br>
                                  
                                  Hence, the mean of sample 1 (\\(\\mu_1\\)) is equal to the mean of sample 2 (\\(\\mu_2\\)). 
                                  <br>Likewise, the mean of sample 2 (\\(\\mu_2\\)) is equal to the mean of sample 1 (\\(\\mu_1\\)).
                                </p>"))
                              ),
                              
                              accordion_panel(
                                HTML("<b>Extra Points:</b>"),
                                withMathJax(HTML("<p>In case you are confused about where all the values for each box model have come from, these come from the sample 1 and 
                                   sample 2 data that you specified above. In particular:
                                  <ul>
                                     <li>\\(s_1\\) represents the sample standard deviation for sample 1 (similar for sample 2). Recall that in a t-test, we estimate
                                     the sample standard devation as the population standard deviation.</li>
                                     <li>\\(n_1 \\) is the number of observation in sample 1 (simlar for sample 2).</li>
                                     <li>\\(\\text{OV}_1 \\) is the observed sample mean for sample 1 (similar for sample 2).</li>
                                  </ul>
                               </p>"))
                              )
                            ),
                            header_colour = "#3179ae"
                          )
                   ),
                   column(3,
                          tight_card(
                            NULL,
                            HTML("<center>"),
                            HTML("<h5><b>Sample 1</b></h5>"),
                            grVizOutput("box_model_sample_1", width = "100%", height = "70%"),
                            HTML("</center>")
                          ),
                   ),
                   column(3,
                          tight_card(
                            NULL,
                            HTML("<center>"),
                            HTML("<h5><b>Sample 2</b></h5>"),
                            grVizOutput("box_model_sample_2", width = "100%", height = "70%"),
                            HTML("</center>")
                          ),
                   )
                 ),
                 
                 HTML("<br><br><br>"),
                 
                 ############ SECTION: The Alternate Hypothesis ############
                 fluidRow(
                   column(7,
                          tight_card(
                            "The Alternate Hypothesis",
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
                            header_colour = "#3179ae"
                          )
                   ),
                   column(5,
                          tight_card(
                            NULL,
                            HTML("<p><b>Null Hypothesis</b></p>"),
                            HTML("<center>"),
                            withMathJax(HTML("<p style='font-size: 16px;'>\\( H_0: \\) \\( \\mu_1 = \\mu_2 \\)</p>")),
                            HTML("</center>"),
                            HTML("<p><b>Alternate Hypothesis</b></p>"),
                            uiOutput('alternate_hypothesis_output'),
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
                              
                              # Assumption 1: Independent Samples
                              accordion_panel(
                                HTML("<b>Assumption 1: Independent and Randomly Chosen Sample</b>"),
                                HTML("<p>The first assumption is that our 2 samples are <b>independent and randomly chosen</b>.</p>"),
                                HTML("<p><span style='color: blue;'><b>How do we check?</b></span> <i>We check by investigating the experimental setup.</i><br><br>
                                      For example, consider we were doing an experiment where we were interested in the effect of a treatment on resting heart rate,
                                      and had a control and treatment group. Independence would be violated if people in the sample were related (family members would share
                                      similar genetics, and similar resting heart rates). Randomness could be violated if we only choose people with certain characteristics for 
                                      one of the groups, such as healthy people for the treatment group. It could appear the treament is reducing these people's heart rate,
                                      but it is actually that they are healthy so already have a comparably low heart rate.</p>")
                              ),
                              
                              # Assumption 2: Normality
                              accordion_panel(
                                HTML("<b>Assumption 2: Normality</b>"),
                                HTML("<p>The second assumption is that each <b>sample's means are normally distributed</b>."),
                                HTML("<p><span style='color: blue;'><b>How do we check?</b></span><br></p>"),
                                HTML("<p>Firstly, it is important to realise that here, we want the distribution of sample means to be normally distributed. Hence, we do not
                                necessarily need the underlying data to be normally distributed, as we can rely on the Central Limit Theorem (CLT) to apply.<br><br>
                                
                                That being said, knowing whether the CLT applies or not is not straight forward. A common convention used online is that if there are 
                                more than 30 points in the sample, then the CLT will 'kick in'. But this is not necessarily true! If the underlying distribution is very skewed
                                and/or assymetric, then you will likely need many more than 30 points. Hence, checking whether the underlying data is nearly normally distributed
                                will give us some insight. This is because if data is nearly normally distributed, you need less points for the CLT to 'kick in'.<br><br> 
                                
                                There are a few different ways that you can check whether the underlying data is normally distributed. For each of the following, you must
                                look at both samples seperately:
                                <ul>
                                  <li>Boxplots: look to see if the plot is symmetric with few outliers.</li>
                                  <li>Histograms: look to see if the frequency (or density) is normally distributed.</li>
                                  <li>QQ-plots: see if the points follow the diagonal QQ-line (see below).</li>
                                  <li>Number of points: You can also make reference to the sample size when making your decision. For example, if you see some slight deviations
                                  in normality, but have a large sample size, you have more confidence that the CLT will apply.</li>
                                </ul></p>"),
                                fluidRow(
                                  column(6,
                                         plotOutput("qqplot_sample_1")
                                  ),
                                  column(6,
                                         plotOutput("qqplot_sample_2")
                                  )
                                ),
                              ),
                                
                              # Assumption 3: Equal Spread
                              accordion_panel(
                                HTML("<b>Assumption 3: Equal Spread</b>"),
                                HTML("<p>The third assumption is that sample 1 and sample 2's <b>population spreads are equal</b>.</p>"),
                                HTML("<p><span style='color: blue;'><b>How do we check?</b></span><br></p>"),
                                HTML("<p>There are 2 main ways we can check this visually:
                                <ul>
                                  <li>Histograms: look to see if the histograms for each sample have similar spreads.</li>
                                  <li>Boxplots: look to see if the boxplots have similar spread (done in the section below).</li>
                                </ul>
                                Unlike the other tests, for a 2-sample t-test, if this assumption is not satisfied, we can make an easy change to how the test statistic
                                is calculated to relax this assumption. If the assumption is not satisfied, we can move from a regular 2-sample t-test to a Welch 2-sample 
                                t-test. In the next section, you will determine whether we do a Welch test or not!
                                </p>")
                              )
                            ),
                            header_colour = "#3179ae"
                        )
                  )
                 ),
                 
                 HTML("<br><br><br>"),
                 
                 fluidRow(
                   
                   column(9,
                          tight_card(
                            "Checking Assumption 3 - Is the Spread Equal?",
                            HTML("<p>
                                 As mentioned in <b>assumption 3</b> an assumption of a standard 2-sample t-test with equal variance is that the population spreads of sample 1 and sample 2 
                                 are equal. However, if this assumption is not satisfied, it is not bad news! We can instead use a different type of 2-sample t-test was relaxes this 
                                 assumption. In particular, the two types of test we can use include:
                                 <ul>
                                    <li><span style='color: blue;'><b>2-Sample T-Test With Equal Variance</b></span>: When the 2 samples have the same (apprxoimately the same) spread.</li>
                                    <li><span style='color: blue;'><b>Welch 2-Sample T-Test</b></span>: When the 2 samples have different spread.</li>
                                 </ul>
                                 Below, you goal is to decide whether we will use a 2-sample t-test with equal variance, or a Welch 2-sample t-test. To make this determination, use a 
                                 combination of the outputs below to make your decision.
                               </p>"),
                              accordion(
                                open = FALSE,
                                accordion_panel(
                                  HTML("<b>Boxplots</b>"),
                                  HTML("<p>
                                    Indicators that the spreads are similar:
                                    <ul>
                                      <li>Is the IQR (the size of the box) similar?</li>
                                      <li>Are the whiskers of the box a similar size?</li>
                                    </ul>
                                  </p>"),
                                  plotOutput("boxplot_assumption_checking")
                                ),
                                accordion_panel(
                                  HTML("<b>Histograms</b>"),
                                  HTML("<p>
                                    Indicators that the spreads are similar:
                                    <ul>
                                      <li>Does the distribution of vlaues look similar?</li>
                                      <li>Does the spread (standard deviation) look similar?</li>
                                    </ul>
                                  </p>"),
                                  plotOutput("histogram_assumption_checking")
                                ),
                                accordion_panel(
                                  HTML("<b>Sample Standard Deviation</b>"),
                                  HTML("<p>
                                    Indicators that the spreads are similar:
                                    <ul>
                                      <li>Do both samples have a similar sample standard deviation?</li>
                                    </ul>
                                  </p>"),
                                  uiOutput("standard_deviation_assumption_checking")
                                ),
                              ),
                            header_colour = "#3179ae"
                          )
                    
                   ),
                   column(3,
                          primary_card(
                            "Equal Spread?",
                            HTML("<p><i>Toggle the switch below to wether the spread is the same or not.</i></p>"),
                            input_switch("spread_toggle", "Same Spread", value = TRUE), 
                            uiOutput("same_spread_output_decision"),
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
                                     HTML("<p><b>Step 1) Calculate Expected Value (SE) and Standard Error (SE)</b></p>"),
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
  
  ################################################################
  
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
      df(n1+n2-1)
      second_string = withMathJax(
        HTML("<p>For a 2-sample t-test with equal variance, we set the degree of freedom equal to:</p>"),
        HTML(paste("$$\\begin{align*} \\text{df} &= n_1 + n_2 - 1\\\\
                                                                 &=", n1, "+", n2, " - 1 \\\\
                                                                 &=", n1 + n2 - 1,  "\\end{align*}$$", sep = ""))
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
  
  
}

shinyApp(ui, server)