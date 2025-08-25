twoSampleTTestUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Title Strip
    fluidRow(
      column(8,
             HTML("<h1>2-Sample t-Test</h1>"),
      ),
      column(4,
             tags$style(HTML(paste0("
              [id='", ns("learning_text"), "'] {
                font-size: 20px;
                padding: 10px 20px;
              }
              "))),
             actionButton(ns("learning_text"), "What is a 2-sample t-test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
      ),
    ),
    HTML("<br>"),
    
    ############ SECTION: Input Data ############
    load_data_2_sample_UI(ns("loading_data")),
    
    HTML("<br><br>"),
    
    # Conditional panel ensures that the rest of the exercise is displayed only if the data has been uploaded.
    conditionalPanel(
      condition = sprintf('output["%s"]', ns("render_rest_of_exercise")),
      
      ############ SECTION: The NULL Hypothesis - Setting up the Box ############
      fluidRow(
        column(6,
               tight_card(
                 "The 'NULL' Hypothesis - Setting up the Box",
                 
                 HTML("<p>Similar to the 1-sample tests, we will be using the box model to represent the null hypothesis. The key difference here, however, is that we don't have
                       input any values. The null hypothesis is set by default! Take some time to read the points below to ensure you understand why the boxes are arranged as
                       they are.</p>"),
                 
                 accordion(
                   
                   open = FALSE,
                   
                   accordion_panel(
                     HTML("<b>Why do we have two boxes?</b>"),
                     withMathJax(HTML("<p>
                                  The first thing you may wonder is why we have two box models for a 2-sample t-test. Unlike the 1-sample t-test, which only had ONE sample, 
                                  the 2-sample t-test has TWO samples, and so we need to have two box models to represent each sample.<br><br>
                                  
                                  More concretely, these 2 samples are different, and so they are likely to come from slightly different distributions. Hence, we need to have
                                  2 different box models (note that the standard deviation \\(s\\) for each is different). The samples are not coming from the same distribution.
                                </p>"))
                   ),
                   
                   accordion_panel(
                     HTML("<b>What is going on with the means of the boxes?</b>"),
                     withMathJax(HTML("<p>
                                  For a 2-sample t-test, under the null hypothesis, we claim that the mean of sample 1 (\\(\\mu_1\\)) is equal to the mean of sample 2 
                                  (\\(\\mu_2\\)). More formally, we write this as \\(H_0: \\mu_1 = \\mu_2\\).<br><br>
                                  
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
                                     the population standard devation as the sample standard deviation.</li>
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
                 grVizOutput(ns("box_model_sample_1"), width = "100%", height = "70%"),
                 HTML("</center>")
               ),
        ),
        column(3,
               tight_card(
                 NULL,
                 HTML("<center>"),
                 HTML("<h5><b>Sample 2</b></h5>"),
                 grVizOutput(ns("box_model_sample_2"), width = "100%", height = "70%"),
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
                 withMathJax(HTML("<p style='font-size: 16px;'>\\( H_0: \\) \\( \\mu_1 = \\mu_2 \\)</p>")),
                 HTML("</center>"),
                 HTML("<p><b>Alternate Hypothesis</b></p>"),
                 uiOutput(ns('alternate_hypothesis_output')),
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
                                      one of the groups, such as healthy people for the treatment group. It could appear the treatment is reducing these people's heart rate,
                                      but it is actually that they are healthy so already have a comparably low heart rate.</p>")
                   ),
                   
                   # Assumption 2: Normality
                   accordion_panel(
                     HTML("<b>Assumption 2: Normality</b>"),
                     HTML("<p>The second assumption is that each <b>sample's means are normally distributed</b>."),
                     HTML("<p><span style='color: blue;'><b>How do we check?</b></span><br></p>"),
                     HTML("<p>Firstly, it is important to realise that here, we want the distribution of sample means to be normally distributed. Hence, we do not
                                necessarily need the underlying data to be normally distributed, as we can rely on the Central Limit Theorem (CLT) to apply.<br><br>
                                
                                That being said, knowing whether the CLT applies or not is not straightforward. A common convention used online is that if there are 
                                more than 30 points in the sample, then the CLT will 'kick in'. But this is not necessarily true! If the underlying distribution is very skewed
                                and/or asymmetric, then you will likely need many more than 30 points. Hence, checking whether the underlying data is nearly normally distributed
                                will give us some insight. This is because if data is nearly normally distributed, you need fewer points for the CLT to 'kick in'.<br><br> 
                                
                                There are a few different ways that you can check whether the underlying data is normally distributed. For each of the following, you must
                                look at both samples separately:
                                <ul>
                                  <li>Boxplots: look to see if the plot is symmetric with few outliers.</li>
                                  <li>Histograms: look to see if the frequency (or density) is normally distributed.</li>
                                  <li>QQ-plots: see if the points follow the diagonal QQ-line (see below).</li>
                                  <li>Number of points: You can also make reference to the sample size when making your decision. For example, if you see some slight deviations
                                  in normality, but have a large sample size, you have more confidence that the CLT will apply.</li>
                                </ul></p>"),
                     fluidRow(
                       column(6,
                              plotOutput(ns("qqplot_sample_1"))
                       ),
                       column(6,
                              plotOutput(ns("qqplot_sample_2"))
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
                                 As mentioned in <b>assumption 3</b>, an assumption of a standard 2-sample t-test with equal variance is that the population spreads of sample 1 and sample 2 
                                 are equal. However, if this assumption is not satisfied, it is not bad news! We can instead use a different type of 2-sample t-test which relaxes this 
                                 assumption. In particular, the two types of test we can use include:
                                 <ul>
                                    <li><span style='color: blue;'><b>2-Sample T-Test With Equal Variance</b></span>: When the 2 samples have the same (approximately the same) spread.</li>
                                    <li><span style='color: blue;'><b>Welch 2-Sample T-Test</b></span>: When the 2 samples have different spreads.</li>
                                 </ul>
                                 Your goal is to decide whether we will use a 2-sample t-test with equal variance, or a Welch 2-sample t-test. To make this determination, use a 
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
                     plotOutput(ns("boxplot_assumption_checking"))
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
                     plotOutput(ns("histogram_assumption_checking"))
                   ),
                   accordion_panel(
                     HTML("<b>Sample Standard Deviation</b>"),
                     HTML("<p>
                                    Indicators that the spreads are similar:
                                    <ul>
                                      <li>Do both samples have a similar sample standard deviation?</li>
                                    </ul>
                                  </p>"),
                     uiOutput(ns("standard_deviation_assumption_checking"))
                   ),
                 ),
                 header_colour = "#3179ae"
               )
               
        ),
        column(3,
               primary_card(
                 "Equal Spread?",
                 HTML("<p><i>Toggle the switch below to determine whether the spread is the same or not.</i></p>"),
                 input_switch(ns("spread_toggle"), "Same Spread", value = TRUE), 
                 uiOutput(ns("same_spread_output_decision")),
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
                 "Conclusion (p-value)",
                 
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
      
      HTML("<br><br><br>"),
      
      ############ SECTION: Confidence Interval ############
      fluidRow(
        column(12,
               tight_card(
                 "Conclusion (Confidence Interval)",
                 HTML("<p>
                      A confidence interval in a two-sample t-test shows the range of plausible values for the difference between the population means at the chosen confidence
                      level. If 0 falls outside this range, we reject the null hypothesis.
                      <br><br>

                      We can also use the confidence interval to guide hypothesis testing: if 0 (representing no difference between the two population means) does not lie within
                      the confidence interval, then we reject the null hypothesis. If 0 does lie within the interval, we do not reject the null.
                      </p>"),
                 fluidRow(
                   column(6,
                          HTML("<p><b>Step 1) What is your confidence level</b>?</p>"),
                          
                          fluidRow(
                            column(1,
                                   withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( \\alpha = \\)</p>"))
                            ),
                            column(3,
                                   numericInput(
                                     ns("confidence_level"),
                                     NULL,
                                     value = 0.95,
                                     min = 0,
                                     max = 1,
                                     width = "100%"
                                   ),
                            ),
                          ),
                      uiOutput(ns("confidence_level_warning")),
                   ),
                   column(6,
                          HTML("<p><b>Step 2) Final Conclusion</b></p>"),
                          uiOutput(ns("confidence_level_output")),
                   )
                 ),
                 header_colour = "#3179ae"
               )
        ),
      ),
      
      HTML("<br><br><br><br><br><br>"),
      
    )
  
  )
}