tCurveMotivationUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    HTML("<h1>Introducing the t-Distribution (Motivation)</h1>"),
    
    ############ SECTION: When we don't know the population sd! ############
    fluidRow(
        column(12,
               tight_card(
                   "When we don't know the population sd!",
                   withMathJax(
                     HTML("
                        <p style='margin-top: 0.75rem; margin-bottom: 0.75rem; line-height: 1.5;'>
                          When determining the standard error (SE) in the 1-sample z-test section, 
                          we assumed that the <b>population standard deviation (denoted by \\(\\sigma\\)) is known</b>:
                        </p>
                      "),
                     HTML("$$\\begin{align*} \\text{SE} &= \\frac{\\color{red}{\\sigma}}{\\sqrt{n}} \\end{align*}$$"),
                     HTML("
                        <p style='margin-top: 0.75rem; margin-bottom: 0.75rem; line-height: 1.5;'>
                          However, in practice, the population standard deviation is usually unknown. 
                          Hence, we often use the <b>sample standard deviation (denoted by \\(s\\)) instead</b>:
                        </p>
                      "),
                     HTML("$$\\begin{align*} \\hat{\\text{SE}} &= \\frac{\\color{red}{s}}{\\sqrt{n}} \\end{align*}$$"),
                     HTML("
                        <p style='margin: 0; line-height: 1.5;'>
                          One observation is that when we substitute the population standard deviation for the sample standard deviation, 
                          we now write \\(\\text{SE}\\) instead of \\(\\hat{\\text{SE}}\\). 
                          The \\(\\hat{}\\) represents that this standard error is an estimate. 
                          We don’t have the true \\(\\text{SE}\\) anymore, as we are using the sample, 
                          which is a subset of the population.<br><br>
                  
                          As the sample size increases, the estimated standard error (\\(\\hat{\\text{SE}}\\)) 
                          will converge to the population standard error (\\(\\text{SE}\\)). 
                          Conceptually, this is because as the sample becomes larger, 
                          it is more representative of the population. 
                          Also, the above is only true if we are doing unbiased sampling!
                        </p>
                      ")
                   ),
                   header_colour = "#3179ae"
               )
        )
    ),
    

    HTML("<br><br>"),

    ############ SECTION: Introducing the t-curve. ############
    fluidRow(

      column(6,
             tight_card(
                "Introducing the t-curve",
                withMathJax(
                  HTML(
                    "<p>
                        Firstly, let’s remind ourselves of the test statistic (\\(\\text{TS}\\)) calculation for a 1-sample z-test:
                    </p>"
                  ),
                  HTML("$$\\begin{align*} \\text{Test Statistic (TS)} &= \\frac{\\text{OV} - \\text{EV}}{\\color{red}{\\text{SE}}} \\end{align*}$$"),
                  HTML(
                    "<p>
                        As a reminder, \\(\\text{OV}\\) stands for observed value and \\(\\text{EV}\\) stands for expected value. As mentioned in the above seection,
                        the \\(\\text{SE}\\) (indicated in red) requires that the population standard deviation is known. When this is not known, we can instead use
                        the estimate standard error (using the sample’s standard deviation):
                    </p>"
                  ),
                  HTML("$$\\begin{align*} \\text{Test Statistic (TS)} &= \\frac{\\text{OV} - \\text{EV}}{\\color{red}{\\hat{\\text{SE}}}} \\end{align*}$$"),
                  HTML("
                    <p>
                       In both cases above, the test statistic formulas look the same. The only difference is that the latter one has an estimate for \\(\\text{SE}\\)
                       (i.e. \\(\\hat{\\text{SE}}\\)). Hence, we  claim that the second test statistic has extra variability because of the uncertainty surrounding
                       \\(\\hat{\\text{SE}}\\).<br><br>

                       How do we account for this extra variability? We adjust the method in which the p-value is found. Hence, to account for this extra variability,
                       like the 1-sample z-test, which uses a normal curve to find the p-value, we use the t-curve. The t-curve (or t-distribution) is similar in
                       appearance to the normal curve, except it contains an extra parameter called degrees of freedom, which adjusts the ‘fatness’ of the curve’s tails.
                       This is evident when playing with the app to the right. When degrees of freedom increases, the tail fatness decreases. You can also see that
                       as the degrees of freedom value increases, the curve approaches the normal distribution.
                    </p>
                  ")
                ),
                header_colour = "#3179ae"
             ),
      ),
      column(6,
             primary_card(
                "Demonstration",
                HTML("<p>The slider below changes the degrees of freedom of the red t-curve in the graph below.</p>"),
                fluidRow(
                  column(8,
                         sliderInput(
                           ns("df_slider"),
                           NULL,
                           min = 1,
                           max = 25,
                           value = 1),
                  ),
                  column(4,
                         checkboxInput(ns("display_normal_curve"), "Display normal curve", TRUE),
                  )
                ),
                plotOutput(ns("changing_df_graph"), width = "100%", height = "300px"),
                header_colour = "#3179ae"
             )
      )
    ),

    HTML("<br><br><br>"),

    ############ SECTION: Introducing the t-curve. ############
    fluidRow(

      column(6,
             tight_card(
               "T-Distribution and P-Values",
               withMathJax(
                 HTML(
                   "<p>
                        Previously, it was mentioned that the parameter called “degrees of freedom” adjusts the ‘fatness’ of the curve’s tails. This can be easily
                        verified in the app above when setting the slider to 1 and then 10. At 1 degree of freedom, the red t-curve has its tails well and truly above
                        the dashed normal curve. However, when looking at 10 degrees of freedom, the red t-curve’s tails have shrunk to be closer to that of the dashed
                        normal curve.<br><br>

                        There is a very logical reason for this! Recall from the ‘1-sample z-test’ exercise that when it comes to finding the p-value, we plot our test
                        statistic and find the area under the curve covering the shaded region. If we have a p-value below our significance level (which is typically
                        \\(\\alpha = 0.05\\)), we reject the null hypothesis.<br><br>

                        As mentioned previously, if we do not have the population standard deviation, we need to account for the extra variability introduced by using
                        the sample standard deviation. Degrees of freedom allow us to do just that! The value of degrees of freedom is directly linked to sample size,
                        meaning larger degrees of freedom are associated with larger sample sizes. When degrees of freedom are equal to 1, this indicates that our sample
                        is tiny (likely contains only 2 points), meaning there is lots of variability/uncertainty and that is why the tails of the t-curve are so fat.
                        The result of this is that the area under the curve will be much larger, meaning that a more extreme test statistic is needed to reject the null
                        hypothesis.<br><br>

                        On the other hand, when degrees of freedom are equal to a larger value (such as 25), the t-curve tails appear much more closely aligned with the
                        normal curve’s tails. This is because we are now taking a much larger sample, and there is less variability to account for.<br><br>

                        The demo to the right allows you to compare the p-values from a normal and t-distribution. You can see that for low values for degrees of freedom,
                        the p-value is much larger than that of the normal distribution. However, as you increase degrees of freedom, the p-values become more similar.
                    </p>"
                 )
               ),
               header_colour = "#3179ae"
             )
      ),
      column(6,
             primary_card(
               title = "Demonstration",
               HTML("<p>Comparison of the p-values for a two-sided alternate hypothesis test using a normal and t-curve.</p>"),
               fluidRow(
                 column(4,
                        numericInput(
                          ns("test_statistic_input"),
                          "Enter Test Statistic:",
                          value = 1
                        )
                 ),
                 column(8,
                        sliderInput(
                          ns("df_slider_demo_2"),
                          "Change Degree of Freedom (T-Curve):",
                          min = 1,
                          max = 50,
                          value = 1),
                 )
               ),
               fluidRow(
                 column(6,
                    HTML("<h5><center><b>Normal Curve (z-tests)</b></center></h5>"),
                    plotOutput(ns("test_stat_normal_plot"), width = "100%", heigh = "250px"),
                    uiOutput(ns("p_value_normal_curve"))
                 ),
                 column(6,
                    HTML("<h5><center><b>T-Curve (t-tests)</b></center></h5>"),
                    plotOutput(ns("test_stat_t_plot"), width = "100%", heigh = "250px"),
                    uiOutput(ns("p_value_t_curve"))
                 )
               ),
               header_colour = "#3179ae"
             )
      )
    ),

    HTML("<br><br><br>"),

    ############ SECTION: Conclusion ############
    fluidRow(
      column(12,
             tight_card(
               "Conclusion",
               HTML("<p>
                  The purpose of this exercise was to develop a conceptual understanding of what the t-distribution is, and why we need it. We did this through discussing
                  the 1-sample z-test, and specifically, how things change when we don't know the population standard deviation. I have left some things vague (such as
                  how do we know which value to set degrees of freedom to), but we will go into greater detail in further exercises.
              </p>"),
               header_colour = "#3179ae"
             )
      )
    ),
    
    HTML("<br><br><br>"),
  )
}