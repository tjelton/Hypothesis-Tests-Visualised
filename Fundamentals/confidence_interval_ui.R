confidenceIntervalUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    HTML("<h1>Confidence Intervals</h1>"),
    
    HTML("<p><b><i>Some of the hypothesis test pages will end by constructing a confidence interval. This page is aimed at developing an intuition for
                     what confidence intervals represent.</i></b></p>"),
    HTML("<br>"),
    
    fluidRow(
      column(12,
             tight_card(
               "What Are Confidence Intervals?",
               HTML("<p>
                      So first, what is a confidence interval? A confidence interval is a range of values calculated from sample data that is used to estimate an 
                      unknown population parameter. When we create a confidence interval, we specify a confidence level as a percentage.
                      <br><br>
                      
                      When thinking about this though the lens of the box model, the box represents our population. So, when applying the central limit theorem to the
                      box model, we can say that if we take a sufficiently large number of draws from a box, then the sample sums (or means) will follow a normal 
                      distribution.
                      <br><br>
                      
                      This might seem a bit vague, so it is best explained through an example. Let’s consider that we have sample data, and we want to estimate the 
                      population mean. This is a realistic scenario. In real life, the population mean is usually unknown, and we want to use the sample to gauge what
                      the population mean is likely to be. If we were to calculate a 95% confidence interval using our sample, we would end up with a range of values.
                      This interval is our estimate for where the true population mean might lie, based on the data we've collected.
                      <br><br>
                      
                      But what does the 95% mean in this context? It does not mean there is a 95% chance that the true mean falls within this particular interval.
                      Once the interval is calculated, the true mean either is or is not in the interval - it's not a matter of probability.
                      <br><br>
                      
                      Instead, the 95% refers to the method used to construct the interval. If we were to repeat this entire process of taking many random samples 
                      from the population and calculating a 95% confidence interval from each one, then about 95% of those intervals would contain the true
                      population mean. So, we are 95% confident in the method, not in any one specific interval.
                      <br><br>
                      
                      This is why it’s called a confidence interval: we have confidence in the procedure used to generate it. We use this idea to quantify 
                      uncertainty and to make more informed judgments based on limited sample data.
                      </p>"),
               header_colour = "#3179ae"
             )
      ),
    ),
    
    HTML("<br><br><br>"),
    
    fluidRow(
      column(5,
             tight_card(
               "Simulating Confidence Intervals",
               HTML("<p>
                     To illustrate confidence intervals, there is a simulation to the right. Do not worry about the method that has been used to generate the
                     confidence intervals, but know that they are 95% confidence intervals.
                     <br><br>
                     
                     Press <b>‘simulate’</b> to generate 100 confidence intervals.
                     <br><br>
                     
                     Here is a breakdown of what we see in the graph:
                     <ul>
                         <li>The dashed vertical line through 100 is the true population mean. This is usually unknown, but we are providing it to illustrate
                         confidence intervals.</li>
                         <li>Each green or red line is a different 95% confidence interval. Each confidence interval was produced from a different sample
                         (the sample is part of the population).</li>
                         <li>If the confidence interval contains the true population mean of 100, the line is green, and red otherwise.</li>
                     </ul>

                     What we should see is that around 95% of the confidence intervals are green, meaning they contain the true population mean. 
                     There will be some variability however, especially as we are only looking at 100 intervals here. Press simulate a few times to see how 
                     the intervals change as new data is encountered.
                     <br><br>
                     
                     There is also a running total to the right which shows you how many confidence intervals that you have generated so far did NOT contain 
                     the true population mean.
                     </p>"),
               header_colour = "#3179ae"
             )
      ),
      column(7,
             primary_card(
               "Demonstration",
               HTML("<center>"),
               actionButton(
                 inputId = ns("simulate_demo_1"), label = HTML('<p>Simulate</p>'),
                 class = "btn btn-primary", style="color: #fff;", width = "40%"
               ),
               HTML("</center>"),
               plotOutput(ns("confidence_interval_plot_demo_1")),
               HTML("<br>"),
               fluidRow(
                 column(9,
                        uiOutput(ns("demo_1_confidence_interval_history"))
                 ),
                 column(3,
                        HTML("<center><br>"),
                        actionButton(
                          inputId = ns("reset_history_demo_1"), label = HTML('<p>Reset History</p>'),
                          class = "btn btn-danger", style="color: #fff;", width = "100%"
                        ),
                        HTML("</center>")
                 )
               ),
               header_colour = "#3179ae"
             )
      ),
      
    ),
    
    HTML("<br><br><br>"),
    
    fluidRow(
      column(5,
             tight_card(
               "How Does The Size of a Confidence Interval Vary?",
               HTML("<p>
                     You might be wondering, what are the different features that can change the size of a confidence interval? There are lots! Toggle
                     the sections below to learn more.
                     </p>"),
               accordion(
                 open = FALSE,
                 
                 accordion_panel(
                   HTML("<b>Changing the Confidence Level</b>"),
                   HTML("<p>
                         In the example above, the confidence interval was 95%. We said that this means if we were to repeat the process of taking many
                         random samples from the population and calculating a 95% confidence interval, then about 95% of those intervals would contain
                         the true population mean.
                         <br><br>
  
                         Hence, if we were to change the confidence interval to something smaller, like 80%, then about 80% of those intervals would 
                         contain the true population mean. As fewer of the intervals contain the true population mean, the intervals are smaller than
                         those when the confidence level is 95%.
                         <br><br>
                         
                         If the confidence level were 99%, then about 99% of the intervals would contain the true population mean. To ensure that the 
                         intervals cover the true population mean, the intervals are larger. Hence, the 99% confidence intervals will be larger than 
                         the 95% ones.
                         <br><br>
                         
                         <b><span style='color: red;'>Key point:</span></b> In general, increasing the confidence level will increase the confidence 
                         interval size.
                         </p>"),
                 ),
                 accordion_panel(
                   HTML("<b>Changing the Sample Size</b>"),
                   HTML("<p>
                         The whole point of a confidence interval constructed from a sample mean is that we want to have an indication of where the 
                         population mean is. This is often desired in statistics because it is costly to collect data from everyone in a population.
                         <br><br>

                         As we collect more data from a population, we get closer and closer to capturing the entire population. For example, consider 
                         our population contained 1000 people, and we wanted to know whether they preferred Coke or Pepsi. If we interviewed 100 people,
                         we are still quite far from the entire population (we have only accounted for 10% of the data). However, if we interviewed 900
                         people, we are now quite close to the entire population (we have accounted for 90% of the data).
                         <br><br>
                         
                         In the case where we interviewed 90% of the population, there is less uncertainty about what the population mean is, and hence
                         we would expect the confidence interval to be smaller than in the case where we only interviewed 10% of the population.
                         <br><br>
                         
                         <b><span style='color: red;'>Key point:</span></b> In general, increasing the sample size will decrease the confidence interval
                         size.
                         </p>"),
                 ),
                 accordion_panel(
                   HTML("<b>Changing the Hypothesis Test Type</b>"),
                   HTML("<p>
                         We will not focus on this too much here, but different hypothesis test types will have different-sized confidence intervals.
                         For example, a confidence interval for a t-test will typically be larger than that of a z-test (because t-curves have fatter
                         tails).
                         </p>"),
                 )
               ),
               HTML("<br>"),
               HTML("<p>
                     Your task - For a given normal distribution (we are taking samples from the specified distribution):
                     <ul>
                      <li>How does the size of the confidence intervals change when <b>adjusting the confidence level</b>?</li>
                      <li>How does the size of the confidence intervals change when <b>adjusting the sample size</b>?</li>
                     </ul>
                     <br>
                     <i>Note: Here, each confidence interval is taken from sampling from the specified normal distribution. This is for learning purposes only.
                     In reality, you would not know the true population mean or sample standard deviation (otherwise, what would be the point of constructing an
                     interval in the first place).</i>U
                     </p>"),
               header_colour = "#3179ae"
             )
      ),
      column(7,
             primary_card(
               "Demonstration",
               fluidRow(
                 column(6,
                        HTML("<br>"),
                        sliderInput(
                          ns("confidence_level"),
                          "Confidence Level",
                          min = 1,
                          max = 99,
                          value = 95,
                          width = "100%"
                        ),
                        sliderInput(
                          ns("sample_size"),
                          "Sample Size",
                          min = 5,
                          max = 100,
                          value = 25,
                          width = "100%"
                        ),
                        HTML("<center>"),
                        actionButton(
                          inputId = ns("simulate_demo_2"), label = HTML('<p>Simulate</p>'),
                          class = "btn btn-primary", style="color: #fff;", width = "40%"
                        ),
                        HTML("</center>"),
                 ),
                 column(1),
                 column(5,
                        HTML("<p><center><b>Specify Population Distribution (Normal Distribution):</b></center></p>"),
                        sliderInput(
                          ns("dist_mean"),
                          "Mean",
                          min = -10,
                          max = 10,
                          value = 0,
                        ),
                        sliderInput(
                          ns("dist_sd"),
                          "Standard Deviation",
                          min = 1,
                          max = 10,
                          value = 1
                        ),
                 )
               ),
               plotOutput(ns("confidence_interval_plot_demo_2")),
               fluidRow(
                 column(9,
                        uiOutput(ns("demo_2_confidence_interval_history"))
                 ),
                 column(3,
                        HTML("<center><br>"),
                        actionButton(
                          inputId = ns("reset_history_demo_2"), label = HTML('<p>Reset History</p>'),
                          class = "btn btn-danger", style="color: #fff;", width = "100%"
                        ),
                        HTML("</center>")
                 )
               ),
               header_colour = "#3179ae"
             )
      ),
      
    ),
    
    HTML("<br><br><br>")
    
  )
}