source("libraries.R")
source("linking_source_files.R")

ui <- page_navbar(
  tags$head(
    tags$style(HTML("
      body {
        zoom: 110%;
      }
    "))
  ),
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

           nav_panel("Box Model - Part 1", 
                     boxModelPart1UI("box_model_part_1")
           ),
           
           nav_panel("Box Model - Part 2", 
                     boxModelPart2UI("box_model_part_2")
           ),
           
           nav_panel("Box Model - Part 3", 
                     boxModelPart3UI("box_model_part_3"),
           ),
           
           nav_panel("Confidence Intervals",
                     
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
                                    population mean. This is a realistic scenario. In real life, the population mean is usually unknown, and we want to use the sample to gague what
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

                                     What we should see is that around 95% of the confidence intervals contain are green, meaning they contain the true population mean. 
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
                                  inputId = "simulate_demo_1", label = HTML('<p>Simulate</p>'),
                                  class = "btn btn-primary", style="color: #fff;", width = "40%"
                                ),
                                HTML("</center>"),
                                plotOutput("confidence_interval_plot_demo_1"),
                                HTML("<br>"),
                                fluidRow(
                                  column(9,
                                         uiOutput("demo_1_confidence_interval_history")
                                  ),
                                  column(3,
                                         HTML("<center><br>"),
                                         actionButton(
                                           inputId = "reset_history_demo_1", label = HTML('<p>Reset History</p>'),
                                           class = "btn btn-danger", style="color: #fff;", width = "100%"
                                         ),
                                         HTML("</center>")
                                  )
                                ),
                                header_colour = "#3179ae"
                              )
                       ),
                       
                     ),
                     
           )
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
                     regressionTTestUI("regression_t_test")
           )
  )
)


server <- function(input, output, session) {
  
  ########## Demo Confidence Intervals 1 ########## 
  lower_bounds_demo_1 <- reactiveVal(c())
  upper_bounds_demo_1 <- reactiveVal(c())
  contains_mean_demo_1 <- reactiveVal(c())
  current_index_for_plotting_demo_1 <- reactiveVal(0)
  
  intervals_generated_so_far <- reactiveVal(0)
  intervals_not_containing_mean <- reactiveVal(0)

  # Simulate - Creating 100 confidence intervals.
  observeEvent(input$simulate_demo_1, {
    current_index_for_plotting_demo_1(length(lower_bounds_demo_1) + 1)
    
    # Create 100 confidence intervals using a normal distribution.
    true_mean = 100
    sample_size = 30
    pop_sd = 15
    lower_bounds = numeric(100)
    upper_bounds = numeric(100)
    contains_mean = logical(100)
    not_containing_mean_count = 0
    for (i in 1:100) {
      sample = rnorm(sample_size, mean = true_mean, sd = pop_sd)
      sample_mean = mean(sample)
      stderr = sd(sample) / sqrt(sample_size)
      margin = qnorm(0.975) * stderr
      lower = sample_mean - margin
      upper = sample_mean + margin
      lower_bounds[i] = lower
      upper_bounds[i] = upper
      contains_mean[i] = lower <= true_mean & upper >= true_mean
      not_containing_mean_count = not_containing_mean_count + !(lower <= true_mean & upper >= true_mean)
    }
    
    # Save data
    lower_bounds_demo_1(lower_bounds)
    upper_bounds_demo_1(upper_bounds)
    contains_mean_demo_1(contains_mean)
    
    # Update historical values
    intervals_generated_so_far(intervals_generated_so_far() + 100)
    intervals_not_containing_mean(intervals_not_containing_mean() + not_containing_mean_count)
  })
  
  observeEvent(input$reset_history_demo_1, {
    intervals_generated_so_far(0)
    intervals_not_containing_mean(0)
  })
  
  # Confidence interval plot.
  output$confidence_interval_plot_demo_1 = renderPlot({
    true_mean = 100
    
    lower_bounds = lower_bounds_demo_1()
    upper_bounds = upper_bounds_demo_1()
    contains_mean = contains_mean_demo_1()
    
    # Set fallback x-limits if no data exists yet
    x_min = if (length(lower_bounds) > 0) min(lower_bounds, na.rm = TRUE) else true_mean - 10
    x_max = if (length(upper_bounds) > 0) max(upper_bounds, na.rm = TRUE) else true_mean + 10
    
    # Set up plot area
    plot(
      NA, xlim = c(x_min, x_max), ylim = c(1, 100),
      xlab = "Value", ylab = "Sample Number",
      main = "Simulated 95% Confidence Intervals"
    )
    
    # Add dashed line for true mean
    abline(v = true_mean, lty = 2)
    
    # Only draw intervals if data is available
    if (length(lower_bounds) > 0) {
      for (i in 1:100) {
        color = if (contains_mean[i]) "green" else "red"
        lines(c(lower_bounds[i], upper_bounds[i]), c(i, i), col = color, lwd = 2)
      }
    }
  })
  
  # History of the confidence intervals that have been generated.
  output$demo_1_confidence_interval_history <- renderUI({
    total_intervals_generated_so_far = intervals_generated_so_far()
    num_intervals_not_containing_mean = intervals_not_containing_mean()
    percentage_containing_mean = 0
    if (total_intervals_generated_so_far != 0) {
      percentage_containing_mean = 100 * (total_intervals_generated_so_far-num_intervals_not_containing_mean)/total_intervals_generated_so_far
    }
    HTML(paste0(
      "<b>Running History:</b><br>",
      "<ul>",
      "<li>Total number of intervals generated so far: ", total_intervals_generated_so_far, "</li>",
      "<li>Total number of intervals NOT containing the population mean: ", num_intervals_not_containing_mean, "</li>",
      "<li>Percentage of intervals containing the population mean: ", round(percentage_containing_mean, 2), "%</li>",
      "</ul>"
    ))
  })
  
  boxModelPart1Server(id = "box_model_part_1")
  boxModelPart2Server(id = "box_model_part_2")
  boxModelPart3Server(id = "box_model_part_3")
  oneSampleZTestServer(id = "1_sample_z_test")
  proportionTestMainServer(id = "proportion_z_test")
  tCurveMotivationServer(id = "t_curve_motivation")
  oneSampleTTestServer(id = "1_sample_t_test")
  pairedTTestServer(id = "paired_t_test")
  twoSampleTTestServer(id = "2_sample_t_test")
  regressionTTestServer(id = "regression_t_test")

}

shinyApp(ui, server)