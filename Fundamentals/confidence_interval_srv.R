confidenceIntervalServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    ########## Demo Confidence Intervals 1 ########## 
    lower_bounds_demo_1 <- reactiveVal(c())
    upper_bounds_demo_1 <- reactiveVal(c())
    contains_mean_demo_1 <- reactiveVal(c())
    
    intervals_generated_so_far <- reactiveVal(0)
    intervals_not_containing_mean <- reactiveVal(0)
    
    # Simulate - Creating 100 confidence intervals.
    observeEvent(input$simulate_demo_1, {
      
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
    
    ########## Demo Confidence Intervals 2 ########## 
    
    lower_bounds_demo_2 <- reactiveVal(c())
    upper_bounds_demo_2 <- reactiveVal(c())
    contains_mean_demo_2 <- reactiveVal(c())
    
    intervals_generated_so_far_2 <- reactiveVal(0)
    intervals_not_containing_mean_2 <- reactiveVal(0)
    
    # Simulate - Creating 100 confidence intervals.
    observeEvent(input$simulate_demo_2, {
      
      # Create 100 confidence intervals using a normal distribution.
      true_mean = input$dist_mean
      sample_size = input$sample_size
      pop_sd = input$dist_sd
      lower_bounds = numeric(100)
      upper_bounds = numeric(100)
      contains_mean = logical(100)
      not_containing_mean_count = 0
      for (i in 1:100) {
        sample = rnorm(sample_size, mean = true_mean, sd = pop_sd)
        sample_mean = mean(sample)
        stderr = sd(sample) / sqrt(sample_size)
        percentile = (1 + (input$confidence_level)/100)/2
        margin = qnorm(percentile) * stderr
        lower = sample_mean - margin
        upper = sample_mean + margin
        lower_bounds[i] = lower
        upper_bounds[i] = upper
        contains_mean[i] = lower <= true_mean & upper >= true_mean
        not_containing_mean_count = not_containing_mean_count + !(lower <= true_mean & upper >= true_mean)
      }
      
      # Save data
      lower_bounds_demo_2(lower_bounds)
      upper_bounds_demo_2(upper_bounds)
      contains_mean_demo_2(contains_mean)
      
      # Update historical values
      intervals_generated_so_far_2(intervals_generated_so_far_2() + 100)
      intervals_not_containing_mean_2(intervals_not_containing_mean_2() + not_containing_mean_count)
    })
    
    # Confidence interval plot.
    output$confidence_interval_plot_demo_2 = renderPlot({
      true_mean <- input$dist_mean
      pop_sd <- max(input$dist_sd, 1e-6)
      
      # Define min and max confidence level allowed
      conf_min <- 1
      conf_max <- 99
      
      # Compute largest margin for fixed axis (at max confidence level & smallest sample size)
      percentile_max <- (1 + conf_max / 100) / 2
      stderr_max <- pop_sd / sqrt(5)
      margin_max <- qnorm(percentile_max) * stderr_max
      
      # Fixed axis limits based on largest possible margin
      x_min <- true_mean - margin_max * 1.2
      x_max <- true_mean + margin_max * 1.2
      
      # Current confidence level (used for titles, actual CI calculation elsewhere)
      conf_level <- min(max(input$confidence_level, conf_min), conf_max)
      percentile <- (1 + conf_level / 100) / 2
      
      lower_bounds <- lower_bounds_demo_2()
      upper_bounds <- upper_bounds_demo_2()
      contains_mean <- contains_mean_demo_2()
      
      plot(
        NA, xlim = c(x_min, x_max), ylim = c(1, 100),
        xlab = "Value", ylab = "Sample Number",
        main = paste0("Simulated ", conf_level, "% Confidence Intervals")
      )
      
      abline(v = true_mean, lty = 2)
      
      if (length(lower_bounds) > 0) {
        for (i in seq_along(lower_bounds)) {
          color <- if (contains_mean[i]) "green" else "red"
          lines(c(lower_bounds[i], upper_bounds[i]), c(i, i), col = color, lwd = 2)
        }
      }
    })
    
    
    # History of the confidence intervals that have been generated.
    output$demo_2_confidence_interval_history <- renderUI({
      total_intervals_generated_so_far = intervals_generated_so_far_2()
      num_intervals_not_containing_mean = intervals_not_containing_mean_2()
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
    
    observeEvent(input$reset_history_demo_2, {
      intervals_generated_so_far_2(0)
      intervals_not_containing_mean_2(0)
    })
    observeEvent(input$dist_mean, {
      intervals_generated_so_far_2(0)
      intervals_not_containing_mean_2(0)
    })
    observeEvent(input$dist_sd, {
      intervals_generated_so_far_2(0)
      intervals_not_containing_mean_2(0)
    })
    observeEvent(input$confidence_level, {
      intervals_generated_so_far_2(0)
      intervals_not_containing_mean_2(0)
    })
    observeEvent(input$sample_size, {
      intervals_generated_so_far_2(0)
      intervals_not_containing_mean_2(0)
    })
    
  })
  
}