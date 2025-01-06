# Function to create a curve and shade the region corresponding to the test statistic and tail.
# Arguments:
#   function_stat: the function to plot (e.g. dnorm is normal density curve, dt is t-curve).
#   stat_function_args: list with the arguments needed to plot the stat function (e.g. for a standard normal curve, list(mean = 0, sd = 1)).
#   test_stat: test statistic as a number.
#   alternate_hypothesis_choice: 1 denotes both upper and lower tail, 2 denotes lower tail, 3 denotes upper tail
curve_shaded_test_stat <- function(function_stat, stat_function_args, test_stat, alternate_hypothesis_choice) {
  
  # Define the plots lowest and highest x-value.
  lower_xlimit_plot = -3.5
  upper_xlimit_plot = 3.5
  if (alternate_hypothesis_choice == 1 && abs(test_stat) > upper_xlimit_plot) {
    lower_xlimit_plot = -abs(test_stat) - 1
    upper_xlimit_plot = abs(test_stat) + 1
  } else if (test_stat < lower_xlimit_plot) {
    lower_xlimit_plot = test_stat - 1
  } else if (test_stat > upper_xlimit_plot) {
    upper_xlimit_plot = test_stat + 1
  }
  
  data <- data.frame(x = seq(lower_xlimit_plot, upper_xlimit_plot, length.out = 100))
  
  # Define general ggplot.
  print("RECHED 24")
  base_plot = ggplot(data, aes(x)) +
    # Plot the normal distribution curve
    stat_function(fun = function_stat, args = stat_function_args, color = "black", size = 1) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.line.y = element_blank(),
      panel.border = element_blank()
    )
  print("REACHED 39")
  # Create a data frame for shading based upon alternate hypothesis choice.
  if (alternate_hypothesis_choice == 1) {
    base_plot = base_plot + 
      # Lower tail
      geom_area(stat = "function", 
                fun = function_stat,
                args = stat_function_args,
                fill = "red",
                alpha = 0.5,
                xlim = c(lower_xlimit_plot, -abs(test_stat))) +
      # Add annotated line on test statistic
      geom_vline(xintercept = -abs(test_stat), linetype = "dashed", color = "blue") +
      annotate("text", x = -abs(test_stat) - 0.8, y = 0.3, 
               label = as.character(round(-abs(test_stat), 2)), color = "blue", hjust = 0) +
      
      # Upper tail
      geom_area(stat = "function", 
                fun = function_stat,
                args = stat_function_args,
                fill = "red",
                alpha = 0.5,
                xlim = c(abs(test_stat), upper_xlimit_plot)) +
      # Add annotated line on test statistic
      geom_vline(xintercept = abs(test_stat), linetype = "dashed", color = "blue") +
      annotate("text", x = abs(test_stat) + 0.25, y = 0.3, 
               label = as.character(round(abs(test_stat), 2)), color = "blue", hjust = 0)
    
  } else if (alternate_hypothesis_choice == 2) {
    base_plot = base_plot +
      # Upper tail
      geom_area(stat = "function", 
                fun = function_stat,
                args = stat_function_args,
                fill = "red",
                alpha = 0.5,
                xlim = c(test_stat, upper_xlimit_plot)) +
      # Add annotated line on test statistic
      geom_vline(xintercept = test_stat, linetype = "dashed", color = "blue") +
      annotate("text", x = test_stat + 0.25, y = 0.3, 
               label = as.character(round(test_stat, 2)), color = "blue", hjust = 0)
    
  } else if (alternate_hypothesis_choice == 3) {
    base_plot = base_plot + 
      # Lower tail
      geom_area(stat = "function", 
                fun = function_stat,
                args = stat_function_args,
                fill = "red",
                alpha = 0.5,
                xlim = c(lower_xlimit_plot, test_stat)) +
      # Add annotated line on test statistic
      geom_vline(xintercept = test_stat, linetype = "dashed", color = "blue") +
      annotate("text", x = test_stat - 0.8, y = 0.3, 
               label = as.character(round(test_stat, 2)), color = "blue", hjust = 0)
  }
  
  return(base_plot)
  
}