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
  
  # Generate x and y values for the curve
  x_vals <- seq(lower_xlimit_plot, upper_xlimit_plot, length.out = 1000)
  y_vals <- do.call(function_stat, c(list(x_vals), stat_function_args))
  
  # Plot the curve
  par(mar = c(4, 0.5, 0.5, 0.5))
  plot(x_vals, y_vals, type = "l", lwd = 2, col = "black",
       xlab = "", ylab = "", yaxt = "n", bty = "n", axes = TRUE)
  axis(1)
  
  # Shade areas depending on hypothesis
  
  # Two-tailed test: shade both sides
  if (alternate_hypothesis_choice == 1) {
    x_shade1 <- x_vals[x_vals <= -abs(test_stat)]
    y_shade1 <- do.call(function_stat, c(list(x_shade1), stat_function_args))
    polygon(c(x_shade1, rev(x_shade1)),
            c(y_shade1, rep(0, length(y_shade1))),
            col = rgb(1, 0, 0, 0.5), border = NA)
    
    x_shade2 <- x_vals[x_vals >= abs(test_stat)]
    y_shade2 <- do.call(function_stat, c(list(x_shade2), stat_function_args))
    polygon(c(x_shade2, rev(x_shade2)),
            c(y_shade2, rep(0, length(y_shade2))),
            col = rgb(1, 0, 0, 0.5), border = NA)
    
    abline(v = c(-abs(test_stat), abs(test_stat)), col = "blue", lty = 2)
    text(-abs(test_stat) - 0.8, 0.3, round(-abs(test_stat), 2), col = "blue", adj = 0)
    text(abs(test_stat) + 0.25, 0.3, round(abs(test_stat), 2), col = "blue", adj = 0)
    
  # Right-tailed test
  } else if (alternate_hypothesis_choice == 2) {
    x_shade <- x_vals[x_vals >= test_stat]
    y_shade <- do.call(function_stat, c(list(x_shade), stat_function_args))
    polygon(c(x_shade, rev(x_shade)),
            c(y_shade, rep(0, length(y_shade))),
            col = rgb(1, 0, 0, 0.5), border = NA)
    
    abline(v = test_stat, col = "blue", lty = 2)
    text(test_stat + 0.25, 0.3, round(test_stat, 2), col = "blue", adj = 0)
    
  # Left-tailed test
  } else if (alternate_hypothesis_choice == 3) {
    x_shade <- x_vals[x_vals <= test_stat]
    y_shade <- do.call(function_stat, c(list(x_shade), stat_function_args))
    polygon(c(x_shade, rev(x_shade)),
            c(y_shade, rep(0, length(y_shade))),
            col = rgb(1, 0, 0, 0.5), border = NA)
    
    abline(v = test_stat, col = "blue", lty = 2)
    text(test_stat - 0.8, 0.3, round(test_stat, 2), col = "blue", adj = 0)
  }
  
}