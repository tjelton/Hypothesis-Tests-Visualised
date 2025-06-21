tCurveMotivationServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$changing_df_graph <- renderPlot({
      par(mar = c(4,0,0,0)) # no margins
      
      x <- seq(-4, 4, length.out = 100)
      y_t <- dt(x, df = input$df_slider)
      y_norm <- dnorm(x, mean = 0, sd = 1)
      
      # Determine y-axis limits so t curve fits fully
      ylim <- range(y_t, y_norm)
      
      # Plot t-distribution curve in black solid line
      plot(x, y_t, type = "l", col = "black", lwd = 1,
           ylab = "", xlab = "", axes = FALSE, ylim = ylim)
      
      # Add x-axis with labels
      axis(1, col = "black")
      
      # Add normal curve if requested - red dashed line
      if (input$display_normal_curve) {
        lines(x, y_norm, col = "red", lwd = 1, lty = "dashed")
      }
    })
    
    output$test_stat_normal_plot = renderPlot({
      return(curve_shaded_test_stat(dnorm, list(mean = 0, sd = 1), as.numeric(input$test_statistic_input), 1))
    })

    output$test_stat_t_plot = renderPlot({
      return(curve_shaded_test_stat(dt, list(df=input$df_slider_demo_2), as.numeric(input$test_statistic_input), 1))
    })

    output$p_value_normal_curve = renderUI({
      p_val = 2 * (1 - pnorm(abs(as.numeric(input$test_statistic_input))))
      p_value = withMathJax(HTML("<p style='font-size: 16px; text-align: center;'>\\( p =", as.character(round(p_val,5)) ,"\\)</p>"))
      return(p_value)
    })

    output$p_value_t_curve = renderUI({
      p_val = 2 * (1 - pt(abs(as.numeric(input$test_statistic_input)), df = input$df_slider_demo_2))
      p_value = withMathJax(HTML("<p style='font-size: 16px; text-align: center;'>\\( p =", as.character(round(p_val,5)) ,"\\)</p>"))
      return(p_value)
    })
    
  
    

  })
    
}