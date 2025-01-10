tCurveMotivationServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$changing_df_graph = renderPlot({

      data <- data.frame(x = seq(-4, 4, length.out = 100))

      plot = ggplot(data, aes(x)) +
        stat_function(fun = dt, args = list(df = input$df_slider), color = "red", size = 0.5) +
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

      if (input$display_normal_curve) {
        plot = plot + stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "black", size = 0.5, linetype = "dashed")
      }

      return(plot)
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