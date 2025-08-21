assumptions_1_sample_t_test_Server <- function(id, sample_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$assumption_2_plots <- renderUI({
        
        qq_plot <- renderPlot({
          qqnorm(sample_data())
          qqline(sample_data(), col = "red")
        })
        
        box_plot <- renderPlot({
          boxplot(sample_data(), main = "Boxplot of Sample Data")
        })
        
        hist_plot <- renderPlot({
          hist(sample_data(),
               main = "Histogram of Sample Data",
               xlab = "",
               breaks = 30)
        })
        
        return(
          fluidRow(
            column(4, qq_plot),
            column(4, box_plot),
            column(4, hist_plot)
          )
        )
      })
    }
  )
}
