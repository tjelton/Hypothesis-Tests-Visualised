boxModelPart2Server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ########## Box with tickets 1,0,0,0 (n = 25) ########## 
    
    output$box_model_n_25 <- renderGrViz({
      string <- paste("
        digraph diagram {
          graph [layout = dot, rankdir = TB]
      
          node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5]
          box [label = '1, 0, 0, 0']
      
          node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]
          sample [label = 'Sample Mean']
      
          edge [minlen = 2]
            box->sample [label = '  n = 25', fontsize = 12, labeldistance = 5]
        }", sep = "")
      grViz(string)
    })
    
    empirical_data_n_25 <- reactiveVal(c())
    
    # Event: 1 repeat.
    observeEvent(input$repeat_1_n_25, {
      value = mean(sample(c(1, 0, 0, 0), size = 25, replace = TRUE))
      empirical_data_n_25(c(empirical_data_n_25(), value))
    })
    
    # Event: 10 repeats.
    observeEvent(input$repeat_10_n_25, {
      value = replicate(10, mean(sample(c(1, 0, 0, 0), size = 25, replace = TRUE)))
      empirical_data_n_25(c(empirical_data_n_25(), value))
    })
    
    # Event: 25 repeats.
    observeEvent(input$repeat_25_n_25, {
      value = replicate(25, mean(sample(c(1, 0, 0, 0), size = 25, replace = TRUE)))
      empirical_data_n_25(c(empirical_data_n_25(), value))
    })
    
    # Event: 100 repeats.
    observeEvent(input$repeat_100_n_25, {
      value = replicate(100, mean(sample(c(1, 0, 0, 0), size = 25, replace = TRUE)))
      empirical_data_n_25(c(empirical_data_n_25(), value))
    })
    
    # Event: reset histogram.
    observeEvent(input$reset_button_n_25, {
      empirical_data_n_25(c())
    })
    
    # Histogram of mean frequencies.
    output$histogram_frequencies_n_25 = renderPlot({
      
      values <- empirical_data_n_25()
      num_values = as.character(length(values))
      
      title_string = paste("Empiricial Distribution of Sample Means (n = ", num_values, ")", sep = "")
      x_axis_string = "Sample Mean Value"
      
      # Main histogram
      if (length(values) > 0) {
        bins_to_include <- length(unique(values))/3
        
        # If over 20 bins, just set to 20 (otherwise too many bins)
        if (bins_to_include > 15) bins_to_include <- 15
        
        range_vals <- range(values)
        breaks_seq <- seq(from = range_vals[1], to = range_vals[2], length.out = bins_to_include + 1)
        
        hist(
          values,
          breaks = breaks_seq,
          freq = FALSE,
          col = "lightgreen",
          border = "black",
          xlab = x_axis_string,
          ylab = "Density",
          main = title_string
        )
        
        
        # Placeholder when no data exists
      } else {
        plot(
          1, type = "n",
          xlim = c(0, 10),
          ylim = c(0, 0.25),
          xlab = x_axis_string,
          ylab = "Density",
          main = title_string,
          axes = FALSE
        )
        axis(1)
        axis(2, at = seq(0, 0.25, by = 0.05), labels = seq(0, 0.25, by = 0.05))
      }
    })
    
    ########## Box with tickets 1,0,0,0 (n = 5) ########## 
    
    output$box_model_n_5 <- renderGrViz({
      string <- paste("
        digraph diagram {
          graph [layout = dot, rankdir = TB]
      
          node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5]
          box [label = '1, 0, 0, 0']
      
          node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]
          sample [label = 'Sample Mean']
      
          edge [minlen = 2]
            box->sample [label = '  n = 5', fontsize = 12, labeldistance = 5]
        }", sep = "")
      grViz(string)
    })
    
    empirical_data_n_5 <- reactiveVal(c())
    
    # Event: 1 repeat.
    observeEvent(input$repeat_1_n_5, {
      value = mean(sample(c(1, 0, 0, 0), size = 5, replace = TRUE))
      empirical_data_n_5(c(empirical_data_n_5(), value))
    })
    
    # Event: 10 repeats.
    observeEvent(input$repeat_10_n_5, {
      value = replicate(10, mean(sample(c(1, 0, 0, 0), size = 5, replace = TRUE)))
      empirical_data_n_5(c(empirical_data_n_5(), value))
    })
    
    # Event: 25 repeats.
    observeEvent(input$repeat_25_n_5, {
      value = replicate(25, mean(sample(c(1, 0, 0, 0), size = 5, replace = TRUE)))
      empirical_data_n_5(c(empirical_data_n_5(), value))
    })
    
    # Event: 100 repeats.
    observeEvent(input$repeat_100_n_5, {
      value = replicate(100, mean(sample(c(1, 0, 0, 0), size = 5, replace = TRUE)))
      empirical_data_n_5(c(empirical_data_n_5(), value))
    })
    
    # Event: reset histogram.
    observeEvent(input$reset_button_n_5, {
      empirical_data_n_5(c())
    })
    
    # Histogram of mean and sum frequencies.
    output$histogram_frequencies_n_5 = renderPlot({
      
      values <- empirical_data_n_5()
      num_values = as.character(length(values))
      
      title_string = paste("Empiricial Distribution of Sample Means (n = ", num_values, ")", sep = "")
      x_axis_string = "Sample Mean Value"
      
      # Main histogram
      if (length(values) > 0) {
        bins_to_include <- length(unique(values)) - 1
        
        range_vals <- range(values)
        breaks_seq <- seq(from = range_vals[1], to = range_vals[2], length.out = bins_to_include + 1)
        
        hist(
          values,
          breaks = breaks_seq,
          freq = FALSE,
          col = "lightgreen",
          border = "black",
          xlab = x_axis_string,
          ylab = "Density",
          main = title_string
        )
        
        
        # Placeholder when no data exists
      } else {
        plot(
          1, type = "n",
          xlim = c(0, 10),
          ylim = c(0, 0.25),
          xlab = x_axis_string,
          ylab = "Density",
          main = title_string,
          axes = FALSE
        )
        axis(1)
        axis(2, at = seq(0, 0.25, by = 0.05), labels = seq(0, 0.25, by = 0.05))
      }
    })
    
    ########## Custom Box Simulation ########## 
    
    custom_box_model_tickets = reactiveVal(c())
    invalid_tickets_string_bool = reactiveVal(FALSE)
    
    # When the sum or mean radio button is updated, reset the data to be empty.
    observeEvent(input$box_sum_or_mean, {
      n_5_empirical_data(c())
      n_25_empirical_data(c())
      n_50_empirical_data(c())
      n_100_empirical_data(c())
    })
    
    observeEvent(input$custom_box_model_example_choice, {
      n_5_empirical_data(c())
      n_25_empirical_data(c())
      n_50_empirical_data(c())
      n_100_empirical_data(c())
      if (input$custom_box_model_example_choice == 1) {
        custom_box_model_tickets(c(1,0))
      } else if (input$custom_box_model_example_choice == 2) {
        custom_box_model_tickets(c(1,0,0,0))
      } else if (input$custom_box_model_example_choice == 3) {
        custom_box_model_tickets(c(1,0,0,0,0,0))
      } else if (input$custom_box_model_example_choice == 4) {
        custom_box_model_tickets(c(1,2,3,4,5,6,7,8,9))
      } else if (input$custom_box_model_example_choice == 5) {
        custom_box_model_tickets(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,100))
      } else {
        custom_box_model_tickets(c(1,0))
      } 
    })
    
    output$custon_box_model = renderGrViz({
      vals = custom_box_model_tickets()
      
      # Initialize empty string and counter
      tickets_string = ""
      count = 0
      
      for (val in vals) {
        count = count + 1
        if (count == 15) {
          tickets_string <- paste(tickets_string, as.character(val), sep = "\n")
          count = 0
        } else {
          tickets_string <- paste(tickets_string, as.character(val), sep = ", ")
        }
      }
      
      # Remove leading comma or newline
      tickets_string = substring(tickets_string, 3)
      
      # Escape newlines for Graphviz
      label_text = gsub("\n", "\\\\n", tickets_string)
      
      # Build the diagram string
      string = paste0("
      digraph diagram {
        graph [layout = dot, rankdir = TB]
        node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5]
        box [label = \"", label_text, "\"]
      }
    ")
      
      grViz(string)
    })
    
    # If the user wants to manually specify a box model, bring up a text box for them to specify their tickets.
    output$custom_box_model_input_button <- renderUI({
      if (input$custom_box_model_example_choice == 6) {
        return(
          tagList(
            HTML("<p>In the text box below, enter the tickets that you wish to place into the box. Only <i>numbers</i> can be
                 added, and <i>each ticket should be seperated by a comma</i>. For example, if you want to have 1x1 ticket
                 and 1x0 ticket in the box, enter <i>1,0</i>."),
            textAreaInput(
              ns("box_tickets_text_entry"),
              NULL,
              value = "1,0",
              width = "100%"
            ),
            fluidRow(
              column(8),
              column(4,
                     actionButton(ns("submit_tickets"), "Set Tickets", class = "btn btn-success", style="color: #fff;", width = "100%")
              )
            ),
            uiOutput(ns("tickets_text_error_message"))
          )
        )
      } else {
        return()
      }
    })
    
    # Process tickets text box strings.
    observeEvent(input$submit_tickets, {
      
      # Extract the values that are separated between the commas.
      characters <- strsplit(input$box_tickets_text_entry, "")[[1]]
      values = c()
      current_string = ""
      for (char in characters) {
        if (char == ",") {
          values = c(values, current_string)
          current_string = ""
        } else {
          current_string = paste(current_string, char, sep = "")
        }
      }
      if (current_string != "") {
        values = c(values, current_string)
      }
      
      # Convert the values to numbers.
      numeric_vec = as.numeric(values)
      
      # Set the tickets in the box to default (1,0) if NA's present, or the string is empty.
      if (any(is.na(numeric_vec))) {
        invalid_tickets_string_bool(TRUE)
        custom_box_model_tickets(c(1,0))
      } else if (length(numeric_vec) <= 1) {
        invalid_tickets_string_bool(TRUE)
        custom_box_model_tickets(c(1,0))
      } else {
        custom_box_model_tickets(numeric_vec)
        invalid_tickets_string_bool(FALSE)
      }
    })
    
    # Error message for when the text box for entering the tickets for the box is invalid
    output$tickets_text_error_message = renderUI({
      if (invalid_tickets_string_bool()) {
        return(
          HTML("<span style='color: red;'><p>Error: One or move values that you added cannot be interpreted. Please carefully
               check what you entered. You must enter at least 2 valid tickets. Setting contents of the box to 1,0,0,0.</p></span>")
        )
      }
    })
    
    n_5_empirical_data = reactiveVal(c())
    n_25_empirical_data = reactiveVal(c())
    n_50_empirical_data = reactiveVal(c())
    n_100_empirical_data = reactiveVal(c())
    
    # Simulate tickets on button click.
    observeEvent(input$simulate, {
      n_5_empirical_data(replicate(10000, simulate_box(input$box_sum_or_mean, 5, custom_box_model_tickets())))
      n_25_empirical_data(replicate(10000, simulate_box(input$box_sum_or_mean, 25, custom_box_model_tickets())))
      n_50_empirical_data(replicate(10000, simulate_box(input$box_sum_or_mean, 50, custom_box_model_tickets())))
      n_100_empirical_data(replicate(10000, simulate_box(input$box_sum_or_mean, 100, custom_box_model_tickets())))
    })
    
    simulate_box <- function(mean_or_sample_as_int, n, box) {
      value = sample(box, n, replace = TRUE)
      if (mean_or_sample_as_int == 2) {
        value = mean(value)
      } else {
        value = sum(value)
      }
      return(value)
    }
    
    make_histogram_plot <- function(values_reactive, title) {
      renderPlot({
        values <- values_reactive()
        num_values <- as.character(length(values))
        
        if (length(values) > 0) {
          bins_to_include <- length(unique(values))
          
          # Cap the number of bins at 15
          if (bins_to_include > 18) bins_to_include <- 18
          
          range_vals <- range(values)
          breaks_seq <- seq(from = range_vals[1], to = range_vals[2], length.out = bins_to_include + 1)
          
          hist(
            values,
            breaks = "Sturges",
            freq = FALSE,
            col = "lightgreen",
            border = "black",
            xlab = "Values",
            ylab = "Density",
            main = title
          )
          
        } else {
          plot(
            1, type = "n",
            xlim = c(0, 10),
            ylim = c(0, 0.25),
            xlab = "Values",
            ylab = "Density",
            main = title,
            axes = FALSE
          )
          axis(1)
          axis(2, at = seq(0, 0.25, by = 0.05), labels = seq(0, 0.25, by = 0.05))
        }
      })
    }
    
    output$histogram_frequencies_custom_n_5 <- make_histogram_plot(n_5_empirical_data, "n = 5")
    output$histogram_frequencies_custom_n_25 <- make_histogram_plot(n_25_empirical_data, "n = 25")
    output$histogram_frequencies_custom_n_50 <- make_histogram_plot(n_50_empirical_data, "n = 50")
    output$histogram_frequencies_custom_n_100 <- make_histogram_plot(n_100_empirical_data, "n = 100")
    
    # Output message to accompany the different examples
    output$examples_additional_information = renderUI({
      if (input$custom_box_model_example_choice == 1) {
        return(
          HTML("<p><span style='color: red;'>What do we see after pressing simulate? </span>When looking from the perspective of sample sums, we see that at a sample size of n = 25, the distribution
             of sample means appear normally distributed. Hence, for this box, values of n greater than or equal to 25 are sufficient. Is is relatively unsurprising that we need
             such a small sample size here. The box is very symmetric (there is an even balance between the '0' and '1' tickets).</p>")
        )
      } else if (input$custom_box_model_example_choice == 2) {
        return(
          HTML("<p><span style='color: red;'>What do we see after pressing simulate? </span>When looking from the perspective of sample sums, we see that at a sample size of n = 25, the distribution
             of sample means appear very close to being normally distributed. Hence, for this box, values of n greater than or equal to 25 are sufficient.</p>")
        )
      } else if (input$custom_box_model_example_choice == 3) {
        return(
          HTML("<p><span style='color: red;'>What do we see after pressing simulate? </span>When looking from the perspective of sample sums, we see that at a sample size of n = 50, the distribution
             of sample means appear very close to being normally distributed. We need a larger sample size for this box, as there is a large imbalance between the '0' and 
             '1' tickets.</p>")
        )
      } else if (input$custom_box_model_example_choice == 4) {
        return(
          HTML("<p><span style='color: red;'>What do we see after pressing simulate? </span>When looking from the perspective of sample sums, we see that at a sample size of n = 5, the distribution
             of sample means appear very close to being normally distributed. A small sample size for this box is sufficient as the tickets are nearly symmetric.</p>")
        )
      } else if (input$custom_box_model_example_choice == 5) {
        return(
          HTML("<p><span style='color: red;'>What do we see after pressing simulate? </span>Even at n = 100, the sample sums or means do not appear to be normally distributed. This is because the
             tickes are incredibly assymetric. This is a classic example where assuming that if n is large  that the CLT must apply (such as n greater than 35 or 50) can be 
             misleading.</p>")
        )
      }
    })

  })
    
}