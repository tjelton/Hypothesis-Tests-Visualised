boxModelPart1Server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Heads and tails box model (with words).
    make_coin_flip_plot <- function() {
      string <- "
        digraph diagram {
          graph [layout = dot, rankdir = TB]
      
          node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5]
          box [label = 'Head (H), Tail (T)']
      
          node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]
          sample [label = 'Sample']
      
          edge [minlen = 2]
            box->sample [label = '  n = 5', fontsize = 12, labeldistance = 5]
        }"
      grViz(string)
    }
    
    output$example_coin_flip_1 <- renderGrViz({
      make_coin_flip_plot()
    })
    
    output$example_coin_flip_2 <- renderGrViz({
      make_coin_flip_plot()
    })
    
    ###################### Simulated Coin Flip Samples (With Words) ###################### 
    
    coin_flip_word_labels = reactiveVal(c())
    count_coin_flip_simulation = reactiveVal(0)
    observeEvent(input$simulate_coin_flip_1, {
      # For the first 10, we just add a new coin flip.
      if (count_coin_flip_simulation() < 10) {
        count_coin_flip_simulation(count_coin_flip_simulation() + 1)
        coin_flip_word_labels(c(coin_flip_word_labels(), paste0(sample(c("H", "T"), size = 5, replace = TRUE), collapse = " ")))
        # For subsequent, we replaced samples that have already been displayed.
      } else {
        count_coin_flip_simulation(count_coin_flip_simulation() + 1)
        current_flip_int = count_coin_flip_simulation()
        current_flip_int = current_flip_int %% 10
        if (current_flip_int == 0) { # Because 10 %% 10 = 0
          current_flip_int = 10
        }
        coin_flip_word_labels_new = coin_flip_word_labels()
        coin_flip_word_labels_new[current_flip_int] = paste0(sample(c("H", "T"), size = 5, replace = TRUE), collapse = " ")
        coin_flip_word_labels(coin_flip_word_labels_new)
      }
    })
    
    # Display the samples. Arranged in yellow circles in 5 row by 2 column arrangment.
    output$simulated_coin_flip_samples <- renderGrViz({
      
      if (count_coin_flip_simulation() == 0) {
        return(NULL)
      }
      
      n <- min(count_coin_flip_simulation(), 10)
      
      labels <- coin_flip_word_labels()
      
      num_rows <- ceiling(n / 2)
      
      table_rows <- ""
      for (i in 1:num_rows) {
        left_label <- labels[(i - 1) * 2 + 1]
        right_index <- (i - 1) * 2 + 2
        right_label <- if (right_index <= n) labels[right_index] else NULL
        
        # Left cell (always present)
        left_cell <- sprintf(
              "<TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' BGCOLOR='#f9ffbd' STYLE='rounded' BORDER='1' COLOR='black' CELLPADDING='4' ALIGN='CENTER' VALIGN='MIDDLE'>
           <FONT POINT-SIZE='12'>%s</FONT>
         </TD>", left_label)
        
        right_cell <- if (!is.null(right_label)) {
          sprintf(
                "<TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' BGCOLOR='#f9ffbd' STYLE='rounded' BORDER='1' COLOR='black' CELLPADDING='4' ALIGN='CENTER' VALIGN='MIDDLE'>
             <FONT POINT-SIZE='12'>%s</FONT>
           </TD>", right_label)
        } else {
          "<TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' CELLPADDING='4'></TD>"
        }
        
        row <- sprintf("<TR>%s%s</TR>", left_cell, right_cell)
        table_rows <- paste0(table_rows, row, "\n")
      }
      
      graph_string <- sprintf("
          digraph diagram {
            node [shape=plaintext]
            graph [layout=dot]
    
            tbl [label=<
              <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='10'>
                %s
              </TABLE>
            >]
          }
        ", table_rows)
      
      grViz(graph_string)
    })
    
    ###################### Sample Single Word Examples ###################### 
    
    output$single_sample_words <- renderGrViz({
      grViz("
        digraph diagram {
          node [shape=plaintext]
          graph [margin=0, rankdir=LR]
    
          tbl [label=<
            <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
              <TR>
                <TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' 
                    BGCOLOR='#f9ffbd' STYLE='rounded' BORDER='1' COLOR='black' 
                    CELLPADDING='4' ALIGN='CENTER' VALIGN='MIDDLE'>
                  <FONT POINT-SIZE='12'>H T T H T</FONT>
                </TD>
              </TR>
            </TABLE>
          >]
        }
      ")
    })
    
    output$single_sample_numbers <- renderGrViz({
      grViz("
        digraph diagram {
          node [shape=plaintext]
          graph [margin=0, rankdir=LR]
    
          tbl [label=<
            <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='0'>
              <TR>
                <TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' 
                    BGCOLOR='#f9ffbd' STYLE='rounded' BORDER='1' COLOR='black' 
                    CELLPADDING='4' ALIGN='CENTER' VALIGN='MIDDLE'>
                  <FONT POINT-SIZE='12'>1 0 0 1 0</FONT>
                </TD>
              </TR>
            </TABLE>
          >]
        }
      ")
    })
    
    ###################### Simulated Coin Flip Samples (Sums and Means) ###################### 
    
    output$coin_flip_numeric_box_model <- renderGrViz({
      string <- paste("
        digraph diagram {
          graph [layout = dot, rankdir = TB]
      
          node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5]
          box [label = '1, 0']
      
          node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]
          sample [label = 'Sample ", input$demonstration_sum_or_mean, "']
      
          edge [minlen = 2]
            box->sample [label = '  n = 5', fontsize = 12, labeldistance = 5]
        }", sep = "")
      grViz(string)
    })
    
    # When the radio button is changed, re-set counter and simulated values.
    observeEvent(input$demonstration_sum_or_mean, {
      coin_flip_number_labels(c())
      count_coin_flip_simulation_2(0)
    })
    
    # Simulate the coin flips, calculating the sum or mean depending on the user choices.
    coin_flip_number_labels = reactiveVal(c())
    count_coin_flip_simulation_2 = reactiveVal(0)
    observeEvent(input$simulate_coin_flip_2, {
      # For the first 10, we just add a new coin flip.
      if (count_coin_flip_simulation_2() < 10) {
        count_coin_flip_simulation_2(count_coin_flip_simulation_2() + 1)
        sample_draw = sample(c(1, 0), size = 5, replace = TRUE)
        # Find the sum or mean of the sample.
        sample_summary = sum(sample_draw)
        if (input$demonstration_sum_or_mean == "Mean") {
          sample_summary = mean(sample_draw)
        }
        result_str <- paste0(
          sample_summary, " (", 
          paste(sample_draw, collapse = " "), 
          ")"
        )
        coin_flip_number_labels(c(coin_flip_number_labels(), result_str))
        # For subsequent, we replaced samples that have already been displayed.
      } else {
        count_coin_flip_simulation_2(count_coin_flip_simulation_2() + 1)
        current_flip_int = count_coin_flip_simulation_2()
        current_flip_int = current_flip_int %% 10
        if (current_flip_int == 0) { # Because 10 %% 10 = 0
          current_flip_int = 10
        }
        sample_draw = sample(c(1, 0), size = 5, replace = TRUE)
        # Find the sum or mean of the sample.
        sample_summary = sum(sample_draw)
        if (input$demonstration_sum_or_mean == "Mean") {
          sample_summary = mean(sample_draw)
        }
        result_str <- paste0(
          sample_summary, " (", 
          paste(sample_draw, collapse = " "), 
          ")"
        )
        coin_flip_number_labels_new = coin_flip_number_labels()
        coin_flip_number_labels_new[current_flip_int] = result_str
        coin_flip_number_labels(coin_flip_number_labels_new)
      }
    })
    
    # Display the samples. Arranged in yellow circles in 5 row by 2 column arrangment.
    output$simulated_coin_flip_samples_numbers <- renderGrViz({
      
      if (count_coin_flip_simulation_2() == 0) {
        return(NULL)
      }
      
      n <- min(count_coin_flip_simulation_2(), 10)
      
      labels <- coin_flip_number_labels()
      
      num_rows <- ceiling(n / 2)
      
      table_rows <- ""
      for (i in 1:num_rows) {
        left_label <- labels[(i - 1) * 2 + 1]
        right_index <- (i - 1) * 2 + 2
        right_label <- if (right_index <= n) labels[right_index] else NULL
        
        # Left cell (always present)
        left_cell <- sprintf(
              "<TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' BGCOLOR='#f9ffbd' STYLE='rounded' BORDER='1' COLOR='black' CELLPADDING='4' ALIGN='CENTER' VALIGN='MIDDLE'>
           <FONT POINT-SIZE='12'>%s</FONT>
         </TD>", left_label)
        
        right_cell <- if (!is.null(right_label)) {
          sprintf(
              "<TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' BGCOLOR='#f9ffbd' STYLE='rounded' BORDER='1' COLOR='black' CELLPADDING='4' ALIGN='CENTER' VALIGN='MIDDLE'>
           <FONT POINT-SIZE='12'>%s</FONT>
         </TD>", right_label)
        } else {
          "<TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' CELLPADDING='4'></TD>"
        }
        
        row <- sprintf("<TR>%s%s</TR>", left_cell, right_cell)
        table_rows <- paste0(table_rows, row, "\n")
      }
      
      graph_string <- sprintf("
        digraph diagram {
          node [shape=plaintext]
          graph [layout=dot]
  
          tbl [label=<
            <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='10'>
              %s
            </TABLE>
          >]
        }
      ", table_rows)
        
      grViz(graph_string)
    })
    
    ###################### Choose your own box ###################### 
    
    ticket_numbers <- reactiveVal(c(1,0,0,0,0,0))
    invalid_tickets_string_bool <- reactiveVal(FALSE)
    number_of_ticket_draws <- reactiveVal(25)
    
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
      numeric_vec <- as.numeric(values)
      
      # Set the tickets in the box to default (1,0,0,0,0,0) if NA's present, or the string is empty.
      if (any(is.na(numeric_vec))) {
        invalid_tickets_string_bool(TRUE)
        ticket_numbers(c(1,0,0,0,0,0))
      } else if (length(numeric_vec) <= 1) {
        invalid_tickets_string_bool(TRUE)
        ticket_numbers(c(1,0,0,0,0,0))
      } else {
        ticket_numbers(numeric_vec)
        invalid_tickets_string_bool(FALSE)
      }
    })
    
    # Error message for when the text box for entering the tickets for the box is invalid
    output$tickets_text_error_message <- renderUI({
      if (invalid_tickets_string_bool()) {
        return(
          HTML("<span style='color: red;'><p>Error: One or move values that you added cannot be interpreted. Please carefully
               check what you entered. You must enter at least 2 valid tickets. Setting contents of the box to 1,0,0,0.</p></span>")
        )
      }
    })
    
    invalid_number_of_draws_bool = reactiveVal(FALSE)
    
    # Process number_of_draws value.
    observeEvent(input$number_of_draws, {
      if (!is.na(input$number_of_draws) && input$number_of_draws >= 1) {
        number_of_ticket_draws(ceiling(input$number_of_draws))
        invalid_number_of_draws_bool(FALSE)
      } else {
        invalid_number_of_draws_bool(TRUE)
        number_of_ticket_draws(25)
      }
    })
    
    # Error message for when the number of draws is invalid
    output$number_of_draws_error_message <- renderUI({
      if (invalid_number_of_draws_bool()) {
        return(
          HTML("<span style='color: red;'><p>Error: The value for the 'number of draws' must be an integer greater than or equal to 1.
               Setting value to 25 until the error is resolved.</p></span>")
        )
      }
    })
    
    output$box_model <- renderGrViz({
      
      your_own_model_number_labels(c())
      count_your_own_box_model(0)
      
      # Place the tickets into a string.
      # If greater than 15 tickets, split tickets onto new line.
      tickets_string = ""
      count = 0
      for (val in ticket_numbers()){
        count = count + 1
        if (count == 15) {
          tickets_string = paste(tickets_string, as.character(val), sep = "\n")
          count = 0
        } else {
          tickets_string = paste(tickets_string, as.character(val), sep = ", ")
        }
      }
      tickets_string <- substring(tickets_string,2)
      
      # Get other elements for the box model
      n = number_of_ticket_draws()
      sample = "Sample Sum"
      if (input$box_sum_or_mean == 2) {
        sample = "Sample Mean"
      }
      
      ##### Specify model ##### 
      # Set up graph and box
      diagram = "digraph diagram { graph [layout = dot, rankdir = TB] node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5] box [label = '"
      diagram = paste(diagram, tickets_string, "']", sep = "")
      
      # Set up sample circle.
      diagram = paste(diagram, " node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]sample [label = '", sample, "']", sep = "")
      
      # Create edge between box and circle.
      # Annotate edge with n value.
      diagram = paste(diagram, " edge [minlen = 2] box->sample [label = '  n = ", n, "', fontsize = 12, labeldistance = 5]}", sep = "")
      
      return (grViz(diagram))
    })
    
    your_own_model_number_labels = reactiveVal(c())
    count_your_own_box_model = reactiveVal(0)
    
    # When the radio button is changed, re-set counter and simulated values.
    observeEvent(input$box_sum_or_mean, {
      your_own_model_number_labels(c())
      count_your_own_box_model(0)
    })
    
    # Simulate the coin flips, calculating the sum or mean depending on the user choices.
    observeEvent(input$simulate_your_own_box_model_button, {
      # For the first 10, we just add a new coin flip.
      if (count_your_own_box_model() < 10) {
        count_your_own_box_model(count_your_own_box_model() + 1)
        sample_draw = sample(ticket_numbers(), size = number_of_ticket_draws(), replace = TRUE)
        # Find the sum or mean of the sample.
        sample_summary = sum(sample_draw)
        if (input$box_sum_or_mean == 2) {
          sample_summary = mean(sample_draw)
        }
        result_str <- sample_summary
        your_own_model_number_labels(c(your_own_model_number_labels(), result_str))
        # For subsequent, we replaced samples that have already been displayed.
      } else {
        count_your_own_box_model(count_your_own_box_model() + 1)
        current_flip_int = count_your_own_box_model()
        current_flip_int = current_flip_int %% 10
        if (current_flip_int == 0) { # Because 10 %% 10 = 0
          current_flip_int = 10
        }
        sample_draw = sample(ticket_numbers(), size = number_of_ticket_draws(), replace = TRUE)
        # Find the sum or mean of the sample.
        sample_summary = sum(sample_draw)
        if (input$box_sum_or_mean == 2) {
          sample_summary = mean(sample_draw)
        }
        result_str <- sample_summary
        labels_new = your_own_model_number_labels()
        labels_new[current_flip_int] = result_str
        your_own_model_number_labels(labels_new)
      }
    })
    
    # Display the sample means or sums for the user defined box. Arranged in yellow circles in 5 row by 2 column arrangement.
    output$simulated_your_own_box_model_values <- renderGrViz({
      
      if (count_your_own_box_model() == 0) {
        return(NULL)
      }
      
      n <- min(count_your_own_box_model(), 10)
      
      labels <- your_own_model_number_labels()
      
      num_rows <- ceiling(n / 2)
      
      table_rows <- ""
      for (i in 1:num_rows) {
        left_label <- labels[(i - 1) * 2 + 1]
        right_index <- (i - 1) * 2 + 2
        right_label <- if (right_index <= n) labels[right_index] else NULL
        
        # Left cell (always present)
        left_cell <- sprintf(
              "<TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' BGCOLOR='#f9ffbd' STYLE='rounded' BORDER='1' COLOR='black' CELLPADDING='4' ALIGN='CENTER' VALIGN='MIDDLE'>
           <FONT POINT-SIZE='12'>%s</FONT>
         </TD>", left_label)
          
        right_cell <- if (!is.null(right_label)) {
          sprintf(
              "<TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' BGCOLOR='#f9ffbd' STYLE='rounded' BORDER='1' COLOR='black' CELLPADDING='4' ALIGN='CENTER' VALIGN='MIDDLE'>
           <FONT POINT-SIZE='12'>%s</FONT>
         </TD>", right_label)
        } else {
          "<TD FIXEDSIZE='TRUE' WIDTH='120' HEIGHT='40' CELLPADDING='4'></TD>"
        }
        
        row <- sprintf("<TR>%s%s</TR>", left_cell, right_cell)
        table_rows <- paste0(table_rows, row, "\n")
      }
      
      graph_string <- sprintf("
        digraph diagram {
          node [shape=plaintext]
          graph [layout=dot]
  
          tbl [label=<
            <TABLE BORDER='0' CELLBORDER='0' CELLSPACING='10'>
              %s
            </TABLE>
          >]
        }
      ", table_rows)
      
      grViz(graph_string)
    })
    
  })
    
}