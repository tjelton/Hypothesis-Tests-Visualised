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
                     
                     HTML("<h1>The Box Model Part 1 - What is the Box Model?</h1>"),
                     
                     fluidRow(
                       column(8,
                              tight_card(
                                "What is the 'box model'?",
                                
                                HTML("<p>
                                The box model, popularised by Freedman, Pisani, and Purves (1978), is a conceptual tool used in statistics to <i>represent chance
                                processes involving random draws</i>. The 'box' contains <i>tickets</i>, each labeled with a <i>possible outcome of a single trial</i>. By 
                                imagining drawing tickets at random (with or without replacement), the model helps us simulate and understand the behavior of random processes.
                                <br><br>
                                
                                This might still seem quite abstract, so in this page, we will break down the box model through the lens of flipping a coin. We
                                know that if you flip one coin, that is, we did <i>one trial</i> of flipping a coin, the coin would either land on a 'head' or a 'tail'
                                (see the box part in the model to the right). Hence, inside of the box, we would place <i>two tickets, a 'head' ticket, and a 'tail' ticket</i>. 
                                You can think of the box as the <i>population of all possible outcomes of a single trial</i> (in this case, the trial is flipping a coin).
                                <br><br>
                                
                                <i>A sample is where we do more than one trial</i>. For example, if we were flipping a coin, you would flip it multiple times — say, 5 flips — and 
                                record the outcome each time. In terms of the box model, this is like drawing 5 tickets at random from the box (with replacement), where each 
                                ticket represents either a head or a tail.
                                <br><br>

                                Why use the box model? As we will seen soon, it helps us reason about random variation, expected values, and the role of chance in statistics.
                                In particular, when looking at different hypothesis tests in other pages of this site, where possible, they will be motivated through the lens of the box model. The goal with
                                this is to develop your conceptual understanding about the hypothesis tests we study. However, it is still possible to engage with most of the content
                                in this web app without understanding the box model!</p>"),
                                header_colour = "#3179ae"
                              ),
                       ),
                       column(4,
                              tight_card(
                                NULL,
                                HTML("<center>"),
                                grVizOutput("example_coin_flip_1", width = "85%", height = "80%"),
                                HTML("</center>")
                              )
                       )
                     ),
                     
                     HTML("<br><br><br>"),
                     
                     fluidRow(
                       
                       column(5,
                              tight_card(
                                "Simulating Box-Model Draws",
                                HTML("<p>For the example that we are using in this section, consider that our sample involes flipping a coin 5 times, and recording
                                     the number of heads and tails we observe. We could think of our sample as 1 experiment with 5 trials, that is 5 flips of a coin.
                                     <br><br>
                                     
                                     For example, in one experiment, we might find that after 5 coin flips, we see 3 heads and 2 tails (HHHTT). Perhaps in the next experiment, 
                                     we got really lucky and had 5 heads (HHHHH).
                                     <br><br>
                                     
                                     Instead of thinking about this as coin flips, another way of thinking about this is that wen have a cardboard box with two tickets in it, a 'H' ticket, and a
                                     'T' ticket. Then, the experiment is drawing a ticket from the box, recording whether we saw a 'H' or 'T', placing the ticket back into 
                                     the box, and then repeating the process until we have drawn from the box 5 times (assuming we are doing 5 trials).
                                     This is what we mean by the box model representing a chance process. Each experiment is itself the result of random draws from the box.
                                     <br><br>
                                     
                                     <span style='color: blue;'><b>Your turn!</b></span> In the section to the right, you can simulate taking different samples from the box model that we just
                                     described.</p>"),
                                header_colour = "#3179ae"
                              ),
                       ),
                       column(7,
                              primary_card(
                                "Demonstration",
                                HTML("<p><b><i>Press 'Simulate' to do 1 experiment of the box-model below.</i></b></p>"),
                                fluidRow(
                                  column(6,
                                         HTML("<center>"),
                                         HTML("<br>"),
                                         grVizOutput("example_coin_flip_2", width = "70%", height = "80%"),
                                         HTML("<br><br>"),
                                         actionButton(
                                           inputId = "simulate_coin_flip_1", label = HTML('<i class="fa fa-plus"></i>Simulate'),
                                           class = "btn btn-success", style="color: #fff;", width = "50%"
                                         ),
                                         HTML("<br><br><br>"),
                                         HTML("</center>"),
                                         
                                  ),
                                  column(6,
                                         grVizOutput("simulated_coin_flip_samples")
                                  )
                                ),
                                header_colour = "#3179ae"
                              )
                       )
                     ),
                     
                     HTML("<br><br><br>"),
                     
                     fluidRow(
                       
                       column(5,
                              tight_card(
                                "Representing the Problem Numerically",
                                HTML("<p>Using the same box model from before, let's consider that after our experiment, we observed the sample:</p>"),
                                fluidRow(
                                  column(3),
                                  column(6, grVizOutput("single_sample_words", height = "80%")),
                                  column(3)
                                ),
                                HTML("<p>
                                     An issue that we will run into is that 'H' and 'T' are not numbers. Ideally, we would like to summarise our sample using a single
                                     number, without having to specify the number of heads and tails individually.
                                     <br><br>
                                     
                                     Because of this, let’s instead change the tickets of our box to the numbers '1' and '0'. We can say that the number '1' represents
                                     drawing a head, and the number '0' represents drawing a tail. Under the same example as before, that means we drew 2 x '1' tickets
                                     and 3 x '0' tickets:
                                     </p>"),
                                fluidRow(
                                  column(3),
                                  column(6, grVizOutput("single_sample_numbers", height = "80%")),
                                  column(3)
                                ),
                                HTML("<p>
                                     Now that the tickets are numeric, we can model the sample using the sum or the mean:
                                     <ul>
                                       <li>Sum: 2 x 1 tickets + 3 x 0 tickets = 2 + 0 = 2</li>
                                       <li>Mean: (2 x 1 tickets + 3 x 0 tickets)/5 = (2 + 0)/5 = 0.4</li>
                                     </ul>
                                     In practice, it doesn’t matter whether we model the sample using the sum or mean, as long as we are consistent throughout.
                                     <br><br>
                                     
                                     <span style='color: blue;'><b>Your turn!</b></span> In the section to the right, you can simulate taking different samples from the 
                                     box model with numeric tickets. You can choose whether you summarise the samples using the sum or the mean. 
                                     </p>"),
                                header_colour = "#3179ae"
                              ),
                       ),
                       column(7,
                              primary_card(
                                "Demonstration",
                                fluidRow(
                                  column(8,
                                         HTML("<p><b><i>Press 'Simulate' to do 1 experiment of the box-model below.</i></b></p>"),
                                  ),
                                  column(4,
                                         radioButtons( 
                                           inputId = "demonstration_sum_or_mean", 
                                           label = "Model using sum or mean:", 
                                           choices = c("Sum", "Mean")
                                         ),
                                  )
                                ),

                                
                                fluidRow(
                                  column(6,
                                         HTML("<center>"),
                                         HTML("<br>"),
                                         grVizOutput("coin_flip_numeric_box_model", width = "70%", height = "80%"),
                                         HTML("<br><br>"),
                                         actionButton(
                                           inputId = "simulate_coin_flip_2", label = HTML('<i class="fa fa-plus"></i>Simulate'),
                                           class = "btn btn-success", style="color: #fff;", width = "50%"
                                         ),
                                         HTML("<br><br><br>"),
                                         HTML("</center>"),
                                         
                                  ),
                                  column(6,
                                         grVizOutput("simulated_coin_flip_samples_numbers")
                                  )
                                ),
                                header_colour = "#3179ae"
                              )
                       ),
                       
                     ),
                     
                     HTML("<br><br><br>"),
                     
                     # Make your own box.
                     fluidRow(
                       column(5,
                              tight_card(
                                "Testing Your Own Box",
                                HTML("<p>
                                     The example box model that we have been using so far was flipping a coin 5 times, but you can create any box model that you want!
                                     <br><br>
                                     
                                     For example, we might be interested in modelling the number of times a die lands on the '1' face after 20 rolls of a dice. If interested in that example,
                                     you would have one '1' ticket in the box, and five '0' tickets (to account for the other five sides on the die).
                                     <br><br>
                                     
                                     Below you can experiment with making your own box model, and then generating samples from it. This time, we will not show you the actual tickets that were drawn
                                     for each sample, rather the mean or sum of the tickets (depending on whether you are modelling the sample using the mean or sum).
                                     </p>"),
                                
                                accordion(
                                  open = FALSE,
                                  
                                  # Step 1: Enter tickets.
                                  accordion_panel(
                                    HTML("<b>Step 1) Tickets</b>"),
                                    HTML("<p>First, we need to specify the tickets that we will be adding to the box.</p>"),
                                    HTML("<p>In the text box below, enter the tickets that you wish to place into the box. Only <i>numbers</i> can be
                                     added, and <i>each ticket should be seperated by a comma</i>. For example, if you want to have 1x1 ticket
                                     and 6x0 tickets in the box, enter <i>1,0,0,0,0,0</i>."),
                                    textAreaInput(
                                      "box_tickets_text_entry",
                                      NULL,
                                      value = "1,0,0,0,0,0",
                                      width = "100%"
                                    ),
                                    fluidRow(
                                      column(8),
                                      column(4,
                                             actionButton("submit_tickets", "Set Tickets", class = "btn btn-success", style="color: #fff;", width = "100%")
                                      )
                                    ),
                                    uiOutput("tickets_text_error_message")
                                  ),
                                  
                                  # Step 2: Set n
                                  accordion_panel(
                                    HTML("<b>Step 2) Number of Draws</b>"),
                                    HTML("<p>Second, we need to specify the number of draws (with replacement) that we will be taking from the box:</p>"),
                                    numericInput(
                                      "number_of_draws",
                                      label = NULL,
                                      value = 20,
                                      min = 1
                                    ),
                                    uiOutput("number_of_draws_error_message")
                                  )
                                ),
                                header_colour = "#3179ae"
                              ),
                       ),
                       
                       column(7,
                              primary_card(
                                "Demonstration",
                                fluidRow(
                                  column(8,
                                         HTML("<p><b><i>Press 'Simulate' to do 1 experiment of the box-model below.</i></b></p>"),
                                  ),
                                  column(4,
                                         radioButtons( 
                                           inputId = "box_sum_or_mean", 
                                           label = "Model using sum or mean:", 
                                           choices = list(
                                             "Sum" = 1,
                                             "Mean" = 2
                                           )
                                         ),
                                  )
                                ),
                                
                                
                                fluidRow(
                                  column(6,
                                         HTML("<center>"),
                                         HTML("<br>"),
                                         grVizOutput("box_model", width = "70%", height = "70%"),
                                         HTML("<br><br>"),
                                         actionButton(
                                           inputId = "simulate_your_own_box_model_button", label = HTML('<i class="fa fa-plus"></i>Simulate'),
                                           class = "btn btn-success", style="color: #fff;", width = "50%"
                                         ),
                                         HTML("<br><br><br>"),
                                         HTML("</center>"),
                                         
                                  ),
                                  column(6,
                                         grVizOutput("simulated_your_own_box_model_values")
                                  )
                                ),
                                header_colour = "#3179ae"
                              )
                       )
                     ),
                     
                     HTML("<br><br><br>"),
                     
           ),
           
           nav_panel("The Box Model", 
                     boxModelMainUI("box_model"),
           ),
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
  
  
  boxModelMainServer(id = "box_model")
  oneSampleZTestServer(id = "1_sample_z_test")
  proportionTestMainServer(id = "proportion_z_test")
  tCurveMotivationServer(id = "t_curve_motivation")
  oneSampleTTestServer(id = "1_sample_t_test")
  pairedTTestServer(id = "paired_t_test")
  twoSampleTTestServer(id = "2_sample_t_test")
  regressionTTestServer(id = "regression_t_test")
  
  
}

shinyApp(ui, server)