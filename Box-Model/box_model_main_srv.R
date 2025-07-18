# We assume that if mean_or_sample_as_int = 1 then we are talking about sum, and mean_or_sample_as_int = 2 is mean.
simulate_box <- function(mean_or_sample_as_int, n, box) {
  value = sample(box, n, replace = TRUE)
  if (mean_or_sample_as_int == 2) {
    value = mean(value)
  } else {
    value = sum(value)
  }
  return(value)
}

boxModelMainServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    ticket_numbers <- reactiveVal(c(1,0,0,0))
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
      
      # Set the tickets in the box to default (1,0,0,0) if NA's present, or the string is empty.
      if (any(is.na(numeric_vec))) {
        invalid_tickets_string_bool(TRUE)
        ticket_numbers(c(1,0,0,0))
      } else if (length(numeric_vec) <= 1) {
        invalid_tickets_string_bool(TRUE)
        ticket_numbers(c(1,0,0,0))
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
    
    
    
    ########## Process empirical sample sums and means ########## 
    empirical_data <- reactiveVal(c())
    
    # Event: 1 repeat.
    observeEvent(input$repeat_1, {
      value = simulate_box(input$box_sum_or_mean, number_of_ticket_draws(), ticket_numbers())
      empirical_data(c(empirical_data(), value))
    })
    
    # Event: 10 repeats.
    observeEvent(input$repeat_10, {
      value = replicate(10, simulate_box(input$box_sum_or_mean, number_of_ticket_draws(), ticket_numbers()))
      empirical_data(c(empirical_data(), value))
    })
    
    # Event: 25 repeats.
    observeEvent(input$repeat_25, {
      value = replicate(25, simulate_box(input$box_sum_or_mean, number_of_ticket_draws(), ticket_numbers()))
      empirical_data(c(empirical_data(), value))
    })
    
    # Event: 100 repeats.
    observeEvent(input$repeat_100, {
      value = replicate(100, simulate_box(input$box_sum_or_mean, number_of_ticket_draws(), ticket_numbers()))
      empirical_data(c(empirical_data(), value))
    })
    
    # Event: reset histogram.
    observeEvent(input$reset_button, {
      empirical_data(c())
    })
    
    ################################################################
    
    ############################ Plots ############################# 
    
    output$box_model <- renderGrViz({
      
      # If there has been a change to the input values (and hence this graph has been re-generated), reset the empirical data.
      empirical_data(c())
      
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
      
      ### This is code that creates the box model.
      # digraph diagram {
      #   graph [layout = dot, rankdir = TB]
      #   
      #   node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5]
      #     box [label = '1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0\n1,0,0,0,0,0,0,0,0,0,0,0,0']
      #     
      #     node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]  
      #     sample [label = '?']
      #     
      #     edge [minlen = 2]
      #       box->sample [label = '  n = 10', fontsize = 12, labeldistance = 5]
      #   }
      
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
    
    # Histogram of mean and sum frequencies.
    output$histogram_frequencies = renderPlot({
      
      values <- empirical_data()
      num_values = as.character(length(values))
      
      title_string = paste("Empiricial Distribution of Sample Sums (n = ", num_values, ")", sep = "")
      x_axis_string = "Sample Sum Value"
      if (input$box_sum_or_mean == 2) {
        title_string = paste("Empiricial Distribution of Sample Means (n = ", num_values, ")", sep = "")
        x_axis_string = "Sample Mean Value"
      }
      
      # Main histogram
      if (length(values) > 0) {
        bins_to_include <- length(unique(values))
        
        # If over 20 bins, just set to 20 (otherwise too many bins)
        if (bins_to_include > 20) bins_to_include <- 20
        
        hist(
          values,
          breaks = bins_to_include,
          freq = FALSE,
          col = "lightgreen",
          border = "black",
          xlab = x_axis_string,
          ylab = "Density",
          main = title_string,
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
  
    # Histogram with normal curve to shown normal curve approximation.
    output$normal_curve_model = renderPlot({
      
      x_axis_string = "Sample Sum Value"
      title_string = "Empirical Distribution of 10000 Sample Sums with\nOverlaid Normal Curve"
      if (input$box_sum_or_mean == 2) {
        title_string = "Empirical Distribution of 10000 Sample Means with\nOverlaid Normal Curve"
        x_axis_string = "Sample Mean Value"
      }
      
      # Get 1000 samples for the histogram.
      data = replicate(10000, simulate_box(input$box_sum_or_mean, number_of_ticket_draws(), ticket_numbers()))
      data = data.frame(values = data)
      
      # Normal curve parameters.
      EV = number_of_ticket_draws() * mean(ticket_numbers())
      SE = sqrt(number_of_ticket_draws()) * popsd(ticket_numbers())
      if (input$box_sum_or_mean == 2) { 
        EV = mean(ticket_numbers())
        SE = popsd(ticket_numbers())/sqrt(number_of_ticket_draws())
      }
      
      bins_to_include = length(table(data)) * 1.3
      if (bins_to_include >= 50) {
        bins_to_include = 50
      }

      hist(
        data$values,
        breaks = bins_to_include,
        freq = FALSE,
        col = "lightgreen",
        border = "black",
        xlab = x_axis_string,
        ylab = "Density",
        main = title_string,
      )
      
      curve(
        dnorm(x, mean = EV, sd = SE),  # normal density with your mean (EV) and sd (SE)
        from = min(data$values),        # start of curve range
        to = max(data$values),          # end of curve range
        add = TRUE,                     # overlay on existing plot
        col = "red",
        lwd = 2                        # line width
      )
    })
    
    # Histogram with normal curve to shown normal curve approximation.
    output$shaded_normal_curve = renderPlot({
      
      # If the lower threshold is greater than the upper threshold, return early.
      if (error_message_interval_flag()) {
        return()
      }
      
      req(EV_string())
      req(SE_string())
      EV_val = as.numeric(EV_string())
      SE_val = as.numeric(SE_string())
      lower_value = lower_boundary_num_store()
      upper_value = upper_boundary_num_store()
      
      # Define the limits for shading
      lower_xlimit_plot <- EV_val - SE_val * 4
      upper_xlimit_plot <- EV_val + SE_val * 4
      
      # When we have infinity values, lower and upper_values set to NA. Set to the lower and upper xlimits.
      if (is.na(lower_value)) {
        lower_value = lower_xlimit_plot
      }
      if (is.na(upper_value)) {
        upper_value = upper_xlimit_plot
      }
      
      # Reset the upper and lower value when they exceed the xlimits.
      if (upper_value > upper_xlimit_plot) {
        upper_value = upper_xlimit_plot
      } else if (lower_value < lower_xlimit_plot) {
        lower_value = lower_xlimit_plot
      }
      
      # Set tight margins: bottom, left, top, right
      par(mar = c(3, 2, 1, 1), xaxs = "i", yaxs = "i")  # Fill space tightly
      
      # Generate x and y values for the normal curve
      x_range <- seq(EV_val - 4 * SE_val, EV_val + 4 * SE_val, length.out = 1000)
      y_values <- dnorm(x_range, mean = EV_val, sd = SE_val)
      
      # Tick positions and labels
      x_ticks <- EV_val + SE_val * (-3:3)
      x_labels <- format(round(x_ticks, 2), nsmall = 2)
      
      # Start empty plot
      plot(x_range, y_values, type = "n",
           axes = FALSE, xlab = "", ylab = "", main = "")
      
      # Shaded region
      shade_x <- seq(lower_value, upper_value, length.out = 1000)
      shade_y <- dnorm(shade_x, mean = EV_val, sd = SE_val)
      
      polygon(c(shade_x, rev(shade_x)),
              c(rep(0, length(shade_y)), rev(shade_y)),
              col = rgb(1, 0, 0, 0.5), border = NA)
      
      lines(x_range, y_values, col = "black", lwd = 2)
      
      # Custom x-axis
      axis(1, at = x_ticks, labels = x_labels, cex.axis = 0.9)
    })
    
    ################################################################
    
    # Text telling the user the boxes mean and popsd.
    output$box_statistics <- renderUI({
      
      mean_ = mean(ticket_numbers())
      sd_ = popsd(ticket_numbers())
      
      mean_statment = HTML(paste("<p><b>Mean of the box:</b> $$ \\mu =  ", as.character(round(mean_, digits = 5)), "$$</p>", sep = ""))
      sd_statment = HTML(paste("<p><b>Population SD of the box: </b> $$ \\sigma = ", as.character(round(sd_, digits = 5)), "$$</p>", sep = ""))
      
      return(tagList(
        withMathJax(
          mean_statment, sd_statment
        )
      ))
    })
    
    # Text instructions for the central limit theorem section
    output$CLT_text_instructions_output <- renderUI({
      sample = "sum"
      if (input$box_sum_or_mean == 2) {
        sample = "mean"
      }
      
      string = paste("<p>Recall that the central limit theorem tells us that if we take a <b>sufficiently large number of draws</b> 
                     from the box, then the <b>sample ", sample, "s will follow an approximately normal distribution</b>.<br><br>Now we will empirically
                     test whether n = ", number_of_ticket_draws(), " is a sufficient number of draws for the central limit theorem to
                     apply.<br><br>To do this, press the button below to repeat the process of drawing from the box ", number_of_ticket_draws(),
                     " and finding the ", sample, ". These will be added to the histogram. If we have taken enough draws from the box, then the 
                     histogram should look normally distributed.", sep = "")
      
      return(HTML(string))
    })
    
    EV_string <- reactiveVal("")
    SE_string <- reactiveVal("")
    
    # Text instructions for whether the CLT applies to this box model.
    output$CLT_satisfied_text <- renderUI({
      sample = "sum"
      if (input$box_sum_or_mean == 2) {
        sample = "mean"
      }
      
      string = paste("<p>Does the data in the histogram above look normally distributed? Ensure that you have repeated the process of
                     drawing from the box, and finding the sample ", sample, "many times. If it does not, scroll back above and update the
                     number of draws in step 2. If it does, continue below!</p>", sep = "")
      
      return(HTML(string))
    })
    
    # Text instructions for whether the CLT applies to this box model.
    output$normal_distribution_text <- renderUI({
      sample = "sum"
      if (input$box_sum_or_mean == 2) {
        sample = "mean"
      }
      
      instructions = HTML(paste("<p>Now that we have confirmed that we are taking a sufficient number of draws for the sample ", sample, "s to follow a
                     normal distribution, we now want to specify this general normal curve. We will set the mean to
                     be equal to the the <b>sample ", sample, "'s</b> expected value, and the standard deviation equal to its standard error:</p>", sep = ""))
      
      # EV and SE text (changes based upon whether the sample sum or mean is being used).
      expected_value = ""
      standard_error = ""
      EV = ""
      SE = ""
      
      # Mean
      if (input$box_sum_or_mean == 2) { 
        EV = as.character(round(mean(ticket_numbers()), 5))
        EV_string(EV)
        expected_value = withMathJax(
          HTML("<p><b>Expected Value:</b></p>"),
          HTML(paste("$$\\begin{align*} \\text{EV} &= \\mu \\\\ &=", EV, "\\end{align*}$$", sep = ""))
        )
        
        SE = as.character(round(popsd(ticket_numbers())/sqrt(number_of_ticket_draws()),5))
        SE_string(SE)
        standard_error = withMathJax(
          HTML("<p><b>Standard Error:</b></p>"),
          HTML(paste("$$\\begin{align*} \\text{SE} &= \\frac{\\sigma}{\\sqrt{n}} \\\\ &= \\frac{", round(popsd(ticket_numbers()), 5) , "}{\\sqrt{", 
                     as.character(number_of_ticket_draws()), "}}\\\\ &= ", SE, "\\end{align*}$$", sep = ""))
        )
        
        # Sum
      } else {
        EV = as.character(round(number_of_ticket_draws() * mean(ticket_numbers()), 5))
        EV_string(EV)
        expected_value = withMathJax(
          HTML("<p><b>Expected Value:</b></p>"),
          HTML(paste("$$\\begin{align*} \\text{EV} &= n \\times \\mu \\\\ &=", as.character(number_of_ticket_draws()), "\\times", round(mean(ticket_numbers()), 5),
                     "\\\\ &= ", EV, "\\end{align*}$$", sep = ""))
        )
        
        SE = as.character(round(sqrt(number_of_ticket_draws()) * popsd(ticket_numbers()),5))
        SE_string(SE)
        standard_error = withMathJax(
          HTML("<p><b>Standard Error:</b></p>"),
          HTML(paste("$$\\begin{align*} \\text{SE} &= \\sqrt{n} \\times \\sigma \\\\ &= \\sqrt{", as.character(number_of_ticket_draws()), "} \\times", 
                     round(popsd(ticket_numbers()), 5), "\\\\ &= ", SE, "\\end{align*}$$", sep = ""))
        )
      }
      
      
      normal_curve_text = HTML(paste("<p>Having found the expected value and standard error, we can model the distribution of the sample ", sample, "s using the
                                following <b>general normal curve:</b></p>", sep = ""))
      
      noraml_curve = withMathJax(
        paste("$$\\begin{align*} \\text{Sample Sum} &\\sim N(\\text{EV}, \\text{SE}^2) \\\\ &= N(", EV, ", ", SE, "^2) \\end{align*}$$", sep = "")
      )
      
      return(
        tagList(
          instructions, expected_value, standard_error, normal_curve_text, noraml_curve
        )
      )
    })
    
    # Text instructions for the finding probabilities section.
    output$finding_probabilities_text <- renderUI({
      
      sample = "sum"
      if (input$box_sum_or_mean == 2) {
        sample = "mean"
      }
      
      instructions = paste("<p>Now that we are modelling the sample ", sample , "s using a normal curve with mean ", EV_string(), " and standard deviation ",
                           SE_string(), " we can start to ask probability based questions like, <br>",
                           "<ul>
                              <li>What is the chance that we see a value greater than", withMathJax("\\(x\\)"), "?</li>",
                           "<li>What is the chance that we see a value between", withMathJax("\\(y\\)"), " and ", withMathJax("\\(z\\)"), "?</li>",
                           "</ul>",
                           "Use the controls below to find the the probabilities that values lie within the ranges you set.</p>")
      return(
        tagList(
          HTML(instructions)
        )
      ) 
    })
    
    error_message_interval_flag <- reactiveVal(FALSE)
    lower_boundary_num_store <- reactiveVal(NA)
    upper_boundary_num_store <- reactiveVal(NA)
    
    # Probability for values within a range text.
    output$probability_answer_text <- renderUI({
      
      lower_boundary_str = ""
      lower_boundary_num = NA 
      if (input$lower_boundary_infinity == TRUE || is.na(input$lower_boundary_numeric)) {
        lower_boundary_str = "-\\infty"
        lower_boundary_num_store(NA)
      } else {
        lower_boundary_str = as.character(input$lower_boundary_numeric)
        lower_boundary_num = input$lower_boundary_numeric
        lower_boundary_num_store(lower_boundary_num)
      }
      
      upper_boundary_str = ""
      upper_boundary_num = NA 
      if (input$upper_boundary_infinity == TRUE || is.na(input$upper_boundary_numeric)) {
        upper_boundary_str = "\\infty"
        upper_boundary_num_store(NA)
      } else {
        upper_boundary_str = as.character(input$upper_boundary_numeric)
        upper_boundary_num = input$upper_boundary_numeric
        upper_boundary_num_store(upper_boundary_num)
      }
      
      # Check that the lower boundary num is not higher than the upper boundary num.
      if (!is.na(lower_boundary_num) && !is.na(upper_boundary_num)) {
        if (lower_boundary_num > upper_boundary_num) {
          error_message_interval_flag(TRUE)
          return()
        } else {
          error_message_interval_flag(FALSE)
        }
      } else {
        error_message_interval_flag(FALSE)
      }
      
      # Find area between the lower and upper boundary.
      area = 1
      # This is the case where we are finding the area from -infty to +infty. This area is simply 1.
      EV_num = as.numeric(EV_string())
      SE_num = as.numeric(SE_string())
      # [-inf, inf]
      if (is.na(lower_boundary_num) && is.na(upper_boundary_num)) {
        area = 1
        # [-inf, x]
      } else if (is.na(lower_boundary_num)) {
        area = pnorm(upper_boundary_num, mean = EV_num, sd = SE_num, lower.tail = TRUE)
        # [x, inf]
      } else if (is.na(upper_boundary_num)) {
        area = pnorm(lower_boundary_num, mean = EV_num, sd = SE_num, lower.tail = FALSE)
        # [x, y]
      } else {
        area  = pnorm(upper_boundary_num, mean = EV_num, sd = SE_num) - pnorm(lower_boundary_num, mean = EV_num, sd = SE_num)
      }
      area = as.character(round(area, digits = 5))
      
      text = paste("<p>The probability that a value lies wthin the range \\([", lower_boundary_str, ",", upper_boundary_str, "]\\) is ", 
                   area, ".</p>", sep = "")
      
      return(
        tagList(
          withMathJax(HTML(text))
        )
      )
    })
    
    # Error message to display if the lower interval is greater than the upper interval.
    output$interval_error_message <- renderUI({
      if (error_message_interval_flag() == TRUE) {
        return(HTML("<p style='color: red;'>ERROR: The lower interval cannot be greater than the upper interval.</p>"))
      }
    })
    
    ############################ Modal Intro ############################# 
    
    
    observeEvent(input$learning_text, {
      showModal(modalDialog(
        title = "The 'Box Model'",
        
        HTML("<p>
            Let’s say that you flip a fair coin 50 times. You would expect to get 25 heads and 25 tails. But what if when you did this experiment, 
            you actually found out that you got 27 heads and 23 tails? Would this be considered unusual?<br><br>
            
            The box model can help us investigate questions like this. But first, what is the box model?<br><br>
            
            
      </p>"),
        fluidRow(
          column(8,
                 HTML("<p>
               <h5><u>What is the 'box model'?</u></h5><br>
            
                The box model is a construct used in statistics to help understand chance processes. As the name suggests, the main feature of the model
                is the box component. Inside this box, we place tickets, which represent the possible outcomes that could occur from a single event.<br><br>
                
                In the case of flipping a coin, the possible outcomes are “head” or “tail.” Because of this, inside the box, we would place two tickets, 
                one with the word “head” and the other with the word “tail.” Below the box is a flow line connected to an oval. Next to the flow line, we 
                indicate the number of draws we are taking from the box (with replacement). In our example, we are flipping the coin 50 times, so we write 
                n = 50. The oval represents the sample. For our example, we can think of this as a store of how many tails and heads we observed when drawing 
                from the box.<br><br>
          </p>")
          ),
          column(4,
                 HTML("<br>"),
                 grVizOutput(ns("example_coin_flip_1"), width = "80%", height = "80%"),
          )
        ),
        fluidRow(
          
          column(8,
                 HTML("<p>
               <h5><u>Representing the Sample – Sum or Mean</u></h5><br>
            
                  For our coin-flipping example, let’s imagine that we flipped the coin 50 times. Using our box model analogy, this means that we randomly 
                  went to the box, picked out a ticket, recorded the value of the ticket, replaced the ticket back into the box, and repeated this process 
                  another 49 times.<br><br>
                  
                  Let’s consider that at the end of doing this, we had 27 “head”s and 23 “tail”s. Under the box model, this represents our sample, which 
                  we represent by the oval.An issue that we will run into is that the words “head” and “tail” are not numbers. Ideally, we would like to 
                  summarise our sample using a <b>single number</b>, without having to specify the number of heads and tails individually.<br><br>
                  
                  Because of this, let’s instead change the tickets of our box to the numbers “1” and “0”. We can say that the number “1” represents drawing
                  a head, and the number “0” represents drawing a tail. Under the same example as before, this means that we drew 27 x “1” tickets and 23 x “0”
                  tickets. Now that the tickets are numeric, we can <b>model the sample</b> using the <b>sum</b> or the <b>mean</b>:
                  
                  <ul>
                    <li>Sum: 27 x 1 tickets + 23 x 0 tickets = 27 + 0 = 27</li>
                    <li>Mean: (27 x 1 tickets + 23 x 0 tickets)/50 = (27 + 0)/50 = 0.54</li>
                  </ul>
                  
                  In practice, it doesn’t matter whether we model the sample using the sum or mean, as long as we are consistent throughout. For the rest of 
                  the discussion here about this coin-flipping example, we will assume that we are modelling the sample using the sum.<br><br>
                </p>")
          ),
          column(4,
                 HTML("<br><br><br><br><br><br>"),
                 grVizOutput(ns("example_coin_flip_2"), width = "80%", height = "80%"),
          ),
        ),
        HTML("<p>
            <h5><u>Modelling Using a Normal Distribution</u></h5><br>
            
            The central limit theorem tells us that if we take a sufficiently large number of draws from a box, then the sample sums (or means) will
            follow a normal distribution. The key word here is “sufficiently larger”. This means that our number of draws (n) must be a relatively high
            number.<br><br>
            
            How high? This depends on the tickets in the box, with emphasis placed on how symmetric they are. Some textbooks will say that if you have 
            greater than 35 draws, then the central limit theorem will hold, but this is not always the case. One way that we can empirically verify 
            whether the central limit theorem holds or not is to simulate taking many samples from the box.<br><br>
            
            For the coin-flipping example, this means that we do the process of drawing 50 times from the box and calculating the sample sum many, many
            times. As we are taking a sufficient number of draws in this example, you would see that the histograms of the sample sums will look normally 
            distributed. You will have an opportunity to do that in this app (for this box model, or another of your choosing), and to see whether the 
            histogram of the sample sums looks normally distributed or not.<br><br>
            
            If the distribution appears to be normally distributed, then we can model the sample sums (or means) using a normal distribution. To do this, 
            we will calculate the expected value (EV) and standard error (SE) which both depend on whether we are modelling using the sample sum or mean, 
            as well as the number of draws and the contents of the box.<br><br>
            
            The normal curve will be defined to have a mean equal to EV, and a standard deviation equal to SE. Using these values, you can start to ask
            probability-style questions. Under our coin-flipping example, we could ask - what is the probability of observing a sample sum of 30 or greater
            (that is, seeing 30 or more heads from 50 flips)?<br><br><br>
            
            <i>Now it’s your turn to experiment with the app below. This was a very brief introduction, but hopefully, by playing and experimenting below, 
            you’ll gain a deeper conceptual understanding of the box model.</i>
      </p>"),
        easyClose = TRUE,
        footer = modalButton("Close"),
      ))
    })
    
    output$example_coin_flip_1 <- renderGrViz({
      string = "digraph diagram {
        graph [layout = dot, rankdir = TB]
      
        node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5]
        box [label = 'Head, Tail']
      
        node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]
        sample [label = 'Sample']
      
        edge [minlen = 2]
          box->sample [label = '  n = 50', fontsize = 12, labeldistance = 5]
        }"
      return(grViz(string))
    })
    
    output$example_coin_flip_2 <- renderGrViz({
      string = "digraph diagram {
        graph [layout = dot, rankdir = TB]
      
        node [shape = box, style = filled, fillcolor = \"#bdfeff\", fontsize = 12, width = 2.5]
        box [label = '1, 0']
      
        node [shape = oval,width = 1.5,fillcolor = \"#f9ffbd\", fontsize = 12]
        sample [label = 'Sample Sum']
      
        edge [minlen = 2]
          box->sample [label = '  n = 50', fontsize = 12, labeldistance = 5]
        }"
      return(grViz(string))
    })
    
    ################################################################
    
  })
  
}