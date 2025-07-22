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
                     boxModelPart1UI("box_model_part_1")
           ),
           
           nav_panel("Box Model - Part 2", 
                     
                     HTML("<h1>The Box Model Part 2 - Central Limit Theorem</h1>"),
                     
                     fluidRow(
                       
                       column(5,
                              tight_card(
                                "Simulating Sample Means",
                                HTML("<p>Consider that we have a box model with one '1' ticket and three '0' tickets. Each sample consists of 25 draws from the box, and we
                                         represent the sample using the mean of the tickets.</p>"),
                                HTML("<center>"),
                                grVizOutput("box_model_n_25", width = "50%", height = "50%"),
                                HTML("</center><br>"),
                                HTML("<p>For example, imagine that from one experiment, we drew 10 '1' tickets and 15 '0' tickets. The sample mean would then be:</p>"),
                                withMathJax(HTML(
                                  "$$\\frac{10 \\times 1 + 15 \\times 0}{25} = \\frac{10}{25} = 0.4$$"
                                )),
                                HTML("<p>
                                   But this example is just one experiment. We could repeat the experiment, and this time get 14 '1' tickets and 11 '0' tickets, giving a 
                                   mean of 0.56. Maybe next time we get 8 '1' ticekts and 17 '0' tickets, giving a mean of 0.32.
                                   <br></p>"),
                                HTML("<p><b>Task 1:</b> To the right, repeat the experiments over and over again. Every time you do a
                                     new experiment, its sample mean will be added to the histogram of the means observed so far. What shape do we see when considering
                                     the distribution of the sample means?</p>"),
                                accordion(
                                  open = FALSE,
                                  accordion_panel(
                                    HTML("<p><b>Task 1 Answer</b></p>"),
                                    HTML("<p>We see that the distribution of the sample means forms an approximate normal distribution.</p>")
                                  )
                                ),
                                header_colour = "#3179ae"
                              ),
                       ),
                       column(7,
                              primary_card(
                                "Demonstration",
                                HTML("<br>"),
                                fluidRow(
                                  column(8,
                                         fluidRow(
                                             column(1),
                                             
                                             # Button for repeating adding the mean or sample sum to the histogram.
                                             column(5,
                                                    actionButton(
                                                      inputId = "repeat_1_n_25", label = HTML('<p>Repeat 1</p>'),
                                                      class = "btn btn-primary", style="color: #fff;", width = "100%"
                                                    ),
                                                    HTML("<br><br>"),
                                                    actionButton(
                                                      inputId = "repeat_25_n_25", label = HTML('<p>Repeat 25</p>'),
                                                      class = "btn btn-primary", style="color: #fff;", width = "100%"
                                                    ),
                                             ),
                                             column(5,
                                                    actionButton(
                                                      inputId = "repeat_10_n_25", label = HTML('<p>Repeat 10</p>'),
                                                      class = "btn btn-primary", style="color: #fff;", width = "100%"
                                                    ),
                                                    HTML("<br><br>"),
                                                    actionButton(
                                                      inputId = "repeat_100_n_25", label = HTML('<p>Repeat 100</p>'),
                                                      class = "btn btn-primary", style="color: #fff;", width = "100%"
                                                    ),
                                             ),
                                         )
                                  ),
                                  column(4,
                                         HTML("<br><center>"),
                                         
                                         # On click, resets the histogram
                                         actionButton(
                                           inputId = "reset_button_n_25", label = HTML('<p>Reset</p>'),
                                           class = "btn btn-danger", style="color: #fff;", width = "80%"
                                         ),
                                         HTML("</center><br>"),
                                  )
                                ),
                                HTML("<br><br>"),
                                HTML("<center>"),
                                plotOutput("histogram_frequencies_n_25", width = "100%", height = "450px"),
                                HTML("</center>"),
                                
                                header_colour = "#3179ae"
                              )
                       ),
                       
                     ),
                     
                     HTML("<br><br><br>"),
                     
                     fluidRow(
                       
                       column(5,
                              tight_card(
                                "When the Draws are Reduced",
                                HTML("<p>Now, let's repeat the task that we did before (repeatedly forming samples) for a new box model that is mostly identical to the one
                                  before, except now, each sample consists of only 5 draws from the box.</p>"),
                                HTML("<center>"),
                                grVizOutput("box_model_n_5", width = "50%", height = "50%"),
                                HTML("</center><br>"),
                                HTML("<p><b>Task 2:</b> Repeat what we did before, but for when we only take 5 draws from the box.
                                     Do we still see a normal distribution?</p>"),
                                accordion(
                                  open = FALSE,
                                  accordion_panel(
                                    HTML("<p><b>Task 2 Answer</b></p>"),
                                    HTML("<p>We see that the distribution of the sample means is no longer normally distributed (there is a tail on the right). Additionally,
                                            we see discrete jumps (for n = 5, it is only possible to have a mean of 0, 0.2, 0.4, 0.6, 0.8, 1), further detracting from the 
                                            normal distribution shape we saw before.</p>")
                                  )
                                ),
                                header_colour = "#3179ae"
                              )
                       ),
                       column(7,
                              primary_card(
                                "Demonstration",
                                HTML("<br>"),
                                fluidRow(
                                  column(8,
                                         fluidRow(
                                           column(1),
                                           
                                           # Button for repeating adding the mean or sample sum to the histogram.
                                           column(5,
                                                  actionButton(
                                                    inputId = "repeat_1_n_5", label = HTML('<p>Repeat 1</p>'),
                                                    class = "btn btn-primary", style="color: #fff;", width = "100%"
                                                  ),
                                                  HTML("<br><br>"),
                                                  actionButton(
                                                    inputId = "repeat_25_n_5", label = HTML('<p>Repeat 25</p>'),
                                                    class = "btn btn-primary", style="color: #fff;", width = "100%"
                                                  ),
                                           ),
                                           column(5,
                                                  actionButton(
                                                    inputId = "repeat_10_n_5", label = HTML('<p>Repeat 10</p>'),
                                                    class = "btn btn-primary", style="color: #fff;", width = "100%"
                                                  ),
                                                  HTML("<br><br>"),
                                                  actionButton(
                                                    inputId = "repeat_100_n_5", label = HTML('<p>Repeat 100</p>'),
                                                    class = "btn btn-primary", style="color: #fff;", width = "100%"
                                                  ),
                                           ),
                                         )
                                  ),
                                  column(4,
                                         HTML("<br><center>"),
                                         
                                         # On click, resets the histogram
                                         actionButton(
                                           inputId = "reset_button_n_5", label = HTML('<p>Reset</p>'),
                                           class = "btn btn-danger", style="color: #fff;", width = "80%"
                                         ),
                                         HTML("</center><br>"),
                                  )
                                ),
                                HTML("<br><br>"),
                                HTML("<center>"),
                                plotOutput("histogram_frequencies_n_5", width = "100%", height = "450px"),
                                HTML("</center>"),
                                
                                header_colour = "#3179ae"
                              )
                       ),
                       
                     ),
                     
                     HTML("<br><br><br>"),
                     
                     fluidRow(
                       column(12,
                              tight_card(
                                "Introducing the Central Limit Theorem (CLT)",
                                HTML("<p>
                                
                                <span style='color: blue;'><b>What is the the central limit theorem?</b></span> The Central Limit Theorem (CLT) states that if you take <b>many 
                                random samples</b> of the same size from any population, the <b>distribution of the sample means (or sums) will tend to look like a normal 
                                distribution</b> as the sample size gets larger — even if the original population is not normally distributed.
                                <br><br>
                                
                                When thinking about this though the lens of the box model, the box represents our population. So, when applying the central limit theorem to the
                                box model, we can say that if we take a sufficiently large number of draws from a box, then the sample sums (or means) will follow a normal 
                                distribution.
                                <br><br>
                                
                                The key words here are <b>sufficiently larger</b>. This means that our number of draws (n) must be a relatively high number. How high? This depends
                                on the tickets in the box.
                                <br><br>
                                
                                For example, before we had two box models with the tickets '1', '0', '0', '0', with the only difference being that one box model had samples formed
                                from 25 draws from the box (i.e. n = 25), with the other formed from 5 draws from the box (i.e. n = 5). From the demonstration, we saw that when
                                we kept repeating the experiment of drawing from the box 25 times and taking the mean, the distribution of the means appeared normally distributed.
                                Hence, 25 draws from the box <b>was sufficiently</b> large enough for the sample means to be normally distributed. However, when we kept repeating 
                                the experiment of drawing from the box 5 times and taking the mean, the distribution wan not normally distributed. Hence, 5 draws from the box
                                <b>was not sufficiently</b> large enough for the sample means to be normally distributed. In this example, we could have also calculated the sample
                                sums instead. We would have seen the same looking sample distributions for the n = 25 and n = 5 box models.
                                <br><br>
                                
                                You may be wondering if there is some set number of draws (i.e. some threshold value for n) that you can use to say that the CLT always applies. 
                                Some people may say that if you have greater than 35 draws, then the CLT will hold, but this is not always the case. You should always look at your
                                underlying data. If the tickets in the box model are symmetric, and are already almost normally distributed, then you will not need very many draws
                                for the CLT to apply. However, if the box were very skewed (imagine a box with one '1' ticket and ninety '0' tickets), they you will likely
                                need many more than 35 draws for the CLT to apply.
                                <br><br>
                                
                                In the activity below, you have the opporunity to specify your own box (as well as trial some boxes that we specified), to investigate how many
                                draws are needed for the sample means (or sums) to be normally distributed.
                                </p>"),
                                header_colour = "#3179ae"
                              )
                       ),
                     ),
                     
                     HTML("<br><br><br>"),
                     
                     # Make your own box.
                     fluidRow(
                       column(5,
                              tight_card(
                                "Different Number of Draws",
                                HTML("<p>
                                     In this demonstration, you will be changing the contents of the box (that is, the tickets in the box). From this box, we will find
                                     samples of size n = 5, 25, 50 and 100. For each of these sample sizes, you will generate 100,000 samples by pressing the 'Sample' button 
                                     below (it may take a few seconds for the samples to be created). You will then look at the different histograms for each sample size to
                                     roughly gauge at what point our sample size was large enough for the central limit theorem to apply.
                                     <br><br>
                                     
                                     You are free to set your own tickets to place in the box, but we recommend you use the practice boxes first, as there is some text
                                     which describes what we see to help guide you.
                                     <br><br>
                                     
                                     <i>Note: Due to how the bin widths are chosen in this exercise, sometimes the distribution of the means and the sums do not match up.
                                     For example, for the example 1 box, it appears that the CLT applies for the sample sums for n = 25, but not for the mean when n = 25.
                                     You should not read into this. The sample sum histograms do seem to better represent the data. In real life, you would play around with
                                     the histogram bin widths until you feel they appropriately represent the data.</i>
                                     <br><br>

                                     <b>Current box:</b>
                                     </p>"
                                ),
                                HTML("<center>"),
                                grVizOutput("custon_box_model", width = "50%", height = "50%"),
                                HTML("</center>"),
                                HTML("<br>"),
                                HTML("<p><b>Box contents:</b></p>"),
                                radioButtons(
                                  "custom_box_model_example_choice",
                                  NULL,
                                  choices = c("Example 1 (Coin Flip)" = 1,
                                              "Example 2 (1 in 4)" = 2,
                                              "Example 3 (Dice)" = 3,
                                              "Example 4 (Many Tickets)" = 4,
                                              "Example 5 (Very Imbalanced)" = 5,
                                              "Choose Your Own!" = 6)
                                ),
                                uiOutput("custom_box_model_input_button"),
                                header_colour = "#3179ae"
                              ),
                       ),
                       
                       column(7,
                              primary_card(
                                "Demonstration",
                                HTML("<br>"),
                                fluidRow(
                                  column(6, 
                                         HTML("<center>"),
                                         actionButton(
                                           inputId = "simulate", label = HTML('<p>Simulate</p>'),
                                           class = "btn btn-primary", style="color: #fff;", width = "40%"
                                         ),
                                         HTML("</center>")
                                  ),
                                  column(6,
                                         HTML("<center>"),
                                         radioButtons(
                                           "box_sum_or_mean",
                                           label = NULL,
                                           choices = list(
                                             "Sum" = 1,
                                             "Mean" = 2
                                           )
                                         ),
                                         HTML("</center>")
                                        )
                                ),
                                
                                fluidRow(
                                  column(6,
                                         HTML("<center>"),
                                         HTML("<br>"),
                                         plotOutput("histogram_frequencies_custom_n_5"),
                                         plotOutput("histogram_frequencies_custom_n_50"),
                                         HTML("</center>")
                                  ),
                                  column(6,
                                         HTML("<center>"),
                                         HTML("<br>"),
                                         plotOutput("histogram_frequencies_custom_n_25"),
                                         plotOutput("histogram_frequencies_custom_n_100"),
                                         HTML("</center>")
                                  )
                                ),
                                uiOutput("examples_additional_information"),
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
            "box_tickets_text_entry",
            NULL,
            value = "1,0",
            width = "100%"
          ),
          fluidRow(
            column(8),
            column(4,
                   actionButton("submit_tickets", "Set Tickets", class = "btn btn-success", style="color: #fff;", width = "100%")
            )
          ),
          uiOutput("tickets_text_error_message")
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
  
  boxModelPart1Server(id = "box_model_part_1")
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