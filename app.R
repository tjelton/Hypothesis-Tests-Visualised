source("libraries.R")
source("Box-Model/box_model_main_srv.R")
source("Box-Model/box_model_main_ui.R")

library(gmp)

options(shiny.autoreload = TRUE)

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Hypothesis Tests Visualised"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Proportion (z-test)", tabName = "proportion_z_test", icon = icon("percent")),
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Box Model", tabName = "box_playground", icon = icon("box"))
    )
  ),
  
  dashboardBody(
    
    # Make the browser zoomed in.
    tags$style(HTML("
      body {
        zoom: 1.25;
      }
    ")),
    
    # tags$head(
    #   tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.8")
    # ),
    
    # Remove top margin of the dashboard body.
    tags$style(HTML("
      .content-wrapper {
        margin-top: -10px !important;
      }
    ")),
    
    tags$head(tags$style(HTML("
      .content {
        padding-left: 25px;
        padding-right: 25px;
      }
    "))),
    
    # Make the modal wider.
    tags$style(HTML("
        .modal-dialog {
          max-width: 1100px !important; /* Adjust this value for desired width */
          width: 100% !important;       /* Adjust this value for desired width */
        }
      ")),
    
    tabItems(
      
      # Home page.
      tabItem(tabName = "home",
              HTML("<h1>Home</h1><br>"),
              HTML("<p><b>Welcome! To get started, click on \"≡\" to navigate to your page of interest!</b></p>"),
              HTML("<p>More coming to the home page and this application soon...</p>"),
              
      ),
      
      # Box plot playground page.
      tabItem(tabName = "box_playground",
              boxModelMainUI("box_model"),
      ),
      
      # Box plot playground page.
      tabItem(tabName = "proportion_z_test",
              
              
              fluidRow(
                column(8,
                       HTML("<h1>Proportion Test (z-test)</h1><br>"),
                ),
                column(4,
                       HTML("<br>"),
                       tags$style(HTML(paste0("
                          [id='", "learning_text", "'] {
                            font-size: 20px; /* Adjust the font size as needed */
                            padding: 10px 20px; /* Optional: adjust padding for a larger button */
                          }
                       "))),
                       actionButton("learning_text", "What is the proportion test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
                ),
              ),
              HTML("<br>"),
              
              ############ SECTION: The NULL Hypotheis - Setting up the Box ############ 
              fluidRow(
                column(7,
                       box(
                         title = HTML("<u><b>The 'NULL' Hypothesis - Setting up the Box</b></u>"),
                         status = "primary", 
                         width = "100%",
                         solidHeader = FALSE,
                         
                         p("We start by using the box model to represent our null hypothesis."),
                         
                         # Step 1: Specify NULL Hypothesis
                         box(
                           title = "Step 1) Specify NULL Hypotheis (Box Tickets)",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>
                              <ul>
                                <li>First, we need to specify the 'null' hypothesis for our proportion test. As it is a proportion, the value must be between <b>0</b> and 
                                  <b>1</b>.</li>
                                <li>This is the proportion we are assuming our null hypothesis is equal to.</li>
                                <li>The null hypothesis changes the tickets in our box that we are drawing from.</li>
                              </ul>
                             </p>"),
                           
                           withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>Given \\( p \\), where \\( p \\) is the proportion of some event:</p>")),
                           fluidRow(
                             column(7),
                             column(2,
                                    withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( H_0: \\) \\( p = \\)</p>"))
                             ),
                             column(3,
                                    numericInput( 
                                      "null_porportion", 
                                      NULL, 
                                      value = 0.5, 
                                      min = 0, 
                                      max = 1, 
                                    ),
                             ),
                           ),
                           uiOutput('null_prop_warning')
                         ),
                         
                         # Step 2: Sample Size
                         box(
                           title = "Step 2) Sample Size (Number of Draws)",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>
                              <ul>
                                <li>Specify how large your sample will be in the text box below.</li>
                                <li>You can think of this as how many tickets we are drawing from the box with replacement.</li>
                                <li>You will enter the observed value (i.e. the proportion you saw from the sample) later.</li>
                              </ul>
                             </p>"),
                           fluidRow(
                             column(7),
                             column(2,
                                    withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( n =  \\)</p>"))
                             ),
                             column(3,
                                    numericInput( 
                                      "number_of_draws",
                                      label = NULL,
                                      value = 25, 
                                      min = 1
                                    ),
                             ),
                           ),
                           uiOutput('n_warning_message')
                         ),
                         
                         # Step 3: Model Type
                         box(
                           title = "Step 3) Model Using Sum or Sample",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>Third, we need to specify whether we will be modelling the sample using the mean or sum of the draws:<br><br></p>"),
                           radioButtons( 
                             "box_sum_or_mean",
                             label = NULL,
                             choices = list( 
                               "Mean" = 2,
                               "Sum" = 1
                             ) 
                           ),
                         )
                         
                       )
                ),
                column(5,
                       # Box model output.
                       box(
                         solidHeader = TRUE,
                         width = "100%",
                         HTML("<center>"),
                         grVizOutput("box_model", width = "70%", height = "70%"),
                         HTML("</center>")
                       ),
                )
              ),
              
              HTML("<br><br><br>"),
              
              ############ SECTION: The Alternate Hypothesis ############ 
              fluidRow(
                column(7,
                       box(
                         title = HTML("<u><b>The Alternate Hypothesis</b></u>"),
                         status = "primary", 
                         width = "100%",
                         solidHeader = FALSE,
                         HTML("<p>Specify what type of alternate hypothesis you will be using below:</p>"),
                         HTML("<br>"),
                         radioButtons( 
                           inputId = "alternate_hypothesis_choice", 
                           label = NULL, 
                           choices = list( 
                             "Two Sided" = 1, 
                             "One Sided (greater than)" = 2, 
                             "One Sided (less than)" = 3 
                           ) 
                         ),
                         
                       )
                ),
                column(5,
                       box(
                         solidHeader = TRUE,
                         width = "100%",
                         HTML("<p><b>Null Hypothesis</b></p>"),
                         uiOutput('null_hypothesis_output'),
                         HTML("<p><b>Alternate Hypothesis</b></p>"),
                         uiOutput('alternate_hypothesis_output'),
                       )
                )
              ),
              
              HTML("<br><br><br>"),
              
              ############ SECTION: Assumptions ############ 
              fluidRow(
                column(12,
                       box(
                         title = HTML("<u><b>Assumptions</b></u>"),
                         status = "primary", 
                         width = "100%",
                         solidHeader = FALSE,
                         HTML("<p>For the hypothesis test to be valid, we need to check the following assumptions:</p>"),
                         
                         # Assumption 1: Independent Samples
                         box(
                           title = "Assumption 1: Independent Sample",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>The first assumption is that our sample is <b>independent and randomly chosen</b>.</p>"),
                           HTML("<p><span style='color: blue;'><b>How do we check?</b></span> <i>We check by investigating the experimental setup.</i><br><br>
                                For example, consider we were investigating data for a proportion test involving human participants. We could read the accompanying scientific
                                publication to understand the methodology they used to gather the people in the sample.</p>")
                         ),
                         
                         # Assumption 2: Normal Approximation
                         box(
                           title = "Assumption 2: Normal Approximation",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           fluidRow(
                             column(6,
                                    uiOutput("assumption2_text_output"),
                                    HTML("<br><br>")
                             ),
                             column(6,
                                    plotOutput("empirical_draws_hist", width = "80%", height = "175px"),
                             )
                           )
                         )
                       )
                )
              ),
              
              HTML("<br><br><br>"),
              
              ############ SECTION: Test Statistics ############ 
              fluidRow(
                column(12,
                       box(
                         title = HTML("<u><b>Test Statistic</b></u>"),
                         status = "primary", 
                         width = "100%",
                         solidHeader = FALSE,
                         
                         fluidRow(
                           column(6,
                                  HTML("<p><b>Step 1) Observed Value (OV)</b></p>"),
                                  uiOutput("observed_value_output"),
                                  fluidRow(
                                    column(2),
                                    column(2,
                                           withMathJax(HTML("<p style='font-size: 16px; text-align: right;'>\\( OV = \\)</p>"))
                                    ),
                                    column(3,
                                           numericInput( 
                                             "observed_value", 
                                             NULL, 
                                             value = 0.7, 
                                             min = 0, 
                                             max = 1, 
                                             width = "100%"
                                           ),
                                    ),
                                  ),
                                  uiOutput("observed_val_warning_message"),
                                  HTML("<p><b>Step 2) Calculate Expected Value (SE) and Standard Error (SE)</b></p>"),
                                  uiOutput("ev_and_se_text")
                           ),
                           column(6,
                                  HTML("<p><b>Step 3) Test Statistic Calculation</b></p>"),
                                  uiOutput("test_statistic_calculation")
                           )
                         ),
                        
                         
                       ),
                ),

              ),
              
              HTML("<br><br><br>"),
              
              ############ SECTION: p-value ############ 
              fluidRow(
                column(6,
                       box(
                         title = HTML("<u><b>p-value</b></u>"),
                         status = "primary", 
                         width = "100%",
                         solidHeader = FALSE,
                         uiOutput("p_value_prelude")
                       )
                )
              )
              
              
              
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  boxModelMainServer(id = "box_model")
  
  # Process the null hypothesis proportion input.
  null_prop = reactiveVal(0.25)
  null_proportion_warning = reactiveVal(FALSE)
  observeEvent(input$null_porportion, {
    if (is.na(input$null_porportion) || input$null_porportion < 0 || input$null_porportion > 1) {
      null_prop(0.25)
      null_proportion_warning(TRUE)
    } else {
      null_prop(input$null_porportion)
      null_proportion_warning(FALSE)
    }
  })
  
  # Error message for when the value for n is invalid.
  output$null_prop_warning <- renderUI({
    if (null_proportion_warning()) {
      return(
        HTML("<span style='color: red;'><p>Error: The value must be between 0 and 1.</p></span>")
      )
    }
  })
  
  # Process the number of draws text input.
  sample_size = reactiveVal(5)
  number_of_draws_warning = reactiveVal(FALSE)
  observeEvent(input$number_of_draws, {
    if (is.na(input$number_of_draws) || input$number_of_draws < 0) {
      sample_size(25)
      number_of_draws_warning(TRUE)
    } else {
      sample_size(ceiling(input$number_of_draws))
      number_of_draws_warning(FALSE)
    }
  })
  
  # Error message for when the value for n is invalid.
  output$n_warning_message <- renderUI({
    if (number_of_draws_warning()) {
      return(
        HTML("<span style='color: red;'><p>Error: The value for n must be a whole number greater than 0.</p></span>")
      )
    }
  })
  
  output$box_model <- renderGrViz({
    
    tickets_string = ""
    
    prop_temp = null_prop() * 100
    
    # Find simplified proportion of 1's and 0's so that they are equivalent to the proportion provided in the null hypothesis.
    gcd_value = gcd(prop_temp, 100 - prop_temp)
    proportion_of_1s = prop_temp / gcd_value
    proportion_of_0s = (100 - prop_temp) / gcd_value
    
    is_float_1s = is.numeric(proportion_of_1s) && floor(proportion_of_1s) != proportion_of_1s
    is_float_0s = is.numeric(proportion_of_0s) && floor(proportion_of_0s) != proportion_of_0s
    
    # Case where there are too many digits. Write using percentages.
    if ((proportion_of_1s + proportion_of_0s) > 10) {
      is_float_1s = FALSE
    }
    
    # When the simplified proportions are floats, it is too hard to write down as integer. Write as percentages.
    if (is_float_1s || is_float_0s) {
      prop_temp_complement = 100 - prop_temp
      tickets_string = paste("1 x ", as.character(round(proportion_of_1s, digits = 2)), "%, 0 x ", as.character(round(proportion_of_0s, digits = 2)), "%", sep = "")
      
    } else {
      
      # Add 1's to the string
      for (i in 1:proportion_of_1s) {
        if (proportion_of_1s == 0) {
          break
        }
        tickets_string = paste(tickets_string, "1,")
      } 
      
      # Add 0's to the string.
      for (i in 1:proportion_of_0s) {
        if (proportion_of_0s == 0) {
          break
        }
        tickets_string = paste(tickets_string, "0,")
      } 
      tickets_string <- sub(",$", "", tickets_string)
      
    }
    
    # Get other elements for the box model
    n = sample_size()
    sample = "Sample Sum"
    if (input$box_sum_or_mean == 2) {
      sample = "Sample Mean"
    }
    
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
  
  
  # Alternate hypothesis (rendered) output.
  output$alternate_hypothesis_output <- renderUI({
    
    
    hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\( p", "\\neq", as.character(null_prop()), "\\)</p>")
    if (input$alternate_hypothesis_choice == 2) {
      hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\( p", ">", as.character(null_prop()), "\\)</p>")
    } else if (input$alternate_hypothesis_choice == 3) {
      hypothesis = paste("<p style='font-size: 16px;'>\\( H_1: \\) \\( p", "<", as.character(null_prop()), "\\)</p>")
      
    }
    return (
      tagList(
        HTML("<center>"),
        withMathJax(HTML(hypothesis)),
        HTML("</center>")
      )
    )
  })
  
  # Null hypothesis (rendered) output.
  output$null_hypothesis_output <- renderUI({
    hypothesis = paste("<p style='font-size: 16px;'>\\( H_0: \\) \\( p", "=", as.character(null_prop()), "\\)</p>")
    return (
      tagList(
        HTML("<center>"),
        withMathJax(HTML(hypothesis)),
        HTML("</center>")
      )
    )
  })
  
  output$assumption2_text_output <- renderUI({
    
    sample = "sum"
    if (input$box_sum_or_mean == 2) {
      sample = "mean"
    }
    
    string_1 = paste("<p>The second assumption is that the sample ", sample, "s follow an approximate normal distribution.</p>", sep = "")
    
    string_2 = paste("<p><span style='color: blue;'><b>How do we check?</b></span><br>
                     <ul>
                        <li>Recall that the central limit theorem tells us that if we take a sufficiently large number of draws from the box,
                             then the sample ", sample, "s will approximately follow a normal distribution. <i>If confused, please see the box model exercise</i></li>
                        <li>One way we can easily tell if the central limit theorem applies is to sample taking many draws from the box, and seeing whether the
                            values appear normally distributed</li>
                        <li>The plot to the left shows the distribution of 10000 simulated samples.</li>
                      <ul>
                      </p>")
    
    return(
      tagList(
        HTML(string_1),
        HTML(string_2)
      )
    )
  })
  
  # Histogram with normal curve to shown normal curve approximation.
  output$empirical_draws_hist = renderPlot({
    
    # Label titles.
    x_axis_string = "Sample Sum Value"
    title_string = "Empirical Distribution of 10000 Sample Sums with\nOverlaid Normal Curve"
    if (input$box_sum_or_mean == 2) {
      title_string = "Empirical Distribution of 10000 Sample Means with\nOverlaid Normal Curve"
      x_axis_string = "Sample Mean Value"
    }
    
    # Get data
    data = 0
    if (input$box_sum_or_mean == 1) {
      data = replicate(10000, sum(sample(c(0,1), sample_size(), replace = TRUE, prob = c(null_prop(), 1-null_prop()))))
    } else if (input$box_sum_or_mean == 2) {
      data = replicate(10000, mean(sample(c(0,1), sample_size(), replace = TRUE, prob = c(null_prop(), 1-null_prop()))))
    }
    
    # Normal curve parameters.
    mean_ = null_prop()
    sd_ = sqrt(null_prop() * (1-null_prop()))
    EV = sample_size() * mean_
    SE = sqrt(sample_size()) *sd_
    if (input$box_sum_or_mean == 2) { 
      EV = mean_
      SE = sd_/sqrt(sample_size())
    }
    
    bins_to_include = length(table(data))
    if (bins_to_include > 20) {
      bins_to_include = 20
    }
    
    plot = ggplot(data.frame(values = data), aes(x = values)) +
      geom_histogram(aes(y = after_stat(density)), bins = bins_to_include, fill = "lightgreen", color = "black") + 
      labs(x = x_axis_string, y = "Density", title = title_string) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.line = element_line(color = "black")
      ) +
      stat_function(fun = dnorm, args = list(mean = EV, sd = SE), color = "red", size = 1)
    
    return(plot)
  })

  output$observed_value_output <- renderUI({
    
    sample = "sum"
    if (input$box_sum_or_mean == 2) {
      sample = "mean"
    }
    
    string = paste("<p><i>What is the observed ", sample, " that you saw from your sample?</i></p>")
    return(
      tagList(
        HTML(string)
      )
    )
    
  })
  
  # Process the number of draws text input.
  observed_val = reactiveVal(0.7)
  observed_val_warning = reactiveVal(FALSE)
  observeEvent(input$observed_value, {
    if (is.na(input$observed_value) || input$observed_value < 0 || input$observed_value > 1) {
      observed_val(0.7)
      observed_val_warning(TRUE)
    } else {
      observed_val(input$observed_value)
      observed_val_warning(FALSE)
    }
  })
  
  # Error message for when the value for n is invalid.
  output$observed_val_warning_message <- renderUI({
    if (observed_val_warning()) {
      return(
        HTML("<span style='color: red;'><p>Error: The observed value must be between 0 and 1.</p></span>")
      )
    }
  })
  
  EV_string = reactiveVal("")
  SE_string = reactiveVal("")
  
  output$ev_and_se_text <- renderUI({
    
    # Find EV and SE.
    mean_ = null_prop()
    sd_ = sqrt(null_prop() * (1-null_prop()))
    EV = sample_size() * mean_
    SE = sqrt(sample_size()) *sd_
    if (input$box_sum_or_mean == 2) { 
      EV = mean_
      SE = sd_/sqrt(sample_size())
    }
    
    # EV and SE text (changes based upon whether the sample sum or mean is being used).
    expected_value = ""
    standard_error = ""
    
    EV_string(as.character(round(EV, 5)))
    SE_string(as.character(round(SE, 5)))
    
    # Mean
    if (input$box_sum_or_mean == 2) { 
      expected_value = withMathJax(
        HTML("<p>Expected Value:</p>"),
        HTML(paste("$$\\begin{align*} \\text{EV} &= \\mu \\\\ &=", EV_string(), "\\end{align*}$$", sep = ""))
      )
      
      standard_error = withMathJax(
        HTML("<p>Standard Error:</p>"),
        HTML(paste("$$\\begin{align*} \\text{SE} &= \\frac{\\sigma}{\\sqrt{n}} \\\\ &= \\frac{", round(sd_, 5) , "}{\\sqrt{", 
                   as.character(sample_size()), "}}\\\\ &= ", SE_string(), "\\end{align*}$$", sep = ""))
      )
      
    # Sum
    } else {
      expected_value = withMathJax(
        HTML("<p>Expected Value:</p>"),
        HTML(paste("$$\\begin{align*} \\text{EV} &= n \\times \\mu \\\\ &=", as.character(sample_size()), "\\times", round(mean_, 5),
                   "\\\\ &= ", EV_string(), "\\end{align*}$$", sep = ""))
      )
      
      standard_error = withMathJax(
        HTML("<p>Standard Error:</p>"),
        HTML(paste("$$\\begin{align*} \\text{SE} &= \\sqrt{n} \\times \\sigma \\\\ &= \\sqrt{", as.character(sample_size()), "} \\times", 
                   round(sd_, 5), "\\\\ &= ", SE_string(), "\\end{align*}$$", sep = ""))
      )
    }
    
    return(
      tagList(
        expected_value, standard_error
      )
    )
  })
  
  test_stat = reactiveVal("")
  output$test_statistic_calculation <- renderUI({
    temp = (observed_val() - as.numeric(EV_string()))/as.numeric(SE_string())
    temp = as.character(temp)
    test_stat(temp)
    t_stat = withMathJax(
      HTML(paste("$$\\begin{align*} \\text{TS} &= \\frac{\\text{OV} - \\text{EV}}{\\text{SE}} \\\\ &= \\frac{", as.character(round(observed_val(),5)), " - ", 
                 EV_string(), "}{", SE_string(), "} \\\\ &= ", test_stat(), "\\end{align*}$$", sep = ""))
    )
    additional_line = HTML(paste("<p style = 'text-align: left;'><span style='color: blue;'><i>The value for the test-statistic is ", test_stat(), ". </i></span></p>", sep = ""))
    return(
      tagList(
        t_stat,
        additional_line
      )
    )
  })
  
  output$p_value_prelude <- renderUI({
    
    first_string = HTML(paste("<p>The p-value is the probability of observing a test-statistic more extreme that our test statistic of ", test_stat(), ".<p>", sep = ""))
    
    
    
    return(
      tagList(
        first_string
      )
    )
    
  })
  
  
}

# Run the application
shinyApp(ui, server, enableBookmarking = "url")