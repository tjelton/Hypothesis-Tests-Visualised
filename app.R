source("libraries.R")
source("Box-Model/template_srv.R")
source("Box-Model/template_ui.R")

library(DiagrammeR)


options(shiny.autoreload = TRUE)

# We assume that if mean_or_sample_as_int = 1 then we are talking about sum, and mean_or_sample_as_int = 2 is mean.
simulate_box <- function(mean_or_sample_as_int, n, box) {
  print("REACHED")
  print(box)
  value = sample(box, n, replace = TRUE)
  print("REACHED")
  if (mean_or_sample_as_int == 2) {
    value = mean(value)
  } else {
    value = sum(value)
  }
  return(value)
}

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Hypothesis Tests Visualised"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Box Model", tabName = "box_playground", icon = icon("box"))
    )
  ),
  
  dashboardBody(
    
    # Make the browser zoomed in.
    tags$style(HTML("
      body {
        zoom: 1.1;
      }
    ")),
    
    # Remove top margin of the dashboard body.
    tags$style(HTML("
      .content-wrapper {
        margin-top: -10px !important;
      }
    ")),
    
    # Change the margin of all text.
    # tags$style(HTML("
    #   .content-wrapper p,
    #   .content-wrapper h1,
    #   .content-wrapper h2,
    #   .content-wrapper h3,
    #   .content-wrapper h4,
    #   .content-wrapper h5,
    #   .content-wrapper h6 {
    #     padding-left: 15px !important;
    #     padding-right: 15px !important;
    #   }
    # ")),
    
    tags$head(tags$style(HTML("
      .content {
        padding-left: 25px;
        padding-right: 25px;
      }
    "))),
    
    tabItems(
      
      # Home page.
      tabItem(tabName = "home",
              HTML("<h1>Home</h1><br>"),
              HTML("<p><b>Welcome! To get started, click on \"≡\" to navigate to your page of interest!</b></p>"),
              HTML("<p>More coming to the home page and this application soon...</p>"),
              
      ),
      
      # Box plot playground page.
      tabItem(tabName = "box_playground",
              #templateUI("template"),
              
              HTML("<h1>Box Model: Playground</h1><br>"),
              HTML("<p style='padding-bottom: 25px;'>Welcome to the box model playground! This page was designed as a way to visualise
                    the box model, and see how if sufficient number of draws are taken from the box, the central limit theorem allows us
                    to to model the sum or means of the draw using the normal curve. We start by specifying the box that we will be using
                    and then verifying that we are taking a sufficient number of draws for the central limit theorem to apply.<p>"),
              
              ############ SECTION: Setting up the Box Model ############ 
              fluidRow(
                column(7,
                       box(
                         title = HTML("<u><b>Box Parameters</b></u>"),
                         status = "primary", 
                         width = "100%",
                         solidHeader = FALSE,
                         
                         HTML("<p>First we need to create the box. Follow steps 1, 2, and 3 to configure the box to be whatever you wish to model.
                       The box will output on the right according to the settings that you pick.</p><br>"),
                         
                         # Step 1: Enter Tickets
                         box(
                           title = "Step 1) Tickets",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>First, we need to specify the tickets that we will be adding to the box.</p>"),
                           HTML("<p>In the text box below, enter the tickets that you wish to place into the box. Only <i>numbers</i> can be
                               added, and <i>each ticket should be seperated by a comma</i>. For example, if you want to have 1x1 ticket
                               and 3x0 tickets in the box, enter <i>1,0,0,0</i>."),
                           textAreaInput( 
                             "box_tickets_text_entry",
                             NULL,
                             value = "1,0,0,0",
                             width = "100%"
                           ),
                           uiOutput("tickets_text_error_message")
                         ),
                         
                         # Step 2: Set n
                         box(
                           title = "Step 2) Number of Draws",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>Second, we need to specify the number of draws (with replacement) that we will be taking from the box:</p>"),
                           numericInput( 
                             "number_of_draws",
                             label = NULL,
                             value = 25, 
                             min = 1
                           ), 
                         ),
                         
                         # Step 3: Model Using Sum or Mean
                         box(
                           title = "Step 3) Model Using Sum or Mean",
                           width = "100%",
                           collapsible = TRUE,
                           collapsed = TRUE,
                           status = "info",
                           solidHeader = FALSE,
                           HTML("<p>Third, we need to specify whether we will be modelling the sample using the mean or sum of the draws:<br><br></p>"),
                           radioButtons( 
                             inputId = "box_sum_or_mean",
                             label = NULL,
                             choices = list( 
                               "Sum" = 1, 
                               "Mean" = 2
                             ) 
                           ),
                           
                         )
                       )
                ),
                
                # Box model output.
                column(5,
                       box(
                         solidHeader = TRUE,
                         width = "100%",
                         HTML("<center>"),
                         grVizOutput('box_model', width = "70%", height = "70%"),
                         HTML("</center>")
                       )
                )
              ),
              
              # Continue button (to display CLT section)
              column(12,
                     HTML("<center>"),
                     actionButton("continue_CLT_section", "Continue", width = "200px", 
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                     HTML("</center>")
                
              ),

              HTML("<br><br>"),
              
              ############ SECTION: Checking Central Limit Theorem ############ 
              fluidRow(
                
                # Only display this section if continue button is pressed.
                conditionalPanel(
                  condition = "typeof continue_CLT_section == \"undefined\"",
                  
                  column(5,
                     box(
                       title = HTML("<u><b>Central Limit Theorem</b></u>"),
                       status = "primary",
                       solidHeader = FALSE,
                       width = "100%",
                       uiOutput("CLT_text_instructions_output"),
                       HTML("<br>"),
                       fluidRow(
                         column(1),
                         
                         # Button for repeating adding the mean or sample sum to the histogram.
                         column(5,
                                actionButton(
                                  inputId = "repeat_1", label = HTML('<i class="fa fa-plus"></i> Repeat 1'),
                                  class = "btn btn-primary", style="color: #fff;", width = "100%"
                                ),
                                HTML("<br><br>"),
                                actionButton(
                                  inputId = "repeat_25", label = HTML('<i class="fa fa-plus"></i> Repeat 25'),
                                  class = "btn btn-primary", style="color: #fff;", width = "100%"
                                ),
                         ),
                         column(5,
                                actionButton(
                                  inputId = "repeat_10", label = HTML('<i class="fa fa-plus"></i> Repeat 10'),
                                  class = "btn btn-primary", style="color: #fff;", width = "100%"
                                ),
                                HTML("<br><br>"),
                                actionButton(
                                  inputId = "repeat_100", label = HTML('<i class="fa fa-plus"></i> Repeat 100'),
                                  class = "btn btn-primary", style="color: #fff;", width = "100%"
                                ),
                         ),
                       ),
                       HTML("<br><center>"),
                       
                       # On click, resets the histogram
                       actionButton(
                         inputId = "reset_button", label = HTML('<i class="fa fa-redo"></i> Reset'),
                         class = "btn btn-danger", style="color: #fff;"
                       ),
                       HTML("</center>")
                     ),

                  ),
                  
                  # Histogram Distribution
                  column(7,
                         box(
                           solidHeader = TRUE,
                           width = "100%",
                           HTML("<center>"),
                           plotOutput("histogram_frequencies"),
                           HTML("</center>")
                         )
                  )
                  
                  
                )
              )
              
              
              
              
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  templateServer(id = "template")
  
  ticket_numbers <- reactiveVal(NULL)
  invalid_tickets_string_bool <- reactiveVal(FALSE)
  number_of_ticket_draws <- reactiveVal(25)
  
  # Process tickets text box strings.
  observeEvent(input$box_tickets_text_entry, {
    
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
    } else if (length(numeric_vec) == 0) {
      invalid_tickets_string_bool(TRUE)
      ticket_numbers(c(1,0,0,0))
    } else {
      ticket_numbers(numeric_vec)
      invalid_tickets_string_bool(FALSE)
    }
  })
  
  # Process number_of_draws value.
  observeEvent(input$number_of_draws, {
    if (!is.na(input$number_of_draws)) {
      number_of_ticket_draws(input$number_of_draws)
    }
  })
  
  # Error message for when the text box for entering the tickets for the box is invalid
  output$tickets_text_error_message <- renderUI({
    if (invalid_tickets_string_bool()) {
      return(
        HTML("<span style='color: red;'><p>Error: One or move values that you added cannot be interpreted. Please carefully
             check what you entered. Setting contents of the box to 1,0,0,0.</p></span>")
      )
    }
  })
  
  # Text instructions for the central limit theorem section
  output$CLT_text_instructions_output <- renderUI({
    sample = "sums"
    if (input$box_sum_or_mean == 2) {
      sample = "means"
    }
    
    string = paste("<p>Recall that the central limit theorem tells us that if we take a <b>sufficiently large number of draws</b> 
                   from the box, then the <b>sample ", sample, " will follow a normal distribution</b>.<br><br>Now we will empirically
                   test whether n = ", number_of_ticket_draws(), " is a sufficient number of draws for the central limit theorem to
                   apply.<br><br>To do this, ....")
  
    return(HTML(string))
  })
  
  output$box_model <- renderGrViz({
    
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
  
  ########## Process empirical sample sums and means ########## 
  empirical_data <- reactiveVal(NULL)
  
  # Event: single draw.
  observeEvent(input$repeat_1, {
    print(simulate_box(input$box_sum_or_mean, number_of_ticket_draws(), ticket_numbers()))
  })
  
  
  
  
  # Histogram of mean and sum frequencies.
  output$histogram_frequencies = renderPlot({
    
      title_string = "Empiricial Distribution of Sample Sums"
      x_axis_string = "Sample Sum Value"
      if (input$box_sum_or_mean == 2) {
        title_string = "Empiricial Distribution of Sample Means"
        x_axis_string = "Sample Mean Value"
      }
      
      # Default empty plot for when no data has been simulated yet.
      plot = ggplot() +
        xlim(0, 10) +  # Set x-axis limits
        ylim(0, 100) +
        labs(x = x_axis_string, y = "Frequency", title = title_string) +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          axis.line = element_line(color = "black")
        )
      
      # If data has been generated, create a histogram
      if (!is.null(empirical_data())) {
        
      }
      
      return(plot)
  })
  
  ####################################################
  ################# CONTINUE BUTTONS ################# 
  ####################################################
  # Delete the continue button once it has been pressed.
  observeEvent(input$continue_CLT_section, {
    removeUI(selector='#continue_CLT_section', immediate=TRUE)
  }, autoDestroy=TRUE)
  
}

# Run the application
shinyApp(ui, server)