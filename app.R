source("libraries.R")
source("linking_source_files.R")

options(shiny.autoreload = TRUE)
#options(shiny.reactlog = TRUE)

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Hypothesis Tests Visualised"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      
      menuItem("Fundamentals" , tabname = "Fundamentals", startExpanded = FALSE,
               menuSubItem("The Box Model", tabName = "box_playground")),
      
      menuItem("Z-Test" , tabname = "z_test", startExpanded = FALSE,
                menuSubItem("1-Sample Z-Test", tabName = "1_sample_z_test"),
                menuSubItem("Proportion (Z-Test)", tabName = "proportion_z_test")
               ),
      
      menuItem("T-Test", tabname = "t_test", startExpanded = FALSE,
                menuSubItem("T-Curve Motivation", tabName = "t_curve_motivation"),
                menuSubItem("1-Sample T-Test", tabName = "1_sample_t_test"),
                menuSubItem("Paired T-Test", tabName = "paired_t_test")
               )
    )
  ),
  
  dashboardBody(
    
    # Make the browser zoomed in.
    tags$style(HTML("
      body {
        zoom: 1.25;
      }
    ")),
    
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
          max-width: 1100px !important;
          width: 100% !important;
        }
      ")),
    
    # Make value box title smaller
    tags$style(HTML("
        .small-box h3 {
          font-size: 25px
        }
      ")),

    tabItems(
      
      # Home page.
      tabItem(tabName = "home",
              
              HTML("<br>"),
              
              fluidRow(
                column(6,
                       infoBox("Welcome!", 
                               "To get started, click on \"≡\" to navigate to your page of interest!",
                               icon = icon("arrow-left"), 
                               color = "light-blue", 
                               fill = TRUE,
                               width = "100%"),
                ),
                column(6,
                       tags$a(
                         href = "https://tjelton.com/2024/11/14/the-hypothesis-tests-visualised-project/", 
                         target = "_blank",
                         valueBox("About", "Click to find out more about the project",
                                  icon = icon("question"), color = "red", width = "100%")
                       ),
                       tags$a(
                         href = "https://docs.google.com/forms/d/e/1FAIpQLSdmyGBjQWRDg-ksPGqSsMuLNsuvnwwVgD0GW1bKvKVu6Rnplg/viewform?usp=sf_link", 
                         target = "_blank",
                         valueBox("Feedback", "Click to report errors or provide suggestions/feedback!",
                                  icon = icon("comment"), color = "olive", width = "100%")
                       )
                )
              ),
      ),
      
      
      tabItem(tabName = "box_playground",
              boxModelMainUI("box_model"),
      ),
      
      tabItem(tabName = "1_sample_z_test",
              oneSampleZTestUI("1_sample_z_test"),
      ),
      
      tabItem(tabName = "proportion_z_test",
              proportionTestMainUI("proportion_test"),
      ),
      
      tabItem(tabName = "t_curve_motivation",
              tCurveMotivationUI("t_curve_motivation")
      ),
      
      tabItem(tabName = "1_sample_t_test",
              oneSampleTTestUI("1_sample_t_test"),
      ),
      
      tabItem(tabName = "paired_t_test",
              
              # Title Strip
              fluidRow(
                column(8,
                       HTML("<h1>Paired t-Test (Work In Progress)</h1><br>"),
                ),
                column(4,
                       HTML("<br>"),
                       tags$style(HTML(paste0("
                          [id='learning_text'] {
                            font-size: 20px;
                            padding: 10px 20px;
                          }
                          "))),
                       actionButton("learning_text", "What is a paired t-test?", class = "btn btn-primary", style="color: #fff;", width = "100%")
                ),
              ),
              HTML("<br>"),
              
              ############ SECTION: Input Data ############
              fluidRow(
                column(7,
                       box(
                         title = HTML("<u><b>Input Sample Data</b></u>"),
                         status = "primary",
                         width = "100%",
                         solidHeader = FALSE,
                         HTML(paste("<p>Information placed here about choosing data...</p>"), sep = ""),
                         HTML("<br>"),
                         
                         # User specifies whether they will use inbuilt data or manually specify data.
                         radioButtons( 
                           inputId = "data_upload_choice", 
                           label = HTML("<p><b>What data source will you be using?</b></p>"), 
                           choices = list( 
                             "Pre-uploaded Data" = "pre_uploaded", 
                             "Manually Specified Data" = "manually_specified" 
                           ),
                           selected = NA
                         ),
                         
                         # Mechanism for user to specify data depending on radio button option.
                         uiOutput("data_upload"),
                         
                         # These will output only for the case that the "pre_uploaded" option is selected.
                         uiOutput("numeric_column_selection_pre_uploaded_data"),
                         uiOutput("pre_uploaded_warning_and_save_data"),

                         # # These will output only for the case that the "manually_specified" option is selected.
                         uiOutput("manual_entry_unequal_samples"),
                         uiOutput("manual_entry_insufficient_unique_values")
                       )
                ),
                column(5,
                         uiOutput("radio_buttons_data_to_plot"),
                         uiOutput("data_upload_plot_section_output")
                )
              )
              
      )
      
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Store the sample data.
  data_condition_1 <- reactiveVal(NULL)
  data_condition_2 <- reactiveVal(NULL)
  data_paired_difference <- reactiveVal(NULL)
  
  # To trigger the pre_uploaded functions to re-run so data is re-updated.
  re_run_flag <- reactiveVal(FALSE)
  
  # Mechanism to allow the user to specify the data sample they will be using.
  output$data_upload <- renderUI({
    
    # Ensure some radio button option has been selected.
    req(input$data_upload_choice)
    
    # Reset data to be null.
    data_condition_1(NULL)
    data_condition_2(NULL)
    data_paired_difference(NULL)
    
    # Pre-uploaded data
    # The pre-uploaded data set comes from the 'PairedData' package: https://cran.r-project.org/web/packages/PairedData/PairedData.pdf
    if (input$data_upload_choice == "pre_uploaded") {

      isolate({
        re_run_flag(!re_run_flag())
      })
      
      return(
        tagList(
          HTML("<br>"),
          selectInput( 
            "data_set_pre_uploaded", 
            "Which data set would you like to analyse?", 
            list("Blink", "BloodLead", "GDO", "GrapeFruit", "HorseBeginners", "Iron", "Meat", "PrisonStress") 
          ),
          HTML("<p><i>Warning: Some of these data sets are not truly paired. However, they are still included here to have more data for experimentation.</i></p>"),
          HTML("<p>Now we need to select what data we want to be in condition 1, and what to be in condition 2. The paired difference will be condition 2 - condition 1.</p>")
        )
      )
    }
    
    # Manual upload data.
    if (input$data_upload_choice == "manually_specified") {
      return(
        tagList(
          HTML("<br>"),
          HTML("<p><b>Enter data into the text box below for each sample.</b>
            <ul>
              <li>All values must be numeric, and numbers should contain no spaces or any characters other than a '.' for a decimal place.</li>
              <li>Each value must be on it's own line, or comma seperated.</li>
              <li>As this is paired data, there must be the same number of values in each textbox.</li>
              <li>Don't forget to press 'Upload' once you are finished!</li>
            </ul></p>"),
          
          # Textbox -> condition 1 data entry.
          HTML("<b>Condition 1:</b>"),
          textAreaInput( 
            "manual_data_upload_textbox_condition_1", 
            NULL, 
            value = ""
          ),

          # Textbox -> condition 2 data entry.
          HTML("<b>Condition 2:</b>"),
          textAreaInput( 
            "manual_data_upload_textbox_condition_2", 
            NULL, 
            value = ""
          ),
          
          # Upload action button. Wrapped in a fluid row to make it right-aligned.
          fluidRow(
            column(8),
            column(4,
                   actionButton(
                     inputId = "load_manual_data",
                     label = "Upload",
                     class = "btn-success text-white",
                     style = "color: #fff;",
                     width = "100%"
                   ),
            )
          )
          
        )
      )
    }
    
  })
  
  # When the user chooses a pre uploaded data set, prompt them to choose the numeric column for condition 1 and 2.
  output$numeric_column_selection_pre_uploaded_data <- renderUI({
    req(input$data_set_pre_uploaded)
    
    # Only display if the pre_uploaded radio button option is selected.
    if (input$data_upload_choice != "pre_uploaded") {
      return()
    }
    
    # Get numeric column names.
    data = get(input$data_set_pre_uploaded) 
    numeric_cols <- names(data)[sapply(data, is.numeric)]
  
    return(
      tagList(
        fluidRow(
          column(6,
               selectInput(
                 "condition_1_pre_uploaded",
                 "Condition 1",
                 numeric_cols,
                 selected = numeric_cols[1]
               )
          ),
          column(6,
                 selectInput(
                   "condition_2_pre_uploaded",
                   "Condition 2",
                   numeric_cols,
                   selected = numeric_cols[2]
                 )
          )
        )
      )
    )
  })
  
  # Warning message when for the pre-uploaded option, conditions 1 and 2 are the same.
  output$pre_uploaded_warning_and_save_data <- renderUI({
    req(input$data_set_pre_uploaded)
    req(input$condition_1_pre_uploaded)
    
    # Only display if the pre_uploaded radio button option is selected.
    if (input$data_upload_choice != "pre_uploaded") {
      return()
    }
    
    # Warning message when the conditions are the same.
    if (input$condition_1_pre_uploaded == input$condition_2_pre_uploaded) {
      data_condition_1(NULL)
      data_condition_2(NULL)
      data_paired_difference(NULL)
      return(
        tagList(
          HTML("<span style='color: red;'><p>Warning: Condition 1 and condition 2 should be different.</p></span>"),
        )
      )
    
    # Otherwise, set the data.
    } else {
      data_condition_1(get(input$data_set_pre_uploaded)[[input$condition_1_pre_uploaded]])
      data_condition_2(get(input$data_set_pre_uploaded)[[input$condition_2_pre_uploaded]])
      data_paired_difference(data_condition_2() - data_condition_1())
      return()
    }
  })
  
  # Reactive variables to keep track of the number of values entered for each condition.
  num_values_cond_1 = reactiveVal(NULL)
  num_values_cond_2 = reactiveVal(NULL)
  error_message_unequal_paired_samples = reactiveVal(FALSE)
  error_message_insufficient_unique_values_manual_upload = reactiveVal(FALSE)
  
  # Observe if the "Upload" button is clicked. This will involve checking the data, and triggering a warning if the data is not in the correct form.
  observeEvent(input$load_manual_data, {
    
    req(input$manual_data_upload_textbox_condition_1)
    req(input$manual_data_upload_textbox_condition_2)
    
    # Re-set warning messages
    error_message_unequal_paired_samples(FALSE)
    error_message_insufficient_unique_values_manual_upload(FALSE)
    
    data_condition_1(NULL)
    data_condition_2(NULL)
    data_paired_difference(NULL)
    
    condition_1 = input$manual_data_upload_textbox_condition_1
    condition_2 = input$manual_data_upload_textbox_condition_2

    # Process condition 1 data.
    splits_cond_1 <- strsplit(condition_1, "[,\n]")[[1]]
    splits_cond_1 <- trimws(splits_cond_1)
    splits_cond_1 = splits_cond_1[splits_cond_1 != ""]
    numeric_cond_1 = as.numeric(splits_cond_1)
    numeric_cond_1 = numeric_cond_1[!is.na(numeric_cond_1)]
    
    # Process condition 2 data.
    splits_cond_2 <- strsplit(condition_2, "[,\n]")[[1]]
    splits_cond_2 <- trimws(splits_cond_2)
    splits_cond_2 = splits_cond_2[splits_cond_2 != ""]
    numeric_cond_2 = as.numeric(splits_cond_2)
    numeric_cond_2 = numeric_cond_2[!is.na(numeric_cond_2)]
    
    # Check if the data is really paired -> otherwise, trigger a warning message.
    if (length(numeric_cond_1) != length(numeric_cond_2)) {
      num_values_cond_1(length(numeric_cond_1))
      num_values_cond_2(length(numeric_cond_2))
      error_message_unequal_paired_samples(TRUE)
      return()
    }
    
    # Find paired difference between the groups.
    paired_difference = numeric_cond_2 - numeric_cond_1

    # Check that there are more than one unique value (table length will be greater than 1). Otherwise, sd = 0, and the test will break.
    adequate_unique <- length(table(paired_difference)) > 1
    
    # In the case that there are not unique values, raise an error with the user.
    if (adequate_unique == FALSE) {
      error_message_insufficient_unique_values_manual_upload(TRUE)
      return()
    }
    
    data_condition_1(numeric_cond_1)
    data_condition_2(numeric_cond_2)
    data_paired_difference(paired_difference)
    
  })
  
  # Error message for when not enogh unique values.
  output$manual_entry_unequal_samples <- renderUI({
    req(input$data_upload_choice)
    
    # Only display if the manually_specified radio button option is selected.
    if (input$data_upload_choice != "manually_specified") {
      return()
    }
    
    if (error_message_unequal_paired_samples()) {
      string = paste("<span style='color: red;'><p>Warning: The number of values in each condition is unequal. Condition 1 has ", 
        as.character(num_values_cond_1()), " values, and condition 2 has ", as.character(num_values_cond_2()), " values.</p></span>", sep = "")
      return(
        tagList(
          HTML("<br>"),
          HTML(string)
        )
      )
    } else {
      return()
    }
  })
  
  # Error message for when not enogh unique values.
  output$manual_entry_insufficient_unique_values <- renderUI({
    req(input$data_upload_choice)
    
    # Only display if the pre_uploaded radio button option is selected.
    if (input$data_upload_choice != "manually_specified") {
      return()
    }
    
    if (error_message_insufficient_unique_values_manual_upload()) {
      string = "<span style='color: red;'><p>Warning: You must have at least two unique values in your manually specified data.</p></span>"
      return(
        tagList(
          HTML("<br>"),
          HTML(string)
        )
      )
    } else {
      return()
    }
  })
  
  # Radio button allowing user to choose which data they want to plot.
  output$radio_buttons_data_to_plot <- renderUI({
  
    # If data has not been uploaded, don't display anything.
    if (is.null(data_paired_difference())) {
      return()
    }
    
    return(
      box(
        solidHeader = TRUE,
        width = "100%",
        radioButtons("data_to_plot", NULL, c("Condition 1" = "Condition_1", 
                                             "Condition 2" = "Condition_2", 
                                             "Paired Difference" = "Paired_Difference"), inline=T)
      )
    )
  })
  
  # Plot UI or warning message output.
  output$data_upload_plot_section_output <- renderUI({
    
    # Text to render alerting the user that they can't proceed until they make a data choice.
    # This is when no data has been set.
    if (is.null(data_paired_difference())) {
      string = "<span style='color: blue;'><p>In order to proceed, you must select some data to act as your sample.</p></span>"
      return(
        tagList(
          box(
            solidHeader = TRUE,
            width = "100%",
            HTML(string)
          )
        )
      )
    }
    
    # If data has been set, display the plots.
    return(
      tagList(
        plotOutput("initial_data_plots",  width = "80%")
      )
    )
  })
  
  # Display input data.
  output$initial_data_plots <- renderPlot({
    
    req(input$data_to_plot)
    
    # If data has not been uploaded, don't display anything.
    if (is.null(data_paired_difference())) {
      return()
    }
    
    data_plotting = data_condition_1()
    if (input$data_to_plot == "Condition_2") {
      data_plotting = data_condition_2()
    } else if (input$data_to_plot == "Paired_Difference") {
      data_plotting = data_paired_difference()
    }
    
    data_plotting = data.frame(data_plotting)
    
    # Boxplot of data
    boxplot = data_plotting %>%
      ggplot() +
      aes(x = data_plotting) +
      geom_boxplot(fill = "blue") +
      labs(title = "Boxplot", x = "Values") +
      theme_minimal() + 
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      )
    
    # Histogram of data
    histogram = data_plotting %>%
      ggplot() +
      aes(x = data_plotting) +
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      labs(title = "Histogram", x = "Values", y = "Frequency") +
      theme_minimal()

    # Place plots on top of each other using cowplot.
    combined = plot_grid(
      boxplot,
      histogram,
      ncol = 1,
      rel_heights = c(1,2)
    )
    
    return(combined)
    
  })

  boxModelMainServer(id = "box_model")
  
  proportionTestMainServer(id = "proportion_test")
  
  oneSampleZTestServer(id = "1_sample_z_test")
  
  tCurveMotivationServer(id = "t_curve_motivation")
  
  oneSampleTTestServer(id = "1_sample_t_test")
  
}

enableBookmarking(store = "url")


# Run the application
shinyApp(ui, server)