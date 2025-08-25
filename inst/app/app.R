source("libraries.R")
source("linking_source_files.R")

ui <- page_navbar(
  tags$head(
    tags$style(HTML("
      body { zoom: 110%; }
      .modal-dialog { max-width: 1100px !important; width: 100% !important; }
    "))
  ),
  id = "main_nav",
  title = "Hypothesis Tests Visualised",
  theme = bs_theme(version = 5, bootswatch = "lumen"),  # You can switch to "lumen", "materia", "sketchy", "united", "yeti" etc.
  
  # Make the modal wider.
  tags$style(HTML("
        .modal-dialog {
          max-width: 1100px !important;
          width: 100% !important;
        }
      "),),
  
  nav_panel(
    title = "Home",
    fluidPage(
      fluidRow(
        column(8,
            h1("Home"),
            HTML("<br>"),
            tight_card(
              "Welcome!",
              HTML("
                  <p>Welcome to the Hypothesis Tests Visualised Project (HTPV)!

                  <ul>
                    <li>This project is aimed at helping people better understand hypothesis tests through an interactive environment.</li>
                    <li> Many of the hypothesis tests on this site are motivated using the box model. If unfamiliar, please refer to the 'Fundamentals' section, and complete 
                    the three 'Box Model' pages.</li>
                    <li><b>To get started</b> on this site, navigate to an exercise by clicking the links in the header above, or the links below!</li>
                  </ul>
                  </p>
                "),
              header_colour = "#3179ae"
            ),
            HTML("<br>"),
            tight_card(
              "Quick Links",
              fluidRow(
                column(4,
                  HTML("<b>Fundamentals 📦</b>"),
                  HTML("<br>"),
                  actionLink("go_box_model_part_1", "Box Model - Part 1", style = "color:blue; text-decoration:underline;"),
                  HTML("<br>"),
                  actionLink("go_box_model_part_2", "Box Model - Part 2", style = "color:blue; text-decoration:underline;"),
                  HTML("<br>"),
                  actionLink("go_box_model_part_3", "Box Model - Part 3", style = "color:blue; text-decoration:underline;"),
                  HTML("<br>"),
                  actionLink("go_confidence_intervals", "Confidence Intervals", style = "color:blue; text-decoration:underline;"),
                  HTML("<br>"),
                ),
                column(4,
                   HTML("<b>Z-Tests 🧪</b>"),
                   HTML("<br>"),
                   actionLink("go_1_sampe_z_test", "1-Sample Z-Test", style = "color:blue; text-decoration:underline;"),
                   HTML("<br>"),
                   actionLink("go_propotion_z_test", "Proportion (Z-test)", style = "color:blue; text-decoration:underline;"),
                   HTML("<br>"),
                ),
                column(4,
                   HTML("<b>T-Tests 🧪</b>"),
                   HTML("<br>"),
                   actionLink("go_t_curve_motivation", "T-Curve Motivation", style = "color:blue; text-decoration:underline;"),
                   HTML("<br>"),
                   actionLink("go_1_sample_t_test", "1-Sample T-Test", style = "color:blue; text-decoration:underline;"),
                   HTML("<br>"),
                   actionLink("go_paired_t_test", "Paired T-Test", style = "color:blue; text-decoration:underline;"),
                   HTML("<br>"),
                   actionLink("go_2_sample_t_test", "2-Sample T-Test", style = "color:blue; text-decoration:underline;"),
                   HTML("<br>"),
                   actionLink("go_regression_t_test", "Regression T-Test", style = "color:blue; text-decoration:underline;"),
                   HTML("<br>"),
                )
              )
            )
        ),
        column(4,
               card(
                 full_screen = FALSE,
                 style = "width: 100%; padding: 0; margin: 0;",
                 
                 # Header with background color and white text,
                 # but no horizontal padding here:
                 card_header(
                   tags$div(
                     HTML("<center>"),
                     "Supporting the Project",
                     HTML("</center>"),
                     style = "padding-left: 0.25rem; padding-right: 0.25rem;"
                   ),
                   style = paste(
                     "background-color:", "#FFD700", ";",
                     "color:", "#000000", ";",
                     "font-size: 1.25rem;",
                     "padding-top: 0.8rem; padding-bottom: 0.8rem;",  # vertical padding only here
                     "line-height: 1.2;",
                     "margin: 0;"
                   )
                 ),
                 
                 # Body with inner div padding:
                 card_body(
                   tags$div(
                     HTML("<center>"),
                     HTML("<p>
                           <br>
                           The best way to support the project is by starring it on GitHub. Click on the button below to go to this project's repository.
                           <br><br>
                           </p>"),
                     actionButton("go_repository", "HTPV Repo", style = "background-color: #FFD700"),
                     HTML("<br><br>"),
                     HTML("</center>"),
                     tags$script(HTML("
                        document.getElementById('go_repository').onclick = function() {
                          window.open('https://github.com/tjelton/Hypothesis-Tests-Visualised', '_blank');
                        }
                      ")),
                     style = "padding-left: 0.25rem; padding-right: 0.25rem;"
                   ),
                   class = "primary-card-content",
                   style = paste(
                     "padding-top: 0.5rem;",
                     "padding-bottom: 0.5rem;",
                     "margin: 0;",
                     "background-color: rgba(255, 215, 0, 0.1);"
                   )
                 )
               ),
               HTML("<br>"),
               card(
                 full_screen = FALSE,
                 style = "width: 100%; padding: 0; margin: 0;",
                 
                 # Header with background color and white text,
                 # but no horizontal padding here:
                 card_header(
                   tags$div(
                     HTML("<center>"),
                     "Something not quite right?",
                     HTML("</center>"),
                     style = "padding-left: 0.25rem; padding-right: 0.25rem;"
                   ),
                   style = paste(
                     "background-color:", "#3179ae", ";",
                     "color:", "#FFFFFF", ";",
                     "font-size: 1.25rem;",
                     "padding-top: 0.8rem; padding-bottom: 0.8rem;",  # vertical padding only here
                     "line-height: 1.2;",
                     "margin: 0;"
                   )
                 ),
                 
                 # Body with inner div padding:
                 card_body(
                   tags$div(
                     HTML("<center>"),
                     HTML("<p>
                           <br>
                           Did you find a bug? Is one of the statistical explanations incorrect, or is the math not quite what you would expect?
                           <br><br>
                           I'd apprecaite you letting me know! Please click on the button above to go the projects respoistory so that you can launch an issue. Important information
                           is provided in the projects README file.
                           <br>
                           </p>"),
                     HTML("<br>"),
                     HTML("</center>"),
                     style = "padding-left: 0.25rem; padding-right: 0.25rem;"
                   ),
                   class = "primary-card-content",
                   style = paste(
                     "padding-top: 0.5rem;",
                     "padding-bottom: 0.5rem;",
                     "margin: 0;",
                     "background-color: rgba(49, 121, 174, 0.1);"
                   )
                 )
               )
        )
      ),
    )
  ),
  
  nav_menu("Fundamentals",
           nav_panel("Box Model - Part 1", 
                     boxModelPart1UI("box_model_part_1")
           ),
           
           nav_panel("Box Model - Part 2", 
                     boxModelPart2UI("box_model_part_2")
           ),
           
           nav_panel("Box Model - Part 3", 
                     boxModelPart3UI("box_model_part_3"),
           ),
           
           nav_panel("Confidence Intervals",
                     confidenceIntervalUI("confidence_interval")
           )
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
  
  ##################### HYPERLINKS ##################### 
  
  observeEvent(input$go_box_model_part_1, {
    updateTabsetPanel(session, "main_nav", selected = "Box Model - Part 1")
  })
  
  observeEvent(input$go_box_model_part_2, {
    updateTabsetPanel(session, "main_nav", selected = "Box Model - Part 2")
  })
  
  observeEvent(input$go_box_model_part_3, {
    updateTabsetPanel(session, "main_nav", selected = "Box Model - Part 3")
  })
  
  observeEvent(input$go_confidence_intervals, {
    updateTabsetPanel(session, "main_nav", selected = "Confidence Intervals")
  })
  
  observeEvent(input$go_1_sampe_z_test, {
    updateTabsetPanel(session, "main_nav", selected = "1-Sample Z-Test")
  })
  
  observeEvent(input$go_propotion_z_test, {
    updateTabsetPanel(session, "main_nav", selected = "Proportion (Z-test)")
  })
  
  observeEvent(input$go_t_curve_motivation, {
    updateTabsetPanel(session, "main_nav", selected = "T-Curve Motivation")
  })
  
  observeEvent(input$go_1_sample_t_test, {
    updateTabsetPanel(session, "main_nav", selected = "1-Sample T-Test")
  })
  
  observeEvent(input$go_paired_t_test, {
    updateTabsetPanel(session, "main_nav", selected = "Paired T-Test")
  })
  
  observeEvent(input$go_2_sample_t_test, {
    updateTabsetPanel(session, "main_nav", selected = "2-Sample T-Test")
  })
  
  observeEvent(input$go_regression_t_test, {
    updateTabsetPanel(session, "main_nav", selected = "Regression T-Test")
  })
  
  ######################################################
  
  boxModelPart1Server(id = "box_model_part_1")
  boxModelPart2Server(id = "box_model_part_2")
  boxModelPart3Server(id = "box_model_part_3")
  confidenceIntervalServer(id = "confidence_interval")
  oneSampleZTestServer(id = "1_sample_z_test")
  proportionTestMainServer(id = "proportion_z_test")
  tCurveMotivationServer(id = "t_curve_motivation")
  oneSampleTTestServer(id = "1_sample_t_test")
  pairedTTestServer(id = "paired_t_test")
  twoSampleTTestServer(id = "2_sample_t_test")
  regressionTTestServer(id = "regression_t_test")

}

shinyApp(ui, server)