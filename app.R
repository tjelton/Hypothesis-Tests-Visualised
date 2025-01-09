source("libraries.R")
source("Box-Model/box_model_main_srv.R")
source("Box-Model/box_model_main_ui.R")
source("z-test/proportion_test_srv.R")
source("z-test/proportion_test_ui.R")
source("z-test/1_sample_z_test_ui.R")
source("z-test/1_sample_z_test_srv.R")
source("Utility/load_data_1_sample_srv.R")
source("Utility/load_data_1_sample_ui.R")

source("Utility/generic_plotting_functions.R")

options(shiny.autoreload = TRUE)
options(shiny.reactlog = TRUE)


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
                menuSubItem("T-Curve Motivation", tabName = "t_curve_motivation")
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
              
              
              HTML("<h1>Introducing the T-Distribution (Motivation)</h1><br>"),
              
              ############ SECTION: When we don't know the population sd! ############
              fluidRow(
                column(12,
                       box(
                         title = HTML("<u><b>When we don't know the population sd!</b></u>"),
                         status = "primary",
                         width = "100%",
                         solidHeader = FALSE,
                         
                         withMathJax(
                            HTML(
                              "<p>
                                  When determining the standard error (SE) in the 1-sample z-test section, we assumed that the <b>population standard deviation (denoted by 
                                  \\(\\sigma  \\)) is known</b>:
                              </p>"
                            ),
                            HTML("$$\\begin{align*} \\text{SE} &= \\frac{\\color{red}{\\sigma}}{\\sqrt{n}} \\end{align*}$$"),
                            HTML(
                              "<p>
                                  However, in practice, the population standard deviation is usually unknown. Hence, we often use the <b>sample standard deviation (denoted by
                                  \\(s \\)) instead</b>:
                              </p>"
                            ),
                            HTML("$$\\begin{align*} \\hat{\\text{SE}} &= \\frac{\\color{red}{s}}{\\sqrt{n}} \\end{align*}$$"),
                            HTML("
                              <p>
                                  One observation is that when we substitute the population standard deviation for the sample standard deviation, we now write \\(\\text{SE}\\) 
                                  instead of \\(\\hat{\\text{SE}}  \\). The \\(\\hat{}\\) represents that this standard error is an estimate. We don’t have the true 
                                  \\(\\text{SE}\\) anymore, as we are using the sample, which is a subset of the population.<br><br>
                                  
                                  As the sample size increases, the estimate standard error (\\(\\hat{\\text{SE}}\\)) will converge to the population standard error 
                                  (\\(\\text{SE}\\)). Conceptually, this is because as the sample becomes larger, it is more representative of the population. Also, the above is 
                                  only true if we are doing unbiased sampling!
                              </p>
                            ")
                          )
                       )
                )
              ),
              
              HTML("<br><br><br>"),
              
              ############ SECTION: Introducing the t-curve. ############
              fluidRow(
                
                column(6,
                       box(
                         title = HTML("<u><b>Introducing the t-curve</b></u>"),
                         status = "primary",
                         width = "100%",
                         solidHeader = FALSE,
                         withMathJax(
                           HTML(
                             "<p>
                                  Firstly, let’s remind ourselves of the test statistic (\\(\\text{TS}\\)) calculation for a 1-sample z-test:
                              </p>"
                           ),
                           HTML("$$\\begin{align*} \\text{Test Statistic (TS)} &= \\frac{\\text{OV} - \\text{EV}}{\\color{red}{\\text{SE}}} \\end{align*}$$"),
                           HTML(
                             "<p>
                                  As a reminder, \\(\\text{OV}\\) stands for observed value and \\(\\text{EV}\\) stands for expected value. As mentioned in the above seection,
                                  the \\(\\text{SE}\\) (indicated in red) requires that the population standard deviation is known. When this is not known, we can instead use 
                                  the estimate standard error (using the sample’s standard deviation):
                              </p>"
                           ),
                           HTML("$$\\begin{align*} \\text{Test Statistic (TS)} &= \\frac{\\text{OV} - \\text{EV}}{\\color{red}{\\hat{\\text{SE}}}} \\end{align*}$$"),
                           HTML("
                              <p>
                                 In both cases above, the test statistic formulas look the same. The only difference is that the latter one has an estimate for \\(\\text{SE}\\) 
                                 (i.e. \\(\\hat{\\text{SE}}\\)). Hence, we  claim that the second test statistic has extra variability because of the uncertainty surrounding 
                                 \\(\\hat{\\text{SE}}\\).<br><br>
                                 
                                 How do we account for this extra variability? We adjust the method in which the p-value is found. Hence, to account for this extra variability, 
                                 like the 1-sample z-test, which uses a normal curve to find the p-value, we use the t-curve. The t-curve (or t-distribution) is similar in 
                                 appearance to the normal curve, except it contains an extra parameter called degrees of freedom, which adjusts the ‘fatness’ of the curve’s tails.
                                 This is evident when playing with the app to the right. When degrees of freedom increases, the tail fatness decreases. You can also see that
                                 as the degrees of freedom value increases, the curve approaches the normal distribution.
                              </p>
                            ")
                         )
                       )
                ), 
                column(6,
                       box(
                         title = "Demonstration",
                         status = "primary",
                         width = "100%",
                         solidHeader = TRUE,
                         
                         HTML("<p>The slider below changes the degrees of freedom (df) of the red t-curve in the graph below.</p>"),
                         fluidRow(
                           column(8,
                              sliderInput(
                                "df_slider", 
                                NULL,
                                min = 1, 
                                max = 30,
                                value = 1),
                           ),
                           column(4,
                              checkboxInput("display_normal_curve", "Display normal curve", TRUE), 
                           )
                         ),
                         
                         plotOutput("changing_df_graph", width = "80%", height = "300px"),
                         height = "550px"
                       )
                          
                )
                
              )
              
              
              
        
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  
  output$changing_df_graph = renderPlot({
    
    data <- data.frame(x = seq(-4, 4, length.out = 100))
    
    plot = ggplot(data, aes(x)) +
      stat_function(fun = dt, args = list(df = input$df_slider), color = "red", size = 0.5) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.y = element_blank(),
        panel.border = element_blank()
      )
    
    if (input$display_normal_curve) {
      plot = plot + stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "black", size = 0.5, linetype = "dashed")
    }
    
    return(plot)
  })
  
  boxModelMainServer(id = "box_model")
  
  proportionTestMainServer(id = "proportion_test")
  
  oneSampleZTestServer(id = "1_sample_z_test")
  
}

# Run the application
shinyApp(ui, server, enableBookmarking = "url")