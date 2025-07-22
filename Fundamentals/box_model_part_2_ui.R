boxModelPart2UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    HTML("<h1>The Box Model Part 2 - Central Limit Theorem</h1>"),
    
    fluidRow(
      
      column(5,
             tight_card(
               "Simulating Sample Means",
               HTML("<p>Consider that we have a box model with one '1' ticket and three '0' tickets. Each sample consists of 25 draws from the box, and we
                                         represent the sample using the mean of the tickets.</p>"),
               HTML("<center>"),
               grVizOutput(ns("box_model_n_25"), width = "50%", height = "50%"),
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
                                   inputId = ns("repeat_1_n_25"), label = HTML('<p>Repeat 1</p>'),
                                   class = "btn btn-primary", style="color: #fff;", width = "100%"
                                 ),
                                 HTML("<br><br>"),
                                 actionButton(
                                   inputId = ns("repeat_25_n_25"), label = HTML('<p>Repeat 25</p>'),
                                   class = "btn btn-primary", style="color: #fff;", width = "100%"
                                 ),
                          ),
                          column(5,
                                 actionButton(
                                   inputId = ns("repeat_10_n_25"), label = HTML('<p>Repeat 10</p>'),
                                   class = "btn btn-primary", style="color: #fff;", width = "100%"
                                 ),
                                 HTML("<br><br>"),
                                 actionButton(
                                   inputId = ns("repeat_100_n_25"), label = HTML('<p>Repeat 100</p>'),
                                   class = "btn btn-primary", style="color: #fff;", width = "100%"
                                 ),
                          ),
                        )
                 ),
                 column(4,
                        HTML("<br><center>"),
                        
                        # On click, resets the histogram
                        actionButton(
                          inputId = ns("reset_button_n_25"), label = HTML('<p>Reset</p>'),
                          class = "btn btn-danger", style="color: #fff;", width = "80%"
                        ),
                        HTML("</center><br>"),
                 )
               ),
               HTML("<br><br>"),
               HTML("<center>"),
               plotOutput(ns("histogram_frequencies_n_25"), width = "100%", height = "450px"),
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
               grVizOutput(ns("box_model_n_5"), width = "50%", height = "50%"),
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
                                   inputId = ns("repeat_1_n_5"), label = HTML('<p>Repeat 1</p>'),
                                   class = "btn btn-primary", style="color: #fff;", width = "100%"
                                 ),
                                 HTML("<br><br>"),
                                 actionButton(
                                   inputId = ns("repeat_25_n_5"), label = HTML('<p>Repeat 25</p>'),
                                   class = "btn btn-primary", style="color: #fff;", width = "100%"
                                 ),
                          ),
                          column(5,
                                 actionButton(
                                   inputId = ns("repeat_10_n_5"), label = HTML('<p>Repeat 10</p>'),
                                   class = "btn btn-primary", style="color: #fff;", width = "100%"
                                 ),
                                 HTML("<br><br>"),
                                 actionButton(
                                   inputId = ns("repeat_100_n_5"), label = HTML('<p>Repeat 100</p>'),
                                   class = "btn btn-primary", style="color: #fff;", width = "100%"
                                 ),
                          ),
                        )
                 ),
                 column(4,
                        HTML("<br><center>"),
                        
                        # On click, resets the histogram
                        actionButton(
                          inputId = ns("reset_button_n_5"), label = HTML('<p>Reset</p>'),
                          class = "btn btn-danger", style="color: #fff;", width = "80%"
                        ),
                        HTML("</center><br>"),
                 )
               ),
               HTML("<br><br>"),
               HTML("<center>"),
               plotOutput(ns("histogram_frequencies_n_5"), width = "100%", height = "450px"),
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
               grVizOutput(ns("custon_box_model"), width = "50%", height = "50%"),
               HTML("</center>"),
               HTML("<br>"),
               HTML("<p><b>Box contents:</b></p>"),
               radioButtons(
                 ns("custom_box_model_example_choice"),
                 NULL,
                 choices = c("Example 1 (Coin Flip)" = 1,
                             "Example 2 (1 in 4)" = 2,
                             "Example 3 (Dice)" = 3,
                             "Example 4 (Many Tickets)" = 4,
                             "Example 5 (Very Imbalanced)" = 5,
                             "Choose Your Own!" = 6)
               ),
               uiOutput(ns("custom_box_model_input_button")),
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
                          inputId = ns("simulate"), label = HTML('<p>Simulate</p>'),
                          class = "btn btn-primary", style="color: #fff;", width = "40%"
                        ),
                        HTML("</center>")
                 ),
                 column(6,
                        HTML("<center>"),
                        radioButtons(
                          ns("box_sum_or_mean"),
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
                        plotOutput(ns("histogram_frequencies_custom_n_5")),
                        plotOutput(ns("histogram_frequencies_custom_n_50")),
                        HTML("</center>")
                 ),
                 column(6,
                        HTML("<center>"),
                        HTML("<br>"),
                        plotOutput(ns("histogram_frequencies_custom_n_25")),
                        plotOutput(ns("histogram_frequencies_custom_n_100")),
                        HTML("</center>")
                 )
               ),
               uiOutput(ns("examples_additional_information")),
               header_colour = "#3179ae"
             )
      )
    ),
    
    HTML("<br><br><br>"),

  )
}