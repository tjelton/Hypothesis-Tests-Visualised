boxModelPart3UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    ############ TITLE ############ 
    fluidRow(
      column(12,
             HTML("<h1>The Box Model Part 3 - Modelling Using a Normal Distribution</h1>"),
      )
    ),

    ############ SECTION: Lesson Prelude ############
    
    fluidRow(
      column(8,
            tight_card(
              "Modelling Using a Normal Distribution",
              HTML("<p>
                    In part 2, we learnt that the central limit theorem tells us that if we take a sufficiently large number of draws from a box, then the sample means (or sums) 
                    will follow a normal distribution. We also showed how you can verify whether the number of draws is sufficinetly large by repeatedly forming samples, taking 
                    the mean (or sum) of each, and observing whether the samples are normally distributed.
                    <br><br>
                    
                    Why do we care about whether the samples are normally distributed? If they are, we can start to answer probability-based questions.
                    <br><br>
                    
                    For example, consider we were interested in the probability of flipping 60 or more heads from 100 coins. To do this, we first construct a box model
                    with the ticket '1' which reperesents landing on a head, and the ticket '0' which represents landing on a tail. As we are flipping 100 coins, that is like
                    drawing from the box 100 times. We will represent this sample using the sum (this box model is picture to the right).
                    <br><br>
                    
                    Here is a very brief overview of how you can use the normal distribution to answer probability questions. The first step is to actually specify the normal
                    distribution which we are using to model the box model to the right. We do this by calculating the expected value (which acts as the mean for the normal
                    distribution) and standard error (which acts as the standard deviation for the normal distribution). Then, to actually find probabilities we are interested in
                    we use the normal distribution we just specified and find areas under the curve (the area gives the probability). For example, if we did want to find the 
                    probability that you flip 60 or more heads from 100 coins, we calculate the area under the curve above 60 (values to the right of 60 indicate more that
                    60 heads). We find that this area is 0.02275, meaning the chance of flipping 60 or more heads from 100 coins is 2.275%.
                    <br><br>
                    
                    This is the end of this quick introduction. It might all see a bit vague at the moment, but the steps below go into much more depth! The best way to learn 
                    more about this is to play around and experiment below with the 'Box Model Playground'. To help you learn, read below for suggested tasks.
                   </p>"),
              accordion(
                open = FALSE,
                accordion_panel(
                  HTML("<p><b>Suggested Exercise 1: Observing 70 Heads</b></p>"),
                  HTML("<p>
                       The first suggested exercise is to recreate the scenario above, but with a slight change. What is the probability if you flipped 100 coins that you observe
                       70 or more heads.
                       <br><br>
                       
                       To do this:
                       <ul>
                          <li>In the 'Box Parameters' mini-section, set the tickets and number of draws to match the box model to the right. Ensure that we are taking the sum
                          for the samples (you could take the mean if you want though, but you will have to change later steps).</li>
                          <li>Verify that n = 100 is a sufficient number of draws for the central limit theorem to apply for this box model.</li>
                          <li>Find the probability of observing a sum of 70 or more. To do this, set the lower boundary to 70, and upper boundary to ∞.</li>
                       </ul>

                       The answer is 3e-05, which means 0.00003 (0.003%).
                       <br><br>
                       
                       <b><span style='color: red;'>Extension Question:</span></b> What is the chance that you observed 40 to 70 heads (inclusive)?
                       </p>")
                ),
                accordion_panel(
                  HTML("<p><b>Suggested Exercise 2: Rolling a Dice (Part 1)</b></p>"),
                  HTML("<p>
                       You have one ice cream left in your house, and you and your brother desperately want it. Your brother turns to you and says \"Let's play a little game.
                       I want you to roll a 6-sided dice 50 times. Every time you roll it, write down the face that the die landed on, and at the end, add up the 50 numbers
                       you recorded from the 50 rolls. If the sum is greater than 150, then you can have the ice cream, and otherwise, I get to eat it.\" You agree to his little
                       game.
                       <br><br>
                       
                       To do this:
                       <ul>
                          <li>In the 'Box Parameters' mini-section, set the tickets to be the numbers 1 through 6, and the number of draws to be 50. Ensure that we are taking 
                          the sum for the samples (your brother asked you for the sum after all).</li>
                          <li>Verify that n = 50 is a sufficient number of draws for the central limit theorem to apply for this box model.
                          <li>Find the probability of observing a sum of 150 or more. To do this, set the lower boundary to 150, and upper boundary to ∞.</li>
                       </ul>

                       We won't give you the <b>EXACT</b> answer this time, but it should be above 90%. 
                       <br><br>

                       <b><span style='color: red;'>Extension Question:</span></b> For the game to be fairer, there should be a 50% change that either you or your brother could 
                       win. Instead of 150, what should the sum be changed to?
                       </p>")
                ),
                accordion_panel(
                  HTML("<p><b>Suggested Exercise 2: Rolling a Dice (Part 2)</b></p>"),
                  HTML("<p>
                         Next week, you and your brother are in the same predicment. One ice cream left - who gets it? Your brother proposes a new game. You roll a 6-sided dice
                         50 times, and take the sums like last time. If the sum is between 145 and 175 (inclusive), you get the ice cream. Otherwise, your brother gets it. What
                         is the probability that you get the ice cream?
                         </p>")
                )
              ),
              header_colour = "#3179ae"
            )
      ),
      column(4,
             tight_card(
               NULL,
               HTML("<center>"),
               grVizOutput(ns("example_coin_flip"), width = "80%", height = "100%"),
               HTML("</center>")
             )
      )
    ),
    
    HTML("<br>"),
    
    
    fluidRow(
      column(12,
             HTML("<center><h2><u>Box Model Playground</u></h2></center>"),
      )
    ),
    
    ############ SECTION: Setting up the Box Model ############
    fluidRow(
      column(7,
             tight_card(
               "Box Parameters",
               HTML("<p>First we need to create the box. Follow steps 1, 2, and 3 to configure the box to be whatever you wish to model.
                      The box will output on the right according to the settings that you pick.</p><br>"),
               
               accordion(
                 open = FALSE,
                 
                 # Step 1: Enter tickets.
                 accordion_panel(
                   HTML("<b>Step 1) Tickets</b>"),
                   "Step 1) Tickets",
                   HTML("<p>First, we need to specify the tickets that we will be adding to the box.</p>"),
                   HTML("<p>In the text box below, enter the tickets that you wish to place into the box. Only <i>numbers</i> can be
                               added, and <i>each ticket should be seperated by a comma</i>. For example, if you want to have 1x1 ticket
                               and 1x0 tickets in the box, enter <i>1,0</i>."),
                   textAreaInput(
                     ns("box_tickets_text_entry"),
                     NULL,
                     value = "1,0",
                     width = "100%"
                   ),
                   fluidRow(
                     column(8),
                     column(4,
                            actionButton(ns("submit_tickets"), "Set Tickets", class = "btn btn-success", style="color: #fff;", width = "100%")
                     )
                   ),
                   uiOutput(ns("tickets_text_error_message"))
                 ),
                 
                 # Step 2: Set n
                 accordion_panel(
                   HTML("<b>Step 2) Number of Draws</b>"),
                   HTML("<p>Second, we need to specify the number of draws (with replacement) that we will be taking from the box:</p>"),
                   numericInput(
                     ns("number_of_draws"),
                     label = NULL,
                     value = 100,
                     min = 1
                   ),
                   uiOutput(ns("number_of_draws_error_message"))
                 ),
                 
                 # Step 3: Model Using Sum or Mean
                 accordion_panel(
                   HTML("<b>Step 3) Model Using Sum or Mean</b>"),
                   HTML("<p>Third, we need to specify whether we will be modelling the sample using the mean or sum of the draws:<br><br></p>"),
                   radioButtons(
                     ns("box_sum_or_mean"),
                     label = NULL,
                     choices = list(
                       "Sum" = 1,
                       "Mean" = 2
                     )
                   )
                 )
               ),
               header_colour = "#3179ae"
             ),
      ),
      
      column(5,
             # Box model output.
             tight_card(
               NULL,
               HTML("<center>"),
               grVizOutput(ns("box_model"), width = "70%", height = "70%"),
               HTML("</center>")
             ),

             # Mean and SD of the box output.
             tight_card(
               NULL,
               uiOutput(outputId = ns("box_statistics"))
             )
      )
    ),
    
    HTML("<br><br><br>"),

    ############ SECTION: Checking Central Limit Theorem ############
    fluidRow(
      column(5,
             tight_card(
               "Central Limit Theorem",
               uiOutput(outputId = ns("CLT_text_instructions_output")),
               HTML("<br>"),
               fluidRow(
                 column(1),

                 # Button for repeating adding the mean or sample sum to the histogram.
                 column(5,
                        actionButton(
                          inputId = ns("repeat_1"), label = HTML('<i class="fa fa-plus"></i> Repeat 1'),
                          class = "btn btn-primary", style="color: #fff;", width = "100%"
                        ),
                        HTML("<br><br>"),
                        actionButton(
                          inputId = ns("repeat_25"), label = HTML('<i class="fa fa-plus"></i> Repeat 25'),
                          class = "btn btn-primary", style="color: #fff;", width = "100%"
                        ),
                 ),
                 column(5,
                        actionButton(
                          inputId = ns("repeat_10"), label = HTML('<i class="fa fa-plus"></i> Repeat 10'),
                          class = "btn btn-primary", style="color: #fff;", width = "100%"
                        ),
                        HTML("<br><br>"),
                        actionButton(
                          inputId = ns("repeat_100"), label = HTML('<i class="fa fa-plus"></i> Repeat 100'),
                          class = "btn btn-primary", style="color: #fff;", width = "100%"
                        ),
                 ),
               ),
               HTML("<br><center>"),

               # On click, resets the histogram
               actionButton(
                 inputId = ns("reset_button"), label = HTML('<i class="fa fa-redo"></i> Reset'),
                 class = "btn btn-danger", style="color: #fff;"
               ),
               HTML("</center><br>"),
               header_colour = "#3179ae"
             ),

      ),

      # Histogram Distribution
      column(7,
             tight_card(
               NULL,
               HTML("<center>"),
               plotOutput(ns("histogram_frequencies"), width = "100%", height = "450px"),
               HTML("</center>")
             ),
             tight_card(
               NULL,
               uiOutput(outputId = ns("CLT_satisfied_text")),
             ),
      )

    ),

    HTML("<br><br><br>"),

    ############ SECTION: Modelling Using the Normal Curve ############
    fluidRow(
      column(5,
             tight_card(
                 "Modelling Using a Normal Distribution",
                 uiOutput(ns("normal_distribution_text")),
                 header_colour = "#3179ae"
              )
      ),
      column(7,
             tight_card(
               NULL,
               HTML("<center>"),
               plotOutput(ns("normal_curve_model"), width = "100%", height = "450px"),
               HTML("</center>")
             )
      )
    ),

    HTML("<br><br><br>"),
    
    ############ SECTION: Finding Probabilities ############
    fluidRow(

      column(6,
             tight_card(
                 "Finding Probabilities",
                 uiOutput(ns("finding_probabilities_text")),
                 HTML("<br>"),
                 fluidRow(
                   column(1),
                   column(4,
                      HTML("<center><p><b>Lower Boundary</b></p></center>"),
                      checkboxInput(ns("lower_boundary_infinity"), HTML(paste("<p>", withMathJax("\\(-\\infty\\)"),"</p>")), FALSE),
                      conditionalPanel(
                        condition = paste0('input[\'', ns('lower_boundary_infinity'), "\'] == false"),
                        #condition = "input.lower_boundary_infinity == false",
                        numericInput(
                          ns("lower_boundary_numeric"),
                          label = NULL,
                          value = 0
                        ),
                      ),
                   ),
                   column(2),
                   column(4,
                      HTML("<center><p><b>Upper Boundary</b></p></center>"),
                      checkboxInput(ns("upper_boundary_infinity"), HTML(paste("<p>", withMathJax("\\(\\infty\\)"),"</p>")), FALSE),
                      conditionalPanel(
                        condition = paste0('input[\'', ns('upper_boundary_infinity'), "\'] == false"),

                        #condition = "input.upper_boundary_infinity == false",
                        numericInput(
                          ns("upper_boundary_numeric"),
                          label = NULL,
                          value = 1
                        ),
                      ),
                   ),
                   column(1)
                 ),
                 uiOutput(ns('interval_error_message')),
                 header_colour = "#3179ae"
             )
      ),
      column(6,
             tight_card(
               NULL,
               HTML("<center>"),
               plotOutput(ns("shaded_normal_curve"), width = "100%", height = "350px"),
               HTML("</center>")
             ),
             tight_card(
               NULL,
               uiOutput(ns("probability_answer_text"))
             ),

      )

    ),
    
    HTML("<br><br><br>"),
    
  )
}