boxModelPart1UI <- function(id) {
  ns <- NS(id)
  tagList(
    
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
               grVizOutput(ns("example_coin_flip_1"), width = "85%", height = "80%"),
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
                        grVizOutput(ns("example_coin_flip_2"), width = "70%", height = "80%"),
                        HTML("<br><br>"),
                        actionButton(
                          inputId = ns("simulate_coin_flip_1"), label = HTML('<i class="fa fa-plus"></i>Simulate'),
                          class = "btn btn-success", style="color: #fff;", width = "50%"
                        ),
                        HTML("<br><br><br>"),
                        HTML("</center>"),
                        
                 ),
                 column(6,
                        grVizOutput(ns("simulated_coin_flip_samples"))
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
                 column(6, grVizOutput(ns("single_sample_words"), height = "80%")),
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
                 column(6, grVizOutput(ns("single_sample_numbers"), height = "80%")),
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
                          inputId = ns("demonstration_sum_or_mean"), 
                          label = "Model using sum or mean:", 
                          choices = c("Sum", "Mean")
                        ),
                 )
               ),
               
               
               fluidRow(
                 column(6,
                        HTML("<center>"),
                        HTML("<br>"),
                        grVizOutput(ns("coin_flip_numeric_box_model"), width = "70%", height = "80%"),
                        HTML("<br><br>"),
                        actionButton(
                          inputId = ns("simulate_coin_flip_2"), label = HTML('<i class="fa fa-plus"></i>Simulate'),
                          class = "btn btn-success", style="color: #fff;", width = "50%"
                        ),
                        HTML("<br><br><br>"),
                        HTML("</center>"),
                        
                 ),
                 column(6,
                        grVizOutput(ns("simulated_coin_flip_samples_numbers"))
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
                     ns("box_tickets_text_entry"),
                     NULL,
                     value = "1,0,0,0,0,0",
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
                     value = 20,
                     min = 1
                   ),
                   uiOutput(ns("number_of_draws_error_message"))
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
                          inputId = ns("box_sum_or_mean"), 
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
                        grVizOutput(ns("box_model"), width = "70%", height = "70%"),
                        HTML("<br><br>"),
                        actionButton(
                          inputId = ns("simulate_your_own_box_model_button"), label = HTML('<i class="fa fa-plus"></i>Simulate'),
                          class = "btn btn-success", style="color: #fff;", width = "50%"
                        ),
                        HTML("<br><br><br>"),
                        HTML("</center>"),
                        
                 ),
                 column(6,
                        grVizOutput(ns("simulated_your_own_box_model_values"))
                 )
               ),
               header_colour = "#3179ae"
             )
      )
    ),
    
    HTML("<br><br><br>"),

  )
}