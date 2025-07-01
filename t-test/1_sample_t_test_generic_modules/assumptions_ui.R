assumptions_1_sample_t_test_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    ############ SECTION: Assumptions ############
    fluidRow(
      column(12,
             tight_card(
               "Assumptions",
               HTML("<p>For the hypothesis test to be valid, we need to check the following assumptions:</p>"),
               
               accordion(
                 open = FALSE,
                 
                 # Assumption 1: Independent Samples
                 accordion_panel(
                   HTML("<b>Assumption 1: Independent and Randomly Chosen Sample</b>"),
                   HTML("<p>The first assumption is that our sample is <b>independent and randomly chosen</b>.</p>"),
                   HTML("<p><span style='color: blue;'><b>How do we check?</b></span> <i>We check by investigating the experimental setup.</i><br><br>
                    For example, consider we were investigating data for a proportion test involving human participants. We could read the accompanying scientific
                    publication to understand the methodology they used to gather the people in the sample.</p>")
                 ),
                 
                 # Assumption 2: Normality
                 accordion_panel(
                   HTML("<b>Assumption 2: Normality</b>"),
                   HTML("<p>The second assumption is that the sample means follow a normal distribution."),
                   HTML("<p><span style='color: blue;'><b>How do we check?</b></span><br>
                    <ul>
                      <li>Recall that the central limit theorem tells us that if we take a sufficiently large number of draws from the box, then the sample
                      means will approximately follow a normal distribution. If confused, please see the box model exercise.</li>
                      <li>One way to gauge whether the central limit theorem holds or not is to see how large our sample is (this is indicated by the \"n\" in
                      the box model above).</li>
                      <li>Many textbooks will say that you can say that you can use the rule of thumb that the central limit theorem will apply if we have 30
                      or more draws. BEWARE - this is not always true! If the distribution of the values is very skewed, you will need much more than 30 draws!</li>
                      <li>You should use a combination of the size of \"n\" and a histogram and boxplot of the sample distributions to help you determine whether
                      the central limit theorem means we can resonably approximate the sample means using a normal distribution.</li>
                    </ul>
                    </p>")
                 ),
               ),
               header_colour = "#3179ae"
             )
      )
    ),
  )
}
