load_data_2_sample_UI <- function(id, test_name = "1-sample t-test") {
  ns <- NS(id)
  tagList(
    
    ############ SECTION: Input Data ############
    fluidRow(
      column(7,
             tight_card(
               "Input Sample Data",
               HTML("<p>As the name suggests, in a 2-sample t-test, we need 2 samples. These 2 samples should be both measuring the same thing 
                                  (i.e, they need to have the same dependent variable). Before choosing the samples, we need to select our data set.</p>"),
               
               # User specifies whether they will use inbuilt data or manually specify data.
               radioButtons( 
                 inputId = ns("data_upload_choice"),
                 label = HTML("<p><b>What data source will you be using?</b></p>"), 
                 choices = list( 
                   "Pre-uploaded Data" = "pre_uploaded", 
                   "Manually Specified Data" = "manually_specified" 
                 ),
                 selected = NA
               ),
               
               # Mechanism for user to specify data depending on radio button option.
               uiOutput(ns("data_upload")),
               
               # These will output only for the case that the "pre_uploaded" option is selected.
               uiOutput(ns("categorical_variable_selection_pre_uploaded")),
               uiOutput(ns("sample_selection_pre_uploaded")),
               uiOutput(ns("dependent_variable_selection_pre_uploaded_data")),
               uiOutput(ns("pre_uploaded_warning_and_save_data")),
               
               
               # These will output only for the case that the "manually_specified" option is selected.
               uiOutput(ns("manual_entry_insufficient_unique_values")),
               header_colour = "#3179ae"
             )
      ),
      column(5,
             tight_card(
               NULL,
               uiOutput(ns("radio_button_plot_type")),
               uiOutput(ns("data_upload_plot_section_output"))
             )
      )
    )

    
  )
}
