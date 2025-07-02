load_data_paired_sample_UI <- function(id, test_name = "1-sample t-test") {
  ns <- NS(id)
  tagList(
    
    ############ SECTION: Input Data ############
    fluidRow(
      column(7,
             tight_card(
               "Input Sample Data",
               HTML("<p>In a paired t-test, we are dealing with paired data. This is where each element in our study has a measurment recorded under two different
               conditions. We can then find the difference between the paired data, to reduce the values into a single data sample. But before doing this, we first 
               have to choose a data set to analyse.</p>"),
               HTML("<br>"),
               
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
               uiOutput(ns("numeric_column_selection_pre_uploaded_data")),
               uiOutput(ns("pre_uploaded_warning_and_save_data")),
               
               # # These will output only for the case that the "manually_specified" option is selected.
               uiOutput(ns("manual_entry_unequal_samples")),
               uiOutput(ns("manual_entry_insufficient_unique_values")),
               header_colour = "#3179ae"
             )
      ),
      column(5,
             tight_card(
               NULL,
               uiOutput(ns("radio_buttons_data_to_plot")),
               uiOutput(ns("data_upload_plot_section_output"))
             )
      )
    )
    
  )
}
