load_data_paired_sample_UI <- function(id, test_name = "1-sample z-test") {
  ns <- NS(id)
  tagList(
    
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
               uiOutput(ns("manual_entry_insufficient_unique_values"))
             )
      ),
      column(5,
             uiOutput(ns("radio_buttons_data_to_plot")),
             uiOutput(ns("data_upload_plot_section_output"))
      )
    )
    
  )
}
