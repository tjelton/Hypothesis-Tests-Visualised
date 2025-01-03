load_1_sample_data_UI <- function(id) {
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
               HTML("<p>In order to do a 1-sample z-test, we first need to proivde our ONE sample that we will be analysing. Below you can
                        choose pre-uploaded data to analysis, or you can manually enter data.</p>"),
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
               uiOutput(ns("numeric_column_seleciton_pre_uploaded_data")),
               uiOutput(ns("separate_categorical_variable_pre_uploaded_data")),
               uiOutput(ns("category_choice_pre_uploaded_data")),
               
               # These will output only for the case that the "manually_specified" option is selected.
               uiOutput(ns("manual_entry_insufficient_unique_values")),
               uiOutput(ns("manual_entry_missing_values_warning"))
             )
      ),
      column(5,
             uiOutput(ns("data_upload_plot_section_output"))
      )
    ),
  
  )
}
