load_data_regression_2_variable_UI <- function(id, test_name = "1-sample t-test") {
  ns <- NS(id)
  tagList(
    
    ############ SECTION: Input Data ############
    fluidRow(
      column(7,
             tight_card(
               "Input Sample Data",
               HTML("<p>In order to do a regression t-test, we need to specify the x-axis and y-axis for the linear model.</p>"),
               
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
               uiOutput(ns("manual_entry_unequal_samples")),
               uiOutput(ns("manual_entry_insufficient_unique_values")),
               header_colour = "#3179ae"
             )
      ),
      column(5,
             tight_card(
               NULL,
               uiOutput(ns("data_upload_plot_section_output"))
             )
      )
    ), 

    
  )
}
