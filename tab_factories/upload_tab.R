#'@details Create the upload tab
#' The upload tab can differ based on whether we are loading data from core-ms
#' If we are loading from core-ms, then we have sub-tabs for processing the
#' core-ms output.
#' 
upload_tab <- function(from_corems = FALSE) {
  if(from_corems) {
    navbarMenu("Core-MS Processing",
      tabPanel("tab1", HTML("This is a tab")),
      tabPanel("tab2", HTML("This is another tab"))
    )
  } else {
    tabPanel(div("Upload", icon('upload')), value = 'Upload',
             fluidRow(
               ## Sidebar panel on Upload tab ##
               column(width = 4,
                      bsCollapse(id = 'upload_collapse', open = c('file_upload'), multiple = TRUE,
                                 bsCollapsePanel(div('Upload two linked csv files', 
                                                     hidden(div(id = 'ok_files', style = 'color:deepskyblue;float:right', icon('ok', lib='glyphicon')
                                                     )
                                                     )
                                 ), value = 'file_upload',  
                                 
                                 # Load e_data file
                                 div(id = "js_file_edata",
                                     fileInput("file_edata", "Upload Data File (.csv)",
                                               multiple = TRUE,
                                               accept = c("text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv"))
                                 ),
                                 
                                 ## Get unique identifier column from e_data ##
                                 uiOutput('edata_id'),
                                 
                                 # Load e_meta file
                                 div(id = "js_file_emeta", fileInput("file_emeta", "Upload Molecular Identification File (.csv)",
                                                                     multiple = TRUE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")))
                                 ),
                                 bsCollapsePanel(div('Specify data structure', 
                                                     hidden(div(id = 'ok_idcols', style = 'color:deepskyblue;float:right', icon('ok', lib='glyphicon')
                                                     )
                                                     )
                                 ), value = 'column_info',  
                                 # Get which instrument generated the data #
                                 inlineCSS('#js_data_scale .filter-option{text-align:center;}'),
                                 div(id = "js_data_scale", pickerInput('data_scale', 
                                                                       label = 'On what scale are your data?',
                                                                       choices = list('Log base 2' = 'log2', 'Log base 10'='log10', 'Natural log'='log', 
                                                                                      'Presence/absence' = 'pres', 'Raw intensity'='abundance'), 
                                                                       selected = 'abundance'
                                    )
                                 ),
                                 div(
                                   id = "js_NA_value",
                                   uiOutput("NA_value_UI")
                                 ),
                                 
                                 tags$hr(style = "margin:20px 0px 20px 0px"),
                                 
                                 # Get whether formulas or elemental columns are included #
                                 div(id = "js_select", radioGroupButtons('select', 
                                                                         label = 'Does this file have formulas 
                                                                        or elemental columns?',
                                                                         choices = list('Formulas' = 1, 
                                                                                        'Elemental Columns' = 2),
                                                                         selected = 'Select an option', justified = TRUE) 
                                 ), 
                                 
                                 # (Conditional on the above selectInput) Formula: 
                                 ##  which column contains the formula? #
                                 conditionalPanel(
                                   condition = "input.select == 1", 
                                   uiOutput('f_column')
                                 ), 
                                 
                                 # (Conditional on the above selectInput) Elemental columns: 
                                 ##  which columns contain the elements?
                                 
                                 inlineCSS('#element_select button {width:100%}'),
                                 hidden(div(id = "element_select", style = 'width:92.5%;margin-left:2.5%;border-radius:4px',
                                            dropdownButton(inputId = "element_dropdown", circle = FALSE, label = "Specify Elemental Count Columns", width = '100%',
                                                           fluidRow(
                                                             column(width = 4,
                                                                    uiOutput("c_column"), 
                                                                    uiOutput("h_column")
                                                             ),
                                                             column(width = 4,
                                                                    uiOutput("n_column"),
                                                                    uiOutput("o_column") 
                                                             ),
                                                             column(width = 4,
                                                                    uiOutput("s_column"), 
                                                                    uiOutput("p_column")
                                                             )
                                                           )
                                            )
                                 )), #hidden div
                                 
                                 tags$hr(style = "margin:20px 0px 20px 0px"),
                                 
                                 # Create an option for Isotopic Analysis
                                 div(id = "js_isotope_yn", radioGroupButtons('isotope_yn',
                                                                             label = 'Were isotopic peaks identified in the molecular assignments file?',
                                                                             choices = list('Yes' = 1,
                                                                                            'No' = 2),
                                                                             selected = 'Select an Option', justified = TRUE)
                                 ),
                                 # Condition on presence of isotope information
                                 conditionalPanel(
                                   condition = "input.isotope_yn == 1",
                                   uiOutput("iso_info_filter_out"),
                                   div(id = "js_iso_info_column", uiOutput('iso_info_column_out')),
                                   div(id = "js_iso_symbol", uiOutput('iso_symbol_out'))
                                 )
                                 )
                      ),
                      tags$hr(),
                      
                      # Action button: pressing this creates the peakData object
                      div(
                        actionButton('upload_click', 'Process Data', icon = icon("cog"), lib = "glyphicon"),
                        hidden(div('Making data object, please wait...', id = 'upload_waiting', class='fadein-out', 
                                   style='font-weight:bold;color:deepskyblue;display:inline'))
                      ),
                      # Summary panel
                      hidden(div(id = 'upload_success', style = 'width:75%;margin-top:10px',
                                 wellPanel(style='border-radius:4px',
                                           # Show 'Success' message if peakData created successfully
                                           uiOutput('success_upload'),        
                                           
                                           # Number of peaks, samples, and peaks with formulas assigned
                                           textOutput('num_peaks'), 
                                           textOutput('num_samples'), 
                                           textOutput('num_peaks_formula')
                                 )
                      ))
                      
               ), # End sidebar panel
               
               column(width = 8,
                      # warnings panel
                      div(id = "warnings_upload", style = "overflow-y:auto;max-height:250px", uiOutput("warnings_upload_UI")),
                      
                      tags$hr(),
                      
                      # Show preview of e_data
                      htmlOutput('edata_text'), 
                      DTOutput("head_edata", width = "90%"),
                      
                      tags$hr(),
                      
                      # Show preview of e_meta
                      htmlOutput('emeta_text'),
                      DTOutput("head_emeta", width = "90%")
                      
               ) # End main panel
               
             ))
  }
}