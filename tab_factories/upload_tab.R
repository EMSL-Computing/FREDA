#'@details Create the upload tab
#' The upload tab can differ based on whether we are loading data from core-ms
#' If we are loading from core-ms, then we have sub-tabs for processing the
#' core-ms output.
#' 
upload_tab <- function(from_corems = FALSE) {
  if(from_corems) {
    corems_tabs() 
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
                      div(id = "warnings_upload", style = "overflow-y:auto;max-height:250px", uiOutput("warnings_upload")),
                      
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

corems_tabs <- function() {
  navbarMenu("Core-MS Processing",
    tabPanel(
      "Create CoreMS Object",
      value = "CoreMS-create",
      fluidRow(
        ## Sidebar panel on Upload tab ##
        column(width = 4,
               bsCollapse(
                 id = 'corems-upload-collapse', open = c("input_args"), multiple = TRUE,
                 bsCollapsePanel(
                   value = "input_args",
                   title = "Specify Column Names",
                   div(id = 'specify_colnames',         
                       uiOutput("index_cname"),
                       uiOutput("obs_mass_cname"),
                       uiOutput("calc_mass_cname"),
                       uiOutput("pheight_cname"),
                       uiOutput("error_cname"),
                       uiOutput("conf_cname"),
                       uiOutput("file_cname"),
                       uiOutput("mono_index_cname"),
                       uiOutput("mf_cname"),
                       uiOutput("c13_cname"),
                       uiOutput("o18_cname"),
                       uiOutput("n15_cname"),
                       uiOutput("s34_cname")
                   ) # end div
                 ) # end Collapse Panel
               ), # end bsCollapse
               
               shiny::actionButton("make_cmsdata", 
                                   "Create CoreMSData Object", 
                                   icon = icon("cog"),
                                   lib = "glyphicon")
        ), # end sidebar column
        
        # main panel 
        column(width = 8,
           bsCollapse(
             id = "corems-upload-summary-collapse", 
             open = c("corems-upload-table", "corems-upload-visualize"), 
             multiple = TRUE,
             bsCollapsePanel(
               title = "Table Summary", value = "corems-upload-table",
               # keeps table compact on page, no line wrapping:
               tags$head(tags$style("#raw_data  {white-space: nowrap;  }")),
               DT::dataTableOutput("cms_raw_data") 
             ),
             bsCollapsePanel(
               title = "Plot Summary", value = "corems-upload-visualize",
               withSpinner(plotlyOutput("cmsdat_plot"), color = "deepskyblue", type = 8) 
             )
           )
        ) # end main column
      ) # end fluidRow
    ),
    ###################### Confidence Filter Panel ######################
    tabPanel(
      "Confidence Filter",
      value = "CoreMS-conf-filter",
      fluidRow(
        # sidebar column
        column(width = 4,
          bsCollapse(id = "filter_collapse", open = c("conf_thresh"), multiple = TRUE,
            bsCollapsePanel(
              title = "Select Confidence Threshold",
              value = "conf_thresh",
              
              sliderInput(inputId = "min_conf",
                         label = "Minimum confidence score:",
                         min = 0,
                         max = 1,
                         value = .5)
            ) # end collapse panel
          ), # end collapse 
          
          shiny::actionButton("apply_conf_filter", 
                              "Filter Data", 
                              icon = icon("cog"),
                              lib = "glyphicon"),
          
          shiny::actionButton("reset_filter", 
                              "Reset Filter", 
                              icon = icon("trash"),
                              lib = "glyphicon")
              
        ), # end sidebar column
       
        column(width = 8,
              DT::dataTableOutput("filt_peaks_dt"),
              plotlyOutput("me_plot"),
              plotlyOutput("cms_filt_plot")
              
        ) # end main column
      ) # end fluidRow
    ), # end conf filter tabPanel
    
    ###################### Unique Formula Assingment Panel ######################
    tabPanel(
      "Formula Assignment",
      value = "CoreMS-formula-assign",
      fluidRow(
        # sidebar column
        column(width = 4,
          bsCollapse(id = 'unq_mf_collapse', open = "unq_mf_assign", multiple = TRUE,
             bsCollapsePanel(title = "Unique Molecular Formula Assignment", 
                             value = "unq_mf_assign",
                             
                             selectInput("unq_mf_method", label = "Method:",
                                         choices = c("Select Method", "Confidence score", "Peak height"))
             ) # end collapse panel
          ), # end collapse
          shiny::actionButton("unique_mf", 
                              "Assign Unique Formula", 
                              icon = icon("cog"),
                              lib = "glyphicon")
              
        ), # end sidebar column
        
        # main column
        column(width = 8,
              plotlyOutput("mf_plot")
        ) # close main column
      ), # close fluidrow
      uiOutput("corems_to_peakdata_UI")
    ) # close unique mf tabPanel
  )
}