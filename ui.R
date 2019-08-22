#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(plotly)
library(shiny)
library(shinyBS)
library(shinyjs)
library(DT)
library(shinycssloaders)
library(shinyWidgets)

# Define UI and wrap everything in a taglist that first calls useShinyjs()
shinyUI(tagList(useShinyjs(),
                navbarPage(title = (windowTitle = "FREDA"),
                   id = "top_page",
                   theme = "yeti.css",
                   ############# Welcome Panel #########################
                   navbarMenu("Welcome",
                              tabPanel(title = "Introduction", class = "background_FTICR",
                                         includeMarkdown("./Welcome to FREDA.md"),
                                          hr(),
                                          div(style = "z-index:1000;position:absolute",
                                            bsButton('all_tutorials', 'See a playlist of video tutorials', 
                                                     onclick = "window.open('https://www.youtube.com/watch?v=uU5Q7r_pEGM&list=PLvozcBqO8i7wsMWo5PnOREX0sHSk3mAjE', '_blank')", 
                                                     style = 'info', icon = icon('facetime-video', lib = 'glyphicon'))
                                          )
                                       ),
                              tabPanel(title = "Data Requirements", class = "background_FTICR", value = 'data_requirements',
                                       includeMarkdown("./DataRequirements.md"),
                                       div(downloadButton('downloadData', 'Download'), style = "z-index:1000;position:absolute")),
                              tabPanel(title = "Resources", class = "background_FTICR",
                                       HTML('<h4> Resources </h4>')),
                              tabPanel(title = "Contact", class = "background_FTICR",
                                       HTML('<h4> Contact </h4>'))
                   ),
                   ################## Upload Panel #######################################
                   tabPanel("Upload",
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
                                        fileInput("file_edata", "Upload CSV Data File",
                                                  multiple = TRUE,
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv"))
                                        ),
                                    
                                    ## Get unique identifier column from e_data ##
                                    uiOutput('edata_id'),
                                    
                                    # Load e_meta file
                                    div(id = "js_file_emeta", fileInput("file_emeta", "Upload CSV Molecular Identification File",
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
                                    inlineCSS(list('#instrument .btn-default, #select .btn-default' = 'font-weight:lighter;',
                                              '#js_data_scale .filter-option'='text-align:center;')),
                                    radioGroupButtons('instrument', 
                                                label = 'What instrument generated this data?',
                                                choices = list('12T or 15T' = '12T', '21T'), 
                                                selected = '12T', justified = TRUE
                                    ), 
                                    div(id = "js_data_scale", pickerInput('data_scale', 
                                                      label = 'On what scale are your data?',
                                                      choices = list('Log base 2' = 'log2', 'Log base 10'='log10', 'Natural log'='log', 
                                                                     'Presence/absence' = 'pres', 'Raw intensity'='abundance'), 
                                                      selected = 'abundance'
                                      )
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
                                      dropdownButton(inputId = "element_dropdown", circle = FALSE, label = "Specify Elemental Count Columns",
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
                              
                            )), # End Upload tab
                   
                   ################## Groups Panel ###############################################
                   tabPanel("Groups",
                          fluidRow(style = "display:flex;flex-direction:row;align-items:stretch",
                            column(4,
                              wellPanel(style = "height:100%",
                                    tags$h4("Define a Group"),
                                    div(id = "js_group_name", textInput("group_name", "Name of this group:")),
                                    uiOutput("group_samples"),
                                    actionButton("add_group", "Add this group"),
                                    br(),
                                    br(),
                                    uiOutput("warnings_groups")
                                )
                            ),
                            column(8,
                              wellPanel(style = "height:100%",
                                dataTableOutput("group_table"),
                                actionButton("remove_group", "Remove selected group")
                              )
                            )
                          ),
                          hr(),
                          actionButton("goto_preprocess_main", "Continue to preprocess tab")
                   ),
                   ################## Preprocess Panel ###############################################
                     tabPanel("Preprocess",
                              
                              sidebarLayout(
                                
                                # Sidebar panel
                                sidebarPanel(
                                  
                                  div(class = "adjustdown",uiOutput("which_calcs")),
                                  
                                  # Action button: add test columns with results to peakData2
                                  div(
                                    disabled(actionButton('preprocess_click', 'Process Data', icon = icon("cog"), lib = "glyphicon")),
                                    hidden(div('Calculating values, please wait...', id = 'preprocess_waiting', 
                                               style = 'font-weight:bold;color:deepskyblue;display:inline', class = 'fadein-out'))
                                    ),
                                  br(),
                                  uiOutput("warnings_preprocess")
                                ), # End sidebar panel
                                
                                mainPanel(
                                  
                                  # Set default main panel width 
                                  width = 8,
  
                                  # Include numeric and categorical summaries in a well panel
                                  
                                  wellPanel(
                                    tags$div(class = "row",
                                             tags$div(class = "col-sm-5", style = "height:350px;overflow-y:scroll;",
                                                      uiOutput("numeric_header"),
                                                      dataTableOutput('numeric_summary')
                                             ),
                                             tags$div(class = "col-sm-7", style = "height:350px;overflow-y:scroll;",
                                                      uiOutput("cat_header"),
                                                      uiOutput('categorical_summary')
                                                      
                                            )
  
                                    )
                                  ),
                                  
                                  # Drop down list: which histogram should be displayed?
                                  uiOutput('which_hist_out'),
                                  
                                  # Plot: histogram
                                  plotlyOutput('preprocess_hist')
                                  
                                ) # End main panel on Preprocess tab #
                                
                              )), # End Preprocess tab #
                   
                   ##################### QUALITY CONTROL PANEL ###########################
                   tabPanel("Quality Control",
                            fluidRow(style = 'display:flex;flex-direction:row;align-items:stretch',
                                    column(4,
                                      wellPanel(style = 'height:100%',
                                              uiOutput('qc_select_groups', style = "width:50%"),
                                              hr(style='margin:2px'),
                                              uiOutput('qc_plot_scale', style = "width:50%"),
                                              textInput('qc_boxplot_xlab', "X-axis"),
                                              textInput('qc_boxplot_ylab', 'Y-axis'),
                                              textInput('qc_boxplot_title', 'Title'),
                                              actionButton('update_boxplot_axes', "Update Boxplot Axes"),
                                              br(), br(),
                                              uiOutput('warnings_qc')
                                      )
                                    ),
                                    column(8,
                                      wellPanel(style = 'height:100%',
                                           div(id='style_qc_boxplots', style='border-style:solid;border-width:1px;padding-top:5px', 
                                               plotlyOutput("qc_boxplots") %>% withSpinner(color = "orange", type = 8)
                                               ),
                                           br(),
                                           div(style = 'margin-left:25%',
                                               actionButton(inputId = "add_qc_boxplot", width = '66%', 
                                                            label = "Save This Plot for Later Download", 
                                                            icon = icon("save")),
                                               hidden(div(id = 'qc_download_ok', style = 'float:right',icon('ok', lib='glyphicon'), "Current plot saved"))
                                            )
                                           )
                                    )
                            ),
                            hr(),
                            actionButton('goto_filter_fromqc', "Continue to the filter tab")
                    ),

                   ################## Filter Panel ##############################################
                   tabPanel("Filter",
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                # Set default width for panel
                                width = 5,

                                # Checkbox: Mass filter yes/no
                                bsCollapse(id = 'filter_sidebar', 
                                           open = c('samplefilt_collapse', 'massfilt_collapse', 'formfilt_collapse', 'molfilt_collapse'), 
                                           multiple = TRUE,
                                  bsCollapsePanel(div('Sample Filter', 
                                                      div(style = "color:deepskyblue;display:inline-block",
                                                           tipify(icon("question-sign", lib = "glyphicon"), 
                                                                  title = "Retain a subset of all samples", 
                                                                  placement = "top", trigger = 'hover')
                                                        ),
                                                      div(style = 'float:right', uiOutput('samplefilter_icon'))
                                                      ), value = 'samplefilt_collapse',
                                    div(class="adjustdown", 
                                        checkboxInput('samplefilter', tags$b("Apply this filter, keeping only the following samples:", style = "display:inline-block"), value = FALSE)
                                    ),
                                    
                                    div(id = "js_filter_samples", 
                                        uiOutput("filter_samples")
                                        )
                                  ),
                                  bsCollapsePanel(div('Mass Filter',
                                                    div(style = "color:deepskyblue;display:inline-block",
                                                                      tipify(icon("question-sign", lib = "glyphicon"), 
                                                                             title = "Retain peaks within a mass range specified below", 
                                                                             placement = "top", trigger = 'hover')
                                                                    ),
                                                    div(style = 'float:right', uiOutput('massfilter_icon'))
                                                  ), value = 'massfilt_collapse',
                                    div(class="adjustdown", checkboxInput('massfilter', tags$b("Apply this filter, removing peaks outside the mass range:", style = "display:inline-block"), value = FALSE)
                                    ),
  
                                    # Numeric: Min/max mass filter
                                    splitLayout(
                                      numericInput('min_mass', 'Minimum Mass value', 
                                                   min = 0, value = 200),
                                      numericInput('max_mass', "Maximum Mass value", 
                                                   min = 0, value = 900)
                                    )
                                  ),
                                 
                                  bsCollapsePanel(div('Molecule Filter', 
                                                      div(style = "color:deepskyblue;display:inline-block", 
                                                               tipify(icon("question-sign", lib = "glyphicon"), 
                                                                      title = "Retain peaks that are observed in a minimum number of samples, specified below", 
                                                                      placement = "top", trigger = 'hover')
                                                          ),
                                                      div(style = 'float:right', uiOutput('molfilter_icon'))
                                                      ),
                                                  value = 'molfilt_collapse', 
                                      # Checkbox: Mass filter yes/no
                                      div(class = "adjustdown", checkboxInput('molfilter', tags$b("Apply this filter, removing peaks with too few observations: ", style = "display:inline-block"), value = FALSE)
                                      ),   
                                      # Drop-down list: Min/max mass filter
                                      uiOutput('minobs')
                                   ),
                                  bsCollapsePanel(div('Formula Presence Filter',
                                                      div(style = "color:deepskyblue;display:inline-block",
                                                          tipify(icon("question-sign", lib = "glyphicon"), 
                                                                 title = "Retain peaks that have a molecular formula specified or calculated from elemental values", 
                                                                 placement = "top", trigger = 'hover')
                                                          ),
                                                      div(style = 'float:right', uiOutput('formfilter_icon'))
                                                      ),
                                                  value = 'formfilt_collapse',
                                    div(class = "adjustdown", checkboxInput('formfilter', tags$b("Apply this filter, removing peaks without a molecular formula", style = "display:inline-block"), value = FALSE)
                                    )
                                   ),
                                   
                                  bsCollapsePanel(div('Implement up to 3 custom filters', 
                                                      div(style = "color:deepskyblue;display:inline-block",
                                                          tipify(icon("question-sign", lib = "glyphicon"), 
                                                                 title = "Filter based on up to 3 variables in the post-processed molecular identification file", 
                                                                 placement = "top", trigger = 'hover')
                                                        ),
                                                      div(style = 'float:right', uiOutput('customfilter_icon'))
                                                      ), 
                                                  value = 'customfilt_collapse',
                                    div(class = "adjustdown", checkboxInput('customfilterz', tags$b("Show dropdowns and apply all specified filters", style = "display:inline-block"), value = FALSE)
                                    ),
                                  
                                    conditionalPanel(id = "custom_cond_panel", condition = "input.customfilterz == true",
                                                       uiOutput("filter1UI"),
                                                       uiOutput("customfilter1UI"),  
                                                       uiOutput("filter2UI"),
                                                       uiOutput("customfilter2UI"),
                                                       uiOutput("filter3UI"),
                                                       uiOutput("customfilter3UI")
                                                     )
                                  )
                                ), # end collapse panel
                                hr(),
                                
                                disabled(
                                  fluidRow(
                                    column(
                                      width = 6, actionButton('filter_click', "Filter Data", icon = icon("cog", lib = "glyphicon"))
                                    ),
                                    column(
                                      width = 6, actionButton('reset_filters', "Reset Filters", icon = icon("trash", lib = "glyphicon"))
                                    )
                                  )
                                ),
                                
                                br(),
                                br(),
                                
                                div(id = "warnings_filter", style = "overflow-y:auto;max-height:150px", uiOutput("warnings_filter"))
                              
                              ), # End sidebar panel on Filter tab
                              
                              mainPanel(
                                
                                # Set default width for panel
                                width = 7,
                                
                                # waiting messages for large data during filtering and plot calculation
                                hidden(div('Applying your filters, please wait...', id = 'calc_filter', class = 'fadein-out', 
                                           style = 'color:deepskyblue;font-weight:bold;margin-bottom:5px')),
                                hidden(div('Drawing your plot, please wait...', id = 'draw_large_filter_plot', class = 'fadein-out', 
                                           style = 'color:deepskyblue;font-weight:bold;margin-bottom:5px')),
                                
                                # Summary panel: display summary of filters
                                wellPanel(
                                  tableOutput('summary_filter')
                                ),
                                
                                # Plot: Show number of peaks before/after filters applied
                                plotOutput('barplot_filter')
                              ) # End main panel on Filter tab
                              
                            )), # End Filter tab
                   
                   ################## Visualize Panel ###############################################
                   tabPanel("Visualize",
                            
                            fluidRow(
                              # Sidebar Panel
                              div(id='viz_sidebar', column(4,
                                
                                # Begin collapsible section                           
                                bsCollapse(id='viz_sidebar', open = c('peakplots', 'axlabs'), multiple=TRUE, 
                                  
                                  # Plot Parameters
                                  bsCollapsePanel(div('Construct a plot', div(style = 'float:right', uiOutput('chooseplots_icon'))), value = 'peakplots',
                                    # Select Plot Type
                                    inlineCSS("#chooseplots .btn{font-size:10.5pt;} #chooseplots .btn-group-container-sw{display:block;}" ),
                                    uiOutput('plot_type', style = "margin-top:-10px"),     

                                    # Select samples/groups
                                    uiOutput('plotUI'),
                                    
                                    uiOutput('pcoa_dist'),
                                    
                                    uiOutput('viztab_select_groups'),
                                    
                                    # Single dropdown for 1 sample/group or....
                                    hidden(div(id = "js_toggle_single", uiOutput("plotUI_single"))),
                                    
                                    # ...two dropdowns and extra options for group comparison
                                    hidden(div(id = "js_toggle_groups", 
                                                        tagList(div(id = "js_whichGroups1", uiOutput("plotUI_comparison_1")), 
                                                                div(id = "js_whichGroups2", uiOutput("plotUI_comparison_2"))
                                                                )
                                                        )
                                                    ),
                                    
                                    hidden(div(id = 'js_summary_fxn', uiOutput("summary_fxn_out", class = "adjustdown")
                                               )
                                           )
                                    
                                  ),
                                  # Axes Options
                                  bsCollapsePanel(div('Axes labels', div(style = 'float:right', uiOutput('axlabs_icon'))), value = 'axlabs',
                                      splitLayout(
                                        uiOutput("title_out"),
                                        tags$div(id = "js_legend_title_input", uiOutput("legend_title_out"))
                                        ),
                                      splitLayout(
                                        uiOutput("x_axis_out"),
                                        uiOutput("y_axis_out")
                                      )
                                  ),
                                  bsCollapsePanel(div('Save plot and view saved plots', div(style = 'float:right', uiOutput('saveplots_icon'))), value = 'downloads',
                                      div(id = "saveplot_ui", style = 'width:90%;padding-left:5%;',
                                            div(disabled(actionButton(inputId = "add_plot", width = '100%', label = "Save Current Plot for Later Download", icon = icon("save")))),
                                            div(style = 'margin-top:4px', uiOutput("view_plots"))
                                      )
                                  )
                                ),
                                      
                                    
                                # Seperate buttons to generate plot or simply update labels without recalculating data
                                
                                disabled(
                                  fluidRow(
                                    column(width = 6,
                                           actionButton("plot_submit", label = "Generate Plot", icon = icon("plus"), lib = "glyphicon")
                                    ),
                                    column(width = 6,
                                           actionButton("update_axes", label = "Update Labels", icon = icon("refresh"), lib = "glyphicon")
                                    )
                                  )
                                ),
                                
                                br(),
                                br(),
                                
                                div(id = "warnings_visualize", style = "overflow-y:auto;max-height:150px", uiOutput("warnings_visualize"))
                                
                              )),# End sidebar conditionals on Visualize tab #
                              
                              # Plot panel and axes/color controls.
                              column(8,
                                inlineCSS("#FxnPlot {width:inherit;}"),
                                wellPanel(style = "margin-top:-10px",
                                      div(style = "display:inline-block;outline-style:dotted;outline-width:thin;width:65%;padding:4px",
                                        plotlyOutput('FxnPlot', width = 'auto', height = '600px') %>% 
                                          withSpinner(color = "orange", type = 8)
                                      )
                                ),

                                # color and van-krevelen bounds dropdowns
                                fluidRow(
                                  column(width = 4, class = "grey_out", id = "js_vk_colors",
                                         disabled(selectInput("vk_colors", "Color by:", choices = NULL, selected = NULL))
                                         ),
                                  column(width = 4, class = "grey_out", id = "js_vkbounds",
                                         disabled(selectInput('vkbounds', 'Display Van Krevelen boundary set:',
                                                     choices = c('BS1' = 'bs1', 'BS2' = 'bs2', 'None' = 0),
                                                     selected = 'bs1'))
                                         )
                                ),
                                
                                # x and y axis variable dropdowns for custom scatter plot
                                fluidRow(
                                  column(width = 4, class = "grey_out", id = "js_scatter_x",
                                         disabled(selectInput("scatter_x", "Horizontal axis variable:", choices = NULL, selected = NULL))
                                        ),
                                  column(width = 4, class = "grey_out", id = "js_scatter_y",
                                         disabled(selectInput("scatter_y", "Vertical axis variable:", choices = NULL, selected = NULL))
                                        )
                                ),
                                
                                # color pallete options and button to flip colorscale direction
                                inlineCSS("#js_colorpal img{margin-top:-9px;}"),
                                tags$div(id = "js_colorpal", uiOutput("colorpal_out"), style = "display:inline-block"),
                                actionButton("flip_colors", "Invert color scale", style = "display:inline-block")
                                )# End main panel on Visualize tab #
                              
                            )# end fluidrow
                    ), # End Visualize tab #
                   
                   ################## Download Panel ##############################################
                   tabPanel('Download',
                            fluidRow(style = "display:flex;flex-direction:row;align-items:stretch",
                              column(width = 6,
                                wellPanel(style = "height:100%",
                                         tags$h4(icon("table", "fa-2x"), tags$b("Processed Data")),
                                         checkboxGroupInput("download_selection", label = "Check Download Selection",
                                                             choices = c('Data File as one .csv and Molecular Identification File as another .csv' = "separate",
                                                                         'Merged Data File and Molecular Identification File as a single .csv' = "merged",
                                                                         'Data summaries for grouped plots' = "group_data"),
                                                             width = "80%")
                                          )
                              ),
                              column(width = 5,
                                     wellPanel(style = "height:100%",
                                          tags$h4(icon("align-left", "fa-2x"), tags$b("Summary Report")),
                                          checkboxInput("report_selection", label = "Download a summary of preprocessing and filtering", value = TRUE)
                                        )
                                     )
                            ),
                            fluidRow(style = "margin-top:10px",
                              column(width = 11,
                                     wellPanel(
                                       fluidRow(
                                       column(width = 2,
                                              tags$h4(icon("image", "fa-2x"), tags$b("Figures"))
                                       )
                                     ),
                                     tags$h5(tags$b("Select figures by row. When clicked, the download selection will highlight.")),
                                     fluidRow(
                                       column(width = 9,
                                              dataTableOutput("parmsTable2")
                                       ),
                                       column(width = 3,
                                              radioButtons(inputId = "image_format", label = "Select an image format",
                                                           choices = c( "png", "pdf", "tiff"), selected = "png")
                                       )
                                     )
                                     )
                              )
                            ),
                            #verbatimTextOutput('x4'),
                            div(style = 'width:75%',
                              actionButton('makezipfile', label = tags$b('Bundle up all selected items'), icon = icon("briefcase"), lib = "glyphicon", style = 'width:45%'),
                              disabled(downloadButton('download_processed_data', tags$b('Download bundle'), style = 'width:45%;float:right'))
                            ),
                            tags$br()
                            
                   ), 
                   
                   ################## Glossary Panel ##############################################
                   tabPanel('Glossary',
                            #mainPanel(
                            #includeHTML("./README/Glossary.html")
                            withMathJax(includeMarkdown("./README/Glossary.md"))
                            # )
                            
                   )
    ),
    div(id = "js_helpbutton", style = "position:absolute;top:3px;right:16px;z-index:1000", 
        tagList(
          hidden(bsButton("helpbutton", "How do I use this page?", style = "info")),
          hidden(bsButton('datareqs_video', 'Data requirements tutorial', 
                   onclick = "window.open('https://youtu.be/uU5Q7r_pEGM', '_blank')", 
                   style = 'info', icon = icon('facetime-video', lib = 'glyphicon')))
        )
    )
  )
)

