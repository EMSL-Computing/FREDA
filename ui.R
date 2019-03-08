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


# Define UI for application that draws a histogram
shinyUI(tagList(useShinyjs(),
                
                # type = 'text/css',".disabled label.control-label {color:red}")
                # ,
                
                navbarPage(title = (windowTitle = "FREDA"),
                   id = "top_page",
                   theme = "yeti.css",
                   ############# Welcome Panel #########################
                   navbarMenu("Welcome",
                              # tabPanel(title = "Introduction",
                              #             includeHTML("./intropage.html")),
                              tabPanel(title = "Introduction", class = "background_FTICR",
                                         includeMarkdown("./Welcome to FREDA.md")),
                              tabPanel(title = "Data Requirements", class = "background_FTICR",
                                       includeMarkdown("./DataRequirements.md"),
                                       # DT::dataTableOutput("example_meta_table"), # in case we want a preview of the data
                                       # DT::dataTableOutput("example_data_table"),
                                       div(downloadButton('downloadData', 'Download'), style = "z-index:1000;position:absolute")),
                              tabPanel(title = "Resources", class = "background_FTICR",
                                       HTML('<h4> Resources </h4>')),
                              tabPanel(title = "Contact", class = "background_FTICR",
                                       HTML('<h4> Contact </h4>'))
                   ),
                   ################## Upload Panel #######################################
                   tabPanel("Upload",
                            sidebarLayout(
                              ## Sidebar panel on Upload tab ##
                              sidebarPanel(
                                # Set width of sidebar panel
                                width = 4,
                                
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
                                                         ".csv"))), 
                                
                                tags$hr(style = "margin:6px"),
                                
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
                                
                                tags$hr(style = "margin:6px"),
                                
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
                                
                                inlineCSS('#element_select button {width:100%;}'),
                                conditionalPanel(id = "element_select",
                                  condition = "input.select == 2",
                                  
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
                                ), 
                                
                                tags$hr(style = "margin:6px"),
                                
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
                                ),
                                
                                tags$hr(),
                                
                                # Action button: pressing this creates the peakICR object
                                actionButton('upload_click', 'Process Data', icon = icon("cog"), lib = "glyphicon")
                                
                              ), # End sidebar panel
                              
                              mainPanel(
                                # warnings panel
                                div(id = "warnings_upload", style = "overflow-y:scroll;max-height:250px", uiOutput("warnings_upload")),
                                
                                tags$hr(),
                                
                                # Summary panel
                                wellPanel(style = 'width:50%',
                                  # Show 'Success' message if peakICR created successfully
                                  uiOutput('success_upload'),        
                                          
                                  # Number of peaks, samples, and peaks with formulas assigned
                                  textOutput('num_peaks'), 
                                  textOutput('num_samples'), 
                                  textOutput('num_peaks_formula')
                                ),
                                
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
                          inlineCSS("#group_samples .dropdown-toggle {background-color:#ffffff;border-radius:4px;}
                                     #group_samples .bootstrap-select{border-radius:4px;}"),
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
                                # plotOutput("groups_plot")
                                actionButton("remove_group", "Remove selected group")
                              )
                            )
                          ),
                          hr(),
                          actionButton("goto_preprocess_main", "Continue to preproces tab")
                   ),
                   ################## Preprocess Panel ###############################################
                     tabPanel("Preprocess",
                              
                              sidebarLayout(
                                
                                # Sidebar panel
                                sidebarPanel(
                                  
                                  div(class = "adjustdown",uiOutput("which_calcs")),
                                  
                                  # Action button: add test columns with reasults to peakIcr2
                                  shinyjs::disabled(actionButton('preprocess_click', 'Process Data', icon = icon("cog"), lib = "glyphicon")),
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
                                              actionButton('update_boxplot_axes', "Reset Boxplot Axes")
                                      )
                                    ),
                                    column(8,
                                      wellPanel(style = 'height:100%',
                                           div(id='style_qc_boxplots', style='border-style:solid;border-width:1px;height:100%', 
                                               plotlyOutput("qc_boxplots", height = '100%') %>% withSpinner(color = "orange", type = 8)
                                               )
                                           # div(id='style_qc_x', style='border-style:solid;border-width:1px;margin-top:3px', plotlyOutput("qc_pcoa_plots") %>% withSpinner(color = "orange", type = 8))
                                           #uiOutput("download_qc")
                                           )
                                    )
                            )
                    ),

                   ################## Filter Panel ##############################################
                   tabPanel("Filter",
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                # Set default width for panel
                                width = 5,

                                # Checkbox: Mass filter yes/no
                                #HTML('<h5><b>Mass Filter</b></h5>')

                                div(class="adjustdown", checkboxInput('samplefilter', 
                                                                      tagList(tags$b("Sample Filter", style = "display:inline-block"),
                                                                              div(style = "color:deepskyblue;display:inline-block",
                                                                                  tipify(icon("question-sign", lib = "glyphicon"), 
                                                                                         title = "Retain a subset of all samples", 
                                                                                         placement = "top", trigger = 'hover')
                                                                              ) 
                                                                      ), 
                                                                      value = FALSE
                                                        )
                                ),
                                
                                div(id = "js_filter_samples", uiOutput("filter_samples")),
                                
                                tags$hr(style = "margin:6px"),
                                
                                div(class="adjustdown", checkboxInput('massfilter', 
                                                                      tagList(tags$b("Mass Filter", style = "display:inline-block"),
                                                                              div(style = "color:deepskyblue;display:inline-block",
                                                                                  tipify(icon("question-sign", lib = "glyphicon"), 
                                                                                        title = "Retain peaks within a mass range specified below", 
                                                                                        placement = "top", trigger = 'hover')
                                                                                  ) 
                                                                            ), 
                                                                     value = FALSE)
                                ),
                                
                                
                                # Numeric: Min/max mass filter
                                splitLayout(
                                  numericInput('min_mass', 'Minimum Mass value', 
                                               min = 0, value = 200),
                                  numericInput('max_mass', "Maximum Mass value", 
                                               min = 0, value = 900)
                                ),
                                
                                tags$hr(style = "margin:6px"),
                                
                                # Checkbox: Mass filter yes/no
                                div(class = "adjustdown", checkboxInput('molfilter', 
                                                                        tagList(tags$b("Molecule Filter", style = "display:inline-block"),
                                                                                div(style = "color:deepskyblue;display:inline-block", 
                                                                                    tipify(icon("question-sign", lib = "glyphicon"), 
                                                                                            title = "Retain peaks that are observed in a minimum number of samples, specified below", 
                                                                                            placement = "top", trigger = 'hover')
                                                                                    )
                                                                              ), 
                                                                        value = FALSE)
                                ),   
                                # Drop-down list: Min/max mass filter
                                uiOutput('minobs'), 
                                
                                tags$hr(style = "margin:6px"),
                                
                                div(class = "adjustdown", checkboxInput('formfilter', 
                                                                        tagList(tags$b("Formula Presence Filter", style = "display:inline-block"),
                                                                                div(style = "color:deepskyblue;display:inline-block",
                                                                                    tipify(icon("question-sign", lib = "glyphicon"), 
                                                                                            title = "Retain peaks that have a molecular formula specified or calculated from elemental values", 
                                                                                            placement = "top", trigger = 'hover')
                                                                                    )
                                                                                ), 
                                                                        value = FALSE)
                                ),
                                
                                tags$hr(style = "margin:6px"),
                                
                                div(class = "adjustdown", checkboxInput('customfilterz', 
                                                                        tagList(tags$b("Implement up to 3 custom filters", style = "display:inline-block"),
                                                                                div(style = "color:deepskyblue;display:inline-block",
                                                                                    tipify(icon("question-sign", lib = "glyphicon"), 
                                                                                           title = "Filter based on up to 3 variables in the post-processed molecular identification file", 
                                                                                           placement = "top", trigger = 'hover')
                                                                                )
                                                                        ), 
                                                                        value = FALSE)
                                ),
                                
                                hr(),
                                
                                inlineCSS("#custom_cond_panel .dropdown-toggle {background-color:#ffffff;border-radius:4px;}
                                          #custom_cond_panel .bootstrap-select{border-radius:4px;}"),
                                conditionalPanel(id = "custom_cond_panel", condition = "input.customfilterz == true",
                                                   uiOutput("filter1UI"),
                                                   uiOutput("customfilter1UI"),  
                                                   uiOutput("filter2UI"),
                                                   uiOutput("customfilter2UI"),
                                                   uiOutput("filter3UI"),
                                                   uiOutput("customfilter3UI")
                                                 ),
                                
                                shinyjs::disabled(
                                  fluidRow(
                                    column(
                                      width = 6, actionButton('filter_click', "Filter Data", icon = icon("cog"), lib = "glyphicon")
                                    ),
                                    column(
                                      width = 6, actionButton('reset_filters', "Reset Filters", icon = icon("trash"), lib = "glyphicon")
                                    )
                                  )
                                ),
                                
                                br(),
                                br(),
                                
                                div(id = "warnings_filter", style = "overflow-y:scroll;max-height:150px", uiOutput("warnings_filter"))
                              
                              ), # End sidebar panel on Filter tab
                              
                              mainPanel(
                                
                                # Set default width for panel
                                width = 7,
                                
                                # Success message if peakIcr2 filtered successfully
                                htmlOutput('filterTest'), 
                                
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
                                bsCollapse(id='viz_sidebar', open = c('peakplots', 'axlabs', 'downloads'), multiple=TRUE, 
                                  
                                  # Plot Parameters
                                  bsCollapsePanel('Construct A Plot', value = 'peakplots',
                                    # Select Plot Type
                                    inlineCSS("#chooseplots .btn{font-size:10.5pt;} #chooseplots .btn-group-container-sw{display:block;}" ),
                                    uiOutput('plot_type', style = "margin-top:-10px"),     

                                    # Select samples/groups
                                    uiOutput("plotUI"),
                                    
                                    uiOutput("pcoa_dist"),
                                    
                                    # Single dropdown for 1 sample/group or....
                                    shinyjs::hidden(div(id = "js_toggle_single", uiOutput("plotUI_single"))),
                                    
                                    # ...two dropdowns and extra options for group comparison
                                    inlineCSS("#js_toggle_groups .dropdown-toggle, #js_toggle_single .dropdown-toggle {background-color:#ffffff;border-radius:4px;}
                                                  #js_toggle_groups .bootstrap-select, #js_toggle_single .bootstrap-select {border-radius:4px;}"),
                                    shinyjs::hidden(div(id = "js_toggle_groups", 
                                                        tagList(div(id = "js_whichGroups1", uiOutput("plotUI_comparison_1")), 
                                                                div(id = "js_whichGroups2", uiOutput("plotUI_comparison_2"))))),
                                    conditionalPanel(condition = "(input.choose_single == 3 || input.choose_single == 4) && input.chooseplots !== '0'", uiOutput("summary_fxn_out", class = "adjustdown")),
                                    
                                    # Label inputs
                                    tags$hr(style = "thickness:5px")
                                    
                                  ),
                                  # Axes Options
                                  bsCollapsePanel('Axes Labels', value = 'axlabs',
                                      splitLayout(
                                        uiOutput("title_out"),
                                        tags$div(id = "js_legend_title_input", uiOutput("legend_title_out"))
                                        ),
                                      splitLayout(
                                        uiOutput("x_axis_out"),
                                        uiOutput("y_axis_out")
                                      )
                                  ),
                                  bsCollapsePanel('Save Plot', value = 'downloads',
                                      inlineCSS("#saveplot_ui .glyphicon{float:left;}"),
                                      div(id = "saveplot_ui", style = "display:inline-block;vertical-align:top;margin:10px 0px 0px 10px",
                                          div(shinyjs::disabled(actionButton(inputId = "add_plot", style = "width:100%", label = "Save This Plot for Later Download", icon = icon("save")))),
                                          br(),
                                          div(uiOutput("view_plots"))
                                      )
                                    
                                  )
                                ),
                                      
                                    
                                      # Seperate buttons to generate plot or simply update labels without recalculating data
                                      
                                      shinyjs::disabled(
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
                                      
                                      div(id = "warnings_visualize", style = "overflow-y:scroll;max-height:150px", uiOutput("warnings_visualize"))
                                  
                                # End sidebar conditionals on Visualize tab #
                              )),
                              
                              # Plot panel and axes/color controls.
                              column(8,
                                wellPanel(style = "margin-top:-10px",
                                      div(style = "display:inline-block;outline-style:auto;outline-width:thin;width:65%",
                                        plotlyOutput('FxnPlot', width = 'auto', height = '600px') %>% 
                                          withSpinner(color = "orange", type = 8)
                                      )
                                ),

                                # color and van-krevelen bounds dropdowns
                                fluidRow(
                                  column(width = 4, class = "grey_out", id = "js_vk_colors",
                                         shinyjs::disabled(selectInput("vk_colors", "Color by:", choices = NULL, selected = NULL))
                                         ),
                                  column(width = 4, class = "grey_out", id = "js_vkbounds",
                                         shinyjs::disabled(selectInput('vkbounds', 'Display Van Krevelen boundary set:',
                                                     choices = c('BS1' = 'bs1', 'BS2' = 'bs2', 'None' = 0),
                                                     selected = 'bs1'))
                                         )
                                ),
                                
                                # x and y axis variable dropdowns for custom scatter plot
                                fluidRow(
                                  column(width = 4, class = "grey_out", id = "js_scatter_x",
                                         shinyjs::disabled(selectInput("scatter_x", "Horizontal Axis Variable:", choices = NULL, selected = NULL))
                                        ),
                                  column(width = 4, class = "grey_out", id = "js_scatter_y",
                                         shinyjs::disabled(selectInput("scatter_y", "Vertical Axis Variable:", choices = NULL, selected = NULL))
                                        )
                                ),
                                
                                # color pallete options and button to flip colorscale direction
                                inlineCSS("#js_colorpal img{margin-top:-9px;}"),
                                tags$div(id = "js_colorpal", uiOutput("colorpal_out"), style = "display:inline-block"),
                                actionButton("flip_colors", "Invert color scale", style = "display:inline-block")
                                )# End main panel on Visualize tab #
                              
                            )
                    ), # End Visualize tab #
                   
                   ################## Download Panel ##############################################
                   tabPanel('Download',
                            fluidRow(style = "display:flex;flex-direction:row;align-items:stretch",
                              column(width = 6,
                                wellPanel(style = "height:100%",
                                         tags$h4(icon("table", "fa-2x"), tags$b("Processed Data")),
                                         checkboxGroupInput("download_selection", label = "Check Download Selection",
                                                             choices = c('Data File as one .csv and Molecular Identification File as another .csv' = "separate",
                                                                         'merged Data File and Molecular Identification File as a single .csv' = "merged",
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
                                              dataTableOutput("parmsTable2", width = "90%")
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
                            disabled(downloadButton('download_processed_data', tags$b('Download Selected Items'), style = "width:45.8333%;margin-left:22.91667%")),
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
    div(id = "js_helpbutton", style = "position:absolute;top:3px;right:16px;z-index:1000", hidden(bsButton("helpbutton", "How do I use this page?", style = "info")))
  )
)

