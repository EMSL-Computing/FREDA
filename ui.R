
# Define UI and wrap everything in a taglist that first calls useShinyjs()
ui <- tagList(useShinyjs(), navbarPage(
                   title = tags$div("FREDA", tags$span(style = "font-size:small", "v1.0.7")), 
                   windowTitle = 'FREDA',
                   id = "top_page",
                   theme = "yeti.css",
                   ############# Welcome Panel #########################
                   navbarMenu("Welcome",
                              tabPanel(title = "Introduction", class = "background_FTICR",
                                        includeMarkdown("./Welcome to FREDA.md"),
                                        h4(tags$b('Citing FREDA')),
                                        tags$p('A publication is forthcoming for FREDA.  In the meantime, we ask that you cite FREDA by url', tags$b('(https://msc-viz.emsl.pnnl.gov/FREDA/)'),'for any figures or analysis included in a publication or report.'),
                                        hr(),
                                        br(),
                                        bsButton('all_tutorials', 'See a playlist of video tutorials', 
                                                 onclick = "window.open('https://www.youtube.com/watch?v=uU5Q7r_pEGM&list=PLvozcBqO8i7wsMWo5PnOREX0sHSk3mAjE', '_blank')", 
                                                 style = 'info', icon = icon('facetime-video', lib = 'glyphicon'))
                                       ),
                              tabPanel(title = "Data Requirements", class = "background_FTICR", value = 'data_requirements',
                                       includeMarkdown("./DataRequirements.md"),
                                       downloadButton('downloadData', 'Download')),
                              tabPanel(title = "Resources/Contact", class = "background_FTICR",
                                       includeMarkdown('resources_and_contact.md')
                                       )
                   ),
                   ################## Groups Panel ###############################################
                   tabPanel(div("Groups", icon('th-large', lib = 'glyphicon')), value = 'Groups',
                            fluidRow(style = "display:flex;flex-direction:row;align-items:stretch",
                                     column(4,
                                            wellPanel(style = "height:100%",
                                                      tags$h4("Define a Group"),
                                                      div(id = "js_group_name", textInput("group_name", "Name of this group:")),
                                                      fluidRow(
                                                        column(6, uiOutput("group_samples_UI")),
                                                        column(6, textInput("group_regex", "Search sample names"))
                                                      ),
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
                     tabPanel(div("Preprocess", icon('cogs')), value = 'Preprocess',
                              
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
                   tabPanel(div("Quality Control", icon('chart-bar')), value = 'Quality Control',
                            fluidRow(style = 'display:flex;flex-direction:row;align-items:stretch',
                                    column(4,
                                      wellPanel(style = 'height:100%',
                                              uiOutput('qc_select_groups', style = "width:50%"),
                                              hr(style='margin:2px'),
                                              uiOutput('qc_plot_scale', style = "width:50%"),
                                              textInput('qc_boxplot_xlab', "X axis label"),
                                              textInput('qc_boxplot_ylab', 'Y axis label'),
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
                                               )
                                           )
                                    )
                            ),
                            hr(),
                            actionButton('goto_filter_fromqc', "Continue to the filter tab")
                    ),

                   ################## Filter Panel ##############################################
                   tabPanel(div("Filter", icon('filter')), value = 'Filter',
                            
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
                                        ),
                                    textInput('filter_regex', 'Search sample names')
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
                                
                                div(id = "warnings_filter", style = "overflow-y:auto;max-height:150px", uiOutput("warnings_filter_UI"))
                              
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
                   navbarMenu(div('Visualize', icon('eye-open', lib = 'glyphicon'), style = 'display:inline-block'),
                     # Main plot creation sub-panel
                     tabPanel(div("Create Plots"), value = 'Visualize',
                              
                              fluidRow(
                                # Sidebar Panel
                                div(id='viz_sidebar_column', column(4,
                                  
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
                                    
                                    bsCollapsePanel(div('Coloring/Appearance', div(style = 'float:right', uiOutput('dynamic_opts_icon'))), value = 'reactive_plot_opts',
                                      # plot options
                                      wellPanel(
                                        # color and van-krevelen bounds dropdowns
                                        fluidRow(
                                          column(width = 6, class = "grey_out", id = "js_vk_colors",
                                                 disabled(selectInput("vk_colors", "Color by:", choices = NULL, selected = NULL))
                                          ),
                                          column(width = 6, class = "grey_out", id = "js_vkbounds",
                                                 disabled(selectInput('vkbounds', 'Van Krevelen boundary set:',
                                                                      choices = c('BS1' = 'bs1', 'BS2' = 'bs2', 'None' = 0),
                                                                      selected = 0))
                                          )
                                        ),
                                        
                                        # x and y axis variable dropdowns for custom scatter plot
                                        fluidRow(
                                          column(width = 6, class = "grey_out", id = "js_scatter_x",
                                                 disabled(selectInput("scatter_x", "Horizontal axis variable:", choices = NULL, selected = NULL))
                                          ),
                                          column(width = 6, class = "grey_out", id = "js_scatter_y",
                                                 disabled(selectInput("scatter_y", "Vertical axis variable:", choices = NULL, selected = NULL))
                                          )
                                        ),
                                        
                                        # color pallete options and button to flip colorscale direction
                                        fluidRow(
                                          column(6,
                                            tags$div(id = "js_colorpal", uiOutput("colorpal_out"), style = "display:inline-block"),
                                            actionButton("flip_colors", "Invert color scale")
                                          )
                                        )
                                      )                
                                    )
                                  )
                                )),# End sidebar conditionals on Visualize tab #
                                
                                # Plot panel and generate plot buttons
                                column(8,
                                  inlineCSS("#FxnPlot {width:inherit;}"),
                                  wellPanel(style = "margin-top:-10px",
                                        div(class = 'plot_border', style = "width:65%",
                                          plotlyOutput('FxnPlot', width = 'auto', height = '600px') %>% 
                                            withSpinner(color = "orange", type = 8)
                                        ),
                                        # Separate buttons to generate plot or simply update labels without recalculating data
                                        disabled(
                                          div(style = 'display:inline-block;margin-top:10px',
                                              bsButton("plot_submit", label = "Generate Plot", icon = icon("plus"), lib = "glyphicon"),
                                              bsButton("update_axes", label = "Update Labels", icon = icon("refresh"), lib = "glyphicon"),
                                              hidden(bsButton('make_goto_linked', label = 'Compare in Linked Plots', icon = icon('link')))
                                          )
                                        ),
                                        
                                        br(),
                                        br(),
                                        
                                        div(id = "warnings_visualize", style = "overflow-y:auto;max-height:150px", uiOutput("warnings_visualize_UI"))
                                  ),
                                )# End main panel on Visualize tab #
                              )# end fluidrow
                      ), 
                     
                     # Linked plots sub-panel
                     tabPanel('Linked Plots', value = 'Linked Plots',
                              bsCollapse(id = 'linked_plots_collapse', open = c('lp_select_plots'), multiple = TRUE,
                                         bsCollapsePanel(title = 'Choose Two Plots to Compare', value = 'lp_select_plots',
                                                         tags$i(info_text[['VALID_LINKED_PLOTS']]),
                                                         tags$hr(),
                                                         DTOutput('lp_plot_table'),
                                                         bsButton('lp_compare_plots', 'Compare These Plots')),
                                         # bsCollapsePanel(title = 'Create A Linked Plot', value = 'lp_create_plot',
                                         #                 DTOutput('lp_plot_table'),
                                         #                 bsButton('lp_create_plot', 'Create and Compare With Selected Plot')),
                                         bsCollapsePanel(title = 'View and Interact', value = 'lp_mainpanel',
                                                         splitLayout(cellArgs = list(class = 'plot_border'),
                                                           withSpinner(plotlyOutput('lp_left', height = '600px'), color = 'orange', type = 8),
                                                           withSpinner(plotlyOutput('lp_right', height = '600px'), color = 'orange', type = 8)
                                                           )
                                                         )
                                         
                                         )
                              )
                     ),# End Visualize tab #
                   
                   ################## Database Mapping Panel ####################
                   tabPanel(div('Database Mapping', icon('th-list', lib = 'glyphicon')), value = 'Database Mapping',
                            fluidRow(style = "display:flex;flex-direction:row;align-items:stretch",
                               column(4,
                                  bsCollapse(id='db_mapping_sidebar', open = c('mappings'), multiple=TRUE, 
                                     bsCollapsePanel('Choose which mappings to calculate', value = 'mappings',
                                                     radioGroupButtons('database_select', label = "Choose a Database", choices = c('Kegg', 'MetaCyc')),
                                                     numericInput('max_records_database', 'Exclude formulae that map to more than this many records:', value = 5),
                                                     checkboxGroupButtons('which_mappings', label = "Include which variables in mapping:",
                                                                       choices = c('Reactions' = 'comp2react', 'Modules' = 'react2mod', 'Pathways' = 'mod2path')),
                                                     hr(),
                                                     tags$p('Make unique rows for which variable?'),
                                                     div( 
                                                      div(style='display:inline-block', 
                                                        uiOutput('which_unique')
                                                      ),
                                                      bsButton('create_mapping', 'Perform Mapping', style = 'info')
                                                      ),
                                                     uiOutput('warnings_database')
                                                  )
                                            )
                                      ),# column 4
                                     column(8,
                                            bsCollapse(id = 'database_tables_parent_collapse', open = 'database_tables', multiple = TRUE,
                                                       bsCollapsePanel('Table Preview', value = 'database_tables',
                                                                       span(id = "toggle_table",
                                                                            div(style = 'display:inline-block;margin-top:10px;margin-right:10px;font-weight:bold', "Display results:"),
                                                                            div(style = 'display:inline-block', radioGroupButtons('which_table', choices = c('Kegg'=1, 'MetaCyc'=2))),
                                                                            div(style = 'display:inline-block', bsButton('save_db_table', 'Save current table', style = 'info')),
                                                                            div(style = 'display:inline-block', bsButton('view_db_tables', uiOutput('n_saved_db_tables'), style = 'info'))
                                                                            
                                                                       ),
                                                                       uiOutput('conditional_database_table')
                                                         ),
                                                       bsCollapsePanel('Summary Counts', value = 'database_plots',
                                                                       DTOutput('mapping_summary'),
                                                                       uiOutput('conditional_database_barplot')
                                                                       # splitLayout(plotlyOutput('kegg_barplot'),
                                                                       #             plotlyOutput('mc_barplot'))
                                                        )
                                                       )# parent collapse
                                            ) # column 8
                                      )#fluidrow
                                ),#tabpanel
                   
                   ################## Download Panel ##############################################
                   tabPanel(div('Download', icon('download')), value = 'Download',
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
                                          tags$h4(icon("align-left", "fa-2x"), tags$b("Summary Report and Database Mapping Tables")),
                                          checkboxInput("report_selection", label = "Download a summary of preprocessing and filtering", value = TRUE),
                                          checkboxInput("download_mappings", label = "Download Database Mapping Tables", value = FALSE)
                                        )
                                     )
                            ),
                            fluidRow(style = "margin-top:10px",
                              column(width = 11,
                                     wellPanel(
                                       div(style='display:flex',
                                         column(width = 5,
                                                tags$h4(icon("image", "fa-2x"), tags$b("Figures")),
                                                tags$h5(tags$b("Select figures by row. When clicked, the download selection will highlight.")),
                                                DTOutput("download_plot_table")
                                         ),
                                         column(width = 7,
                                                div(style = 'height:100%', class = 'plot_border', uiOutput('download_plot'))
                                         )
                                       ),
                                       hr(),
                                       br(),
                                       div(style = 'display:flex',
                                           div(style = 'float:left;width:50%;margin-left:10px',
                                             bsButton('mark_plot_download', 'Select/de-select for download', icon = icon('minus')),
                                             bsButton('remove_plot_download', 'Remove selected plot', icon = icon('remove'))
                                           ),
                                           div(style='float:right;width:50%', class='spaced_flexbox',
                                              inlineCSS('[id=download_dimensions] > .shiny-input-container {width:49%;display:inline-block;}'), 
                                              div(id = 'download_dimensions',
                                                numericInput('download_img_width', 'Download width', value = 1600, min=100, max=2000, step = 1),
                                                numericInput('download_img_height', 'Download height', value = 900, min=100, max=2000, step = 1)
                                              ),
                                              radioButtons(inputId = "image_format", label = "Select an image format",
                                                          choices = c( "png", "pdf", "jpeg"), selected = "png", inline = TRUE)
                                           )
                                       ),
                                       uiOutput('warnings_download')
                                     )
                              )
                            ),
                            div(style = 'width:75%',
                              actionButton('makezipfile', label = tags$b('Bundle up all selected items'), icon = icon("briefcase"), lib = "glyphicon", style = 'width:45%'),
                              disabled(downloadButton('download_processed_data', tags$b('Download bundle'), style = 'width:45%;float:right'))
                            ),
                            tags$br()
                            
                   ), 
                   
                   ################## Glossary Panel ##############################################
                   tabPanel(div('Glossary', icon('question-sign', lib = 'glyphicon')), value = 'Glossary',
                            #mainPanel(
                            #includeHTML("./README/Glossary.html")
                            withMathJax(includeMarkdown("./README/Glossary.md"))
                            # )
                            
                   )
    ),
    div(id = "js_helpbutton", style = "position:absolute;top:3px;right:16px;z-index:1000;width:11%", 
          div(style = 'float:right;width:25%',
              tipify(
                hidden(bsButton("helpbutton", icon("question-sign", lib = "glyphicon", style = "color:white"), style = "info")),
                ttip_text[['page_help']]
              )
          ),
          div(style = 'float:right;width:50%%;margin-left:1px;margin-right:1px',
              tipify(
                hidden(bsButton("viewplots", uiOutput('viewplots_label', style = 'float:left;margin-right:10px'), style = "info", icon = icon("folder-open", lib = "glyphicon"))),
                ttip_text[['plot_review']]
              )
          ),
          div(style = 'float:right;width:25%',
            tipify(
              hidden(bsButton("saveplot", icon("save", lib = "glyphicon"), style = "info")),
              ttip_text[['plot_save']]
            )
          ),
          hidden(bsButton('datareqs_video', 'Data requirements tutorial', 
                   onclick = "window.open('https://youtu.be/uU5Q7r_pEGM', '_blank')", 
                   style = 'info', icon = icon('facetime-video', lib = 'glyphicon'))),
    ),
    uiOutput("enter_debugger")
  )

