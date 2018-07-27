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
                                       downloadButton('downloadData', 'Download')),
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
                                width = 5,
                                
                                # Load e_data file
                                div(id = "js_file_edata", fileInput("file_edata", "Upload CSV Data File",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv"))),
                                
                                ## Get unique identifier column from e_data ##
                                div(id = "js_edata_id", uiOutput('edata_id')),
                                
                                # Load e_meta file
                                div(id = "js_file_emeta", fileInput("file_emeta", "Upload CSV Molecular Identification File",
                                              multiple = TRUE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv"))), 
                                
                                # Horizontal rule #
                                tags$hr(),
                                
                                # Get which instrument generated the data #
                                selectInput('instrument', 
                                            label = 'What instrument generated this data?',
                                            choices = list(# 'Select an option' = 0,
                                              '12T or 15T' = '12T', '21T'), 
                                            selected = 'Select an option'
                                ), 
                                
                                # Get whether formulas or elemental columns are included #
                                selectInput('select', 
                                            label = 'Does this file have formulas 
                                            or elemental columns?',
                                            choices = list('Select an option' = 0, 
                                                           'Formulas' = 1, 
                                                           'Elemental Columns' = 2),
                                            selected = 'Select an option' 
                                ), 
                                
                                
                                # (Conditional on the above selectInput) Formula: 
                                ##  which column contains the formula? #
                                conditionalPanel(
                                  condition = "input.select == 1", 
                                  uiOutput('f_column')
                                ), 
                                
                                # (Conditional on the above selectInput) Elemental columns: 
                                ##  which columns contain the elements?
                                conditionalPanel(id = "element_select",
                                  condition = "input.select == 2",
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
                                ), 
                                # Create an option for Isotopic Analysis
                                selectInput('isotope_yn',
                                            label = 'Were isotopic peaks identified in the molecular assignments file?',
                                            choices = list('Select an Option' = 0,
                                                           'Yes' = 1,
                                                           'No' = 2),
                                            selected = 'Select an Option'
                                ),
                                # Condition on presence of isotope information
                                conditionalPanel(
                                  condition = "input.isotope_yn == 1",
                                  uiOutput("iso_info_filter_out"),
                                  div(id = "js_iso_info_column", uiOutput('iso_info_column_out')),
                                  div(id = "js_iso_symbol", uiOutput('iso_symbol_out'))
                                ),
                                # # Condition on absence of isotope information
                                # conditionalPanel(
                                #   condition = "input.isotope_yn == 2",
                                #   uiOutput('c13_column')
                                # ),


                                
                                tags$hr(),
                                
                                # Action button: pressing this creates the peakICR object
                                actionButton('upload_click', 'Process Data', icon = icon("cog"), lib = "glyphicon")
                                
                              ), # End sidebar panel
                              
                              mainPanel(
                                
                                # Set default width of panel
                                width = 7,
                                
                                # Show 'Success' message if peakICR created successfully
                                
                                div(id = "warnings", style = "overflow-y:scroll;max-height:250px", uiOutput("warnings")),
                                uiOutput('success_upload'),
                                
                                # Summary panel
                                wellPanel(
                                  
                                  # Number of peaks, samples, and peaks with formulas assigned
                                  textOutput('num_peaks'), 
                                  textOutput('num_samples'), 
                                  textOutput('num_peaks_formula')
                                ),
                                
                                # Horizontal ruler
                                tags$hr(), 
                                
                                # Show preview of e_data
                                htmlOutput('edata_text'), 
                                DTOutput("head_edata", width = "90%"),
                                
                                # Horizontal rule
                                tags$hr(),
                                
                                # Show preview of e_meta
                                htmlOutput('emeta_text'),
                                DTOutput("head_emeta", width = "90%")
                                
                              ) # End main panel
                              
                            )), # End Upload tab
                   ################## Preprocess Panel ###############################################
                     tabPanel("Preprocess",
                              
                              bsButton("preprocess_help", "How do I use this page?", style = "info"),
                              
                              br(),
                              br(),
                              
                              sidebarLayout(
                                
                                # Sidebar panel
                                sidebarPanel(
                                  
                                  uiOutput("which_calcs"),
                              
                                  # Action button: add test columns with reasults to peakIcr2
                                  shinyjs::disabled(actionButton('preprocess_click', 'Process Data', icon = icon("cog"), lib = "glyphicon"))
                                ), # End sidebar panel
                                
                                mainPanel(
                                  
                                  # Set default main panel width 
                                  width = 8,
  
                                  # Include numeric and categorical summaries in a well panel
                                  
                                  wellPanel(
                                    tags$div(class = "row",
                                             tags$div(class = "col-sm-5",
                                                      uiOutput("numeric_header"),
                                                      dataTableOutput('numeric_summary')
                                             ),
                                             tags$div(class = "col-sm-7",
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

                   
                   ################## Filter Panel ##############################################
                   tabPanel("Filter", 
                            
                            bsButton("filter_help", "How do I use this page?", style = "info"),
                            
                            br(),
                            br(),
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                # Set default width for panel
                                width = 5,

                                # Checkbox: Mass filter yes/no
                                #HTML('<h5><b>Mass Filter</b></h5>')
                                
                            
                                checkboxInput('massfilter', tags$b("Mass Filter") ,value = FALSE),
                                
                                
                                # Numeric: Min/max mass filter
                                splitLayout(
                                  numericInput('min_mass', 'Minimum Mass value', 
                                               min = 0, value = 200),
                                  numericInput('max_mass', "Maximum Mass value", 
                                               min = 0, value = 900)
                                ),
                                
                                # Checkbox: Mass filter yes/no
                                checkboxInput('molfilter', tags$b("Molecule Filter"), value = FALSE),
                                
                                # Drop-down list: Min/max mass filter
                                uiOutput('minobs'), 
                                
                                checkboxInput('formfilter', tags$b("Formula Presence Filter"), value = FALSE),
                            
                                # checkboxInput('customfilterz', label = "Implement up to 3 custom filters", value = FALSE),
                                # uiOutput("filter1UI"),
                                # uiOutput("customfilter1UI"),
                                #   conditionalPanel(condition = "input.custom1 !== 'Select item'",
                                #                   uiOutput("filter2UI"),
                                #                   uiOutput("customfilter2UI")
                                #   ),
                                #   conditionalPanel(condition = "input.custom2 !== 'Select item'",
                                #                    uiOutput("filter3UI"),
                                #                    uiOutput("customfilter3UI")
                                #   ),
                                shinyjs::disabled(
                                  fluidRow(
                                    column(
                                      width = 6, actionButton('filter_click', "Filter Data", icon = icon("cog"), lib = "glyphicon")
                                    ),
                                    column(
                                      width = 6, actionButton('reset_filters', "Reset Filters", icon = icon("trash"), lib = "glyphicon")
                                    )
                                  )
                                )
                              
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
                            
                            bsButton("visualize_help", "How do I use this page?", style = "info"),
                            
                            br(),
                            br(),
                            
                            sidebarLayout(
                              
                              # Sidebar Panel
                              sidebarPanel(
                                
                                # Select Plot Type
                                uiOutput('plot_type'),
                                uiOutput("plotUI"),
                                uiOutput("plotUI_cond"),
                                # uiOutput("plotUI_2"),
                                
                                uiOutput("title_out"),
                                uiOutput("x_axis_out"),
                                uiOutput("y_axis_out"),
                                tags$div(id = "js_legend_title_input", uiOutput("legend_title_out")),

                                splitLayout(
                                  shinyjs::disabled(
                                    actionButton("plot_submit", label = "Generate Plot"),
                                    actionButton("update_axes", label = "Update Labels")
                                  )
                                )
                              ),# End sidebar conditionals on Visualize tab #
                              
                              mainPanel(
                                #tags$div(plotlyOutput('FxnPlot'), class = "square"),


                                wellPanel(
                                  plotlyOutput('FxnPlot', width = '700px', height = '600px')
                                ),
                                # width = 7,


                                fluidRow(
                                  column(width = 4, class = "grey_out", id = "js_vk_colors",
                                         shinyjs::disabled(selectInput("vk_colors", "Color By:", choices = NULL, selected = NULL))
                                         ),
                                  column(width = 5, class = "grey_out", id = "js_vkbounds",
                                         shinyjs::disabled(selectInput('vkbounds', 'Use Van Krevelen boundary set:',
                                                     choices = c('BS1' = 'bs1', 'BS2' = 'bs2', 'None' = 0),
                                                     selected = 'bs1'))
                                         )
                                  
                                ),
                                
                                fluidRow(
                                  column(width = 4, class = "grey_out", id = "js_scatter_x",
                                         shinyjs::disabled(selectInput("scatter_x", "Horizontal Axis Variable:", choices = NULL, selected = NULL))
                                        ),
                                  column(width = 4, class = "grey_out", id = "js_scatter_y",
                                         shinyjs::disabled(selectInput("scatter_y", "Vertical Axis Variable:", choices = NULL, selected = NULL))
                                        )
                                ),
                                
                                tags$div(id = "js_colorpal", uiOutput("colorpal_out")),
                                
                                br(),
                                hr(),
                                shinyjs::disabled(actionButton(inputId = "add_plot", label = "Store these plot parameters", icon = icon("save"))),
                                br(),
                                br(),
                                dataTableOutput("parmsTable", width = "55%")
                                )# End main panel on Visualize tab #
                              
                            )), # End Visualize tab #
                   
                   ################## Download Panel ##############################################
                   tabPanel('Download',
                            fluidRow(
                              column(width = 7,
                                wellPanel(fluidRow(
                                  column(width = 10,
                                         tags$h4(icon("table", "fa-2x"), tags$b("Processed Data"))
                                         )
                                  ),
                                          checkboxGroupInput("download_selection", label = "Check Download Selection",
                                                             choices = c('Data File as one .csv and Molecular Identification File as another .csv' = "separate",
                                                                         'merged Data File and Molecular Identification File as a single .csv' = "merged"),
                                                             width = "80%")
                                          )
                              ),
                              column(width = 5,
                                     wellPanel(fluidRow(
                                       column(width = 8,
                                              tags$h4(icon("align-left", "fa-2x"), tags$b("Summary Report"))
                                       )
                                     ),
                                     checkboxGroupInput("report_selection", label = "Check Download Selection", choices = "Coming Soon"),
                                    br()
                                     )
                                     )
                            ),
                            fluidRow(
                              column(width = 12,
                                     wellPanel(
                                       fluidRow(
                                       column(width = 2,
                                              tags$h4(icon("image", "fa-2x"), tags$b("Figures"))
                                       )
                                     ),
                                     # tags$div(class = "row",
                                     #          icon("image", "fa-4x"),
                                     #          tags$h3(tags$b("Figures"))
                                     # ),
                                     tags$h5(tags$b("Select figures by row. When clicked, the download selection will highlight.")),
                                     fluidRow(
                                       column(width = 9,
                                              dataTableOutput("parmsTable2", width = "90%")
                                       ),
                                       column(width = 3,
                                              radioButtons(inputId = "image_format", label = "Select an image format",
                                                           choices = c("svg", "pdf", "tiff", "png"), selected = "svg")
                                       )
                                     )
                                     )
                              )
                            ),
                            #verbatimTextOutput('x4'),
                            downloadButton('download_processed_data', tags$b('Download Selected Items'), style = "width:100%"),
                            tags$br()
                            
                   ), 
                   
                   ################## Glossary Panel ##############################################
                   tabPanel('Glossary',
                            #mainPanel(
                            #includeHTML("./README/Glossary.html")
                            withMathJax(includeMarkdown("./README/Glossary.md"))
                            # )
                            
                   )
    )
  )
)

