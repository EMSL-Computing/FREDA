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
library(DT)

# Define UI for application that draws a histogram
shinyUI(navbarPage(title = (windowTitle = "FREDA"),
                   id = "top_page",
                   theme = "yeti.css",
                   ############# Welcome Panel #########################
                   navbarMenu("Welcome",
                              tabPanel(title = "Introduction",
                                       includeMarkdown("./Welcome to FREDA.md")),
                              tabPanel(title = "Data Requirements",
                                       includeMarkdown("./DataRequirements.md"),
                                       # DT::dataTableOutput("example_meta_table"), # in case we want a preview of the data
                                       # DT::dataTableOutput("example_data_table"),
                                       downloadButton('downloadData', 'Download')),
                              tabPanel(title = "Resources",
                                       HTML('<h4> Resources </h4>')),
                              tabPanel(title = "Contact",
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
                                fileInput("file_edata", "Upload CSV Data File",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                
                                ## Get unique identifier column from e_data ##
                                uiOutput('edata_id'),
                                
                                # Load e_meta file
                                fileInput("file_emeta", "Upload CSV Molecular Identification File",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")), 
                                
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
                                conditionalPanel(
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
                                  uiOutput('iso_info_column_out'),
                                  uiOutput('iso_symbol_out')
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
                                actionButton('preprocess_click', 'Process Data', icon = icon("cog"), lib = "glyphicon")
                                
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
                                
                                fluidRow(
                                  column(
                                    width = 6, actionButton('filter_click', "Filter Data", icon = icon("cog"), lib = "glyphicon")
                                  ),
                                  column(
                                    width = 6, actionButton('reset_filters', "Reset Filters", icon = icon("trash"), lib = "glyphicon")
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
                                uiOutput("legend_title_out"),

                                splitLayout(
                                  actionButton("plot_submit", label = "Submit")
                                )
                              ),# End sidebar conditionals on Visualize tab #
                              
                              mainPanel(
                                #tags$div(plotlyOutput('FxnPlot'), class = "square"),


                                wellPanel(
                                  plotlyOutput('FxnPlot', width = '700px', height = '600px')
                                ),
                                # width = 7,


                                conditionalPanel(
                                  condition = "input.chooseplots == 'Van Krevelen Plot'",
                                  # Set default width to 7
                                  
                                  # Drop down list: Use boundary?
                                  uiOutput("vkbounds_out")
                                  
                                ),
                                conditionalPanel(
                                  condition = "input.chooseplots !== null",
                                  # Set default width to 7
                                  
                                  # Drop down list: Use boundary?
                                  # selectInput('vkbounds', 'Use Van Krevelen boundary set:',
                                  #             choices = c('BS1' = 'bs1', 'BS2' = 'bs2', 'None' = 0),
                                  #             selected = 'bs1')
                                  selectInput("vk_colors", "Color By:", choices = NULL, selected = NULL)
                                  
                                ),
                                conditionalPanel(
                                  condition = "input.chooseplots == 'Custom Scatter Plot'",
                                  fluidRow(
                                    column(width = 4,
                                      selectInput("scatter_x", "Horizontal Axis Variable:", choices = NULL, selected = NULL)
                                    ),
                                    column(width = 4,
                                      selectInput("scatter_y", "Vertical Axis Variable:", choices = NULL, selected = NULL)
                                    )
                                  )
                                ),
                                br(),
                                hr(),
                                actionButton(inputId = "add_plot", label = "I want to download a hi-res version of this plot on the Download tab", icon = icon("download")),
                                br(),
                                br(),
                                dataTableOutput("parmsTable", width = "55%")
                                )# End main panel on Visualize tab #
                              
                            )), # End Visualize tab #
                   
                   ################## Download Panel ##############################################
                   tabPanel('Download',
                            checkboxGroupInput("download_selection", label = "Select Processed Data to Download",
                                               choices = c('Data File as one .csv and Molecular Identification File as another .csv' = "separate",
                                                           'merged Data File and Molecular Identification File as a single .csv' = "merged"),
                                                width = "40%"),
                            hr(),
                            checkboxInput("report_selection", label = "Report (Coming Soon)"),
                            hr(),
                            p("Figures"),
                            p("Please select figures to download in the table below by clicking on the row. When clicked, the selection will highlight."),
                            dataTableOutput("parmsTable2", width = "55%"),
                            radioButtons(inputId = "image_format", label = "Select an image format",
                                         choices = c("svg", "pdf", "tiff", "png"), selected = "svg"),
                            verbatimTextOutput('x4'),
                            hr(),
                            downloadButton('download_processed_data', 'Download Selected Items')
                            
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

