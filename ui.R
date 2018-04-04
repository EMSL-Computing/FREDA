#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(DT)
library(markdown)

# Define UI for application that draws a histogram
shinyUI(navbarPage(title = (windowTitle = "FREDA"),
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
                                fileInput("file_edata", "Upload CSV e_data",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                
                                ## Get unique identifier column from e_data ##
                                uiOutput('edata_id'),
                                
                                # Load e_meta file
                                fileInput("file_emeta", "Upload CSV e_meta",
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
                                              '12T', '21T'), 
                                            selected = 'Select an option'
                                ), 
                                
                                # Create an option for Isotopic Analysis
                                selectInput('isotope_yn',
                                            label = 'Do you have information for isotopes?',
                                            choices = list('Select an Option' = 0,
                                                           'Yes' = 1,
                                                           'No' = 2),
                                            selected = 'Select an Option'
                                ),
                                # Condition on presence of isotope information
                                conditionalPanel(
                                  condition = "input.isotope_yn == 1",
                                  uiOutput('iso_info_column'),
                                  uiOutput('iso_symbol')
                                ),
                                # # Condition on absence of isotope information
                                # conditionalPanel(
                                #   condition = "input.isotope_yn == 2",
                                #   uiOutput('c13_column')
                                # ),
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
                                  uiOutput("c_column"), 
                                  uiOutput("h_column"), 
                                  uiOutput("n_column"), 
                                  uiOutput("o_column"), 
                                  uiOutput("s_column"), 
                                  uiOutput("p_column")
                                ), 
                                
                                # HOrizontal rule
                                tags$hr(),
                                
                                # Action button: pressing this creates the peakICR object
                                actionButton('upload_click', 'Process Data')
                                
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
                              #dataTableOutput("head_edata"), 
                              DTOutput("head_edata", width = "90%"),
                              
                              # Horizontal rule
                              tags$hr(),
                              
                              # Show preview of e_meta
                              htmlOutput('emeta_text'),
                              #dataTableOutput('head_emeta'),
                              DTOutput("head_emeta", width = "90%")
                              
                            ) # End main panel
                            
                            )), # End Upload tab
                   
                   ################## Filter Panel ##############################################
                   tabPanel("Filter", 
                            
                            sidebarLayout(
                              sidebarPanel(
                                
                                # Set default width for panel
                                width = 5,
                                
                                # Checkbox: Mass filter yes/no
                                checkboxInput('massfilter', HTML('<h5><b>Mass Filter</b></h5>')),
                                
                                # Numeric: Min/max mass filter
                                numericInput('min_mass', 'Minimum Mass value', 
                                             min = 0, value = 200),
                                numericInput('max_mass', "Maximum Mass value", 
                                             min = 0, value = 900),
                                
                                # Checkbox: Mass filter yes/no
                                checkboxInput('molfilter', HTML('<h5><b>Molecule Filter</b></h5>')),
                                
                                # Drop-down list: Min/max mass filter
                                uiOutput('minobs'), 
                                actionButton('filter_click', "Filter Data")
                                
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
                   
                   ################## Preprocess Panel ###############################################
                   tabPanel("Preprocess",
                            
                            sidebarLayout(
                              
                              # Sidebar panel
                              sidebarPanel(
                                # 
                                # # Test: Display message at top of sidebar
                                # 'By default, O:C, H:C, Kendrick Mass, 
                                #               and Kendrick defect will be calculated.',
                                # 
                                # # Horizontal rule
                                # tags$hr(),
                                
                                # Checkbox: which tests should also be applied
                                checkboxGroupInput('tests', 'Calculate:',
                                                   c('O:C and H:C' = 'calc_vankrev',
                                                     'Kendrick Mass and Defect' = 'calc_kendrick',
                                                     'NOSC' = 'calc_nosc', 
                                                     'Gibbs Free Energy' = 'calc_gibbs', 
                                                     'Aromaticity and Modified Aromaticity' = 'calc_aroma',  
                                                     'DBE and DBE - O' = 'calc_dbe'), 
                                                   selected = c('calc_vankrev', 'calc_kendrick')), 
                                # Action button: add test columns with reasults to peakIcr2
                                actionButton('preprocess_click', 'Process Data')
                                
                              ), # End sidebar panel
                              
                              mainPanel(
                                
                                # Set default main panel width 
                                width = 7,
                                
                                # Summary panel for preprocess tab
                                wellPanel(
                                  tableOutput('summary_preprocess')
                                ), 
                                
                                # Drop down list: which histogram should be displayed?
                                uiOutput('which_hist'),
                                
                                # Plot: histogram
                                plotlyOutput('preprocess_hist')
                                
                              ) # End main panel on Preprocess tab #
                              
                            )), # End Preprocess tab #
                   
                   ################## Visualize Panel ###############################################
                   tabPanel("Visualize", 
                            
                            sidebarLayout(
                              
                              # Sidebar Panel
                              sidebarPanel(
                                
                                # Drop down list: Van Krevelen or Kendrick plot?
                                selectInput('chooseplots', 'I want to plot a', 
                                            choices = c('Van Krevelen Plot' = 1, 
                                                        'Kendrick Plot' = 2,
                                                        'Select an Option' = 0),
                                            selected = 0
                                ), 
                                # UI options will change depending on plot type. Stub
                                # that out here with conditional panels
                                #------------------ Van Krev Conditionals -------------------#
                                # conditionalPanel(
                                #   condition = 'input.chooseplots == 1',
                                #   
                                #   # Drop down list: single samples or multiple?
                                #   selectInput('choose_single', 'I want to plot using:',
                                #               choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples by group' = 2, 'A comparison of groups' = 3),
                                #               selected = 0), 
                                #   
                                #   # (Conditional on choose_single) If Multiple: show options for grouping
                                #   conditionalPanel(
                                #     condition = 'input.choose_single == 2',
                                #     
                                #       fluidRow(
                                #         ######### MAKE GROUPS MUTUALLY EXCLUSIVE ##########
                                #         # Column with width 6: which samples are in Group 1?
                                #         column(12, 
                                #                uiOutput('whichGroups1')
                                #         )#,
                                #         
                                #         # Column with width 6: which samples are in Group 2?
                                #         # column(6, 
                                #         #        uiOutput('whichGroups2')
                                #         # )
                                #       )
                                #       
                                #     ), # End conditional output multiple samples#
                                #   
                                #   # (Conditional on choose_single) If single: choose sample
                                #   conditionalPanel(
                                #     condition = 'input.choose_single == 1',
                                #     
                                #       uiOutput('whichSample')
                                #     ), # End conditional output, single sample #
                                #   actionButton("plot_submit", label = "Sumbit")
                                # ),
                                # 
                                # 
                                # 
                                # 
                                # # #------------------ Kendrick Conditionals -------------------#
                                # conditionalPanel(
                                #   condition = 'input.chooseplots == 2',
                                # 
                                #   # Drop down list: single samples or multiple?
                                #   selectInput('choose_single2', 'I want to plot using:',
                                #               choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples by group' = 2, 'A comparison of groups' = 3),
                                #               selected = 0),
                                # 
                                #   # (Conditional on choose_single) If Multiple: show options for grouping
                                #   conditionalPanel(
                                #     condition = 'input.choose_single2 == 2',
                                # 
                                #     fluidRow(
                                #       ######### MAKE GROUPS MUTUALLY EXCLUSIVE ##########
                                #       # Column with width 6: which samples are in Group 1?
                                #       column(12,
                                #              uiOutput('whichGroups1')
                                #       )#,
                                # 
                                #       # Column with width 6: which samples are in Group 2?
                                #       # column(6,
                                #       #        uiOutput('whichGroups2')
                                #       # )
                                #     )
                                # 
                                #   ), # End conditional output multiple samples#
                                # 
                                #   # (Conditional on choose_single) If single: choose sample
                                #   conditionalPanel(
                                #     condition = 'input.choose_single2 == 1',
                                # 
                                #     uiOutput('whichSample')
                                #   ), # End conditional output, single sample #
                                #   actionButton("plot_submit2", label = "Sumbit")
                                # )
                                uiOutput("plotUI")
                              ),# End sidebar conditionals on Visualize tab #
                              
                              mainPanel(
                                plotlyOutput('FxnPlot'),
                                width = 7,
                                conditionalPanel(
                                  condition = 'input.chooseplots == 1',
                                  # Set default width to 7
                                  
                                  # Drop down list: Use boundary?
                                  selectInput('vkbounds', 'Use Van Krevelen boundary set:',
                                              choices = c('BS1' = 'bs1', 'BS2' = 'bs2', 'None' = 0),
                                              selected = 'bs1')
                                  
                                ),
                                uiOutput('vk_colors')
                                #,
                                # conditionalPanel(
                                #   condition = 'input.chooseplots == 2',
                                #   width = 7,
                                #   uiOutput('vk_colors')
                                #   # Plot: Kendrick plot
                                #  # plotlyOutput('FxnPlot')
                                # )
                              )# End main panel on Visualize tab #
                              
                            )), # End Visualize tab #
                   
                   ################## Download Panel ##############################################
                   tabPanel('Download'), 
                   
                   ################## Glossary Panel ##############################################
                   tabPanel('Glossary',
                            #mainPanel(
                            #includeHTML("./README/Glossary.html")
                            includeMarkdown("./README/Glossary.md")
                            # )
                            
                   )
                   )
)

