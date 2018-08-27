# show/hide button for certain pages
observeEvent(input$top_page,{
  toggleElement("helpbutton", condition = input$top_page %in% c("Upload", "Preprocess", "Filter", "Visualize"))
})

# Help Button
observeEvent(input$helpbutton,{
  if(input$top_page == "Upload"){
    showModal(
      modalDialog("",
                  tags$p("The purpose of this page is to upload your data and specify its structure, which at mininum must adhere to the description 
                         in the 'Data Requirements' section of the Welcome dropdown"),
                  br(),
                  tags$p("Generally, the steps involved are:"),
                  HTML("<ul>
                          <li>Browse to and select your data file and molecular identification file in the first and second upload prompts respectively.</li>
                          <li>Indicate the column, contained in both files, which contains the ID's for each peak.</li>
                          <li>Indicate whether your molecular identification file contains elemental or full formula columns.</li>
                              <ul>
                                  <li>If you select Elemental Columns, 6 dropdowns will appear asking for which columns contain elemental information; FREDA will attempt to auto-populate them</li>
                                  <li>If you select Formulas, one dropdown will appear asking which column contains formula information</li>
                              </ul>
                          <li>Indicate whether or not there is isotopic information contained in the molecular identification file. (If you select no, proceed to click 'Process Data'.</li>
                          <li>If you selected yes, identify whether you would like to filter isotopic peaks, which column contains this information, and the symbol which identifies presence.  Then hit 'Process Data'</li>
                       </ul>")
                  )
    )
    
  }
  
  if(input$top_page == "Preprocess"){
      showModal(
        modalDialog("",
                    tags$p("Here you can tell FREDA to compute certain values based on your input data.  The result of these calculations will be appended
                           to your molecular identification file and can be used as filtering variables in the next tab.\n"),
                    br(),
                    HTML("<p> Check boxes to select which values you want calculated and then hit 'Process Data'.  
                         <span style = font-weight:bold>Element ratios are selected by default as they are required to produce Van-Krevelen
                         and Kendrick plots.</span>  \n Table summaries and an interactive histogram/bar chart of the values you selected will be generated.<p>")
                    )
                )
  }
  else if(input$top_page == "Filter"){    
      showModal(
        modalDialog("",
                    tags$p("This page allows you to filter the data by various metrics.\n 
                           The default options are to:"),
                    tags$ul(
                      tags$li("Retain peaks within a mass range (Mass Filter)"),
                      tags$li("Retain peaks that appear a minimum number of times across all samples (Molecule Filter)"),
                      tags$li("Retain peaks that have elemental information - either elemental columns or a full formula column (Formula Filter)")
                    ),
                    tags$p("Additionally, one can filter by up to three variables contained in the molecular identification file.\n
                           As you select options, a plot will update showing the remaining observations after the application of each filter.\n"),
                    tags$p("Check boxes to select which filters to apply, specify filtering criteria by a range for numeric data or a selection of values for categorical data and then click 'Filter Data'"))
              )
  }
  else if(input$top_page == "Visualize"){    
      showModal(
        modalDialog("",
                    
                    HTML(
                      "<div><p>This page is used to generate plots from your processed data.  Roughly from top to bottom on the left panel, do the following:</p>
                          <ul>
                            <li>Select the type of plot you want to generate.</li>
                            <li>Choose whether you would like to plot a single sample, multiple samples, or a comparison of groups</li>
                            <ul>
                              <li>If you selected a single sample, a dropdown will appear asking you to specify which one.</li>
                              <li>If you selected multiple samples by group, the same dropdown will appear.  Select samples that should be grouped.  <b>One</b> group will be created with the samples you specify.</li>
                              <li>If you selected a comparison of groups, two group dropdowns will appear.  Select samples that you want included in each of the two groups.
                                  Further, there will be dropdowns requesting how you would like to compare the two groups.</li>
                            </ul>
                            <li>If desired, specify axis and title labels and hit 'Generate Plot'</li>
                          </ul>
                          
                      </div>"
                      
                    ),
                    
                    tags$p("A plot will appear and can be customized to color by certain calculated values.  
                           Van Krevelen boundaries can be displayed for VK-plots.
                           Custom scatterplots will allow for selection of arbitrary x and y axes."),
                    tags$p("If you want to change plot labels without recalculating the plot, you can hit 'Update Labels' instead of 'Generate Plot'"),
                    hr(),
                    tags$p("Certain menu options may 'grey out' during navigation, indicating disabled functionality for a plot type, 
                           or because certain values were not calculated during preprocessing")
                    
                    )
                )
  }
  
})

# # Help text
# observeEvent(input$filter_help,{
#   showModal(
#     modalDialog("",
#                 tags$p("This page allows you to filter the data by various metrics.\n 
#                        The default options are to:", style = "color:CornFlowerBlue"),
#                 tags$ul(
#                   tags$li("Retain peaks within a mass range (Mass Filter)"),
#                   tags$li("Retain peaks that appear a minimum number of times across all samples (Molecule Filter)"),
#                   tags$li("Retain peaks that have elemental information - either elemental columns or a full formula column (Formula Filter)"),
#                   style = "color:CornFlowerBlue"
#                 ),
#                 tags$p("Additionally, one can filter by up to three variables contained in the molecular identification file.\n
#                        As you select options, a plot will update showing the remaining observations after the application of each filter.\n",
#                        style = "color:CornFlowerBlue"),
#                 tags$p("Check boxes to select which filters to apply, specify filtering criteria by a range for numeric data or a selection of values for categorical data and then click 'Filter Data'",
#                        style = "color:CornFlowerBlue"))
#     )
# })
# 
# # Help Button
# observeEvent(input$visualize_help,{
#   showModal(
#     modalDialog("",
#                 tags$p("This page is used to generate plots from your processed data.  In order from top to bottom on the left panel, do the following:\n",
#                        style = "color:CornFlowerBlue"),
#                 tags$ul(
#                   tags$li("Select the type of plot you want to generate."),
#                   tags$li("Choose whether you would like to plot a single sample, multiple samples, or a comparison of groups"),
#                   tags$li("If you selected a single sample, specify which one.  If you selected multiple samples by group, select samples that 
#                           should be grouped. If you selected a comparison of groups, two group dropdowns will appear; select samples that
#                           you want included in each of the two groups"),
#                   tags$li("If desired, specify axis and title labels and hit 'Generate Plot'\n"),
#                   
#                   style = "color:CornFlowerBlue"),
#                 tags$p("A plot will appear and can be customized to color by certain calculated values.  
#                        Van Krevelen boundaries can be displayed for VK-plots.
#                        Custom scatterplots will allow for selection of arbitrary x and y axes.", style = "color:CornFlowerBlue"),
#                 hr(),
#                 tags$p("Certain menu options may 'grey_out' during navigation, indicating disabled functionality for a plot type, 
#                        or because certain values were not calculated during preprocessing")
#     )
#   )
# })

# tags$p("This page is used to generate plots from your processed data.  In order from top to bottom on the left panel, do the following:\n",
#        style = "color:CornFlowerBlue"),
# tags$ul(
#   tags$li("Select the type of plot you want to generate."),
#   tags$li("Choose whether you would like to plot a single sample, multiple samples, or a comparison of groups"),
#   tags$ul(
#     tags$li("If you selected a single sample, specify which one."),
#     tags$li("If you selected multiple samples by group, select samples that should be grouped.  Specifically, one group will be created with the samples you specify"),
#     tags$li("If you selected a comparison of groups, two group dropdowns will appear; select samples that you want included in each of the two groups.<br/>
#             Further, there will be dropdowns requesting how you would like to compare the two groups.  The options allow you to specify how one should determine presence/absence of a peak
#             within a group, how uniqueness should be determined (by simple presence/absence or using a G-test), and the thresholds/p-values used in the comparisons")
#   ),
#   tags$li("If desired, specify axis and title labels and hit 'Generate Plot'\n"),
#   
#   style = "color:CornFlowerBlue"),
# tags$p("A plot will appear and can be customized to color by certain calculated values.  
#        Van Krevelen boundaries can be displayed for VK-plots.
#        Custom scatterplots will allow for selection of arbitrary x and y axes.", style = "color:CornFlowerBlue"),
# hr(),
# tags$p("Certain menu options may 'grey_out' during navigation, indicating disabled functionality for a plot type, 
#        or because certain values were not calculated during preprocessing")