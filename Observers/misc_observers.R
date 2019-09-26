#uncomment to do postmortem debugging

# observeEvent(revals$peakData2,{
#   peakData2 <<- revals$peakData2
# })

# observeEvent(c(reactiveValuesToList(revals), reactiveValuesToList(tables)), {
#   revals_postmortem <<- reactiveValuesToList(revals)
#   tables_postmortem <<- reactiveValuesToList(tables)
# })

observeEvent(input$top_page,{
  toggleElement("js_saveplot", condition = input$top_page %in% c("Upload", "Groups", "Preprocess", "Quality Control", 'Filter', 'Visualize', 'Database Mapping'))
  
}, priority = 10, ignoreInit = FALSE)

# multipurpose observer for page transitions
observeEvent(input$top_page,{
  toggleElement('helpbutton', condition = input$top_page %in% c('Upload', 'Groups', 'Preprocess', 'Filter', 'Visualize', 'Download', 'Database Mapping', 'Quality Control'))

  if(input$top_page %in% c('Quality Control', 'Filter', 'Visualize', 'Database Mapping')){
      if(is.null(revals$peakData2)){
        revals$peakData2 <- revals$uploaded_data
      }
  }
  
  toggleElement('datareqs_video', condition = input$top_page == 'data_requirements')
}, priority = 10)

# show data requirements video on welcome page
# observeEvent(input$welcome_menu,{
#   toggleElement('datareqs_video', condition = input$welcome_menu == 'data_requirements')
# })

# control drawing of filter plot for large data, show warnings on qc and filter that dynamic plotting is disabled
observeEvent(uploaded_data_dim(),{
  if(uploaded_data_dim() <= max_cells){
    revals$redraw_largedata <- TRUE
  }
  
  revals$warningmessage_qc$not_dynamic <- if(uploaded_data_dim() > max_cells) "style = 'color:deepskyblue;font-weight:bold'>Dynamic plot disabled for large data.  Press 'Update Boxplot Axes' to display plot." else NULL
  revals$warningmessage_filter$not_dynamic <- if(uploaded_data_dim() > max_cells) "style = 'color:deepskyblue;font-weight:bold'>Dynamic plot disabled for large data.  Table and barplot will be displayed upon review." else NULL
})

video_footer <- function(id, url){
  tagList(
    bsButton(id, 'Video Walkthrough', onclick =paste0("window.open('", url, "', '_blank')"), style = 'info', icon = icon('facetime-video', lib = 'glyphicon')),
    modalButton("Dismiss")
  )
}

# Help Button
observeEvent(input$helpbutton,{
  if(input$top_page == "Upload"){
    showModal(
      modalDialog("",
                  tags$h4(tags$b("Upload your data and specify its structure")),
                  br(),
                  tags$p("Generally, the steps involved are:"),
                  HTML("<ol>
                          <li>Browse to and select your data file from the first download prompt.</li>
                          <li>Browse to and select your molecular identification file from the second download prompt.</li>
                          <li>In the 'Choose column with ID's' dropdown, indicate the column (contained in both files) which contains the ID's for each peak.</li>
                          <li>Indicate whether your molecular identification file contains elemental or full formula columns.</li>
                              <ul>
                                  <li>If you select Elemental Columns, 6 dropdowns will appear asking for which columns contain elemental information; FREDA will attempt to auto-populate them</li>
                                  <li>If you select Formulas, one dropdown will appear asking which column contains formula information</li>
                              </ul>
                          <li>Indicate whether or not there is isotopic information contained in the molecular identification file. (If you select no, proceed to click 'Process Data'.</li>
                          <li>If you selected yes, identify whether you would like to filter isotopic peaks, which column contains this information, and the symbol which identifies presence.  Then hit 'Process Data'</li>
                       </ol>"),
                  footer = video_footer('upload_video', 'https://youtu.be/MYccEwz67K4')
                  )
    )
    
  }
  else if (input$top_page == "Groups"){
    showModal(
      modalDialog("",
                  tags$h4(tags$b("Define groups of samples\n")),
                  br(),
                  tags$p("On the left panel, input a name for your group, select the samples to include, and click 'add this group'.\n
                       A table entry will appear on the right panel.  You can select a row of that table and click 'Remove selected group' to remove the 
                       group."),
                  br(), br(),
                  tags$p("Groups are allowed to share samples, there will be a warning under the side panel if you want to avoid this.
                         Plotting of overlapping groups is not allowed in some cases."),
                  footer = video_footer('groups_video', 'https://youtu.be/zyJADBxw3rA')
                    )
                  )
    
  }
  else if(input$top_page == "Preprocess"){
      showModal(
        modalDialog("",
                    tags$h4(tags$b("Have FREDA compute additional values of interest")),
                    tags$p("Check boxes to select which values you want calculated and then hit 'Process Data'.",
                            br(),br(), 
                            "The result of these calculations will be appended to your molecular identification file and can be used as filtering variables in the next tab.\n",
                           tags$span(style = 'font-weight:bold', 'Element ratios are selected by default as they are required to produce Van-Krevelen and Kendrick plots.'),
                           'Table summaries and an interactive histogram/bar chart of the values you selected will be generated.'
                           ),
                    footer = video_footer('preprocess_video', 'https://youtu.be/h99fuBt3Tc8')
                    )
                )
  }
  else if(input$top_page == "Filter"){
      showModal(
        modalDialog("",
                    tags$h4(tags$b("Filter the data by samples or by variables")), 
                    tags$p("The default options are to:"),
                    tags$ul(
                      tags$li("Retain certain samples and drop the rest (Sample Filter)."),
                      tags$li("Retain peaks within a mass range (Mass Filter)"),
                      tags$li("Retain peaks that appear a minimum number of times across all samples (Molecule Filter)"),
                      tags$li("Retain peaks that have elemental information - either elemental columns or a full formula column (Formula Filter)")
                    ),
                    tags$p("Additionally, one can filter by up to three variables contained in the molecular identification file.\n
                           As you select options, a plot will update showing the remaining observations after the application of each filter.\n"),
                    tags$p("Check boxes to select which filters to apply, specify filtering criteria by a range for numeric data or a selection of values for categorical data and then click 'Filter Data'"),
                    footer = video_footer('filter_video', 'https://youtu.be/zgRizald6x8')
                    )
              )
  }
  else if(input$top_page == "Quality Control"){
    showModal(
      modalDialog("",
                  tags$h4(tags$b("Inspect and save boxplots of your data")), 
                  tags$p("This is a simple page to review boxplots and summary statistics of your data."),
                  tags$p("Select samples and axes options and click 'Update Boxplot Axes' to draw a new plot."),
                  tags$p("Underneath the plot window you can save the currently displayed plot"),
                  footer = video_footer('filter_video', 'https://youtu.be/9r68469sDmE')
      )
    )
  }
  else if(input$top_page == "Visualize"){    
      showModal(
        modalDialog("",
                    tags$h4(tags$b('Generate plots from your processed data')),
                    br(),
                    HTML(
                      "<div><p>Roughly from top to bottom on the left panel, do the following:</p>
                          <ol>
                            <li>Select the type of plot you want to generate.</li>
                            <li>Choose whether you would like to plot a <b>single sample, multiple samples, or a comparison of two samples/groups</b>
                              <ul>
                                <li>If you selected a single sample, a dropdown will appear asking you to specify which one.</li>
                                <li>If you selected multiple samples by group, the same dropdown will appear.  Select samples that should be grouped.  <b>One</b> group will be created with the samples you specify.</li>
                                <li>If you selected a comparison of groups or a comparison of two samples, two dropdowns will appear.  Select samples or groups that you want to perform a comparison on.
                                    Further, there will be dropdowns requesting how you would like to compare the two groups.</li>
                              </ul>
                            </li>
                            <li>If desired, specify axis and title labels and hit 'Generate Plot'</li>
                          </ol>
                          
                      </div>"
                      
                    ),
                    hr(),
                    tags$p("A plot will appear and can be customized by selection menus beneath."),
                    tags$p("Plots can be saved and reviewed in the 'Savee plot and view saved plots' collapsible sidebar"),
                    footer = video_footer('visualize_video', 'https://youtu.be/fONBzKCyyiA')
                    )
                )
  }
  else if(input$top_page == "Database"){
    showModal(
      modalDialog("",
                  tags$h4(tags$b("Map your observed peak masses to compounds, reactions, modules, and pathways/superpathways.\n")),
                  br(),
                  tags$p('UNDER CONSTRUCTION'),
                  footer = video_footer('database_video', 'http://google.com')
                  )
                  )
  }
  else if(input$top_page == "Download"){
    showModal(
      modalDialog("",
                  tags$h4(tags$b("Download plots, .csv's and a report of the results of your FREDA session.")),
                  br(),
                  tags$p('Check boxes to tell FREDA to include the described file in your download'),
                  tags$p('If you created plots, a table will be displayed giving information on them.  The rows of this table can be selected, which tells FREDA that they should be included in the download.'),
                  tags$p('Once you are satisfied with your selection, click "Bundle up all selected items" which will prepare them for download.  Then click "Download bundle" to download all items as a compressed .zip file.'),
                  footer = video_footer('download_video', 'https://youtu.be/qL-XlK8s80s')
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