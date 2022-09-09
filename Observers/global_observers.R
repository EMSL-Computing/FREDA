# uncomment to do postmortem debugging/enable browser button

# observeEvent(revals$peakData2,{
#   peakData2 <<- revals$peakData2
# })
#
# observeEvent(c(reactiveValuesToList(revals), reactiveValuesToList(tables)), {
#   revals_postmortem <<- reactiveValuesToList(revals)
#   tables_postmortem <<- reactiveValuesToList(tables)
# })
#
# observeEvent(reactiveValuesToList(plots), {
#   plots_postmortem <<- reactiveValuesToList(plots)
# })

observeEvent(input$debugger, {
  browser()
})

#

# display buttons depending on page selection
observeEvent(input$top_page, {
  condition = input$top_page %in% c("Upload", "Groups", "Preprocess", "Quality Control", 'Filter', 'Visualize', 'Database Mapping', 'Linked Plots')
  toggleElement("viewplots", condition = condition)
  toggleElement('saveplot', condition = condition)

}, priority = 10, ignoreInit = FALSE)

# observeEvent(input$top_page,{
#   toggleElement("js_saveplot", condition = input$top_page %in% c("Upload", "Groups", "Preprocess", "Quality Control", 'Filter', 'Visualize', 'Database Mapping', 'Linked Plots'))
#
# }, priority = 10, ignoreInit = FALSE)

# multipurpose observer for page transitions
observeEvent(input$top_page, {
  toggleElement('helpbutton', condition = input$top_page %in%
    c('Upload', 'Groups', 'Preprocess', 'Filter', 'Visualize', 'Download', 'Quality Control', 'Linked Plots', 'Database Mapping'))

  if (input$top_page %in% c('Quality Control', 'Filter', 'Visualize', 'Database Mapping')) {
    if (is.null(revals$peakData2)) {
      revals$peakData2 <- revals$uploaded_data
    }
  }

  # button specific to data requirements page
  toggleElement('datareqs_video', condition = input$top_page == 'data_requirements')
}, priority = 10)

# # store the current plot in a list of all plots
observeEvent(input$saveplot, {
  req(!is.null(plots$last_plot[[input$top_page]]))
  # keeps plot names unique
  ind <- input$saveplot - revals$reset_counter

  # initialize a new line
  # TODO: standardize column names for newrecords and plot_table, currently just reassigns column names before rowbinding, seems bad.
  newRecords <- data.frame(FileName = NA, Download = dt_checkmark, PlotType = NA, SampleType = NA, Group_1_Samples = NA, Group_2_Samples = NA, BoundarySet = NA,
    ColorBy = NA, x_var = NA, y_var = NA, pres_thresh = NA, absn_thresh = NA, pval = NA, compfn = NA, stringsAsFactors = FALSE)

  # Main viztab, single record
  if (input$top_page == 'Visualize') {
    # which type of plot
    newRecords$FileName <- ifelse(is.na(input$title_input) | input$title_input == '', sprintf('Plot_%s', ind), sprintf('Plot_%s_(%s)', ind, input$title_input))
    newRecords$PlotType <- input$chooseplots
    # Single or Multiple Samples
    newRecords$SampleType <- ifelse(input$chooseplots == "PCOA Plot", 'None',
      switch(as.character(input$choose_single), '1' = 'Single Sample', '2' = 'Single Group of Samples', '3' = 'Comparison of Two Groups', '4' = 'Comparison of Two Samples')
    )
    # Sample(s) in The first group (depends on input$choose_single to decide if this is a single or multiple sample list)
    newRecords$Group_1_Samples <- ifelse(input$choose_single %in% c(1, 2), yes = paste(input$whichSamples, collapse = ','), no = paste(g1_samples(), collapse = ','))
    # Sample(s) in the second group. Automatically NA if input$choose_single is single sample or single group
    newRecords$Group_2_Samples <- ifelse(input$choose_single %in% c(3, 4), yes = paste(g2_samples(), collapse = ','), no = 'None')
    # Boundary set borders to use (NA for non-Van Krevelen plots)
    newRecords$BoundarySet <- ifelse(input$chooseplots == 'Van Krevelen Plot', yes = ifelse(input$vkbounds == 0, 'None', input$vkbounds), no = 'None')
    newRecords$ColorBy <- ifelse(input$chooseplots == 'PCOA Plot', 'None', input$vk_colors)
    newRecords$x_var <- input$scatter_x
    newRecords$y_var <- input$scatter_y

    newRecords$x_var <- switch(input$chooseplots, 'Van Krevelen Plot' = 'O:C Ratio', 'Kendrick Plot' = 'Kendrick Mass',
      'Density Plot' = input$vk_colors, 'Custom Scatter Plot' = input$scatter_x,
      'PCOA Plot' = paste0('Principal Component ', input$scatter_x))
    newRecords$y_var <- switch(input$chooseplots, 'Van Krevelen Plot' = 'H:C Ratio', 'Kendrick Plot' = 'Kendrick Defect',
      'Density Plot' = 'Density', 'Custom Scatter Plot' = input$scatter_y,
      'PCOA Plot' = paste0('Principal Component ', input$scatter_y))


    newRecords$compfn <- ifelse(isTRUE(input$choose_single %in% c(3, 4)) & isTRUE(input$summary_fxn != ""),
      switch(input$summary_fxn,
        "select_none" = "None",
        "uniqueness_gtest" = "G test",
        "uniqueness_nsamps" = "Presence/absence thresholds",
        "uniqueness_prop" = "Presence/absence thresholds"),
      no = "None")

    # special storage options for single and two-group plots
    if (input$choose_single == 2) {
      # store edata_result of summarizeGroups()
      plots$plot_data[[newRecords$FileName]] <- plot_data()$e_data
    }

    if (input$choose_single %in% c(3, 4)) {
      # store edata result of summarizeGroupComparisons()
      plots$plot_data[[newRecords$FileName]] <- plot_data()$e_data

      # parameters specific to group comparison plots
      newRecords$pres_thresh <- input$pres_thresh
      newRecords$absn_thresh <- input$absn_thresh
      newRecords$pval <- input$pval
    }
  }
  # QC tab, single record
  else if (input$top_page == 'Quality Control') {
    # which type of plot
    newRecords$FileName <- ifelse(is.na(input$qc_boxplot_title) | input$qc_boxplot_title == '', sprintf('Plot_%s', ind), sprintf('Plot_%s_%s', ind, input$qc_boxplot_title))
    newRecords$PlotType <- paste0('QC boxplot with scale:  ', input$qc_plot_scale)

    # Sample(s) in The first group (depends on input$choose_single to decide if this is a single or multiple sample list)
    newRecords$Group_1_Samples <- if (!is.null(input$qc_select_groups)) (revals$groups_list[input$qc_select_groups] %>% unlist() %>% unique() %>% setdiff(revals$removed_samples) %>% paste(collapse = ', ')) else 'All Samples'
  }
  # Linked plots, two plots are saved
  else if (input$top_page == "Linked Plots") {
    temp <- data.frame()
    for (name in lapply(plots$last_plot[[input$top_page]], names)) {
      # copy the information from the selected linked plot and just change its name
      newRecords[1, ] <- linked_plots_table() %>% filter(`File Name` == name) %>% slice(1)
      newRecords$FileName <- sprintf('Linked_Pair_%s_(%s)', ind, name)
      newRecords$PlotType <- sprintf('(LINKED) %s', newRecords$PlotType)
      temp <- rbind(temp, newRecords)
    }
    newRecords <- temp
    rm(temp)
  }
  else {
    newRecords$FileName <- sprintf('Plot_%s:%s_tab', ind, input$top_page)
  }

  # store the current plots in a reactiveValue for later download
  if (inherits(plots$last_plot[[input$top_page]], 'list')) {
    for (i in 1:nrow(newRecords)) {
      # TODO:  Currently storing a single element list for every element of plots$last_plot[[input$top_page]], a bit ugly.
      plots$plot_list[[newRecords$FileName[i]]] <- plots$last_plot[[input$top_page]][[i]][[1]]
    }
  }
  else {
    plots$plot_list[[newRecords$FileName]] <- plots$last_plot[[input$top_page]]
  }

  colnames(newRecords) <- colnames(plots$plot_table)
  plots$plot_table <- plots$plot_table %>% rbind(newRecords)

  # TODO: check table and plot list name consistency

  plots$last_plot[[input$top_page]] <- NULL

  # wooooo css
  addCssClass("viewplots", "pulse_bow")
  Sys.sleep(0.6)
  removeCssClass("viewplots", "pulse_bow")

})

# display modal dialog of saved plot info
observeEvent(input$viewplots, {
  showModal(modalDialog(
    tags$h4('Click on a row to view a plot.  You can select/deselect a plot for inclusion in the final download (Download Tab).'),
    DTOutput("modal_plot_table"),
    uiOutput('modal_plot'),

    footer = tagList(
      # div(disabled(actionButton(inputId = "add_plot", width = '100%', label = "Save Current Plot for Later Download", icon = icon("save"))))
      div(style = 'float:left',
        bsButton('mark_plot', 'Select/de-select for download', icon = icon('minus')),
        bsButton('remove_plot', 'Remove selected plot', icon = icon('remove'))
      ),
      modalButton("Dismiss")
    ),
    size = 'l')
  )
})

# update button text for adding/removing from download queue
observeEvent(c(input$modal_plot_table_rows_selected, input$download_plot_table_rows_selected), {
  cond = plots$plot_table[input$modal_plot_table_rows_selected, 2] == dt_minus
  cond_download = plots$plot_table[input$download_plot_table_rows_selected, 2] == dt_minus

  if (isTRUE(cond)) {
    updateButton(session, 'mark_plot', icon = icon('plus'))
  }
  else {
    updateButton(session, 'mark_plot', icon = icon('minus'))
  }

  if (isTRUE(cond_download)) {
    updateButton(session, 'mark_plot_download', icon = icon('plus'))
  }
  else {
    updateButton(session, 'mark_plot_download', icon = icon('minus'))
  }

})

# remove or add a plot from the download queue
observeEvent(input$mark_plot, {
  req(length(input$modal_plot_table_rows_selected) > 0)
  cond = plots$plot_table[input$modal_plot_table_rows_selected, 2] == dt_minus

  if (cond) {
    plots$plot_table[input$modal_plot_table_rows_selected, 2] <- dt_checkmark
  }
  else {
    plots$plot_table[input$modal_plot_table_rows_selected, 2] <- dt_minus
  }
})

# remove the selected plot on button click
# need to remove the entry plots$plot_table and the corresponding plot in plots$plot_list
observeEvent(input$remove_plot, {
  req(length(input$modal_plot_table_rows_selected) > 0)
  plot_name = plots$plot_table[input$modal_plot_table_rows_selected, 1]

  plots$plot_table <- plots$plot_table %>% filter(`File Name` != plot_name)
  plots$plot_list[[plot_name]] <- NULL
  plots$plot_data[[plot_name]] <- NULL
})

# control drawing of filter plot for large data, show warnings on qc and filter that dynamic plotting is disabled
observeEvent(uploaded_data_dim(), {
  if (uploaded_data_dim() <= max_cells) {
    revals$redraw_largedata <- TRUE
  }

  revals$warningmessage_qc$not_dynamic <- if (uploaded_data_dim() > max_cells) "style = 'color:deepskyblue;font-weight:bold'>Dynamic plot disabled for large data.  Press 'Update Boxplot Axes' to display plot." else NULL
  revals$warningmessage_filter$not_dynamic <- if (uploaded_data_dim() > max_cells) "style = 'color:deepskyblue;font-weight:bold'>Dynamic plot disabled for large data.  Table and barplot will be displayed upon review." else NULL
})

video_footer <- function(id, url) {
  tagList(
    bsButton(id, 'Video Walkthrough', onclick = paste0("window.open('", url, "', '_blank')"), style = 'info', icon = icon('facetime-video', lib = 'glyphicon')),
    modalButton("Dismiss")
  )
}

# Help Button
observeEvent(input$helpbutton, {
  if (input$top_page == "Upload") {
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
  else if (input$top_page == "Groups") {
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
  else if (input$top_page == "Preprocess") {
    showModal(
      modalDialog("",
        tags$h4(tags$b("Have FREDA compute additional values of interest")),
        tags$p("Check boxes to select which values you want calculated and then hit 'Process Data'.",
          br(), br(),
          "The result of these calculations will be appended to your molecular identification file and can be used as filtering variables in the next tab.\n",
          tags$span(style = 'font-weight:bold', 'Element ratios are selected by default as they are required to produce Van-Krevelen and Kendrick plots.'),
          'Table summaries and an interactive histogram/bar chart of the values you selected will be generated.'
        ),
        footer = video_footer('preprocess_video', 'https://youtu.be/h99fuBt3Tc8')
      )
    )
  }
  else if (input$top_page == "Filter") {
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
  else if (input$top_page == "Quality Control") {
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
  else if (input$top_page == "Visualize") {
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
  else if (input$top_page == "Linked Plots") {
    showModal(
      modalDialog("",
        tags$h4(tags$b("Interactively compare two plots side by side.")),
        br(),
        tags$p('In the top collapsible panel, you will see a list of valid plots.  Not all plots are valid, currently linking supports Van-Krevelen, Kendrick, single sample density, and custom scatter plots'),
        tags$p('Select exactly two of these plots by clicking on the entries in the table and then click "Compare These Plots"'),
        tags$p('In the bottom panel, you will see your plots appear side-by side.  Click and drag to select points or bins in one plot and FREDA will highlight both your selection, and the corresponding points in the other plot.')
      )
    )
  }
  else if (input$top_page == "Database Mapping") {
    showModal(
      modalDialog("",
        tags$h4(tags$b("Map your observed peak masses to compounds, reactions, modules, and pathways/superpathways.\n")),
        br(),
        tags$p('In the left panel, specify the following from top to bottom:'),
        tags$ul(
          tags$li("Select if you would like to use Kegg or Metacyc to map your peaks."),
          tags$li("A number which, if a peak maps to more than that many database elements, it is exluded from the results."),
          tags$li("Which database values you would like included.  Any value selected must also have the value(s) to its left selected.  I.e. if you would like Kegg modules, you must also select Kegg reactions"),
          tags$li("Which variable that you have calculated to expand such that each row of the results contains a single value of this variable.")
        )
      )
    )
  }
  else if (input$top_page == "Download") {
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
