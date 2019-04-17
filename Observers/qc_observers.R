observeEvent(input$goto_filter_fromqc,{
  updateTabsetPanel(session, 'top_page', 'Filter')
})

# temporarily allow drawing of plot for large data
observeEvent(input$update_boxplot_axes,{
  revals$redraw_largedata <- TRUE
}, priority = 10)

# inform user if data is large and dynamic plotting is disabled
observeEvent(peakData2_dim(),{
  revals$warningmessage_qc <- if(peakData2_dim() > max_cells) "style = 'color:deepskyblue;font-weight:bold'>Dynamic plot disabled for large data.  Press 'Update Boxplot Axes' to display plot." else NULL
})

# save currently displayed boxplot
observeEvent(input$add_qc_boxplot,{
  # counter which begins at 1 even if a filter reset has occurred.
  ind <- input$add_plot + input$add_qc_boxplot - revals$reset_counter
  # initialize a new line
  newLine <- data.frame(FileName = NA, PlotType = NA, SampleType = NA, Group_1_Samples = NA,  Group_2_Samples = NA, BoundarySet = NA,
                        ColorBy = NA, x_var = NA, y_var = NA, pres_thresh = NA, absn_thresh = NA, pval = NA, compfn = NA)
  
  # which type of plot
  newLine$FileName <- ifelse(is.na(input$qc_boxplot_title) | input$qc_boxplot_title == '', paste0('Plot_', ind), paste0('Plot', ind, '_', input$qc_boxplot_title))
  newLine$PlotType <- paste0('QC boxplot with scale:  ', input$qc_plot_scale)

  # Sample(s) in The first group (depends on input$choose_single to decide if this is a single or multiple sample list)
  newLine$Group_1_Samples <- if(!is.null(input$qc_select_groups)) (revals$groups_list[input$qc_select_groups] %>% unlist() %>% unique() %>% setdiff(revals$removed_samples) %>% paste(collapse=', ')) else 'All Samples'

  # Prettified colnames
  colnames(newLine) <- c("File Name", "Plot Type", "Sample Type", "Group 1 Samples", "Group 2 Samples", "Boundary Set",
                         "Color By Variable", "X Variable", "Y Variable", "Presence Threshold", "Absence Threshold", "P-Value", "Comparisons Method")
  
  if (ind == 1) {
    # replace the existing line on the first click
    parmTable$parms[ind, ] <- newLine
    exportTestValues(parmTable_1 = parmTable$parms)
  } else {
    # concat every new line after
    parmTable$parms <- rbind(parmTable$parms, newLine)
    exportTestValues(parmTable_1 = parmTable$parms)
  }
  
  # store the current plot in a reactiveValue for later download
  revals$plot_list[[ind]] <- revals$current_qc_boxplot
  
  #dont save the plot twice
  disable('add_qc_boxplot')
  shinyjs::show('qc_download_ok')
  
})