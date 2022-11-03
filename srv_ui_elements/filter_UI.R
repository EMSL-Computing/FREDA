list(
  #### Main Panel (Filter Tab) ####
  
  # Show table from summaryFilt
  # Depends on: summaryFilterDataFrame
  output$summary_filter <- renderTable({
    
    # Set default results: NA if no filters selected
    afterResults <- c(NA, NA, NA, NA)
    last_filt_ind <- max(which(summaryFilterDataFrame()[,'dispText'] != 'NA'))
    afterResults <- unlist(summaryFilterDataFrame()[last_filt_ind, c('sum_peaks', 'assigned', 'min_mass', 'max_mass')])
    
    # Find which row in summaryFilterDataFrame represents the Unfiltered information
    rowNum = which(summaryFilterDataFrame()$data_state == 'Unfiltered')
    
    # Create a dataframe out of Before and After results from summaryFilterDataFrame
    summary_table <- data.frame('Before' = as.numeric(unlist(summaryFilterDataFrame()[rowNum, c('sum_peaks', 'assigned', 
                                                                                                'min_mass', 'max_mass')])),
                                'After' = as.numeric(afterResults),
                                row.names = c('Number of peaks',
                                              'Number of peaks assigned a formula', 
                                              'Minimum mass observed', 
                                              'Maximum Mass observed'), stringsAsFactors = FALSE)
    
    # Format the last two rows of this table to have decimal places and the first two rows to have a comma
    # this requires converting the table to a string, keep two copies in case the string changes
    display_table <- summary_table
    
    display_table[1:2, 1] <- formatC(round(summary_table[1:2,1]), big.mark = ",", format = "d")
    display_table[1:2, 2] <- formatC(round(summary_table[1:2,2]), big.mark = ",", format = "d")
    display_table[3:4, 1] <- formatC(round(summary_table[3:4, 1], digits = 4), format = "f", big.mark = ",")
    display_table[3:4, 2] <- formatC(round(summary_table[3:4, 2], digits = 4), format = "f", big.mark = ",")
    
    #___test-export___
    exportTestValues(rem_peaks = as.numeric(summaryFilterDataFrame()[last_filt_ind, 'sum_peaks']))
    
    return(display_table)
  }, # End code portion of summary_filter
  
  # Options: include rownames, no decimal places
  rownames = TRUE
  ), # End summary_filter
  
  # Plot bar chart
  # Depends on: summaryFilterDataFrame
  output$barplot_filter <- renderPlot({
    summaryFilterDataFrame()
    req(isolate(revals$redraw_filter_plot) == TRUE | (isolate(uploaded_data_dim()) > max_cells))
    
    filter_inds <- c(TRUE, isolate(input$samplefilter) & length(isolate(input$keep_samples)) > 0, isolate(input$massfilter), isolate(input$molfilter), isolate(input$formfilter), 
                     any(c(isolate(input$custom1), isolate(input$custom2), isolate(input$custom3)) != "Select item") & isolate(input$customfilterz))
    
    which_filts <- c("Unfiltered", "After Sample Filter", "After Mass Filter", "After Molecule Filter", "After Formula Filter", "After Custom Filters")[filter_inds]
    
    # Melt dataframe into 2 objects
    ggdata_barplot <- melt(summaryFilterDataFrame()[,c('data_state', 'assigned', 'unassigned')]) %>% filter(data_state %in% which_filts)
    ggdata_text <- summaryFilterDataFrame()[, c('data_state', 'sum_peaks', 'dispText')] %>% filter(data_state %in% which_filts)
    
    # Aesthetic purposes: get max height, divide by 30, use as offset in geom_text
    num_displaced <- round(ggdata_text[1, 2] / 35, digits = -1)
    
    shinyjs::hide('draw_large_filter_plot')
    
    # Plot using ggplot2
    p <- ggplot() + geom_bar(data = ggdata_barplot, aes(x = data_state, y = value, fill = variable), stat = 'identity') +
          theme_bw(base_size = 16) + 
          geom_text(data = ggdata_text, aes(x = data_state, y = sum_peaks + num_displaced, label = dispText), size = 6) + 
          scale_fill_brewer(name = 'Peak Type', labels = c('Formulae Assigned', 'Formulae Unassigned'), palette="Blues") + 
          labs(x = 'Data State', y = 'Number of peaks') 
    
    isolate(plots$last_plot[[input$top_page]] <- p)
    
    p
    
  }), # End barplot_filter #
  
  #### Sidebar Panel (Filter Tab) ####
  
  # Drop down list: Minimum Number of observations
  # Depends on edata_cnames()
  output$minobs <- renderUI({
    selectInput('minobs', "Minimum number observed", choices = seq(1, max(length(input$keep_samples),1), 1), selected = 2)
  }), # End minobs
  
  # Sample selection for sample filter
  output$filter_samples <- renderUI({
    req(revals$uploaded_data)
    inds = grepl(input$filter_regex, revals$uploaded_data$f_data[,getFDataColName(revals$uploaded_data)])
    subset = revals$uploaded_data$f_data[,getFDataColName(revals$uploaded_data)][inds]
    selectInput('keep_samples', NULL, choices = revals$uploaded_data$f_data[,getFDataColName(revals$uploaded_data)], selected = subset, multiple = TRUE)
  }),
  
  ### icon control for filter tab collapsible sections
  output$massfilter_icon <- renderUI({
    req(input$top_page == 'Filter')
    clicked = if(input$massfilter) div(id = 'ok_massfilt', style = 'color:deepskyblue;display:inline-block;margin-right:5px', icon('ok', lib='glyphicon')) else NULL
    if('massfilt_collapse' %in% input$filter_sidebar){
      div(
        clicked, 
        icon('chevron-up', lib = 'glyphicon')
      )
    }
    else{
      div(
        clicked, 
        icon('chevron-down', lib = 'glyphicon')
      )
    }
  }),
  
  output$samplefilter_icon <- renderUI({
    req(input$top_page == 'Filter')
    clicked = if(input$samplefilter) div(id = 'ok_samplefilter', style = 'color:deepskyblue;display:inline-block;margin-right:5px', icon('ok', lib='glyphicon')) else NULL
    if('samplefilt_collapse' %in% input$filter_sidebar){
      div(
        clicked, 
        icon('chevron-up', lib = 'glyphicon')
      )
    }
    else{
      div(
        clicked, 
        icon('chevron-down', lib = 'glyphicon')
      )
    }
  }),
  
  output$formfilter_icon <- renderUI({
    req(input$top_page == 'Filter')
    clicked = if(input$formfilter) div(id = 'ok_formfilter', style = 'color:deepskyblue;display:inline-block;margin-right:5px', icon('ok', lib='glyphicon')) else NULL
    if('formfilt_collapse' %in% input$filter_sidebar){
      div(
        clicked, 
        icon('chevron-up', lib = 'glyphicon')
      )
    }
    else{
      div(
        clicked, 
        icon('chevron-down', lib = 'glyphicon')
      )
    }
  }),
  
  output$molfilter_icon <- renderUI({
    req(input$top_page == 'Filter')
    clicked = if(input$molfilter) div(id = 'ok_molfilter', style = 'color:deepskyblue;display:inline-block;margin-right:5px', icon('ok', lib='glyphicon')) else NULL
    if('molfilt_collapse' %in% input$filter_sidebar){
      div(
        clicked, 
        icon('chevron-up', lib = 'glyphicon')
      )
    }
    else{
      div(
        clicked, 
        icon('chevron-down', lib = 'glyphicon')
      )
    }
  }),
  
  output$customfilter_icon <- renderUI({
    req(input$top_page == 'Filter')
    clicked = if(input$customfilterz) div(id = 'ok_customfilterz', style = 'color:deepskyblue;display:inline-block;margin-right:5px', icon('ok', lib='glyphicon')) else NULL
    if('customfilt_collapse' %in% input$filter_sidebar){
      div(
        clicked, 
        icon('chevron-up', lib = 'glyphicon')
      )
    }
    else{
      div(
        clicked, 
        icon('chevron-down', lib = 'glyphicon')
      )
    }
  }),
  #
  
  # filter warnings
  output$warnings_filter_UI <- renderUI({
    HTML(lapply(revals$warningmessage_filter, function(el){paste0("<p ", el, "</p>")}) %>%
           paste(collapse = ""))
  })
)