list(
  # Y axis scale select for boxplots
  output$qc_plot_scale <- renderUI({
    validate(need(!is.null(revals$peakData2), message = "No data object found, please verify you have successfully uploaded data"))
    pickerInput("qc_plot_scale", "Plot on scale:", 
                choices = list('Log base 2' = 'log2', 'Log base 10'='log10', 'Natural log'='log', 
                               'Presence/absence' = 'pres', 'Raw intensity'='abundance'), 
                selected = 'pres')
    
  }),
  
  # Group selection
  output$qc_select_groups <- renderUI({
    pickerInput("qc_select_groups", "Select groups:", 
                choices = names(revals$groups_list),
                multiple = TRUE
    )
  }),
  
  # Boxplots
  output$qc_boxplots <- renderPlotly({
    input$update_boxplot_axes
    req(!is.null(revals$peakData2))
    req(!is.null(input$qc_plot_scale))
    if(uploaded_data_dim() > max_cells) isolate(on.exit(revals$redraw_largedata <- FALSE))
    req(isolate(revals$redraw_largedata))
    
    color_by <- if(isTRUE(input$qc_plot_scale %in% c('log2', 'log10', 'log', 'abundance'))) 'groups' else 'molform'
    ds = attributes(revals$peakData2)$data_info$data_scale
    
    # subset the data if a group is selected
    if(isTRUE(all(input$qc_select_groups %in% names(revals$groups_list)) & !is.null(input$qc_select_groups))){
      # get set of unique samples in all selected groups
      samples <- revals$groups_list[input$qc_select_groups] %>% unlist() %>% unique() %>% setdiff(revals$removed_samples)
      temp_peakData2 <- subset(revals$peakData2, samples = samples)
    } 
    else temp_peakData2 <- revals$peakData2
    
    # if their data scale selection does not match the object's data scale, transform before plotting
    if(isTRUE(ds != input$qc_plot_scale)){
      p <- plot(edata_transform(temp_peakData2, input$qc_plot_scale), 
                xlabel=isolate(input$qc_boxplot_xlab), ylabel=isolate(input$qc_boxplot_ylab), 
                title = isolate(input$qc_boxplot_title),colorBy = color_by) %>% layout(margin = list(b = 100), xaxis = list(tickangle = 45))
    }
    else p <- plot(temp_peakData2, xlabel=isolate(input$qc_boxplot_xlab), ylabel=isolate(input$qc_boxplot_ylab), 
                   title = isolate(input$qc_boxplot_title), colorBy=color_by) %>% layout(margin = list(b = 100), xaxis = list(tickangle = 45))
    
    isolate(plots$last_plot[[input$top_page]] <- p)
    
    toWebGL(p)
  }),
  
  # qc warnings
  output$warnings_qc <- renderUI({
    HTML(lapply(revals$warningmessage_qc, function(el){
      paste0("<p ", el, "</p>")
    }) %>%
      paste(collapse = "")
    )
  })
)