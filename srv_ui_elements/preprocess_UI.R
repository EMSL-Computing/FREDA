list(
  # Populate List from CSV File calculation_options.csv
  output$which_calcs <- renderUI({
    choices <- calc_opts$Function
    names(choices) <- calc_opts$DisplayName
    tooltip_checkbox("tests", "What Values Should be Calculated?", choices, selected = c("calc_element_ratios", "calc_kendrick"),
                     extensions = lapply(1:length(choices), function(i){
                       div(style = "color:deepskyblue;display:inline-block",
                           tipify(icon("question-sign", lib = "glyphicon"), title = calc_opts$Info[i], placement = "top", trigger = 'hover')
                       )
                     })
    )
  }),
  
  # Warnings for preprocess tab
  output$warnings_preprocess <- renderUI({
    HTML(paste(revals$warningmessage_preprocess, collapse = ""))
  }),
  
  # Plot the histogram chosen above
  # Depends on: which_hist
  output$preprocess_hist <- renderPlotly({
    
    # Error handling: Require some columns to be selected
    req(input$which_hist)
    
    isolate({
      # Save column name for later display
      columnName <- input$which_hist
      
      # set display name
      displayName <- calc_vars %>% filter(ColumnName == columnName) %>%
        pluck("DisplayName")
      
      # Plot histogram using plotly
      p <- plot_ly(x = revals$uploaded_data$e_meta[,columnName], type = 'histogram') %>%
        layout( title = paste('Observed distribution of', displayName),
                scene = list(
                  xaxis = list(title = displayName),
                  yaxis = list(title = 'Frequency')))
      p$elementId <- NULL
      
      #____test export_____
      exportTestValues(preprocess_hist = p, hist_attrs = p$x$attrs[[p$x$cur_data]], hist_layout = p$x$layout, hist_visdat = p$x$visdat[[p$x$cur_data]]())
      
      plots$last_plot <- p
      
      return(p)
    })
    
  }) # End process_hist
)