list(
  # show a table of saved tables
  output$saved_db_table <- renderDT(tables$saved_db_info, options = list(scrollX = TRUE), 
                                    selection = 'single', escape = FALSE),
  
  # show the table selected in 'output$saved_db_table'
  output$selected_db_table <- renderDT({
    req(length(input$saved_db_table_rows_selected) > 0)
    tables$mapping_tables[[tables$saved_db_info[input$saved_db_table_rows_selected,'Tables']]]
  },
  options = list(scrollX = TRUE),
  selection = 'single', escape = FALSE),
  
  # determine which variable(s) to make a unique row for each
  output$which_unique <- renderUI({
    choices = c('None', 'Reactions' = 'REACTION', 'Modules' = 'MODULE', 'Pathways' = 'PATHWAY')
    chosen_vars <- c(T, input$comp2react_x, input$react2mod_x, input$mod2path_x)
    choices = choices[chosen_vars]
    
    pickerInput('which_unique', NULL, 
                choices = choices,
                multiple = FALSE)
  }),
  
  # KeggData table output
  output$kegg_table <- renderDT({
    req(!is.null(tables$kegg_table), cancelOutput = TRUE)
    temp <- tables$kegg_table
    target_columns = which(colnames(temp) %in% c('MODULE', 'REACTION', 'PATHWAY'))
    temp <- temp %>%
      mutate_at(target_columns, function(x){paste0('<div style="width:200px;overflow-x:auto">', x, '</div>')})
    temp
  },
  options = list(scrollX = TRUE, 
                 pageLength = 15, 
                 columnDefs = list(list(width = '200px', targets = '_all'))),
  server = TRUE, escape = FALSE),
  
  # MetaCyc table output
  output$mc_table <- renderDT({
    req(!is.null(tables$mc_table), cancelOutput = TRUE)
    temp <- tables$mc_table
    target_columns = which(colnames(temp) %in% c('MODULE', 'REACTION', 'SUPERPATHWAY'))
    temp <- temp %>%
      mutate_at(target_columns, function(x){paste0('<div style="width:200px;overflow-x:auto">', x, '</div>')})
    temp
  },
  options = list(scrollX = TRUE, 
                 pageLength = 15, 
                 columnDefs = list(list(width = '200px', targets = '_all'))),
  server = TRUE, escape = FALSE),
  
  # Display Kegg or Metacyc depending on button selection
  output$conditional_database_table <- renderUI({
    if(input$which_table == 1){
      DTOutput('kegg_table')
    }
    else if(input$which_table == 2){
      DTOutput('mc_table')
    }
  }),
  
  # button label for viewing saved tables
  output$n_saved_db_tables <- renderUI({
    tags$span(paste0('View saved tables: (', length(tables$mapping_tables), ')'))
  }),
  
  # summary counts from kegg table
  output$mapping_summary <- renderDT({
    req(revals$uploaded_data, revals$peakData2)
    req(!is.null(tables$kegg_table) | !is.null(tables$mc_table))
    
    if(input$which_table == 1){
      compare_table <- tables$kegg_table
    }
    else if(input$which_table == 2){
      compare_table <- tables$mc_table
    }
    
    n_peaks = revals$uploaded_data$e_meta %>% pluck(getMassColName(revals$uploaded_data)) %>% unique() %>% length()
    n_peaks_formula = revals$peakData2$e_meta %>% filter(!is.na(!!rlang::sym(getMFColName(revals$peakData2)))) %>% nrow()
    
    if(!is.null(tables$kegg_table)){
      n_map_to_kegg = tables$kegg_table %>% 
        dplyr::select(matches('compound', ignore.case=TRUE)) %>% 
        unique() %>% {!is.na(.)} %>% sum()
    }
    else n_map_to_kegg <- 'no table'
    
    if(!is.null(tables$mc_table)){
      n_map_to_mc = tables$mc_table %>% 
        dplyr::select(matches('compound', ignore.case=TRUE)) %>% 
        unique() %>% {!is.na(.)} %>% sum()
    }
    else n_map_to_mc <- 'no table'
    
    data.frame('Original No. peaks' = n_peaks, 'No. peaks with formula (filtered data)' = n_peaks_formula, 'No. peaks mapping to Kegg' = n_map_to_kegg, 'No. peaks mapping to Metacyc' = n_map_to_mc, stringsAsFactors = FALSE, check.names = FALSE)
    
  }),
  
  # kegg barplot
  output$kegg_barplot <- renderPlotly({
    req(tables$kegg_table, revals$peakData2)
    tables$kegg_table %>% 
      group_by(!!rlang::sym(getMassColName(revals$peakData2))) %>% 
      mutate(n = n()) %>% 
      slice(1) %>% 
      plot_ly(x = ~n, type = 'histogram') %>% 
      layout(title = 'Histogram of number of matching Metacyc compounds', 
             xaxis = list(title = 'Database elements peak maps to'), 
             yaxis = list(title = 'Number of peaks'))
  }),
  
  # mc barplot
  output$mc_barplot <- renderPlotly({
    req(tables$mc_table, revals$peakData2)
    tables$mc_table %>% 
      group_by(!!rlang::sym(getMassColName(revals$peakData2))) %>% 
      mutate(n = n()) %>% 
      slice(1) %>% 
      plot_ly(x = ~n, type = 'histogram') %>% 
      layout(title = 'Histogram of number of matching Metacyc compounds', 
             xaxis = list(title = 'Database elements peak maps to'), 
             yaxis = list(title = 'Number of peaks'))
  }),
  
  output$warnings_database <- renderUI({
    HTML(paste(revals$warningmessage_database, collapse = ""))
  })

)
