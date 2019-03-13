# Object: display choices for various dropdowns
emeta_display_choices <- reactive({
  input$preprocess_click
  
  # do not allow mass or isotopic info column as options
  drop_cols <- c(attr(peakData2, "cnames")$mass_cname,
                 input$iso_info_column)
  
  # get column names 
  column_choices <- peakData2$e_meta %>% 
    dplyr::select(-one_of(drop_cols)) %>%
    dplyr::select(which(sapply(., function(col){ length(unique(col)) < 12 } ) | sapply(., is.numeric))) %>% #dont include columns with too many categories
    colnames() 
  
  #columns included in calculation_options.csv get their prettified names, everything else gets the column name
  names(column_choices) <- lapply(column_choices, function(x){
    if (x %in% calc_vars$ColumnName){
      calc_vars %>% filter(ColumnName == x) %>% pluck("DisplayName")
    }
    else x
  }) %>% unlist()
  
  #____test export_____
  exportTestValues(display_names = column_choices)
  
  column_choices
  
})
