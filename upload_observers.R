observeEvent(c(input$edata_id_col, input$file_edata, input$file_emeta),{

  conditions <- c(input$edata_id_col == 'Select one', 
                  !(input$edata_id_col %in% edata_cnames() & input$edata_id_col %in% emeta_cnames()))
  if(conditions[1]){
    content = "Please select a unique identifier column"
  }
  else if(conditions[2]){
    content = "The chosen ID column does not exist in one or both of the Data/Molecular Identification Files"
  }
  else content = NULL
  
  toggleCssClass("js_edata_id", "attention", any(conditions))
  revals$warningmessage$warnidcol <- content

})

observeEvent(c(input$iso_info_column, input$iso_symbol, input$isotope_yn), {
  
  if(input$isotope_yn != "1"){
    revals$warningmessage$warniso <- NULL
  }
  else{
    req(!is.null(input$iso_info_column))
    req(!is.null(input$iso_symbol))
  
  
    conditions <- input$iso_info_column == "0"
  
    if(conditions){
      content <- "Please choose a column of isotopic information"
    }
    else if(!(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol))){
      conditions[2] <- !(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol))
      content <- "The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise."
    }
    else content = NULL
    
    toggleCssClass("js_iso_info_column", "attention", conditions[1])
    toggleCssClass("js_iso_symbol", "attention", isTRUE(conditions[2]))
    revals$warningmessage$warniso <- content
  }
})

observeEvent(c(Edata(), Emeta(), input$edata_id_col), {
  conditions <- FALSE
  
  if(input$edata_id_col %in% edata_cnames() & input$edata_id_col %in% emeta_cnames()){
    conditions = sum(!(Edata()[,input$edata_id_col] %in% Emeta()[,input$edata_id_col])) != 0
    
    if(conditions){
      indices <- (Edata()[,input$edata_id_col] %in% Emeta()[,input$edata_id_col])
      
      content = paste0("The following peaks in the data file are not present in the molecular identification file: ", 
                       paste(setdiff(Edata()[,input$edata_id_col], Emeta()[,input$edata_id_col]), collapse = ", "))
    }
    else content = NULL
  }
  else content <- NULL
  
  toggleCssClass("js_file_edata", "attention-upload", any(conditions))
  toggleCssClass("js_file_emeta", "attention-upload", any(conditions))
  revals$warningmessage$idcolmismatch <- content
  
})

observeEvent(c(input$c_column, input$h_column, input$n_column, 
               input$o_column, input$s_column, input$p_column),{
  
  elcols <- c(input$c_column, input$h_column, input$n_column, 
              input$o_column, input$s_column, input$p_column)
  conditions <- any(elcols == 'Select a column')
  
  if(conditions[1]){
    content = "One or more element selections are missing, Please double-check drop-down options."
  }else if(any(lapply(elcols, function(col){!is.numeric(Emeta()[,col])}))){
    conditions[2] <- any(lapply(elcols, function(col){!is.numeric(Emeta()[,col])}))
    content = "One or more elemental columns are non-numeric."
  }
  else content = NULL
  
  toggleCssClass("element_select", "attention", any(conditions))
  revals$warningmessage$elements <- content
  
  
})


