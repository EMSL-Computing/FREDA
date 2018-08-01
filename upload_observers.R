# Files Selected?
observeEvent(c(input$top_page, input$file_edata, input$file_emeta), {
  toggleCssClass("js_file_edata", "suggest-upload", is.null(input$file_edata))
  toggleCssClass("js_file_emeta", "suggest-upload", is.null(input$file_emeta))
  
  if(!is.null(input$file_edata) & !is.null(input$file_emeta)){
    revals$warningmessage$upload <- NULL
  }
  
}, priority = 2)

# ID column not selected
observeEvent(input$edata_id_col,{
  condition <- input$edata_id_col == 'Select one'

  if(condition){
    content = "style = 'color:deepskyblue'>Please select a unique identifier column"
  }
  else content = NULL

  toggleCssClass("edata_id", "suggest", condition)
  #toggleCssClass("edata_id", "attention", conditions[2] & !conditions[1])
  revals$warningmessage$warnidcol <- content

}, priority = 1)

# ID column not present in both files, highlight cascade for element/isotope dropdowns
observeEvent(c(input$edata_id_col, Edata(), Emeta(), input$select, input$isotope_yn),{
  req(Edata(), Emeta(), input$edata_id_col != "Select one")
  
  condition <- (input$edata_id_col %in% edata_cnames() & input$edata_id_col %in% emeta_cnames())
  condition[2] <- input$select == 0
  condition[3] <- input$isotope_yn == 0
  
  print(all(c(condition[1], !condition[2], condition[3])))
  
  
  if(!condition[1]){
    content = "style = 'color:red'>The chosen ID column does not exist in one or both of the Data/Molecular Identification Files"
  }
  else content = NULL
  
  if(all(condition[1:2])){
    content2 = "style = 'color:deepskyblue'>Please select either Formula or Elemental columns"
  }
  else content2 = NULL
  
  if(all(c(condition[1], !condition[2], condition[3]))){
    content3 = "style = 'color:deepskyblue'>Please indicate whether isotope information is present in the molecular identification file"
  }
  else content3 = NULL

  toggleCssClass("edata_id", "attention", !condition[1])
  toggleCssClass("js_select", "suggest", all(condition[1:2]))
  toggleCssClass("js_isotope_yn", "suggest", all(c(condition[1], !condition[2], condition[3])))
  revals$warningmessage$warnidcol <- content
  revals$warningmessage$chooselement <- content2
  revals$warningmessage$chooseiso <- content3

})


# Peak ID column mismatch
observeEvent(c(Edata(), Emeta(), input$edata_id_col), {
  conditions <- FALSE
  
  if(input$edata_id_col %in% edata_cnames() & input$edata_id_col %in% emeta_cnames()){
    conditions = sum(!(Edata()[,input$edata_id_col] %in% Emeta()[,input$edata_id_col])) != 0
    
    if(conditions){
      indices <- (Edata()[,input$edata_id_col] %in% Emeta()[,input$edata_id_col])
      
      content = paste0("style = 'color:red'>The following peaks in the data file are not present in the molecular identification file: ", 
                       paste(setdiff(Edata()[,input$edata_id_col], Emeta()[,input$edata_id_col]), collapse = ", "))
    }
    else content = NULL
  }
  else content <- NULL
  
  toggleCssClass("js_file_edata", "attention-upload", any(conditions))
  toggleCssClass("js_file_emeta", "attention-upload", any(conditions))
  revals$warningmessage$idcolmismatch <- content
  
})

# Highlight

# ISO info column not selected or doesn't contain selected symbol
observeEvent(c(input$iso_info_column, input$iso_symbol, input$isotope_yn), {
  
  if(input$isotope_yn != "1"){
    revals$warningmessage$warniso <- NULL
  }
  else{
    req(!is.null(input$iso_info_column))
    req(!is.null(input$iso_symbol))
  
    conditions <- input$iso_info_column == "0"
  
    if(conditions){
      content <- "style = 'color:deepskyblue'>Please choose a column of isotopic information"
    }
    else if(!(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol))){
      conditions[2] <- !(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol))
      content <- "style = 'color:red'>The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise."
    }
    else content = NULL
    
    toggleCssClass("js_iso_info_column", "suggest", conditions[1])
    toggleCssClass("js_iso_symbol", "attention", isTRUE(conditions[2]))
    revals$warningmessage$warniso <- content
  }
})

# Non-numeric or non-selected elemental columns
observeEvent(c(input$c_column, input$h_column, input$n_column, 
               input$o_column, input$s_column, input$p_column),{
  
  elcols <- c(input$c_column, input$h_column, input$n_column, 
              input$o_column, input$s_column, input$p_column)
  conditions <- any(elcols == 'Select a column')
  
  if(conditions[1]){
    content = "style = 'color:deepskyblue'>One or more element selections are missing, Please double-check drop-down options."
  }else if(any(lapply(elcols, function(col){!is.numeric(Emeta()[,col])}))){
    conditions[2] <- any(  lapply(elcols, function(col){!is.numeric(Emeta()[,col])})  %>%  unlist())
    content = "style = 'color:red'>One or more elemental columns are non-numeric."
  }
  else content = NULL
  
  toggleCssClass("element_select", "blueoutline", conditions[1])
  toggleCssClass("element_select", "redoutline", isTRUE(conditions[2]))
  revals$warningmessage$elements <- content
  
})

# Non-character elemental column 
observeEvent(input$f_column,{
  
  req(input$f_column != 'Select one')
  conditions <- FALSE
  
  if(!is.character(Emeta()[,req(input$f_column)])){
    conditions <- !is.character(Emeta()[,input$f_column])
    content = "style = 'color:red'>Specified formula column does not contain characters"
  }
  else content <- NULL
  
  toggleCssClass("f_column", "attention", conditions)
  revals$warningmessage$formula_col <- content
  
})




