# Files Selected?
observeEvent(c(input$top_page, input$file_edata, input$file_emeta), {
  req(input$top_page == "Upload")
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
  
  conditions <- c(!(input$edata_id_col %in% edata_cnames() & input$edata_id_col %in% emeta_cnames()),
                  isTRUE(input$select == 0),
                  isTRUE(input$isotope_yn == 0),
                  isTRUE(input$f_column == "Select one"))
  
  if(conditions[1]){
    content = "style = 'color:red'>The chosen ID column does not exist in one or both of the Data/Molecular Identification Files"
  }
  else content = NULL
  
  if(all(!conditions[1], conditions[2])){
    content2 = "style = 'color:deepskyblue'>Please select either Formula or Elemental columns"
  }
  else content2 = NULL
  
  if(input$select != 1){
    revals$warningmessage$formula_col <- NULL
  }

  toggleCssClass("edata_id", "attention", conditions[1])
  toggleCssClass("js_select", "suggest", all(!conditions[1], conditions[2]))
  revals$warningmessage$warnidcol <- content
  revals$warningmessage$chooselement <- content2
  

})


# Peak ID column mismatch, gets a separate observer for code-cleanliness
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
observeEvent(c(input$iso_info_column, input$iso_symbol, input$isotope_yn, input$select), {
  
  if(input$isotope_yn != "1"){
    revals$warningmessage$warniso <- NULL
  }
  else{
    req(!is.null(input$iso_info_column))
    req(!is.null(input$iso_symbol))
  
    conditions <- input$iso_info_column == "0"
  
    if(conditions){
      if(input$select != 0){
        content <- "style = 'color:deepskyblue'>Please choose a column of isotopic information"
      }
      else content <- NULL
    }
    else if(!(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol))){
      conditions[2] <- !(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol))
      content <- "style = 'color:red'>The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise."
    }
    else content = NULL
    
    toggleCssClass("js_iso_info_column", "suggest", conditions[1] & input$select != 0)
    toggleCssClass("js_iso_symbol", "attention", isTRUE(conditions[2]))
    revals$warningmessage$warniso <- content
  }
})

# Non-numeric or non-selected elemental columns
observeEvent(c(input$c_column, input$h_column, input$n_column, 
               input$o_column, input$s_column, input$p_column, 
               input$select, input$isotope_yn),{
  
  req(Edata(), Emeta(), input$edata_id_col != "Select one")               
                 
  elcols <- c(input$c_column, input$h_column, input$n_column, 
              input$o_column, input$s_column, input$p_column)
  conditions <- any(elcols == 'Select a column')
  
  if(conditions[1]){
    if(input$select == 2){
      content = "style = 'color:deepskyblue'>One or more element selections are missing, Please double-check drop-down options."
    }
    else content = NULL
    content_isoyn = NULL
  }else if(any(lapply(elcols, function(col){!is.numeric(Emeta()[,col])})) & input$select == 2){
    conditions[2] <- any(  lapply(elcols, function(col){!is.numeric(Emeta()[,col])})  %>%  unlist())
    if(input$select == 2){
      content = "style = 'color:deepskyblue'>One or more elemental columns are non-numeric"
    }
    else content = NULL
    content_isoyn = NULL
  }
  else{
    content = NULL
    if(input$isotope_yn == 0 & input$select != 0){
      content_isoyn = "style = 'color:deepskyblue'>Please indicate whether isotope information is present in the molecular identification file"
    }
    else content_isoyn = NULL
  }
  
  toggleCssClass("element_select", "blueoutline", conditions[1])
  toggleCssClass("element_select", "redoutline", isTRUE(conditions[2]))
  toggleCssClass("js_isotope_yn", "suggest", all(!any(conditions), input$isotope_yn == 0, input$select != 0))
  revals$warningmessage$elements <- content
  revals$warningmessage$chooseiso <- content_isoyn
  
})

# Non-character elemental column 
observeEvent(c(input$f_column,input$select, input$isotope_yn),{
  
  req(Edata(), Emeta(), input$edata_id_col != "Select one", input$f_column)
  
  conditions <- FALSE
  conditions[2] <- input$f_column == "Select one"
  print(conditions[2])
  
  if(conditions[2]){
    if(input$select == 1){
      content = "style = 'color:deepskyblue'>Specify a column which contains alphanumeric formulae"
    }
    else content = NULL
      content_isoyn = NULL
  }
  else if(!is.character(Emeta()[,input$f_column]) & input$select == 1){
    conditions[1] <- !is.character(Emeta()[,input$f_column])
    if(input$select == 1){
      content = "style = 'color:red'>Specified formula column does not contain character strings"
    }
    else content = NULL
    content_isoyn = NULL
  }
  else{
    content = NULL
    if(input$isotope_yn == 0 & input$select != 0){
      content_isoyn = "style = 'color:deepskyblue'>Please indicate whether isotope information is present in the molecular identification file"
    }
    else content_isoyn = NULL
  }
  
  toggleCssClass("f_column", "attention", conditions[1])
  toggleCssClass("f_column", "suggest", !conditions[1] & conditions[2])
  toggleCssClass("js_isotope_yn", "suggest", all(!any(conditions), input$isotope_yn == 0, input$select != 0))
  revals$warningmessage$chooseiso <- content_isoyn
  revals$warningmessage$formula_col <- content
  
})




