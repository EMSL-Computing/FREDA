# Main:  Create peakData when upload button or preprocess button clicked
observeEvent(input$upload_click, {
  # prevent multiple clicks
  shinyjs::disable('upload_click')
  shinyjs::show('upload_waiting', anim=T)
  on.exit({
    shinyjs::enable('upload_click')
    shinyjs::hide('upload_waiting', anim=T)
  })
  
  # Error handling: unique identifier chosen
  validate(need(input$edata_id_col != 'Select one', 'Please select a unique identifier column'),
           need(input$edata_id_col %in% edata_cnames() & input$edata_id_col %in% emeta_cnames(),
                message = "The chosen ID column does not exist in one or both of the Data/Molecular Identification"))
  
  validate(         
    need(input$select != 0, 'Please select either Formula or Elemental columns'),
    need(input$isotope_yn != 0, 'Please select yes or no on information for isotopes'),
    need(sum(!(Edata()[,input$edata_id_col] %in% Emeta()[,input$edata_id_col])) == 0, 
         'Not all peaks in data file are present in molecular identification file, please add/remove these peaks to emeta / from edata')
    
  ) # End error handling #
  
  ## If formula column chosen
  if (input$select == 1) {
    
    # Error handling: f_column chosen and  (if chosen) is of class 'character'
    validate(
      need((input$f_column != 'Select one'),
           'Please select a formula column'),
      need({
        if (input$f_column != 'Select one') 
          is.character(Emeta()[,input$f_column])
        else 
          FALSE
      }, # End 'need'
      
      'Formula column is not a character vector. Please select another.')
      
    ) # End error handling #
    tryCatch({
      revals$warningmessage_upload$makeobject_error <- NULL
      if (input$isotope_yn == 1 & isTRUE(input$iso_info_filter == 1)) { # If there's C13 # 
        
        # Error handling: entered isotopic notation must exist in the isotope information column
        validate(
          need(input$iso_info_column != "0", message = "Please choose a column of isotopic information"),
          need(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol),
               'The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise.')
        ) # End error handling
        
        res <- as.peakData(e_data = Edata(), f_data = fdata(),
                           e_meta = Emeta(), edata_cname = input$edata_id_col, 
                           fdata_cname = 'SampleId', mass_cname = input$edata_id_col,
                           mf_cname = input$f_column,
                           isotopic_cname = input$iso_info_column,
                           isotopic_notation = as.character(input$iso_symbol),
                           check_rows = TRUE, data_scale = input$data_scale)
        
      } # End C13 / no C13 if statement
      
      if (input$isotope_yn == 2 | isTRUE(input$iso_info_filter) != 1) { #no C13
        # Calculate peakDataData with formula column
        res <- as.peakData(e_data = Edata(), f_data = fdata(),
                           e_meta = Emeta(), edata_cname = input$edata_id_col, 
                           fdata_cname = 'SampleId', mass_cname = input$edata_id_col,
                           mf_cname = input$f_column,
                           check_rows = TRUE, data_scale = input$data_scale)
      } 
    },
    error = function(e){
      msg = paste0('Error making your peakData: \n System error: ', e)
      revals$warningmessage_upload$makeobject_error <<- sprintf("<p style = 'color:red'>%s</p>", msg)
    })
  }
  
  # If elemental columns chosen
  if (input$select == 2){
    
    ## Error handling: all drop down columns nonempty and of class 'numeric'
    
    # first check that H and C columns are specified and numeric...
    validate(
      need({(input$c_column != 'Select a column') & 
          (input$h_column != 'Select a column')
      }, 
      'Hydrogen/carbon columns are required. Please double-check drop-down options.')
    )
    validate(
      need({
        all(is.numeric(Emeta()[,input$c_column])) &
          all(is.numeric(Emeta()[,input$h_column]))
      }, 
      'One or more elemental columns are non-numeric.')
    )
    # ...then check that -if- other columns are selected, they are numeric
    for(col in c('n_column', 'o_column', 's_column', 'p_column')){
      if(input[[col]] != 'Select a column'){
        validate(need(is.numeric(Emeta()[,input[[col]]]), 'One or more elemental columns are non-numeric.'))
      } 
    }# End error handling #
    tryCatch({
      revals$warningmessage_upload$makeobject_error <- NULL
      # If no C13
      if (input$isotope_yn == 2 | isTRUE(input$iso_info_filter == 2)) {
        # Create peakData object
        res <- as.peakData(e_data = Edata(), f_data = fdata(),
                           e_meta = Emeta(), edata_cname = input$edata_id_col, 
                           fdata_cname = 'SampleId', mass_cname = input$edata_id_col,
                           c_cname = input$c_column, h_cname = input$h_column, 
                           n_cname = if(input$n_column == 'Select a column') NULL else input$n_column,
                           o_cname = if(input$o_column == 'Select a column') NULL else input$o_column, 
                           s_cname = if(input$s_column == 'Select a column') NULL else input$s_column, 
                           p_cname = if(input$p_column == 'Select a column') NULL else input$p_column,
                           check_rows = TRUE, data_scale = input$data_scale)
        
      }
      if (input$isotope_yn == 1 & isTRUE(input$iso_info_filter == 1)) { # If there's C13 # 
        
        # Error handling: entered isotopic notation must exist in the isotope information column
        validate(need(input$iso_info_column != "0", message = "Please choose a column of isotopic information"))
        validate(need(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol),
                      'The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise.')
        ) # End error handling
        
        res <- as.peakData(e_data = Edata(), f_data = fdata(),
                           e_meta = Emeta(), edata_cname = input$edata_id_col, 
                           fdata_cname = 'SampleId', mass_cname = input$edata_id_col,
                           c_cname = input$c_column, h_cname = input$h_column, 
                           n_cname = if(input$n_column == 'Select a column') NULL else input$n_column,
                           o_cname = if(input$o_column == 'Select a column') NULL else input$o_column, 
                           s_cname = if(input$s_column == 'Select a column') NULL else input$s_column, 
                           p_cname = if(input$p_column == 'Select a column') NULL else input$p_column,
                           isotopic_cname = input$iso_info_column,
                           isotopic_notation = as.character(input$iso_symbol),
                           check_rows = TRUE, data_scale = input$data_scale)
        
      } # End C13 / no C13 if statement
      
      if (input$NA_value != "NA"){
        res <- edata_replace(res, input$NA_value, NA) 
      }
    },
    error = function(e){
      msg = paste0('Error making your peakData: \n System error: ', e)
      revals$warningmessage_upload$makeobject_error <<- sprintf("<p style = 'color:red'>%s</p>", msg)
    })
    
  } # End elemental column if statement
  
  if(exists('res')){
    shinyjs::show('upload_success')
    
    # reset 'removed samples' reval
    revals$removed_samples <- list()
    revals$groups_list <- list()
    updateCollapse(session, 'upload_collapse', close = c('file_upload', 'column_info'))
    shinyjs::show('ok_idcols')
    
    revals$uploaded_data <- res
    
    exportTestValues(uploaded_data = res)
  }
  
}) # End peakData creation

# if edata is big, warn the user and prevent plotting of filter barplot
observeEvent(Edata(),{
  if(prod(dim(Edata()[,-1])) > max_cells){
    content <- "style = 'color:deepskyblue;font-weight:bold'>Large data file detected, some plotting options and interactivity may be disabled for performance"
    revals$redraw_largedata <- FALSE
  }
  else content <- NULL
  revals$warningmessage_upload$largedata <- content
})

# Files Selected?
observeEvent(c(input$top_page, input$file_edata, input$file_emeta), {
  req(input$top_page == "Upload")
  toggleCssClass("js_file_edata", "suggest-upload", is.null(input$file_edata))
  toggleCssClass("js_file_emeta", "suggest-upload", is.null(input$file_emeta))
  
  if(!is.null(input$file_edata) & !is.null(input$file_emeta)){
    revals$warningmessage_upload$upload <- NULL
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
  revals$warningmessage_upload$warnidcol <- content

}, priority = 1)

# ID column not present in both files, highlight cascade for element/isotope dropdowns
observeEvent(c(input$edata_id_col, Edata(), Emeta(), input$select, input$isotope_yn),{
  req(Edata(), Emeta(), input$edata_id_col != "Select one")
  
  conditions <- c(!(input$edata_id_col %in% edata_cnames() & input$edata_id_col %in% emeta_cnames()),
                  isTRUE(is.null(input$select)),
                  isTRUE(is.null(input$isotope_yn)),
                  isTRUE(input$f_column == "Select one"),
                  sum(!(Edata()[[input$edata_id_col]] %in% Emeta()[[input$edata_id_col]])) == 0)
  
  if(conditions[1]){
    content = "style = 'color:red'>The chosen ID column does not exist in one or both of the Data/Molecular Identification Files"
  }
  else if(conditions[5]){
    content = NULL
    # close top panel
    Sys.sleep(0.6)
    updateCollapse(session, 'upload_collapse', close = 'file_upload', open = 'column_info')
  }
  else content = NULL
  
  if(all(!conditions[1], conditions[2])){
    content2 = "style = 'color:deepskyblue'>Please select either Formula or Elemental columns"
  }
  else content2 = NULL
  
  if(isTRUE(input$select != 1)){
    revals$warningmessage_upload$formula_col <- NULL
  }

  toggleCssClass("edata_id", "attention", conditions[1])
  toggleCssClass("js_select", "suggest", all(!conditions[1], conditions[2]))
  toggle('ok_files', condition = !conditions[1] & conditions[5])
  revals$warningmessage_upload$warnidcol <- content
  revals$warningmessage_upload$chooselement <- content2
  

})

observeEvent(input$select,{
  toggleElement('element_select', condition = input$select == 2)
})

# Peak ID column mismatch, gets a separate observer for code-cleanliness
observeEvent(c(Edata(), Emeta(), input$edata_id_col), {
  req(input$edata_id_col)
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
  revals$warningmessage_upload$idcolmismatch <- content
  
})

# Highlight

# ISO info column not selected or doesn't contain selected symbol
observeEvent(c(input$iso_info_column, input$iso_symbol, input$isotope_yn, input$select), {
  
  if(isTRUE(input$isotope_yn != "1")){
    revals$warningmessage_upload$warniso <- NULL
  }
  else{
    req(!is.null(input$iso_info_column))
    req(!is.null(input$iso_symbol))
  
    conditions <- isTRUE(input$iso_info_column == "0")
  
    if(conditions){
      if(isTRUE(input$select != 0)){
        content <- "style = 'color:deepskyblue'>Please choose a column of isotopic information"
      }
      else content <- NULL
    }
    else if(isTRUE(!(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol)))){
      conditions[2] <- isTRUE(!(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol)))
      content <- "style = 'color:red'>The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise."
    }
    else content = NULL
    
    toggleCssClass("js_iso_info_column", "suggest", conditions[1] & input$select != 0)
    toggleCssClass("js_iso_symbol", "attention", isTRUE(conditions[2]))
    revals$warningmessage_upload$warniso <- content
  }
})

# Non-numeric or non-selected elemental columns
observeEvent(c(input$c_column, input$h_column, input$n_column, 
               input$o_column, input$s_column, input$p_column, 
               input$select, input$isotope_yn),{
  
  req(Edata(), Emeta(), input$edata_id_col != "Select one")               
                 
  elcols <- c(input$c_column, input$h_column)
  conditions <- isTRUE(any(elcols == 'Select a column') | any(is.null(elcols)))
  
  if(conditions[1]){
    if(isTRUE(input$select == 2)){
      content = "style = 'color:deepskyblue'>One or more element selections are missing, Please double-check drop-down options."
    }
    else content = NULL
    content_isoyn = NULL
  }else if(isTRUE(any(sapply(elcols, function(col){!is.numeric(Emeta()[,col])}))) & isTRUE(input$select == 2)){
    conditions[2] <- isTRUE(any(sapply(elcols, function(col){!is.numeric(Emeta()[,col])})))
    if(isTRUE(input$select == 2)){
      content = "style = 'color:red'>One or more elemental columns are non-numeric"
    }
    else content = NULL
    content_isoyn = NULL
  }
  else{
    content = NULL
    if(isTRUE(is.null(input$isotope_yn) & input$select != 0)){
      content_isoyn = "style = 'color:deepskyblue'>Please indicate whether isotope information is present in the molecular identification file"
    }
    else content_isoyn = NULL
  }
  
  toggleCssClass("element_select", "blueoutline", isTRUE(conditions[1]))
  toggleCssClass("element_select", "redoutline", isTRUE(conditions[2]))
  toggleCssClass("js_isotope_yn", "suggest", all(!any(conditions), is.null(input$isotope_yn), input$select != 0))
  revals$warningmessage_upload$elements <- content
  revals$warningmessage_upload$chooseiso <- content_isoyn
  
})

# Non-character elemental column 
observeEvent(c(input$f_column,input$select, input$isotope_yn),{
  
  req(Edata(), Emeta(), input$edata_id_col != "Select one", input$f_column)
  
  conditions <- FALSE
  conditions[2] <- input$f_column == "Select one"
  
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
    if(is.null(input$isotope_yn) & input$select != 0){
      content_isoyn = "style = 'color:deepskyblue'>Please indicate whether isotope information is present in the molecular identification file"
    }
    else content_isoyn = NULL
  }
  
  toggleCssClass("f_column", "attention", conditions[1])
  toggleCssClass("f_column", "suggest", !conditions[1] & conditions[2])
  toggleCssClass("js_isotope_yn", "suggest", all(!any(conditions), is.null(input$isotope_yn), input$select != 0))
  revals$warningmessage_upload$chooseiso <- content_isoyn
  revals$warningmessage_upload$formula_col <- content
  
})

# warning dialog on iso filter
observeEvent(input$iso_info_filter,{
  if (input$iso_info_filter == 2)
    showModal(
      modalDialog("",h4("Warning!", style = "color:Orange; font-weight:bold"),
                  HTML("<p style = color:Orange; font-weight:bold> Leaving isotopic peaks in the data may confound analysis/visualization results. We recommend filtering isotopic peaks.")
      )
    )
})

# Show success message when revals$uploaded_data is sucessfully created 
observeEvent(revals$uploaded_data,{
  #Error handling: revals$uploaded_data must exist
  req(revals$uploaded_data)
  req(input$top_page == 'Upload')
  
  #___test-export___
  if (isTRUE(getOption("shiny.testmode"))) {
    revals$peakData_export <- revals$uploaded_data
  }
  
  showModal(
    modalDialog(
      title = "Upload Success",
      fluidRow(
        column(10, align = "center", offset = 1,
               HTML('<h4 style= "color:#1A5276">Your data has been successfully uploaded. 
                    You may proceed to the subsequent tabs for analysis.</h4>'),
               hr(),
               actionButton("upload_dismiss", "Review results", width = '75%'),
               actionButton("goto_groups", "Continue to groups tab", style = "margin:5px;width:75%"),
               actionButton("goto_preprocess", "Skip to preprocess tab", width = '75%')
               )
      )
      ,footer = NULL)
  )
  
  message('enabling data upload inputs')
  # enable inputs that should only be available if data is sucessfully uploaded
  disabled_inputs <- c("preprocess_click", "filter_click", "reset_filters", "plot_submit", "update_axes", "visualize_goto_linked")
  lapply(disabled_inputs, enable)
  
})

# modal dialog behavior
observeEvent(input$upload_dismiss,{removeModal()})

observeEvent(input$goto_groups, {
  updateTabsetPanel(session, "top_page", selected = "Groups")
  removeModal()
})

observeEvent(input$goto_preprocess, {
  updateTabsetPanel(session, "top_page", selected = "Preprocess")
  removeModal()
})




