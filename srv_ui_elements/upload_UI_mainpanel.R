list(
  # Summary: Display number of peaks and samples
  output$num_peaks <- renderText({
    revals$uploaded_data
    c('Number of peaks: ', nrow(revals$uploaded_data$e_data))
    
  }), # End num_peaks
  
  output$num_samples <- renderText({
    revals$uploaded_data
    c('Number of samples: ', (length(edata_cnames()) - 1))
    
  }), # End num_samples # 
  
  # Display success message OR display errors
  output$success_upload <- renderUI({
    
    # Error handling: revals$uploaded_data must exist
    req(revals$uploaded_data)
    
    # If no errors, show Success message
    HTML('<h4 style= "color:#1A5276">Your data object is created, and can be manipulated in subsequent tabs.</h4>')
    
  }), # End success #
  
  # Display explanation for e_meta
  output$emeta_text <- renderUI({
    
    req(Emeta())
    HTML('<h4>Displaying Uploaded Molecular Identification File</h4>')
    
  }),# End emeta_text
  
  # Display explanation above e_data
  output$edata_text <- renderUI({
    
    # Error handling: Edata() must exist
    req(Edata())
    
    HTML('<h4>Displaying Uploaded Data File</h4>')
    
  }), # End edata_text
  
  # display list of warnings pasted on separate lines
  output$warnings_upload_UI <- renderUI({
    HTML(lapply(revals$warningmessage_upload, function(el){
      paste0("<p ", el, "</p>")
    }) %>%
      paste(collapse = "")
    )
  }),
  
  # e_data display
  output$head_edata <- DT::renderDT({
    tmp <- Edata()
    
    # coerce logical to character for display purposes.
    tmp[, which(sapply(tmp, is.logical))] <- as.character(tmp[, which(sapply(tmp, is.logical))])
    tmp
  },
  options = list(scrollX = TRUE)),
  
  # e_meta display
  output$head_emeta <- DT::renderDataTable({
    tmp <- Emeta()
    
    # coerce logical to character for display purposes.
    tmp[, which(sapply(tmp, is.logical))] <- as.character(tmp[, which(sapply(tmp, is.logical))])
    tmp
  },
  options = list(scrollX = TRUE)),
  
  # Summary: Display number of peaks with formulas
  output$num_peaks_formula <- renderText({
    
    # Error handling: Require e_meta and others
    req(Emeta())
    req(input$select != 0)
    
    # Scope: Set up num_rows_formula to edit in if statements
    num_rows_formula = nrow(Edata())
    
    # If f_columns have been identified
    if (input$select == 1){
      
      # Error handling: need formula column
      req(input$f_column)
      req(input$f_column != 'Select one')
      req(is.character(Emeta()[,input$f_column]))
      
      # Count all non-NA columns
      f_column <- Emeta()[,input$f_column]
      
      # Count all nonempty and non-NA entries
      num_rows_formula <- length(which((!is.na(f_column)) & (f_column != "")))
      
    } else if (input$select == 2) { # If elemental columns have been identified
      
      # Error handling: drop down columns must exist and be numeric
      req({
        (input$c_column != 'Select a column') && 
          (input$h_column != 'Select a column') && 
          (input$n_column != 'Select a column') &&
          (input$o_column != 'Select a column') &&
          (input$s_column != 'Select a column') &&
          (input$p_column != 'Select a column') &&
          all(is.numeric(Emeta()[,input$c_column])) &&
          all(is.numeric(Emeta()[,input$h_column])) &&
          all(is.numeric(Emeta()[,input$n_column])) &&
          all(is.numeric(Emeta()[,input$o_column])) &&
          all(is.numeric(Emeta()[,input$s_column])) &&
          all(is.numeric(Emeta()[,input$p_column]))
      }) # End error handling for elemental columns #
      
      # Set up list of column names
      elem_cnames <- c(input$c_column, input$h_column, 
                       input$n_column, input$o_column, 
                       input$s_column, input$p_column)
      
      # Create data frame of all elemental columns to sum across
      elem_columns <- data.frame(Emeta()[,elem_cnames])
      req(input$isotope_yn)
      req(input$iso_info_filter)
      # If isotopic information is included and matching entered notation, filter out where isotopes = denoted symbol
      if (input$isotope_yn == 1 & input$iso_info_filter == 1) {
        req(input$iso_info_column)
        validate(need(input$iso_info_column != 0, message = "Please choose a column of isotopic information"))
        if (any(Emeta()[,input$iso_info_column] %in% input$iso_symbol)) {
          iso <- Emeta()[,input$iso_info_column]
          elem_columns <- elem_columns[-(which(as.character(iso) == as.character(input$iso_symbol))),]
        }
      }# End if isotopic information is chosen and correctly denoted#
      
      # Count all remaining rows with nonzero sums
      num_rows_formula <- length(which(rowSums(elem_columns) > 0))
      
    } # End elemental columns option
    
    validate(
      need(!is.null(revals$uploaded_data), message = "")
    )
    # Display number of peaks/rows with formula assigned
    c('Number of peaks with formulas: ', num_rows_formula)
    
    
  }) # End num_peaks_formula in summary panel
)