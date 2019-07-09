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
  output$warnings_upload <- renderUI({
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
  options = list(scrollX = TRUE))
)