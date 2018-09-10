observeEvent(c(input$download_selection, input$parmsTable2_rows_selected), {
  toggleState("download_processed_data", condition = !(is.null(input$download_selection) & is.null(input$parmsTable2_rows_selected)))
  
}, ignoreNULL = FALSE)