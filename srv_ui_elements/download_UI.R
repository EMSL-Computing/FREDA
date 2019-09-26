list(
  # copy the table from the visualize tab so as not to confuse javascript
  output$parmsTable2 <- DT::renderDataTable(parmTable$parms,
                                            options = list(scrollX = TRUE),
                                            server = TRUE),
  
  # print the selected indices
  output$x4 <- renderPrint({
    s = input$parmsTable2_rows_selected    
    
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
  })
)