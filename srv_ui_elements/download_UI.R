list(
  # copy the table from the visualize tab so as not to confuse javascript
  output$download_plot_table <- DT::renderDataTable(plots$plot_table,
                                            options = list(scrollX = TRUE, columnDefs = list(list(className = 'nowrap_scroll', targets = '_all'))), 
                                            escape = FALSE, selection = 'single')
)