list(
  # copy the table from the visualize tab so as not to confuse javascript
  output$download_plot_table <- DT::renderDataTable(plots$plot_table_download,
    options = list(scrollX = TRUE, columnDefs = list(list(className = 'nowrap_scroll', targets = '_all'))),
    escape = FALSE, selection = 'single'),

  ### Two outputs since we need to conditionally render either plotly or ggplot objects
  output$download_plotly <- renderPlotly({
    req(length(input$download_plot_table_rows_selected) > 0, cancelOutput = TRUE)
    ind <- input$download_plot_table_rows_selected
    plot_name <- plots$plot_table[ind, 1]
    return(toWebGL(plots$plot_list[[plot_name]]))
  }),

  output$download_ggplot <- renderPlot({
    req(length(input$download_plot_table_rows_selected) > 0, cancelOutput = TRUE)
    ind <- input$download_plot_table_rows_selected
    plot_name <- plots$plot_table[ind, 1]
    return(plots$plot_list[[plot_name]])
  }),

  # display the selected plot from the modal table, depending on what type of plot is selected
  output$download_plot <- renderUI({
    req(length(input$download_plot_table_rows_selected) > 0, cancelOutput = TRUE)
    ind <- input$download_plot_table_rows_selected
    plot_name <- plots$plot_table[ind, 1]
    if (inherits(plots$plot_list[[plot_name]], 'plotly')) {
      plotlyOutput('download_plotly', width = 'auto', height = 'auto')
    }
    else if (inherits(plots$plot_list[[plot_name]], 'ggplot')) {
      plotOutput('download_ggplot', width = 'auto', height = 'auto')
    }
  }),

  #
  output$warnings_download <- renderUI({
    HTML(lapply(revals$warningmessage_download, function(el) {paste0("<p ", el, "</p>")}) %>%
      paste(collapse = ""))
  })
)
