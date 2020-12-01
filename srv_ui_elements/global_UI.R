list(
  # view plot table button UI
  output$viewplots_label <- renderUI({
    n_plots <- nrow(plots$plot_table)
    tags$span(sprintf("(%i)", n_plots))
  }),
  
  # display table of plots in modal dialog
  output$modal_plot_table <- renderDataTable(plots$plot_table, 
                                             options = list(scrollX = TRUE, columnDefs = list(list(className = 'nowrap_scroll', targets = '_all'))), 
                                             escape = FALSE, selection = 'single'),
  
  ### Two outputs since we need to conditionally render either plotly or ggplot objects
  output$modal_plotly <- renderPlotly({
    req(length(input$modal_plot_table_rows_selected) > 0, cancelOutput = TRUE)
    ind <- input$modal_plot_table_rows_selected
    plot_name <- plots$plot_table[ind, 1]
    return(toWebGL(plots$plot_list[[plot_name]]))
  }), 
  
  output$modal_ggplot <- renderPlot({
    req(length(input$modal_plot_table_rows_selected) > 0, cancelOutput = TRUE)
    ind <- input$modal_plot_table_rows_selected
    plot_name <- plots$plot_table[ind, 1]
    return(plots$plot_list[[plot_name]])
  }),
  
  # display the selected plot from the modal table, depending on what type of plot is selected
  output$modal_plot <- renderUI({
    req(length(input$modal_plot_table_rows_selected) > 0, cancelOutput = TRUE)
    ind <- input$modal_plot_table_rows_selected
    plot_name <- plots$plot_table[ind, 1]
    if(inherits(plots$plot_list[[plot_name]], 'plotly')){
      plotlyOutput('modal_plotly', width = 'auto', height = '500px')
    }
    else if(inherits(plots$plot_list[[plot_name]], 'ggplot')){
      plotOutput('modal_ggplot', width = 'auto', height = '500px')
    } 
  })
)