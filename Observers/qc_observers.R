observeEvent(input$goto_filter_fromqc,{
  updateTabsetPanel(session, 'top_page', 'Filter')
})

# temporarily allow drawing of plot for large data
observeEvent(input$update_boxplot_axes,{
  revals$redraw_largedata <- TRUE
}, priority = 10)


## things to remove input$add_qc_boxplot