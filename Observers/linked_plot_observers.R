# stores which plot we interacted with last
lp_lastEvent <- reactiveValues(source = "none", trigger = 1)

observeEvent(input$lp_compare_plots, {
  # browser()
  req(length(input$lp_plot_table_rows_selected) == 2)
  inds <- input$lp_plot_table_rows_selected

  plot_name1 <- linked_plots_table()[inds[1], 1]
  plot_name2 <- linked_plots_table()[inds[2], 1]

  plots$linked_plots$left <- plots$plot_list[[plot_name1]] %>%
    layout(dragmode = "select")
  plots$linked_plots$right <- plots$plot_list[[plot_name2]] %>%
    layout(dragmode = "select")

  plots$linked_plots$left$x$source <- 'left_source'
  plots$linked_plots$right$x$source <- 'right_source'

  lp_lastEvent$source <<- NULL

  updateCollapse(session, id = 'linked_plots_collapse', open = c('lp_mainpanel'))

}, priority = 9)

# Observe plotly-selected event from vk_source
observeEvent(input$`plotly_selected-left_source`, {
  message('updated source left')
  lp_lastEvent$source <- "left_source"
  lp_lastEvent$trigger = -lp_lastEvent$trigger
}, priority = 10)

# Observe plotly-selected event from kendrick_source
observeEvent(input$`plotly_selected-right_source`, {
  message('updated source right')
  lp_lastEvent$source <- "right_source"
  lp_lastEvent$trigger = -lp_lastEvent$trigger
}, priority = 10)
