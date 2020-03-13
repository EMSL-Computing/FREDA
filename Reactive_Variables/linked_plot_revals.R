# just a filtered version of the plots table which excludes plots that cannot be linked
linked_plots_table <- reactive({
  plots$plot_table_download %>% 
    filter(!(`Plot Type` %in% c('Density Plot', 'PCOA Plot') & !(`Sample Type` %in% c('Single Sample'))))
})