# just a filtered version of the plots table which excludes plots that cannot be linked
linked_plots_table <- reactive({
  # conditions determining whether the plot is comparable
  is_density = plots$plot_table_download['Plot Type'] == 'Density Plot'
  is_single_sample = plots$plot_table_download['Sample Type'] == 'Single Sample'
  is_pcoa = plots$plot_table_download['Plot Type'] == 'PCOA Plot'
  has_sample = !is.na(plots$plot_table_download['Sample Type'])
  
  plots$plot_table_download %>% 
    filter(!((is_density & !is_single_sample) | is_pcoa) & has_sample)
})