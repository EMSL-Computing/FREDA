list(
  # warning messages for viztab
  output$warnings_visualize_UI <- renderUI({
    HTML(paste(revals$warningmessage_visualize, collapse = ""))
  }),
  
  # icon control for viztab collapsible sections
  output$chooseplots_icon <- renderUI({
    req(input$top_page == 'Visualize')
    if('peakplots' %in% input$viz_sidebar)
      icon('chevron-up', lib = 'glyphicon')
    else icon('chevron-down', lib = 'glyphicon')
  }),
  
  output$axlabs_icon <- renderUI({
    req(input$top_page == 'Visualize')
    if('axlabs' %in% input$viz_sidebar)
      icon('chevron-up', lib = 'glyphicon')
    else icon('chevron-down', lib = 'glyphicon')
  }),
  
  output$saveplots_icon <- renderUI({
    req(input$top_page == 'Visualize')
    if('downloads' %in% input$viz_sidebar)
      icon('chevron-up', lib = 'glyphicon')
    else icon('chevron-down', lib = 'glyphicon')
  }),
  
  output$dynamic_opts_icon <- renderUI({
    req(input$top_page == 'Visualize')
    if('reactive_plot_opts' %in% input$viz_sidebar)
      icon('chevron-up', lib = 'glyphicon')
    else icon('chevron-down', lib = 'glyphicon')
  })
  #
)