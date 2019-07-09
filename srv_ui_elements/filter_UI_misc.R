list(
  # filter warnings
  output$warnings_filter <- renderUI({
    HTML(lapply(revals$warningmessage_filter, function(el){paste0("<p ", el, "</p>")}) %>%
           paste(collapse = ""))
  }),
  
  ### icon control for filter tab collapsible sections
  output$massfilter_icon <- renderUI({
    req(input$top_page == 'Filter')
    clicked = if(input$massfilter) div(id = 'ok_massfilt', style = 'color:deepskyblue;display:inline-block;margin-right:5px', icon('ok', lib='glyphicon')) else NULL
    if('massfilt_collapse' %in% input$filter_sidebar){
      div(
        clicked, 
        icon('chevron-up', lib = 'glyphicon')
      )
    }
    else{
      div(
        clicked, 
        icon('chevron-down', lib = 'glyphicon')
      )
    }
  }),
  
  output$samplefilter_icon <- renderUI({
    req(input$top_page == 'Filter')
    clicked = if(input$samplefilter) div(id = 'ok_samplefilter', style = 'color:deepskyblue;display:inline-block;margin-right:5px', icon('ok', lib='glyphicon')) else NULL
    if('samplefilt_collapse' %in% input$filter_sidebar){
      div(
        clicked, 
        icon('chevron-up', lib = 'glyphicon')
      )
    }
    else{
      div(
        clicked, 
        icon('chevron-down', lib = 'glyphicon')
      )
    }
  }),
  
  output$formfilter_icon <- renderUI({
    req(input$top_page == 'Filter')
    clicked = if(input$formfilter) div(id = 'ok_formfilter', style = 'color:deepskyblue;display:inline-block;margin-right:5px', icon('ok', lib='glyphicon')) else NULL
    if('formfilt_collapse' %in% input$filter_sidebar){
      div(
        clicked, 
        icon('chevron-up', lib = 'glyphicon')
      )
    }
    else{
      div(
        clicked, 
        icon('chevron-down', lib = 'glyphicon')
      )
    }
  }),
  
  output$molfilter_icon <- renderUI({
    req(input$top_page == 'Filter')
    clicked = if(input$molfilter) div(id = 'ok_molfilter', style = 'color:deepskyblue;display:inline-block;margin-right:5px', icon('ok', lib='glyphicon')) else NULL
    if('molfilt_collapse' %in% input$filter_sidebar){
      div(
        clicked, 
        icon('chevron-up', lib = 'glyphicon')
      )
    }
    else{
      div(
        clicked, 
        icon('chevron-down', lib = 'glyphicon')
      )
    }
  }),
  
  output$customfilter_icon <- renderUI({
    req(input$top_page == 'Filter')
    clicked = if(input$customfilterz) div(id = 'ok_customfilterz', style = 'color:deepskyblue;display:inline-block;margin-right:5px', icon('ok', lib='glyphicon')) else NULL
    if('customfilt_collapse' %in% input$filter_sidebar){
      div(
        clicked, 
        icon('chevron-up', lib = 'glyphicon')
      )
    }
    else{
      div(
        clicked, 
        icon('chevron-down', lib = 'glyphicon')
      )
    }
  })
  #
)