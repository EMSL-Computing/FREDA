### Summary Panel: Display table summaries of numeric and categorical columns in e_meta ###

observeEvent(c(revals$numeric_cols, revals$categorical_cols),{
  if(isTRUE(nrow(revals$numeric_cols) > 0)){
    columns <- summaryPreprocess(isolate(revals$peakData2), revals$numeric_cols) %>% colnames()
    
    revals$preprocess_tables$numeric <- summaryPreprocess(isolate(revals$peakData2), revals$numeric_cols) %>%
                                          datatable(options = list(dom = "t", pageLength = nrow(.))) %>% 
                                          formatRound(columns, digits = 2)
  }
  
  if(isTRUE(nrow(revals$categorical_cols) > 0)){
    revals$preprocess_tables$categorical <- summaryPreprocess(revals$peakData2, revals$categorical_cols, categorical = TRUE)
  }
  
})

# For numeric columns:
observeEvent(revals$preprocess_tables,{
  
  if(length(revals$preprocess_tables$numeric) > 0){
    # Create Table Output
    output$numeric_summary <- DT::renderDataTable({revals$preprocess_tables$numeric}) 
    
    # Summary Header
    output$numeric_header <- renderUI(tags$p("Summary Statistics for Numeric Variables"))
  }
  
  if(length(revals$preprocess_tables$categorical) > 0){
    for(i in 1:length(revals$preprocess_tables$categorical)){
      output[[paste0('Table_',i)]] <- DT::renderDataTable({revals$preprocess_tables$categorical[[i]]}, options = list(scrollX = TRUE, dom = "t"))
      Sys.sleep(0.5)
    }
    
    revals$ntables <- length(revals$preprocess_tables$categorical)
    
    output$cat_header <- renderUI(tags$p("Counts for Categorical Variables"))
  }
})

# The renderUI call that takes input from the above observer
output$categorical_summary <- renderUI({
  req(revals$ntables)
  
  if(isTRUE(nrow(isolate(revals$categorical_cols)) == 0)) NULL
  else{
    tagList(lapply(1:revals$ntables, function(i){
      DT::dataTableOutput(paste0('Table_',i))
                })
    )
  }
})

## END TABLE SUMMARY SECTION ##

# display warning if nothing selected
observeEvent(input$tests, {
  revals$warningmessage_preprocess$no_selection <- if(!isTRUE(length(input$tests) > 0)) "<p style = 'color:deepskyblue'>Select at least one test to calculate</p>" else NULL
}, ignoreNULL = FALSE)

# Success dialogs
observeEvent(input$preprocess_click,{
  req(revals$peakData2)
  validate(need(input$tests, message = "Please choose at least one test to calculate"))
  
  showModal(
    modalDialog(title = "Preprocess Success",
                fluidRow(
                  column(10, align = "center", offset = 1,
                         HTML('<h4 style= "color:#1A5276">Your data has been preprocessed.  Calculated variables have been added to the molecular identification file and can be used in subsequent filtering and visualization.</h4>'),
                         hr(),
                         actionButton('preprocess_dismiss', 'Review results', width = '75%'),
                         actionButton('goto_qc', 'Go to the QC tab to see some boxplots', style = 'width:75%;margin:5px'),
                         actionButton('goto_filter', 'Continue to filtering', width = '75%')
                  )
                )
                ,footer = NULL)
  ) 
}) # End successMessage

observeEvent(input$preprocess_dismiss,{removeModal()})
observeEvent(input$goto_filter,{
  updateTabsetPanel(session, "top_page", selected = "Filter")
  removeModal()
})
observeEvent(input$goto_qc,{
  updateTabsetPanel(session, 'top_page', selected = 'Quality Control')
  removeModal()
})