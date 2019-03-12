### Summary Panel: Display table summaries of numeric and categorical columns in e_meta ###

# For numeric columns:
observe({
  
  req(nrow(revals$numeric_cols) > 0)
  
  # Create Table Output
  output$numeric_summary <- DT::renderDataTable({
    columns <- summaryPreprocess(peakIcr2, revals$numeric_cols) %>% colnames()
    
    summaryPreprocess(peakIcr2, revals$numeric_cols) %>%
      datatable(options = list(dom = "t", pageLength = nrow(.))) %>% 
      formatRound(columns, digits = 2)
  }) 
  
  # Summary Header
  output$numeric_header <- renderUI(tags$p("Summary Statistics for Numeric Variables"))
  
})

# For Categorical Columns

# This observer assigns renderTable calls to various output ID's and passes them to the renderUI call immediately below
observe({
  req(nrow(revals$categorical_cols) > 0) 
  
  # List of tables which will be passed to renderTable()
  table_list <- summaryPreprocess(peakIcr2, revals$categorical_cols, categorical = TRUE)
  
  # Reactive variable which lets lapply know how many output ID's to generate depending on number of categorical variables selected
  revals$ntables <- length(table_list)
  
  # Call renderTable on each table and assign it to an output ID
  lapply(1:length(table_list), function(i){
    output[[paste0('Table_',i)]] <- DT::renderDataTable({table_list[[i]]}, options = list(scrollX = TRUE, dom = "t"))
  })
  
  # Summary Header
  output$cat_header <- renderUI(tags$p("Counts for Categorical Variables"))
})

# The renderUI call that takes input from the above observer
output$categorical_summary <- renderUI({
  req(revals$categorical_cols)
  
  if(isTRUE(nrow(revals$categorical_cols) == 0)) NULL
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
  req(peakIcr2)
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