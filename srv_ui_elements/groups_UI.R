list(
  # sample names selector based on the sample names of revals$uploaded_data
  output$group_samples_UI <- renderUI({
    validate(need(sample_names(), message = "Upload data before defining groups"))
    req(!is.null(input$group_regex))
    # filter sample names
    inds = grepl(input$group_regex, sample_names())
    
    pickerInput("group_samples", "Samples to include in this group:", choices = sample_names()[inds], 
                options =  pickerOptions(dropupAuto = FALSE, actionsBox = TRUE), multiple = TRUE)
  }),
  
  # table which displays stored groups
  output$group_table <- DT::renderDataTable(groupstab_df(),
                                            selection = 'single',
                                            options = list(scrollX = TRUE)),
  
  output$warnings_groups <- renderUI({
    HTML(paste(revals$warningmessage_groups, collapse = ""))
  })
)