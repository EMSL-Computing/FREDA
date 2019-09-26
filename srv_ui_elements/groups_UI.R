list(
  # sample names selector based on the sample names of revals$uploaded_data
  output$group_samples <- renderUI({
    validate(need(sample_names(), message = "Upload data before defining groups"))
    pickerInput("group_samples", "Samples to include in this group:", choices = sample_names(), 
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