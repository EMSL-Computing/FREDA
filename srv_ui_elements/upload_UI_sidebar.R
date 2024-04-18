list(
  # Drop down list: Get edata unique identifier
  output$edata_id <- renderUI({
    # Drop down list with options from column names
    selectInput("edata_id_col", "Choose column with IDs",
      choices = c('Select one', edata_cnames()))
  }), # End edata_id #

  output$NA_value_UI <- renderUI({
    validate(need(Edata(), "Upload data file"))

    n_zeros = sum(Edata() == 0)
    prop_zeros = n_zeros / (prod(dim(Edata())) - nrow(Edata()))
    value = if (isTRUE(prop_zeros > 0.1)) "0" else "NA"
    textInput("NA_value", "What value specifies missing data?", value = value)
  }),

  # Drop-down lists: Choose formula column
  output$f_column <- renderUI({
    selectInput("f_column", "Choose formula column",
      choices = c('Select one', emeta_cnames()))
  }), # End f_column #

  # Drop-down lists: Select which column represents C / H / N / O / etc
  # First try to locate the column name with a grepl
  # Note: All require emeta_cnames()

  output$c_column <- renderUI({
    selectInput("c_column", "Carbon: ",
                choices = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^c$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^c$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  }),
  
  output$h_column <- renderUI({
    selectInput("h_column", "Hydrogen: ",
      choices = c('Select a column', emeta_cnames()),
      selected = ifelse(grepl("^h$", tolower(emeta_cnames())),
        yes = emeta_cnames()[grepl("^h$", tolower(emeta_cnames()))][1],
        no = 'Select a column'))

  }),
  
  output$add_ONSP <- renderUI({
    checkboxInput("add_ONSP", label="Add standard elements O, N, S, P if detected in data?",
                  value = FALSE)
  }),
  
  output$extra_element_name <- renderUI({
    selectInput("extra_element_name", "Possible Elements:",
                choices = c('Select an element', element_names),
                selected = NULL )
  }),
  
  output$extra_element_col <- renderUI({
    selectInput("extra_element_col", "Element Column Name",
                choices = c('Select a column', emeta_cnames()),
                selected = NULL )
  }),
  
  output$add_element_column_button <- renderUI({
    actionButton("add_element_column_button","Add Element")
 }),
 
  output$added_elements <- renderDataTable({
    if (length(extra_elements()) != 0){
      data.frame("Element"=names(extra_elements()), 
                 "Column Name"=unlist(extra_elements()), 
                 row.names=NULL, check.names = FALSE)
    }
  }, options=list(searching=FALSE, paging=FALSE)),
 
  output$remove_element_row_button <- renderUI({
    actionButton("remove_element_row_button", "Remove Selected Elements")
  }),

  ### END of Element Addition

  # C13 #
  output$iso_info_filter_out <- renderUI({
    radioGroupButtons(inputId = "iso_info_filter", label = "Filter isotopic peaks?",
      choices = list('Yes' = 1,
        'No' = 2),
      selected = 'Yes',
      justified = TRUE
    )
  }),

  output$iso_info_column_out <- renderUI({
    req(input$iso_info_filter)
    if (input$iso_info_filter == 1) {
      selectInput("iso_info_column", "Which column contains isotopic information?",
        choices = c('Select a column' = '0', emeta_cnames()))
    } else (return(NULL))
  }),

  output$iso_symbol_out <- renderUI({
    req(input$iso_info_filter)
    if (input$iso_info_filter == 1) {
      textInput("iso_symbol", label = "Enter a symbol denoting isotopic notation:",
        value = "1")
    } else (return(NULL))
  })
)
