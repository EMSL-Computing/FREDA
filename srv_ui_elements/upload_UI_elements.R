list(
  # Drop-down lists: Select which column represents C / H / N / O / etc
  # First try to locate the column name with a grepl
  # Note: All require emeta_cnames()
  output$c_column <- renderUI({
    selectInput("c_column", "Carbon column:",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^c$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^c$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  }),
  
  output$h_column <- renderUI({
    
    selectInput("h_column", "Hydrogen column:",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^h$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^h$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  }),
  
  output$n_column <- renderUI({
    
    selectInput("n_column", "Nitrogen column:",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^n$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^n$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  }),
  
  output$o_column <- renderUI({
    
    selectInput("o_column", "Oxygen column:",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^o$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^o$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  }), 
  
  output$s_column <- renderUI({
    
    selectInput("s_column", "Sulfur column:",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^s$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^s$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  }),
  
  output$p_column <- renderUI({
    
    selectInput("p_column", "Phosphorus column:",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^p$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^p$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
  }),
  ### END of CHNOSP DROP DOWN LISTS ###
  
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
    req(input$iso_info_filter )
    if (input$iso_info_filter == 1) {
      selectInput("iso_info_column", "Which column contains isotopic information?",
                  choices  = c('Select a column' = '0', emeta_cnames()))
    } else (return(NULL))
  }),
  
  output$iso_symbol_out <- renderUI({
    req(input$iso_info_filter )
    if (input$iso_info_filter == 1) {
      textInput("iso_symbol", label = "Enter a symbol denoting isotopic notation:",
                value = "1")
    } else (return(NULL))
  })
)