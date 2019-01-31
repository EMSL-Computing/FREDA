# Object: display choices for various dropdowns
emeta_display_choices <- reactive({
  input$preprocess_click
  
  # do not allow mass or isotopic info column as options
  drop_cols <- c(attr(peakIcr2, "cnames")$mass_cname,
                 input$iso_info_column)
  
  # get column names 
  column_choices <- peakIcr2$e_meta %>% 
    dplyr::select(-one_of(drop_cols)) %>%
    dplyr::select(which(sapply(., function(col){ length(unique(col)) < 12 } ) | sapply(., is.numeric))) %>% #dont include columns with too many categories
    colnames() 
  
  #columns included in calculation_options.csv get their prettified names, everything else gets the column name
  names(column_choices) <- lapply(column_choices, function(x){
    if (x %in% calc_vars$ColumnName){
      calc_vars %>% filter(ColumnName == x) %>% pluck("DisplayName")
    }
    else x
  }) %>% unlist()
  
  #____test export_____
  exportTestValues(display_names = column_choices)
  
  column_choices
  
})

# Object: Create 'Success' message if everything works out, show errors if not
# Note: Created when Filter action button is clicked
successMessage <- eventReactive(input$filter_click, {
  
  # If mass filter is checked
  if (input$massfilter) {
    
    # Error handling: need 0 < minMass < maxMass
    validate(need((input$min_mass < input$max_mass),'Minimum mass must be less than maximum mass'), 
             need((input$min_mass > 0),'Minimum mass must be greater than 0'),
             need((input$min_mass && input$max_mass),'Both minimum and maximum mass required to filter')
             
    ) # End error handling
  }
  showModal(
    modalDialog(title = "Filter Success",
                fluidRow(
                  column(10, align = "center", offset = 1,
                         HTML('<h4 style= "color:#1A5276">Your data has been filtered using mass and/or minimum observations. 
                              You may proceed to the next tabs for subsequnt analysis.</h4>'),
                         hr(),
                         actionButton("filter_dismiss", "Review results", width = '75%'),
                         br(),
                         br(),
                         actionButton("goto_viz", "Continue to Visualization", width = '75%')
                         )
                )
                ,footer = NULL)
  )
  
  HTML('<h4 style= "color:#1A5276">You may now proceed to visualization</h4>')
  
}) # End successMessage