list(
  # Populate List from CSV File calculation_options.csv
  output$which_calcs <- renderUI({
    choices <- calc_opts$Function
    names(choices) <- calc_opts$DisplayName

    # create list of extra html elements to add to the checkboxgroup
    extensions = lapply(1:length(choices), function(i) {
      div(style = "color:deepskyblue;display:inline-block",
        tipify(icon("question-sign", lib = "glyphicon"), title = calc_opts$Info[i], placement = "top", trigger = 'hover')
      )
    })

    isolate({
      # create a list of html objects which are extra options for certain functions, these will appear below the checkbox.
      options = lapply(choices, function(x){
        if(x == 'calc_element_ratios' & 'calc_element_ratios' %in% input$tests){
          available_standard_ratios <- if(all( c("O", "P", "N") %in% names(isolate(extra_elements())) )) 'O:C, H:C, N:C, P:C, N:P' else 'H:C'
          standard_ratios <- if(is.null(input$element_ratios)) available_standard_ratios else input$element_ratios
          div(style='padding-left:20px;',
              tipify(icon("question-sign", lib = "glyphicon"), element_ratios_info, placement = "top", trigger = 'hover'),
              div(style = 'display:inline-block', textInput('element_ratios', 'List desired ratios:', value = standard_ratios))
          )
        }
        else if(x == 'calc_kendrick' & 'calc_kendrick' %in% input$tests){
          kendrick_selected = if(is.null(input$base_unit)) 'CH2' else input$base_unit
          div(style='padding-left:20px;', 
              tipify(icon("question-sign", lib = "glyphicon"), kendrick_opts_info, placement = "top", trigger = 'hover'),
              div(style = 'width:50%;display:inline-block', 
                  pickerInput('base_unit', 'Choose Base Compound', choices = c('CH2', 'CO2', 'H2', 'H2O', 'CHO'), selected = kendrick_selected, multiple = T)
              )
          )
        }
        else if (x == 'calc_dbe' & 'calc_dbe' %in% input$tests) {
          dbe_selected = if (is.null(input$dbe_valences)) 'C4HN3O2S2P3' else input$dbe_valences
          div(style='padding-left:20px;',
            tipify(icon("question-sign", lib = "glyphicon"), dbe_opts_info, placement = "top", trigger = 'hover'),
            div(style = 'display:inline-block', textInput('dbe_valences', 'Specify DBE valences', value = dbe_selected))
          )
        }
        else NULL
      })

      tagList(options)
    })

    selected = if (is.null(input$tests)) c("calc_element_ratios", "calc_kendrick") else input$tests

    tooltip_checkbox("tests", "What Values Should be Calculated?", choices, selected = selected,
      extensions = extensions,
      options = options
    )
  }),

  # Warnings for preprocess tab
  output$warnings_preprocess <- renderUI({
    HTML(paste(revals$warningmessage_preprocess, collapse = ""))
  }),

  # Plot the histogram chosen above
  # Depends on: which_hist
  output$preprocess_hist <- renderPlotly({

    # Error handling: Require some columns to be selected
    req(input$which_hist)

    isolate({
      # Save column name for later display
      columnName <- input$which_hist

      # set display name
      displayName <- calc_vars %>% filter(ColumnName == columnName) %>%
        pluck("DisplayName")

      # Plot histogram using plotly
      p <- plot_ly(x = revals$uploaded_data$e_meta[, columnName], type = 'histogram') %>%
        layout(title = paste('Observed distribution of', displayName),
          scene = list(
            xaxis = list(title = displayName),
            yaxis = list(title = 'Frequency')))
      p$elementId <- NULL

      # ____test export_____
      exportTestValues(preprocess_hist = p, hist_attrs = p$x$attrs[[p$x$cur_data]], hist_layout = p$x$layout, hist_visdat = p$x$visdat[[p$x$cur_data]]())

      isolate(plots$last_plot[[input$top_page]] <- p)

      return(toWebGL(p))
    })

  }) # End process_hist

)
