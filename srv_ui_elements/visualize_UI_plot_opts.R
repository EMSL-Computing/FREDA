list(
  # Axis and title label input menus
  output$title_out <- renderUI({
    
    validate(
      need(!is.null(input$chooseplots), message = "")
    )
    textInput(inputId = "title_input", label = "Plot title", value = "")
  }),
  
  output$x_axis_out <- renderUI({
    validate(
      need(!is.null(input$chooseplots), message = "")
    )
    textInput(inputId = "x_axis_input", label = "X axis label", value = plot_defaults()$xlabel)
  }),
  
  output$y_axis_out <- renderUI({
    validate(
      need(!is.null(input$chooseplots), message = "")
    )
    textInput(inputId = "y_axis_input", label = "Y axis label", value = plot_defaults()$ylabel)
  }),
  
  # legend input
  output$legend_title_out <- renderUI({
    validate(
      need(!is.null(input$chooseplots), message = "")
    )
    if (input$chooseplots == "Density Plot"){
      addCssClass("js_legend_title_input", "grey_out")
      disabled(textInput(inputId = "legend_title_input", label = "Legend label", value = ""))
    }
    else {
      removeCssClass("js_legend_title_input", "grey_out")
      textInput(inputId = "legend_title_input", label = "Legend label", value = "")
    }
    
  }),
  
  # view plot table button UI
  output$view_plots <- renderUI({
    n_plots <- nrow(parmTable$parms[which(rowSums(!is.na(parmTable$parms))!=0),])
    actionButton(inputId = "view_plots_btn", width = '100%', 
                 label = sprintf("View Table Info of %i Saved Plots", n_plots), icon = icon("folder-open", lib = "glyphicon"))
  }),
  
  # color palette selection (main panel)
  output$colorpal_out <- renderUI({
    choices = c("YlOrRd", "YlGnBu", "YlGn", "RdYlGn")
    
    extensions <- lapply(choices, function(choice){
      tags$img(src = paste0(choice, ".png"), width = "100px", height = "25px")
    })
    
    if (isTRUE(input$chooseplots == "Density Plot")){
      addClass("js_colorpal", "grey_out")
      disabled(colored_radiobuttons(inputId = "colorpal", label = "Pick a coloring scheme", inline = TRUE,
                                    choices = choices, extensions = extensions))
    }else {
      removeClass("js_colorpal", "grey_out")
      colored_radiobuttons(inputId = "colorpal", label = "Pick a coloring scheme", inline = TRUE,
                           choices = choices, extensions = extensions)
    }
  })
)