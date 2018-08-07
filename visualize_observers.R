# Help Button
observeEvent(input$visualize_help,{
  showModal(
    modalDialog("",
                tags$p("This page is used to generate plots from your processed data.  In order from top to bottom on the left panel, do the following:\n",
                       style = "color:CornFlowerBlue"),
                tags$ul(
                  tags$li("Select the type of plot you want to generate."),
                  tags$li("Choose whether you would like to plot a single sample, multiple samples, or a comparison of groups"),
                  tags$li("If you selected a single sample, specify which one.  If you selected multiple samples by group, select samples that 
                          should be grouped. If you selected a comparison of groups, two group dropdowns will appear; select samples that
                          you want included in each of the two groups"),
                  tags$li("If desired, specify axis and title labels and hit 'Generate Plot'\n"),
                  
                  style = "color:CornFlowerBlue"),
                tags$p("A plot will appear and can be customized to color by certain calculated values.  
                       Van Krevelen boundaries can be displayed for VK-plots.
                       Custom scatterplots will allow for selection of arbitrary x and y axes.", style = "color:CornFlowerBlue"),
                hr(),
                tags$p("Certain menu options may 'grey_out' during navigation, indicating disabled functionality for a plot type, 
                       or because certain values were not calculated during preprocessing")
                )
                )
})

# shinyjs helpers
observeEvent(c(input$top_page, input$chooseplots, input$choose_single, input$whichSamples, input$whichGroups1, input$whichGroups2),{
  req(input$top_page == "Visualize")
  toggleCssClass("plot_type", "suggest", input$chooseplots == 0)
  toggleCssClass("plotUI", "suggest", input$chooseplots != 0 & input$choose_single == 0)
  toggleCssClass("js_whichSamples", "suggest", input$choose_single %in% c(1,2) & is.null(input$whichSamples))
  toggleCssClass("js_whichGroups1", "suggest", input$choose_single == 3 & is.null(input$whichGroups1))
  toggleCssClass("js_whichGroups2", "suggest", input$choose_single == 3 & is.null(input$whichGroups2))
  toggleCssClass("plotUI_cond", "suggest", input$choose_single == 3 & all(is.null(input$whichGroups1), is.null(input$whichGroups2)))
  
})

# logical reactive value that clears the plot if a new type is selected
v <- reactiveValues(clearPlot = TRUE)
observeEvent(c(input$chooseplots, input$choose_single, input$whichSamples), {
  v$clearPlot <- TRUE
  disable("add_plot")
}, priority = 10)
observeEvent(input$plot_submit, {
  v$clearPlot <- FALSE
}, priority = 10)

# color scale inversion
observeEvent(input$flip_colors, {
  toggleCssClass("js_colorpal", "img-hor")
})

# make the options mutually exclusive when doing a comparison of two groups
observeEvent(input$whichGroups2,{
  updateSelectInput(session, "whichGroups1", choices = setdiff(sample_names(), input$whichGroups2), selected = input$whichGroups1)
})
observeEvent(input$whichGroups1,{
  updateSelectInput(session, "whichGroups2", choices = setdiff(sample_names(), input$whichGroups1), selected = input$whichGroups2)
})

# Multi purpose observer on input$chooseplots
observeEvent(input$chooseplots, {
  # Pre-populate dropdowns so users can select colors and custom scatterplot axes before submitting plot.
  updateSelectInput(session, 'vk_colors', choices = emeta_display_choices(), selected = emeta_display_choices()[1])
  updateSelectInput(session, 'scatter_x', choices = emeta_display_choices(), selected = emeta_display_choices()[2])
  updateSelectInput(session, 'scatter_y', choices = emeta_display_choices(), selected = emeta_display_choices()[3])
  
  
  # Rest of this observer controls shinyjs disable/enable behavior for reactive plot dropdowns
  dropdown_ids <- c("vkbounds", "vk_colors", "scatter_x", "scatter_y", "colorpal", "legend_title_input")
  choices = list('Van Krevelen Plot' = c("vk_colors", "vkbounds", "colorpal", "legend_title_input"), 
                 'Kendrick Plot' = c("vk_colors", "colorpal", "legend_title_input"),
                 'Density Plot' = "vk_colors", 
                 'Custom Scatter Plot' = c("vk_colors", "scatter_x", "scatter_y", "colorpal", "legend_title_input"), 
                 'Select an Option' = "0")
  
  # if(input$chooseplots %in% c("Custom Scatter Plot") | nrow(peakIcr2$f_data) == 1){
  #   disable("choose_single")
  #   addCssClass("choose_single", "grey_out")
  # }
  # else{
  #   enable("choose_single")
  #   removeCssClass("choose_single", "grey_out")
  # }
  
  lapply(dropdown_ids, function(inputid){
    if(inputid %in% choices[[input$chooseplots]]){
      enable(inputid)
      removeCssClass(paste0("js_",inputid), "grey_out")
    }
    else{
      disable(inputid)
      addCssClass(paste0("js_",inputid), "grey_out")
    }
  })
})

# maintain mutual exclusivity of scatterplot axes and colors
observeEvent(c(input$scatter_x, input$vk_colors),{
  updateSelectInput(session, "scatter_y", 
                    choices = revals$axes_choices[!(revals$axes_choices %in% c(input$scatter_x, input$vk_colors))], 
                    selected = input$scatter_y)
})

observeEvent(c(input$scatter_y, input$vk_colors),{
  updateSelectInput(session, "scatter_x", 
                    choices = revals$axes_choices[!(revals$axes_choices %in% c(input$scatter_y, input$vk_colors))], 
                    selected = input$scatter_x)
})

observeEvent(c(input$scatter_x, input$scatter_y),{
  updateSelectInput(session, "vk_colors", 
                    choices = revals$color_by_choices[!(revals$color_by_choices %in% c(input$scatter_y, input$scatter_x))], 
                    selected = input$vk_colors)
})

# Observer which greys-out colorscale selection if we have not selected a numeric column to color by
observeEvent(numeric_selected(),{
  req(input$chooseplots != "Density Plot")
  if(numeric_selected()){
    enable("colorpal")
    removeCssClass("js_colorpal", "grey_out")
    
    enable("legend_title_input")
    removeCssClass("js_legend_title_input", "grey_out")
  }
  else if(!numeric_selected()){
    disable("colorpal")
    addCssClass("js_colorpal", "grey_out")
    
    disable("legend_title_input")
    addCssClass("js_legend_title_input", "grey_out")
  }
  
  
})
