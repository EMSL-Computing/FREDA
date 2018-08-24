# shinyjs helpers
observeEvent(c(input$top_page, input$chooseplots, input$choose_single, input$whichSamples, input$whichGroups1, input$whichGroups2,
               input$summary_fxn, input$pres_thresh, input$pres_fn, input$absn_thresh, input$pval),{
                 req(input$top_page == "Visualize")
                 
                 # conditionally apply blue outlines to help user along
                 toggleCssClass("plot_type", "suggest", input$chooseplots == 0)
                 toggleCssClass("plotUI", "suggest", input$chooseplots != 0 & input$choose_single == 0)
                 toggleCssClass("js_whichSamples", "suggest", any(input$choose_single %in% c(1,2) & is.null(input$whichSamples), input$choose_single == 2 & isTRUE(length(input$whichSamples) < 2)))
                 toggleCssClass("js_whichGroups1", "suggest", input$choose_single == 3 & is.null(input$whichGroups1))
                 toggleCssClass("js_whichGroups2", "suggest", input$choose_single == 3 & is.null(input$whichGroups2))
                 toggleCssClass("plotUI_cond", "suggest", input$choose_single == 3 & all(is.null(input$whichGroups1), is.null(input$whichGroups2)))
                 toggleCssClass("js_summary_fxn", "suggest", input$choose_single == 3 & all(!is.null(input$whichGroups1), !is.null(input$whichGroups2)) & input$summary_fxn == "select_none")
                 
                 # simple state toggling
                 ### warning visuals for summary comparison plots
                 ### displays warning message and error box if user does something like selects a p-value not in (0,1)
                 
                 # conditions different between counts and proportion 
                 if (isTRUE(input$pres_fn == "nsamps")){
                   # g test warning conditions different from pres/absn conditions
                   if(isTRUE(input$summary_fxn == "uniqueness_gtest")){
                     # logical conditions that are TRUE if the user did something wrong
                     cond_pval <- any(input$pval <= 0, input$pval >= 1)
                     cond_pres <- any(input$pres_thresh > min(length(input$whichGroups1), length(input$whichGroups2)), 
                                      input$pres_thresh < 1, 
                                      !is.numeric(input$pres_thresh))
                     toggleCssClass("js_pval", "attention", cond_pval)
                     toggleCssClass("js_pres_thresh", "attention", cond_pres)
                     
                     # warning message content displayed below dropdowns
                     content_pval <- if(isTRUE(cond_pval)) "style = 'color:red'>P-value must be between 0 and 1" else NULL
                     content_pres <- if(isTRUE(cond_pres)) "style = 'color:red'>Presence threshold must be a numeric value of at least 1 and no more than the minimum number of samples in a group" else NULL
                     content_absn <- NULL
                   }
                   else if(isTRUE(input$summary_fxn == "uniqueness_nsamps")){
                     cond_pres <- any(input$pres_thresh > min(length(input$whichGroups1), length(input$whichGroups2)), input$pres_thresh < 1,
                                      !is.numeric(input$pres_thresh), input$absn_thresh >= input$pres_thresh)
                     cond_absn <- any(input$absn_thresh > min(length(input$whichGroups1), length(input$whichGroups2)) - 1, input$absn_thresh < 0, 
                                      !is.numeric(input$absn_thresh), input$absn_thresh >= input$pres_thresh)
                     
                     toggleCssClass("js_pres_thresh", "attention", cond_pres)
                     toggleCssClass("js_absn_thresh", "attention", cond_absn)
                     
                     content_pval <- NULL
                     content_pres <- if(isTRUE(cond_pres)) "style = 'color:red'>Presence threshold must be a numeric value of at least 1, no more than the minimum number of samples in a group, and greater than the absence threshold." else NULL
                     content_absn <- if(isTRUE(cond_absn)) "style = 'color:red'>Absence threshold must be a numeric value less than the minimum group size and less than the presence threshold." else NULL
                   }
                   else content_pval <- content_pres <- content_absn <- NULL
                 }
                 else if (isTRUE(input$pres_fn == "prop")){
                   if(isTRUE(input$summary_fxn == "uniqueness_gtest")){
                     cond_pval <- any(input$pval <= 0, input$pval >= 1)
                     cond_pres <- any(input$pres_thresh > 1, input$pres_thresh <= 0, !is.numeric(input$pres_thresh))
                     toggleCssClass("js_pval", "attention", cond_pval)
                     toggleCssClass("js_pres_thresh", "attention", cond_pres)
                     
                     content_pval <- if(isTRUE(cond_pval)) "style = 'color:red'>P-value must be between 0 and 1" else NULL
                     content_pres <- if(isTRUE(cond_pres)) "style = 'color:red'>Presence threshold must be a numeric value greater than 0 and at most 1" else NULL
                     content_absn <- NULL
                   }
                   else if(isTRUE(input$summary_fxn == "uniqueness_prop")){
                     cond_pres <- any(input$pres_thresh > 1, input$pres_thresh <= 0, !is.numeric(input$pres_thresh), input$absn_thresh >= input$pres_thresh)
                     cond_absn <- any(input$absn_thresh >= 1, input$absn_thresh < 0, !is.numeric(input$absn_thresh), input$absn_thresh >= input$pres_thresh)
                     
                     toggleCssClass("js_pres_thresh", "attention", cond_pres)
                     toggleCssClass("js_absn_thresh", "attention", cond_absn)
                     
                     content_pval <- NULL
                     content_pres <- if(isTRUE(cond_pres)) "style = 'color:red'>Presence threshold must be a numeric value greater than 0, at most 1, and greater than the absence threshold" else NULL
                     content_absn <- if(isTRUE(cond_absn)) "style = 'color:red'>Absence threshold must be a non-negative numeric value less than the presence threshold." else NULL
                     
                   }
                   else content_pval <- content_pres <- content_absn <- NULL
                 }
                 else content_pval <- content_pres <- content_absn <- NULL
                 
                 # store warning messages in reactive list.  this list is split and rendered in output$warningmessage_visualize
                 revals$warningmessage_visualize$content_pval <- content_pval
                 revals$warningmessage_visualize$content_pres <- content_pres
                 revals$warningmessage_visualize$content_absn <- content_absn
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
}, ignoreNULL = FALSE)
observeEvent(input$whichGroups1,{
  updateSelectInput(session, "whichGroups2", choices = setdiff(sample_names(), input$whichGroups1), selected = input$whichGroups2)
}, ignoreNULL = FALSE)

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
    toggleState(inputid, condition = inputid %in% choices[[input$chooseplots]])
    toggleCssClass(paste0("js_", inputid), "grey_out", condition = !(inputid %in% choices[[input$chooseplots]]))
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

### Summary comparison plot selection control ###
observeEvent(c(input$pres_fn, input$whichGroups1, input$whichGroups2),{
  cond_smallgrp <- any(length(input$whichGroups1) < 3, length(input$whichGroups2) < 3)
  content <- if(cond_smallgrp) "style = 'color:grey'>G-test disabled for groups smaller than size 3" else NULL
  
  if (input$pres_fn == "nsamps"){
    if(cond_smallgrp){
      choices = c("Select one" = "select_none", "Presence/absence thresholds" = "uniqueness_nsamps")
    }
    else choices = c("Select one" = "select_none", "G test" = "uniqueness_gtest", "Presence/absence thresholds" = "uniqueness_nsamps")
    updateNumericInput(session, "thresh", min = 1, max = min(length(input$whichGroups1), length(input$whichGroups2)))
  }
  else if (input$pres_fn == "prop"){
    if(cond_smallgrp){
      choices = c("Select one" = "select_none", "Presence/absence thresholds" = "uniqueness_prop")
    }
    choices = c("Select one" = "select_none", "G test" = "uniqueness_gtest", "Presence/absence thresholds" = "uniqueness_prop")
    updateNumericInput(session, "thresh", min = 0, max = 1)
  }
  selected = ifelse(input$summary_fxn %in% c("uniqueness_nsamps", "uniqueness_prop"), choices["Presence/absence thresholds"], input$summary_fxn)
  updateSelectInput(session, "summary_fxn", choices = choices, selected = selected)
  revals$warningmessage_visualize$small_groups <- content
})

# Control state for presence/absence threshold and p-value inputs
observeEvent(input$summary_fxn,{
  req(input$chooseplots != "Density Plot")
  toggleState("pval", input$summary_fxn == "uniqueness_gtest")
  toggleCssClass("js_pval", "grey_out", condition = input$summary_fxn != "uniqueness_gtest")
  toggleState("absn_thresh", input$summary_fxn != "uniqueness_gtest")
  toggleCssClass("js_absn_thresh", "grey_out", condition = input$summary_fxn == "uniqueness_gtest")
})
####                                 ###