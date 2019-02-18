# plot selection update on page change
observeEvent(input$top_page, {
  req(input$top_page == "Visualize")
  updateRadioGroupButtons(session, inputId = "chooseplots", selected = revals$chooseplots)
  revals$chooseplots <- input$chooseplots
}, priority = 10)

# display modal dialog of saved plot info
observeEvent(input$view_plots,{
  showModal(modalDialog(
    dataTableOutput("parmsTable")
    )
  )
})

# animate the saved plots button to indicate your plot was saved
observeEvent(input$add_plot,{
  addCssClass("view_plots", "pulse")
  Sys.sleep(0.6)
  removeCssClass("view_plots", "pulse")
})

# shinyjs helpers and reactive value storage for selection inputs
observeEvent(c(input$top_page, input$chooseplots, input$choose_single, input$whichSamples, 
               g1_samples(), g2_samples()),{
    req(input$top_page == "Visualize")
    # show/hide dropdowns for sample selection depending on single sample/single group/group comparison
    toggle("js_toggle_groups", condition = input$choose_single %in% c(3,4))
    toggle("js_toggle_single", condition = input$choose_single %in% c(1,2))

     # conditionally apply blue outlines to help user along sample selection process
    toggleCssClass("plot_type", "suggest", is.null(input$chooseplots))
    toggleCssClass("plotUI", "suggest", !is.null(input$chooseplots) & input$choose_single == 0)
    toggleCssClass("js_whichSamples", "suggest", any(input$choose_single %in% c(1,2) & is.null(input$whichSamples), 
                                                     input$choose_single == 2 & any(length(input$whichSamples) < 2, is.null(input$whichSamples))))
    toggleCssClass("js_whichGroups1", "suggest", input$choose_single == 3 & is.null(g1_samples()))
    toggleCssClass("js_whichGroups2", "suggest", input$choose_single == 3 & is.null(g2_samples()))

    toggleElement("warnings_visualize", condition = isTRUE(input$choose_single != 0 & !is.null(input$chooseplots)))
   
    revals$chooseplots <- input$chooseplots
  })



# helpers for summary functions
observeEvent(c(input$top_page, input$choose_single, g1_samples(), g2_samples(),
               input$summary_fxn, input$pres_thresh, input$pres_fn, input$absn_thresh, input$pval),{
                 req(input$top_page == "Visualize")

                 toggleCssClass("js_summary_fxn", "suggest", input$choose_single %in% c(3,4) & all(!is.null(g1_samples()), !is.null(g2_samples())) & !(input$summary_fxn %in% fticRanalysis:::getGroupComparisonSummaryFunctionNames()))
                 
                 # conditions different between counts and proportion 
                 if (isTRUE(input$pres_fn == "nsamps") & isTRUE(input$choose_single %in% c(3,4))){
                   # g test warning conditions different from pres/absn conditions
                   if(isTRUE(input$summary_fxn == "uniqueness_gtest")){
                     # logical conditions that are TRUE if the user did something wrong
                     cond_pval <- any(input$pval <= 0, input$pval >= 1)
                     cond_pres <- any(input$pres_thresh > min(length(g1_samples()), length(g2_samples())), 
                                      input$pres_thresh < 1, 
                                      !is.numeric(input$pres_thresh))
                     toggleCssClass("js_pval", "attention", cond_pval)
                     toggleCssClass("js_pres_thresh", "attention", cond_pres)
                     
                     # warning message content displayed below dropdowns
                     content_pval <- if(isTRUE(cond_pval)) "<p style = 'color:red'>P-value must be between 0 and 1</p>" else NULL
                     content_pres <- if(isTRUE(cond_pres)) "<p style = 'color:red'>Presence threshold must be a numeric value of at least 1 and no more than the minimum number of samples in a group</p>" else NULL
                     content_absn <- NULL
                   }
                   else if(isTRUE(input$summary_fxn == "uniqueness_nsamps") & isTRUE(input$choose_single %in% c(3,4))){
                     cond_pres <- any(input$pres_thresh > min(length(g1_samples()), length(g2_samples())), input$pres_thresh < 1,
                                      !is.numeric(input$pres_thresh), input$absn_thresh >= input$pres_thresh)
                     cond_absn <- any(input$absn_thresh > min(length(g1_samples()), length(g2_samples())) - 1, input$absn_thresh < 0, 
                                      !is.numeric(input$absn_thresh), input$absn_thresh >= input$pres_thresh)
                     
                     toggleCssClass("js_pres_thresh", "attention", cond_pres)
                     toggleCssClass("js_absn_thresh", "attention", cond_absn)
                     
                     content_pval <- NULL
                     content_pres <- if(isTRUE(cond_pres)) "<p style = 'color:red'>Presence threshold must be a numeric value of at least 1, no more than the minimum number of samples in a group, and greater than the absence threshold.</p>" else NULL
                     content_absn <- if(isTRUE(cond_absn)) "<p style = 'color:red'>Absence threshold must be a numeric value less than the minimum group size and less than the presence threshold.</p>" else NULL
                   }
                   else content_pval <- content_pres <- content_absn <- NULL
                 }
                 else if (isTRUE(input$pres_fn == "prop") & isTRUE(input$choose_single %in% c(3,4))){
                   if(isTRUE(input$summary_fxn == "uniqueness_gtest")){
                     cond_pval <- any(input$pval <= 0, input$pval >= 1)
                     cond_pres <- any(input$pres_thresh > 1, input$pres_thresh <= 0, !is.numeric(input$pres_thresh))
                     toggleCssClass("js_pval", "attention", cond_pval)
                     toggleCssClass("js_pres_thresh", "attention", cond_pres)
                     
                     content_pval <- if(isTRUE(cond_pval)) "<p style = 'color:red'>P-value must be between 0 and 1</p>" else NULL
                     content_pres <- if(isTRUE(cond_pres)) "<p style = 'color:red'>Presence threshold must be a numeric value greater than 0 and at most 1</p>" else NULL
                     content_absn <- NULL
                   }
                   else if(isTRUE(input$summary_fxn == "uniqueness_prop") & isTRUE(input$choose_single %in% c(3,4))){
                     cond_pres <- any(input$pres_thresh > 1, input$pres_thresh <= 0, !is.numeric(input$pres_thresh), input$absn_thresh >= input$pres_thresh)
                     cond_absn <- any(input$absn_thresh >= 1, input$absn_thresh < 0, !is.numeric(input$absn_thresh), input$absn_thresh >= input$pres_thresh)
                     
                     toggleCssClass("js_pres_thresh", "attention", cond_pres)
                     toggleCssClass("js_absn_thresh", "attention", cond_absn)
                     
                     content_pval <- NULL
                     content_pres <- if(isTRUE(cond_pres)) "<p style = 'color:red'>Presence threshold must be a numeric value greater than 0, at most 1, and greater than the absence threshold.</p>" else NULL
                     content_absn <- if(isTRUE(cond_absn)) "<p style = 'color:red'>Absence threshold must be a non-negative numeric value less than the presence threshold.</p>" else NULL
                     
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
observeEvent(c(input$chooseplots, input$choose_single, input$whichSamples, 
               g1_samples(), g2_samples()), {
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
  req(exists("peakIcr2"))
  updatePickerInput(session, "whichGroups1", choices = setdiff(names(revals$groups_list), input$whichGroups2), selected = input$whichGroups1)
})
observeEvent(input$whichGroups1,{
  req(exists("peakIcr2"))
  updatePickerInput(session, "whichGroups2", choices = setdiff(names(revals$groups_list), input$whichGroups1), selected = input$whichGroups2)
})

# make the options mutually exclusive when doing a comparison of two samples
observeEvent(input$whichSample2,{
  req(exists("peakIcr2"))
  updatePickerInput(session, "whichSample1", 
                    choices = setdiff(colnames(peakIcr2$e_data[-which(colnames(peakIcr2$e_data) == getEDataColName(peakIcr2))]), input$whichSample2),
                    selected = input$whichSample1)
})
observeEvent(input$whichSample1,{
  req(exists("peakIcr2"))
  updatePickerInput(session, "whichSample2", 
                    choices = setdiff(colnames(peakIcr2$e_data[-which(colnames(peakIcr2$e_data) == getEDataColName(peakIcr2))]), input$whichSample1),
                    selected = input$whichSample2)
})

# Multi purpose observer on input$chooseplots
observeEvent(input$chooseplots, {
  # Pre-populate dropdowns so users can select colors and custom scatterplot axes before submitting plot.
  # Need a vector of the numeric columns to pass to scatterplot
  numeric_cols <- which(sapply(peakIcr2$e_meta %>% dplyr::select(emeta_display_choices()), is.numeric))
  
  color_select_label <- if(input$chooseplots == 'Density Plot') "Plot Distribution of Variable:" else "Color by:"
  
  updateSelectInput(session, 'vk_colors', label = color_select_label, choices = emeta_display_choices(), selected = emeta_display_choices()[1])
  updateSelectInput(session, 'scatter_x', choices = emeta_display_choices()[numeric_cols][-3], selected = emeta_display_choices()[numeric_cols][2])
  updateSelectInput(session, 'scatter_y', choices = emeta_display_choices()[numeric_cols][-2], selected = emeta_display_choices()[numeric_cols][3])
  
  # Rest of this observer controls shinyjs disable/enable behavior for reactive plot dropdowns
  dropdown_ids <- c("vkbounds", "vk_colors", "scatter_x", "scatter_y", "colorpal", "legend_title_input")
  choices = list('Van Krevelen Plot' = c("vk_colors", "vkbounds", "colorpal", "legend_title_input"), 
                 'Kendrick Plot' = c("vk_colors", "colorpal", "legend_title_input"),
                 'Density Plot' = "vk_colors", 
                 'Custom Scatter Plot' = c("vk_colors", "scatter_x", "scatter_y", "colorpal", "legend_title_input"), 
                 'Select an Option' = "0")
  
  # Toggle axes and coloring options depending on plot type
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

# disable plot_submit and add warning if groups are overlapping
observeEvent(c(input$whichGroups1, input$whichGroups2, input$chooseplots, g1_samples(), g2_samples()),{
  overlap <- intersect(g1_samples(), g2_samples())
  cond_overlap <- length(overlap) != 0
  
  toggleState("plot_submit", !(cond_overlap & isTRUE(input$choose_single == 3)))
  revals$warningmessage_visualize$group_overlap <- if(cond_overlap & isTRUE(input$choose_single == 3)) sprintf("<p style = color:red>Please choose mutually exclusive groups.  The following samples were present in both groups: %s.</p>", paste(overlap, collapse = ", ")) else NULL
  
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
observeEvent(c(input$pres_fn, g1_samples(), g2_samples(), input$choose_single),{
  
  cond_smallgrp <- any(length(g1_samples()) < 3, length(g2_samples()) < 3) & isTRUE(input$choose_single %in% c(3,4)) & input$chooseplots != "Density Plot" 
  # cond_onesample <- any(length(input$whichGroups1) < 2, length(input$whichGroups2) < 2) & isTRUE(input$choose_single == 3) & input$chooseplots != "Density Plot"
  content <- if(cond_smallgrp & isTRUE(input$summary_fxn == "uniqueness_gtest")) "<p style = 'color:deepskyblue'>G-test disabled for groups with less than 3 samples</p>" else NULL
  # content_onesample <- if(cond_onesample) "style = 'color:deepskyblue'>Input at least 2 samples per group for group comparison." else NULL
  
  if (isTRUE(input$pres_fn == "nsamps")){
    if(cond_smallgrp){
      choices = c("Select one" = "select_none", "Presence/absence thresholds" = "uniqueness_nsamps")
    }
    else choices = c("Select one" = "select_none", "G test" = "uniqueness_gtest", "Presence/absence thresholds" = "uniqueness_nsamps")
    updateNumericInput(session, "thresh", min = 1, max = min(length(g1_samples()), length(g2_samples())))
  }
  else if (isTRUE(input$pres_fn == "prop")){
    if(cond_smallgrp){
      choices = c("Select one" = "select_none", "Presence/absence thresholds" = "uniqueness_prop")
    }
    else choices = c("Select one" = "select_none", "G test" = "uniqueness_gtest", "Presence/absence thresholds" = "uniqueness_prop")
    updateNumericInput(session, "thresh", min = 0, max = 1)
  }
  else choices = NULL
  
  selected = ifelse(input$summary_fxn %in% c("uniqueness_nsamps", "uniqueness_prop"), choices["Presence/absence thresholds"], input$summary_fxn)
  updateSelectInput(session, "summary_fxn", choices = choices, selected = selected)
  revals$warningmessage_visualize$small_groups <-  content 
  # revals$warningmessage_visualize$one_sample <- content_onesample 
})

# Control state for presence/absence threshold and p-value inputs
observeEvent(input$summary_fxn,{
  req(input$chooseplots != "Density Plot")
  toggleState("pval", input$summary_fxn == "uniqueness_gtest")
  toggleCssClass("js_pval", "grey_out", condition = input$summary_fxn != "uniqueness_gtest")
  toggleState("absn_thresh", input$summary_fxn != "uniqueness_gtest")
  toggleCssClass("js_absn_thresh", "grey_out", condition = input$summary_fxn == "uniqueness_gtest")
})

# Control coloring choices depending on vk bounds selection
observeEvent(input$vkbounds, {
  req(isTRUE(input$choose_single == 1) & isTRUE(input$chooseplots == "Van Krevelen Plot"))

  if(input$vkbounds == 0){
    selected <- if(input$vk_colors %in% c("bs1", "bs2")) NULL else input$vk_colors
    updateSelectInput(session, "vk_colors", 
                      choices = c('Van Krevelen Boundary Set 1' = 'bs1','Van Krevelen Boundary Set 2' = 'bs2', revals$color_by_choices[!(revals$color_by_choices %in% c("bs1", "bs2"))]),
                      selected = input$vk_colors)
  }
  else if(input$vkbounds == 'bs1'){
    selected <- if(input$vk_colors == "bs2") NULL else input$vk_colors
    updateSelectInput(session, "vk_colors", 
                      choices = c('Van Krevelen Boundary Set' = 'bs1', revals$color_by_choices[!(revals$color_by_choices %in% c("bs1", "bs2"))]),
                      selected = selected)
  }
  else if(input$vkbounds == "bs2"){
    selected <- if(input$vk_colors == "bs1") NULL else input$vk_colors
    updateSelectInput(session, "vk_colors", 
                      choices = c('Van Krevelen Boundary Set' = 'bs2', revals$color_by_choices[!(revals$color_by_choices %in% c("bs1", "bs2"))]),
                      selected = selected)
  }
})

# Observer which stores sample selections so user (me testing the app) doesn't have to re-input
observeEvent(c(input$whichGroups1, input$whichGroups2, input$whichSamples, input$whichSample1, input$whichSample2),{
  revals$group_1 <- input$whichGroups1
  revals$group_2 <- input$whichGroups2
  if(isTRUE(input$choose_single == 1)) revals$single_sample <- input$whichSamples
  if(isTRUE(input$choose_single == 2)) revals$single_group <- input$whichSamples
  if(isTRUE(input$choose_single == 4)){
  revals$sample_1 <- input$whichSample1
  revals$sample_2 <- input$whichSample2
  }
})
####                                 ###


