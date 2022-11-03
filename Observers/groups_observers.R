# update groups_list on button click
observeEvent(input$add_group, {
  req(!is.null(input$group_name) & input$group_name != "")
  req(length(input$group_samples) > 0)
  req(!(input$group_name %in% names(revals$groups_list)))
  
  revals$groups_list[[input$group_name]] <- input$group_samples

  updateTextInput(session, "group_name", value = "")
  updatePickerInput(session, "group_samples", selected = setdiff(sample_names(), c(unlist(revals$groups_list), input$group_samples)))
  
  exportTestValues(groups_list = revals$groups_list)
})

# shinyjs observer which disables input if selection is not valid
observeEvent(c(input$group_name, input$group_samples),{
  cond_noname <- is.null(input$group_name) | input$group_name == ""
  cond_nosamples <- length(input$group_samples) == 0
  cond_invalid_name <- input$group_name %in% names(revals$groups_list)
  
  revals$warningmessage_groups$noname <- if(cond_noname) "<p style=color:deepskyblue>Please input a name for this group</p>" else NULL
  revals$warningmessage_groups$badname <- if(cond_invalid_name) "<p style=color:red>This group name has already been used</p>" else NULL
  revals$warningmessage_groups$nosamples <- if(cond_nosamples & !cond_noname) "<p style=color:deepskyblue>Please select at least one sample for this group</p>" else NULL
  toggleCssClass("js_group_name", "suggest", cond_noname)
  toggleCssClass("js_group_name", "attention", cond_invalid_name & !cond_noname)
  toggleCssClass("group_samples", "suggest", cond_nosamples)
  toggleState("add_group", condition = !any(cond_noname, cond_nosamples, cond_invalid_name))
  
})

# observer on sample seletion that warns of overlapping groups
observeEvent(c(input$group_name, input$group_samples, input$remove_group), {
  # empty group name condition
  cond_noname <- is.null(input$group_name) | input$group_name == ""
  
  # list of intersections across already stored groups
  xsamples <- lapply(revals$groups_list, function(x){
    intersect(x, input$group_samples)
  })
  
  # list indices that correspond to groups that have overlap
  xinds <- which(lapply(xsamples, length) > 0)
  
  if(length(xinds) > 0 & !cond_noname){
    warn_string <- "<p style=color:deepskyblue>The following groups already contain some of the samples selected:</p>"
    for(ind in xinds){
      warn_string <- paste0(warn_string, sprintf("<p style = color:deepskyblue>%s: %s</p>", names(revals$groups_list)[ind], paste(xsamples[[ind]], collapse = ", ")))
      
    }
    revals$warningmessage_groups$sample_intersect <- warn_string
  }
  else revals$warningmessage_groups$sample_intersect <- NULL
  
})

# toggle remove button
observeEvent(input$group_table_rows_selected,{
  toggleState("remove_group", length(input$group_table_rows_selected) > 0)
}, ignoreNULL = F)

# remove table and list entries of selected row on button click
observeEvent(input$remove_group,{
  req(input$group_table_rows_selected)
  ind <- input$group_table_rows_selected
  
  revals$groups_list[[ind]] <- NULL
}, priority = 10)

# continue to preprocess
observeEvent(input$goto_preprocess_main,{
  updateTabsetPanel(session, "top_page", selected = "Preprocess")
})

