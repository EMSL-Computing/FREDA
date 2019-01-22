# creates three observers, one for each custom filter column selection dropdown
lapply(1:3, function(i){
  
  # store input name
  el <- paste0("custom", i)
  
  # create a single observer
  observeEvent(input[[el]], {
    
    revals$redraw_filter_plot <- FALSE
    
    # observer first creates the range/category dropdowns
    output[[paste0("customfilter", i, "UI")]] <- renderUI({
            
            # check that something is selected and the user has selected custom filters
            if (isTRUE(isolate(input[[el]]) != "Select item") & input$customfilterz == TRUE){
  
              # if theres no selection, we start with false, else we keep the current value of the NA checkbox
              ischecked = ifelse(is.null(isolate(input[[paste0("na_custom",i)]])), FALSE, isolate(input[[paste0("na_custom",i)]]))
  
              # if the filter applies to numeric data, allow inputs for min, max, and keep NA checkbox
              if (is.numeric(uploaded_data()$e_meta[, isolate(input[[el]])])) {
  
                # if no min or max specificed, automatically fill in max, otherwise use value currently specified
                min = min(uploaded_data()$e_meta[, isolate(input[[el]])], na.rm = TRUE)
                max = max(uploaded_data()$e_meta[, isolate(input[[el]])], na.rm = TRUE)
                
                # if user has defined min/max, use their values, otherwise default to data min/max
                isolate({
                  cond_min = ifelse(is.null(input[[paste0("minimum_custom",i)]]), min, input[[paste0("minimum_custom",i)]])
                  cond_max = ifelse(is.null(input[[paste0("maximum_custom",i)]]), max, input[[paste0("maximum_custom",i)]])
                })
                
                # create range and keepNA? inputs
                splitLayout(id = paste0("js_range_custom", i), style = "position:relative;top:-15px;padding-left:5px", class = "squeezesplitlayout", cellWidths = c("40%", "40%", "20%"),
                            numericInput(inputId = paste0("minimum_custom",i), label = "Min", value = min, min = min, max = cond_max, step = 0.01),
                            numericInput(inputId = paste0("maximum_custom",i), label = "Max", value = max, min = cond_min, max = max, step = 0.01),
                            tagList(
                              br(),
                              checkboxInput(inputId = paste0("na_custom",i), label = "Keep NAs?", value = ischecked)
                            )
                )
                
              # if the filter applies to categorical data, populate a box of options and keep NA checkbox
              } else if (!is.numeric(uploaded_data()$e_meta[, isolate(input[[el]])]) & input$customfilterz == TRUE) {
  
                  cats <- unique(uploaded_data()$e_meta[, isolate(input[[el]])]) %>% setdiff(NA)
                  
                  splitLayout(id = paste0("js_range_custom", i),
                              selectInput(inputId = paste0("categorical_custom",i), label = "Categories to Keep", choices = cats,
                                          multiple = TRUE, selected = cats),
                              tagList(
                                br(),
                                checkboxInput(inputId = paste0("na_custom",i), label = "Keep NAs?", value = ischecked)
                              )
                  )
                }
            }
            # If we initialized with select item, return an invisible panel with a stored value
            else if (isTRUE(isolate(input[[el]]) == "Select item") & input$customfilterz == TRUE){
              conditionalPanel("false", {
                tagList(
                  numericInput(inputId = paste0("minimum_custom",i), label = "Min", value = NULL, min = 0, max = 1, step = 0.01),
                  numericInput(inputId = paste0("maximum_custom",i), label = "Max", value = NULL, min = 0, max = 1, step = 0.01),
                  checkboxInput(inputId = paste0("na_custom",i), label = "Keep NAs?", value = NULL),
                  selectInput(inputId = paste0("categorical_custom",i), label = "Categories to Keep", choices = NULL, selected = NULL)
                )
              })
              
            }
          })
    
    # assign null to revals if we went back to no selection
    if(is.null(input[[el]]) | input[[el]] == "Select item"){
      revals[[paste0("custom", i, "_ids")]] <- NULL
    }
    
  }, priority = 10)
})

# creates dropdowns when custom filter checkbox is clicked
observeEvent(input$customfilterz, {
  charlist <- list("first", "second", "third")
  
  # if checked on:  create three column selector dropdowns for custom filters
  if(input$customfilterz){
    lapply(1:3, function(i){
      output[[paste0("filter",i,"UI")]] <- renderUI({
        selectInput(paste0("custom",i), label = tags$b(paste0("Select ",charlist[[i]]," filter item")),
                    choices = c("Select item", isolate(emeta_display_choices())))
      })
    })
  }
  # if checked off, NULLify output objects and reactive ids
  else{
    lapply(1:3, function(i){
      revals[[paste0("custom", i, "_ids")]] <- NULL
      output[[paste0("filter",i,"UI")]] <- NULL
    })
    
  }
})

# create observers on each custom filter.  Each observer reacts to changes in the numeric range or categories and stores a table of retained ids in a reactive value
# these reactive values are what will invalidate the preview table (summaryFilterDataFrame())
lapply(1:3, function(i){
  
  # variable which stores the input id of a particular dropdow
  el = paste0("custom", i)
  
  observeEvent(c(input[[paste0("minimum_custom", i)]], input[[paste0("maximum_custom", i)]], 
                 input[[paste0("categorical_custom", i)]], input[[paste0("na_custom", i)]]), {
                  req(input[[el]] != "Select item")
                   
                   revals$redraw_filter_plot <- FALSE
                   
                   # require that the input has a real selection
                   if(input$customfilterz == TRUE){
                     
                   req(all(!is.null(input[[paste0("minimum_custom", i)]]), !is.null(input[[paste0("maximum_custom", i)]])) | !is.null(input[[paste0("categorical_custom", i)]]))
            
                   # if the filter applies to numeric data, allow inputs for min, max, and keep NA checkbox
                     if (is.numeric(uploaded_data()$e_meta[, input[[el]]])) {
                       
                       revals[[paste0("custom", i, "_ids")]] <- emeta_filter(uploaded_data(), input[[el]]) %>% 
                         # sorry about this filter statement, i swear dplyr is cool
                         filter((emeta_value >= input[[paste0("minimum_custom", i)]] & emeta_value <= input[[paste0("maximum_custom", i)]]) | (is.na(emeta_value) & input[[paste0("na_custom", i)]])) %>%
                         pluck(1) %>% as.character()
                       
                       # if the filter applies to categorical data, populate a box of options and keep NA checkbox
                     } 
                     
                     else if (!is.numeric(uploaded_data()$e_meta[, input[[el]]]) & input$customfilterz == TRUE) {
                       
                       revals[[paste0("custom", i, "_ids")]] <- emeta_filter(uploaded_data(), input[[el]]) %>% 
                         filter(emeta_value %in% input[[paste0("categorical_custom", i)]] | (is.na(emeta_value) & input[[paste0("na_custom", i)]])) %>%
                         pluck(1) %>% as.character()
                     }
                   }
                   else revals[[paste0("custom", i, "_ids")]] <- NULL
                   
                 })
  
})

# delay behavior for when users changed input ranges / categories.  prevent redrawing multiple times
observe({
  c(sampfilter_ids(), massfilter_ids(), molfilter_ids(), formfilter_ids(), revals$custom1_ids, revals$custom2_ids, revals$custom3_ids)
 
  if(isolate(revals$redraw_filter_plot == FALSE)){
    invalidateLater(1000, session)
    isolate(revals$redraw_filter_plot <- TRUE)
  }
  else{
    isolate(revals$reac_filter_plot <- !revals$reac_filter_plot)
  }
  
})

# Three observers maintain mutual exclusivity of custom filter column choices (can be redone in lapply loop)
observeEvent(c(input$custom2, input$custom3), {
  updateSelectInput(session, "custom1", 
                    choices = c("Select item", setdiff(emeta_display_choices(), c(input$custom2, input$custom3))), 
                    selected = input$custom1)
})

observeEvent(c(input$custom1, input$custom3), {
  updateSelectInput(session, "custom2", 
                    choices = c("Select item", setdiff(emeta_display_choices(), c(input$custom1, input$custom3))), 
                    selected = input$custom2)
})

observeEvent(c(input$custom1, input$custom2), {
  updateSelectInput(session, "custom3", 
                    choices = c("Select item", setdiff(emeta_display_choices(), c(input$custom1, input$custom2))), 
                    selected = input$custom3)
})

# observers on numeric values which raise warnings for invalid entries and disable filter button
lapply(1:3, function(i){
  min = paste0("minimum_custom", i)
  max = paste0("maximum_custom", i)
  col = paste0("custom", i)
  
  observeEvent(c(input[[min]],input[[max]]), {
    req(input[[col]] != "Select item")
    min = min(uploaded_data()$e_meta[, input[[col]]], na.rm = TRUE)
    max = max(uploaded_data()$e_meta[, input[[col]]], na.rm = TRUE)
    
    isolate({
      cond_min = ifelse(is.null(input[[paste0("minimum_custom",i)]]), min, input[[paste0("minimum_custom",i)]])
      cond_max = ifelse(is.null(input[[paste0("maximum_custom",i)]]), max, input[[paste0("maximum_custom",i)]])
    })
    
    # specifies bad ranges
    condition = cond_min >= cond_max
    
    # put up warning and disable filter button
    toggleCssClass(paste0("js_range_custom", i), "attention", condition = condition)
    toggleState(paste0("filter_click"), condition = !condition)
    
  })
  
})


#-------- Reset Activity -------#
# Allow a 'reset' that restores the uploaded object and unchecks the filter
# boxes.  Will display a popup that warns the user of plot erasure and gives 
# the option to reset or to go back without clearing filters.
observeEvent(input$reset_filters,{
  showModal(modalDialog(
    
    ##### There is probably a better way to code the display behavior of this dialog -DC
    
    fluidPage(
      fluidRow(
        column(10, align = "center", offset = 1,
               tags$p("Caution:  If you reset filters, plots you have currently stored for download will be ERASED.  If you want to keep the plots you have already created, download them now.  A summary of the data with the current filter settings will be included in your download.", style = "color:red;font:bold", align = "center"),
               actionButton("clear_filters_yes", "Yes, clear filters without saving plots.", width = '100%'),
               br(),
               br(),
               br(),
               actionButton("clear_filters_no", "No, take me back.", width = '100%')
        ))),
    footer = NULL
  )
  )
})

# if they click no, just exit the modal dialog
observeEvent(input$clear_filters_no,{
  removeModal()
})

# if they click yes, reset data and exit modal dialog
observeEvent(input$clear_filters_yes, {
  if (f$clearFilters) {
    updateCheckboxInput(session = session, inputId = "massfilter", value = FALSE)
    updateCheckboxInput(session = session, inputId = "molfilter", value = FALSE)
    updateCheckboxInput(session = session, inputId = "formfilter", value = FALSE)
    updateCheckboxInput(session = session, inputId = "customfilterz", value = FALSE)
  }
  
  # reset parameter table, plot_list, and group plot data.  revert to pre-filtered data
  parmTable$parms <- data.frame("File Name" = NA, "Plot Type" = NA, "Sample Type" = NA, "Group 1 Samples" = NA, "Group 2 Samples" = NA, "Boundary Set" = NA,
                                "Color By Variable" = NA, "X Variable" = NA, "Y Variable" = NA, "Presence Threshold" = NA, "Absence Threshold" = NA, "P-Value" = NA,
                                "Comparisons Method" = NA, check.names = FALSE)
  revals$plot_list <- list()
  revals$plot_data <- list()
  peakIcr2 <<- uploaded_data()
  #
  
  # counter to control plot storage indices in case of reset
  revals$reset_counter <- input$add_plot
  
  removeModal()
})

# Dismiss success message or move to next page?
observeEvent(input$filter_dismiss,{removeModal()})
observeEvent(input$goto_viz,{
  updateTabsetPanel(session, "top_page", selected = "Visualize")
  removeModal()
})







