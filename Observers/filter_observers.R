# Event: Create filtered nonreactive revals$peakData2 when action button clicked
# Depends on action button 'filter_click'
observeEvent(input$filter_click, {
  shinyjs::show('calc_filter', anim = T)
  shinyjs::disable('filter_click')
  on.exit({ 
    shinyjs::enable('filter_click')
    shinyjs::hide('calc_filter', anim = T)
  })
  
  # if the data is already filtered start over from the uploaded data
  if (any(c("moleculeFilt", "massFilt", "formulaFilt") %in% names(attributes(revals$peakData2)$filters)) | 
      any(grepl("emetaFilt", names(attributes(revals$peakData2)$filters))) | 
      !all(colnames(revals$peakData2$e_data) %in% colnames(revals$uploaded_data$e_data))){
    revals$peakData2 <- revals$uploaded_data
  }
  
  n_filters = sum(sapply(list(input$massfilter, input$molfilter, input$samplefilter, input$formfilter, input$custom1, input$custom2, input$custom3), isTRUE))
  
  tryCatch({
    revals$warningmessage_filter$apply_fail <- NULL
    withProgress(message = "Applying filters....",{
      # Apply sample filter
      if(input$samplefilter){
        req(length(input$keep_samples) > 0)
        revals$peakData2 <- subset(revals$peakData2, samples = input$keep_samples, check_rows = TRUE)
        revals$removed_samples <- c(revals$removed_samples, setdiff(sample_names(), input$keep_samples))
        
        # remove empty lists
        if(length(revals$groups_list) > 0){
          
          # get indices of now empty groups
          inds <- sapply(revals$groups_list, function(el){
            length(intersect(el, input$keep_samples)) == 0
          })
          
          revals$groups_list[inds] <- NULL
        }
        incProgress(1/n_filters, detail = 'Sample filter done.')
      }else revals$removed_samples <- list()
      
      # Apply mass filter
      if (input$massfilter){
        
        # Error handling: Min mass less than max mass, but greater than 0
        req(input$min_mass < input$max_mass)
        req(input$min_mass > 0)
        
        # Create and apply mass filter to nonreactive peakData object
        filterMass <- mass_filter(revals$peakData2)
        revals$peakData2 <- applyFilt(filterMass, revals$peakData2, min_mass = as.numeric(input$min_mass), 
                                      max_mass = as.numeric(input$max_mass))
        rm(filterMass)
        incProgress(1/n_filters, detail = 'Mass filter done.')
      }
      
      # Apply molecule filter
      if (input$molfilter) {
        
        # Create and apply molecule filter to nonreactive peakData object
        filterMols <- molecule_filter(revals$peakData2)
        revals$peakData2 <- applyFilt(filterMols, revals$peakData2, min_num = as.integer(input$minobs))
        rm(filterMols)
        incProgress(1/n_filters, detail = 'Molecule filter done.')
      } # End molecule filter if statement
      
      # Apply formula filter
      if (input$formfilter){
        filterForm <- formula_filter(revals$peakData2)
        revals$peakData2 <- applyFilt(filterForm, revals$peakData2)
        rm(filterForm)
        incProgress(1/n_filters, detail = 'Formula filter done.')
      }
      
      # Apply custom filters
      if (input$customfilterz){
        
        #apply the filter for each input
        for(i in 1:3){
          
          #require that a selection has been made for filter i
          if (input[[paste0("custom",i)]] == "Select item") return(NULL)
          
          #make the filter based on selection
          filter <- emeta_filter(revals$peakData2, input[[paste0("custom",i)]])
          
          # if numeric, apply filter with specified max and min values
          if (is.numeric(revals$peakData2$e_meta[,input[[paste0("custom",i)]]])){
            req(input[[paste0("minimum_custom",i)]], input[[paste0("maximum_custom", i)]])
            revals$peakData2 <- applyFilt(filter, revals$peakData2,
                                          min_val = input[[paste0("minimum_custom",i)]], 
                                          max_val = input[[paste0("maximum_custom", i)]], 
                                          na.rm = !input[[paste0("na_custom",i)]])
            
          }
          # else apply with selected categories
          else if (!is.numeric(revals$peakData2$e_meta[,input[[paste0("custom",i)]]])){
            req(input[[paste0("categorical_custom",i)]])
            revals$peakData2 <- applyFilt(filter, revals$peakData2, 
                                          cats = input[[paste0("categorical_custom",i)]], 
                                          na.rm = !input[[paste0("na_custom",i)]])
          }
          
          rm(filter)
          incProgress(1/n_filters, detail = sprintf('Custom filter %s done', i))
        }
        
      }
    })
  },error = function(e){
    filt_msg = paste0('Something went wrong applying your filters \n System error:  ', e)
    revals$warningmessage_filter$apply_fail <- sprintf("<p style = 'color:red'>%s</p>", filt_msg)
  })
  
  if(!exists('filt_msg')){
    # display success modal
    showModal(
      modalDialog(title = "Filter Success",
                  fluidRow(
                    column(10, align = "center", offset = 1,
                           HTML('<h4 style= "color:#1A5276">Your data has been filtered.</h4>
                                <h4 style= "color:#1A5276">The filtered data is stored and will be reset if you re-upload or re-process data.</h4>'),
                           hr(),
                           actionButton("filter_dismiss", "Review Results", width = '75%'),
                           br(),
                           br(),
                           actionButton("goto_viz", "Continue to Visualization", width = '75%')
                           )
                  )
                  ,footer = NULL)
    )
  }
  
  #__test-export__
  exportTestValues(peakData2 = revals$peakData2)
  
}) # End creating revals$peakData2

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
              if (is.numeric(revals$uploaded_data$e_meta[, isolate(input[[el]])])) {
  
                # if no min or max specificed, automatically fill in max, otherwise use value currently specified
                min = min(revals$uploaded_data$e_meta[, isolate(input[[el]])], na.rm = TRUE)
                max = max(revals$uploaded_data$e_meta[, isolate(input[[el]])], na.rm = TRUE)
                
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
                              div(class = 'adjustdown', style = 'float:right;margin-right:10%', checkboxInput(inputId = paste0("na_custom",i), label = "Keep NAs?", value = ischecked))
                            )
                )
                
              # if the filter applies to categorical data, populate a box of options and keep NA checkbox
              } else if (!is.numeric(revals$uploaded_data$e_meta[, isolate(input[[el]])]) & input$customfilterz == TRUE) {
  
                  cats <- unique(revals$uploaded_data$e_meta[, isolate(input[[el]])]) %>% setdiff(NA)
                  
                  splitLayout(id = paste0("js_range_custom", i), cellArgs = list(style = "overflow:visible"),

                              pickerInput(inputId = paste0("categorical_custom",i), label = "Categories to Keep", choices = cats,
                                          multiple = TRUE, selected = cats, options =  pickerOptions(dropupAuto = FALSE, actionsBox = TRUE)),
                              tagList(
                                br(),
                                div(class = 'adjustdown', style = 'float:right;margin-right:10%', checkboxInput(inputId = paste0("na_custom",i), label = "Keep NAs?", value = ischecked))
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
                  pickerInput(inputId = paste0("categorical_custom",i), label = "Categories to Keep", choices = NULL, selected = NULL)
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
      revals$filter_click_disable[[paste0("disable_custom", i)]] <- TRUE
    })
    
  }
})

# create observers on each custom filter.  Each observer reacts to changes in the numeric range or categories and stores a table of retained ids in a reactive value
# these reactive values are what will invalidate the preview table (summaryFilterDataFrame())
lapply(1:3, function(i){
  
  # variable which stores the input id of a particular dropdow
  el = paste0("custom", i)
  
  observeEvent(c(input[[paste0("minimum_custom", i)]], input[[paste0("maximum_custom", i)]], 
                 input[[paste0("categorical_custom", i)]], input[[paste0("na_custom", i)]],
                 input$top_page, revals$redraw_largedata), {
                   req(input[[el]] != "Select item" & !is.null(input[[el]]))
                   
                   revals$redraw_filter_plot <- FALSE
                   
                   if(!revals$redraw_largedata){
                     revals[[paste0("custom", i, "_ids")]] <- NULL
                   }
                   # require that the input has a real selection
                   if(input$customfilterz == TRUE){
                     
                   req(all(!is.null(input[[paste0("minimum_custom", i)]]), !is.null(input[[paste0("maximum_custom", i)]])) | !is.null(input[[paste0("categorical_custom", i)]]))
            
                   # if the filter applies to numeric data, allow inputs for min, max, and keep NA checkbox
                     if (is.numeric(revals$uploaded_data$e_meta[, input[[el]]])) {
                       
                       revals[[paste0("custom", i, "_ids")]] <- emeta_filter(revals$uploaded_data, input[[el]]) %>% 
                         # sorry about this filter statement, i swear dplyr is cool
                         filter((emeta_value >= input[[paste0("minimum_custom", i)]] & emeta_value <= input[[paste0("maximum_custom", i)]]) | (is.na(emeta_value) & input[[paste0("na_custom", i)]])) %>%
                         pluck(1) %>% as.character()
                       
                       # if the filter applies to categorical data, populate a box of options and keep NA checkbox
                     } 
                     
                     else if (!is.numeric(revals$uploaded_data$e_meta[, input[[el]]]) & input$customfilterz == TRUE) {
                       
                       revals[[paste0("custom", i, "_ids")]] <- emeta_filter(revals$uploaded_data, input[[el]]) %>% 
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
    invalidateLater(800, session)
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
  
  observeEvent(c(input[[min]],input[[max]], input$customfilterz), {
    if(input$customfilterz & isTRUE(input[[col]] != "Select item")){
      min = min(revals$uploaded_data$e_meta[, input[[col]]], na.rm = TRUE)
      max = max(revals$uploaded_data$e_meta[, input[[col]]], na.rm = TRUE)
      
      isolate({
        cond_min = ifelse(is.null(input[[paste0("minimum_custom",i)]]), min, input[[paste0("minimum_custom",i)]])
        cond_max = ifelse(is.null(input[[paste0("maximum_custom",i)]]), max, input[[paste0("maximum_custom",i)]])
      })
      
      # specifies bad ranges
      condition = isTRUE(cond_min >= cond_max)
      
      # put up warning and disable filter button
      toggleCssClass(paste0("js_range_custom", i), "attention", condition = condition)
      revals$filter_click_disable[[paste0("disable_custom", i)]] <- if(condition) FALSE else TRUE
      revals$warningmessage_filter$bad_custom_range = if(isTRUE(condition)) "style = 'color:red'>Invalid range for a custom filter" else NULL
    }
    else {
      revals$warningmessage_filter$bad_custom_range = NULL
    }
  })
  
})

# warn and disable filter for invalid mass range
observeEvent(c(input$min_mass, input$max_mass, input$massfilter), {
    cond = (isTRUE(input$min_mass >= input$max_mass) | input$min_mass == 0) & input$massfilter
    toggleCssClass("min_mass", "attention", condition = !cond)
    revals$filter_click_disable$cond_mass <- if(isTRUE(cond)) FALSE else TRUE
    revals$warningmessage_filter$bad_mass_range = if(isTRUE(cond)) "style = 'color:red'>Invalid range for mass filter.  Must be 0 < min < max" else NULL
})

# check that at least one sample is selected
observeEvent(c(input$keep_samples, input$samplefilter),{
  cond = length(input$keep_samples) == 0 & input$samplefilter
  toggleCssClass("js_filter_samples", "attention", condition = cond)
  revals$warningmessage_filter$nosamples <- if(isTRUE(cond)) "style = 'color:red'>Choose at least one sample to keep" else NULL
  revals$filter_click_disable$cond_samples <- if(isTRUE(cond)) FALSE else TRUE
}, ignoreNULL = F)

# check that not all rows are filtered by checking the last non-NA row of summaryFilterDataFrame()
observeEvent(summaryFilterDataFrame(),{
  filter_inds <- c(TRUE, isolate(input$samplefilter) & length(isolate(input$keep_samples)) > 0, isolate(input$massfilter), isolate(input$molfilter), isolate(input$formfilter), 
                   any(c(isolate(input$custom1), isolate(input$custom2), isolate(input$custom3)) != "Select item") & isolate(input$customfilterz))
  final_peaks <- summaryFilterDataFrame()[max(which(filter_inds)), 'sum_peaks']
  cond = isTRUE(final_peaks > 0)
  revals$filter_click_disable$cond_peaks <- if(!cond) FALSE else TRUE
  revals$warningmessage_filter$nopeaks <- if(!cond) "style = 'color:red'>Your filters removed all rows or have value errors, adjust some values and try again" else NULL
})

# all conditions must be met to click filter
observe({
  cond = all(unlist(revals$filter_click_disable))
  toggleState("filter_click", condition = cond)
})

# ----- Filter Reset Setup -----# 
# f$clearFilters simply allows/denies the destruction of filters and plots, and the rest of data to pre-filtered state.
f <- reactiveValues(clearFilters = FALSE)
observeEvent(input$clear_filters_yes, {
  f$clearFilters <- TRUE
}, priority = 10)

observeEvent(input$filter_click, {
  f$clearFilters <- FALSE
}, priority = 10)

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
               actionButton("clear_filters_no", "No, take me back.", width = '100%')
        ))),
    footer = NULL
  )
  )
})

observeEvent(c(input$samplefilter, input$keep_samples),{
  # a list which contains vectors of logical indicating whether that sample will be kept or not
  samples_tf <- lapply(revals$groups_list, function(el){ el %in% input$keep_samples })
  
  # there is at least one group with samples that will be dropped, and the checkbox is clicked
  cond <- length(which(!sapply(samples_tf, all))) > 0 & input$samplefilter
  
  if(cond){
    warn_string <- "<p style=color:deepskyblue>The following groups will have some of their samples removed:</p>"
    
    # for every list element, if not all are kept, append a warning
    for(i in 1:length(samples_tf)){
      if(!all(samples_tf[[i]])){
        rmv_warning <- if(sum(samples_tf[[i]]) == 0) "<span style = color:red>This group will be removed</span>" else ""
        warn_string <- paste0(warn_string, 
                              sprintf("<p style = color:deepskyblue>%s | Samples remaining: %i.  %s</p>", names(samples_tf)[i], sum(samples_tf[[i]]), rmv_warning))
      }
    }
  }
  
  revals$warningmessage_filter$removed_groups <- if(cond) warn_string else NULL
})

# if they click no, just exit the modal dialog
observeEvent(input$clear_filters_no,{
  removeModal()
})

# if they click yes, reset data and exit modal dialog
observeEvent(input$clear_filters_yes, {
  if (f$clearFilters) {
    updateCheckboxInput(session, inputId = "massfilter", value = FALSE)
    updateCheckboxInput(session, inputId = "molfilter", value = FALSE)
    updateCheckboxInput(session, inputId = "formfilter", value = FALSE)
    updateCheckboxInput(session, inputId = "customfilterz", value = FALSE)
    updateCheckboxInput(session, inputId = "samplefilter", value = FALSE)
  }
  
  # reset parameter table, plot_list, and group plot data.  revert to pre-filtered data
  plots$plot_table <- data.frame("File Name" = character(0), 'Download?' = character(0), "Plot Type" = character(0), "Sample Type" = character(0), "Group 1 Samples" = character(0), 
                                 "Group 2 Samples" = character(0), "Boundary Set" = character(0), "Color By Variable" = character(0), "X Variable" = character(0), 
                                 "Y Variable" = character(0), "Presence Threshold" = character(0), "Absence Threshold" = character(0), "P-Value" = character(0),
                                 "Comparisons Method" = character(0), check.names = FALSE, stringsAsFactors = FALSE)
  plots$plot_list <- list()
  plots$plot_data <- list()
  revals$peakData2 <- revals$uploaded_data
  #
  
  # counter to control plot storage indices in case of reset
  revals$reset_counter <- input$saveplot
  
  # reset 'removed samples' reval
  revals$removed_samples <- list()
  
  removeModal()
})

# Dismiss success message, draw large data plots, or move to next page?
observeEvent(input$filter_dismiss,{
  removeModal()
  if(uploaded_data_dim() > max_cells){
    shinyjs::show('draw_large_filter_plot')
    revals$redraw_largedata <- TRUE
    revals$react_largedata <- !revals$react_largedata
  }
})

observeEvent(input$goto_viz,{
  updateTabsetPanel(session, "top_page", selected = "Visualize")
  removeModal()
})
