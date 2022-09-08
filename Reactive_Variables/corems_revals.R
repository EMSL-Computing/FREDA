#'@details The columns of the table resulting from reading in the multiple 
#'corems files
corems_cols <- reactive({
  req(corems_revals[['combined_tables']])
  colnames(corems_revals[['combined_tables']])
})

# Create CoreMSData object upon button click
cms_data <- eventReactive(input$make_cmsdata, {
  req(corems_revals[['combined_tables']])
  
  args = list(corems_revals[['combined_tables']])
  
  # Collect arguments specified by the user
  for(argname in COREMSDATA_ARGS) {
    if(isTRUE(input[[argname]] == NULLSELECT__) | !isTruthy(input[[argname]])) {
      args[[argname]] <-  NULL
    } else {
      args[[argname]] <-  input[[argname]]  
    }
  }
  
  
  cms_dat <- do.call(as.CoreMSData, args) 

  return(cms_dat)
})

#'@details create conf_filt object for confidence filtering panel
conf_filt_obj <- reactive({
  conf_filter(cms_data())
})

#' @details The filtered corems data.
cms_data_filtered <- eventReactive(input$apply_conf_filter, {
  applyFilt(conf_filt_obj(), cms_data(), min_conf = input$min_conf)
})

########## Unique MF Assignment Tab ##########
cms_dat_unq_mf <- eventReactive(input$unique_mf, {  
  req(input$unq_mf_method)
  if (input$unq_mf_method == "Confidence score") {method <- "confidence"}
  if (input$unq_mf_method == "Peak height") {method <- "peak_intensity"}
  
  unq_dat <- unique_mf_assignment(cms_data_filtered(), method)
  return(unq_dat)
})

#' @details Columns selected for creating the coreMS object.  Used to maintain
#' mutual exclusivity
selected_coremsData <- reactive({
  lapply(COREMSDATA_ARGS, function(x) input[[x]])
})

#'@details Remaining choices for as.coreMSdata dropdowns
coreMS_remaining_choices <- reactive({
  setdiff(corems_cols(), selected_coremsData())
})