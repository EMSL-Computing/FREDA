#'@details Convert the filtered corems data to peakData
#'cms_dat_unq_mf() is converted into peakData using CoreMSData_to_ftmsData and
#'the result is stored in revals$uploaded_data
observeEvent(
  c(
    input$corems_to_peakdata,
    input$corems_to_peakdata_modal
  ),{
    
  req(cms_dat_unq_mf())
  req(isTruthy(input$corems_to_peakdata > 0) | isTruthy(input$corems_to_peakdata_modal > 0))
  
  revals$uploaded_data <- revals$peakData2 <- NULL
  
  res <- tryCatch({
    ftmsRanalysis::CoreMSData_to_ftmsData(cms_dat_unq_mf())
  },
  error = function(e){
    msg = paste0('Error converting your coreMS data to peakData: \n System error: ', e)
    revals$warningmessage_corems$corems_to_peakdata <<- sprintf("<p style = 'color:red'>%s</p>", msg)
    revals$warningmessage_corems$corems_to_peakdata
  })
  
  if(inherits(res, "peakData")){
    # need a fake f_data column.
    if(ncol(res$f_data) == 1) {
      res$f_data[,2] <- NA
    }
    
    revals$uploaded_data <- res 
  } else {
    if(inherits(res, "character")) {
      msg = res  
    } else {
      msg = "Error converting your coreMS data to peakData"
    }
    
    showNotification(
      HTML(msg),
      duration = NULL,
      type = "error"
    )
  }
}, ignoreInit = T)

#'@details Show a modal for the completion of corems data
observeEvent(cms_data(), {
  req(cms_data(), input$top_page == "CoreMS-create")
  
  updateCollapse(session, id = "corems-upload-summary-collapse",
                 open = c("corems-upload-visualize"), close = c("corems-upload-table"))
  
  showModal(
    # defined in srv_ui_elements/corems_UI.R
    corems_obj_creation_modal()
  )
})

#'@details Go to the corems filter tab from the object creation success tab.
observeEvent(input$goto_corems_filter, {
  req(input$top_page == "CoreMS-create")
  updateTabsetPanel(inputId = "top_page", selected = "CoreMS-conf-filter")
  removeModal()
})

#'@details Move the user to the create CoreMSData tab after successful upload
observeEvent(input$goto_corems_creation, {
  updateTabsetPanel(inputId = "top_page", selected = "CoreMS-create")
  removeModal()
})

#'@details (CoreMS filter tab) Show summary plot and show progression modal
observeEvent(cms_data_filtered(), {
  req(cms_data_filtered())
  updateCollapse(session, id = "corems-filter-summary-collapse",
                 open = c("viz"))
  updateTabsetPanel(inputId = "corems-viz-tabset", selected = "filt_summary_plot")
  
  showModal(corems_filter_modal())
})

#'@details Move user to the formula assignment tab after successful filtering
observeEvent(input$goto_corems_formula, {
  updateTabsetPanel(inputId = "top_page", selected = "CoreMS-formula-assign")
  removeModal()
})

#'@details display table visualization panels and prompt user for next steps 
#'when unique molecular formula are assigned.
observeEvent(cms_dat_unq_mf(), {
  hide("corems_to_peakdata_toggle")
  req(cms_dat_unq_mf())
  updateCollapse(session, id = "corems-assign-formula", open = c("viz", "tables"))
  showModal(corems_unq_mf_modal())
  show("corems_to_peakdata_toggle")
})
