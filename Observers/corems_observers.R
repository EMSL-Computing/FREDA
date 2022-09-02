#'@details Convert the filtered corems data to peakData
#'cms_dat_unq_mf() is converted into peakData using CoreMSData_to_ftmsData and
#'the result is stored in revals$uploaded_data
observeEvent(input$corems_to_peakdata, {
  req(cms_dat_unq_mf())
  
  res <- tryCatch({
    ftmsRanalysis::CoreMSData_to_ftmsData(cms_dat_unq_mf())
  },
  error = function(e){
    msg = paste0('Error converting your coreMS data to peakData: \n System error: ', e)
    revals$warningmessage_corems$corems_to_peakdata <<- sprintf("<p style = 'color:red'>%s</p>", msg)
    NULL
  })
  
  if(!is.null(res)){
    # need a fake f_data column.
    if(ncol(res$f_data) == 1) {
      res$f_data[,2] <- NA
    }
    
    revals$uploaded_data <- res 
  }
})