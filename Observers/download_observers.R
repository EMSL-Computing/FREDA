observeEvent(revals$fs, {
  if(length(revals$fs) > 0){
    download_condition = sum(file.exists(revals$fs)) > 0
  }
  else download_condition = FALSE
    
  toggleState("download_processed_data", condition = download_condition)
  
}, ignoreNULL = FALSE)
