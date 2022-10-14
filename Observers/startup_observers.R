#'@details Parse and store header parameters.  If 'corems-prefix' is passed, load 
#'the files and display a different tab for 'upload'.
observe({
  query <- parseQueryString(session$clientData$url_search)
  
  # establish minio connection if we are pulling cloud resources
  if(any(names(query) %in% VALID_MINIO_HEADER_PARAMS)) {
    library(mapDataAccess)
    minio_con <<- mapDataAccess::map_data_connection("./cfg/minio_config.yml")
  }
  
  isolate({
    # store header params in a reactive variable
    for(key in names(query)){
      header_params[[key]] <- query[[key]]
      message(sprintf("INFO: stored parameter %s: %s", key, query[[key]]))
    }
    
    if('corems-prefix' %in% names(query)) {
      withProgress(message = "Loading core-ms files...", value = 1, {
        uris <- reticulate::iterate(
          minio_con$client$list_objects(
            minio_con$bucket,
            prefix = header_params[['corems-prefix']], 
            recursive = TRUE),
          function(x) x$object_name
        )
        
        if(length(uris) > 0) {
          tryCatch({
            fpaths <- lapply(uris, function(uri) {
              mapDataAccess::get_file(
                minio_con, id = uri, filename = file.path(tempfile(), basename(uri)), 
                use_dir = FALSE
              )
            })
            
            names(fpaths) <- sapply(fpaths, basename)
            
            for(name in names(fpaths)) {
              corems_samples[[name]] <- read_csv(fpaths[[name]])
            }
            
            modalmessage <- div(class = "column-scroll-sm",
              HTML(info_text[["COREMS_UPLOAD_SUCCESS"]]), 
              HTML(paste(names(fpaths), collapse = "<br>"))
            )
          }, error = function(e) {
            modalmessage <<- div(sprintf(info_text[["COREMS_UPLOAD_ERROR"]], e))
          })
        } else {
          modalmessage <- div(info_text[["COREMS_UPLOAD_NOSAMPS"]])
        }
        
        showModal(modalDialog(modalmessage, title = "Core-MS Upload"))
        
      })
    }
    
    insertTab(
      "top_page",
      target = "Welcome",
      tab = upload_tab(length(names(corems_samples)) > 0),
      position = "after"  
    )
    
  })
})
