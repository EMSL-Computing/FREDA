#' @details core-ms files loaded through a header parameter that points to a
#' 'folder' in minio containing all files.
corems_samples <- reactiveValues()

observe({
  req(!is.null(header_params[['corems-uri']]))
  
  isolate({
    withProgress(message = "Loading core-ms files...", value = 1, {
      uris <- reticulate::iterate(
        minio_con$client$list_objects(
          minio_con$bucket,
          prefix = header_params[['corems-uri']], 
          recursive = TRUE),
       function(x) x$object_name
       )
      
      fpaths <- lapply(uris, function(uri) {
        mapDataAccess::get_file(
          minio_con, id = uri, filename = file.path(tempfile(), basename(uri))
        )
      })
      
      names(fpaths) <- sapply(fpaths, basename)
      
      for(name in names(fpaths)) {
        corems_samples[[name]] <- read_csv(fpaths[[name]])
      }
    })
  })
})