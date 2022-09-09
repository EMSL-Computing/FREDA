#' @details Parse and store header parameters.  If 'corems-prefix' is passed, load
#' the files and display a different tab for 'upload'.
observe({
  query <- parseQueryString(session$clientData$url_search)

  # establish minio connection if we are pulling cloud resources
  if (any(names(query) %in% VALID_MINIO_HEADER_PARAMS)) {
    cfg_location = if (Sys.getenv("MINIO_CONFIG_PATH") == "") "./cfg/minio_config.yml" else Sys.getenv("MINIO_CONFIG_PATH")
    minio_con <<- mapDataAccess::map_data_connection(cfg_location)
  }

  on.exit({
    Sys.sleep(1)
    hide("loading-gray-overlay")
  })

  isolate({
    # store header params in a reactive variable
    for (key in names(query)) {
      header_params[[key]] <- query[[key]]
      message(sprintf("INFO: stored parameter %s: %s", key, query[[key]]))
    }

    if ('corems-prefix' %in% names(query)) {
      html(selector = "#loading-gray-overlay > div", html = "Loading Core-MS data...")

      uris <- reticulate::iterate(
        minio_con$client$list_objects(
          minio_con$bucket,
          prefix = header_params[['corems-prefix']],
          recursive = TRUE),
        function(x) x$object_name
      )

      if (length(uris) > 0) {
        tryCatch({
          fpaths <- lapply(uris, function(uri) {
            mapDataAccess::get_file(
              minio_con, id = uri, filename = file.path(tempfile(), basename(uri)),
              use_dir = FALSE
            )
          })

          names(fpaths) <- sapply(fpaths, function(x) basename(tools::file_path_sans_ext(x))) %>%
            make.unique()

          corems_revals[['combined_tables']] <- ftmsRanalysis::read_CoreMS_data(
            unlist(fpaths),
            sample_names = names(fpaths)
          )

          for (name in names(fpaths)) {
            corems_revals[['tables']][[name]] <- read_csv(fpaths[[name]])
            corems_revals[['fpaths']][[name]] <- fpaths[[name]]
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

      # defined in srv_ui_elements/corems_UI.R
      showModal(corems_upload_modal(modalmessage))
    }

    insertTab(
      "top_page",
      target = "Welcome",
      tab = upload_tab(length(corems_revals[['combined_tables']]) > 0),
      position = "after"
    )
  })
})
