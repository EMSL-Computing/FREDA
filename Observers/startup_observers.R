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

          modalmessage <- store_corems(fpaths)

        }, error = function(e) {
          modalmessage <<- div(sprintf(info_text[["COREMS_UPLOAD_ERROR"]], e))
        })
      } else {
        modalmessage <- div(info_text[["COREMS_UPLOAD_NOSAMPS"]])
      }

      # defined in srv_ui_elements/corems_UI.R
      showModal(corems_upload_success_modal(modalmessage))
    }

    # if we're not coming from minio, ask whether they have multiple CoreMS files to upload.
    # if they don't automatically insert the Core
    if (length(corems_revals[['combined_tables']]) == 0) {
      showModal(upload_type_modal())
    } else {
      insertTab(
        "top_page",
        target = "Welcome",
        tab = upload_tab(from_corems = TRUE),
        position = "after"
      )
    }
  })
})

#' create the modal asking for what type of file input the user has.
upload_type_modal <- function() {
  modalDialog(
    title = "What type of data are you uploading?",
    tags$p("Users have the option of uploading a single aligned data file along with a molecular identification file as defined in the data requirements page, or multiple files representing unaligned samples from the output of CoreMS."),
    footer = tagList(
      actionButton("upload_type_modal_single", "Single aligned data file"),
      actionButton("upload_type_modal_multiple", "Multiple unaligned files (CoreMS output)")
    )
  )
}

#' @details Insert the manual CoreMS upload tab
observeEvent(input$upload_type_modal_multiple, {
  insertTab(
    "top_page",
    target = "Welcome",
    tab = upload_tab(from_corems = TRUE),
    position = "after"
  )
  removeModal()

  showModal(corems_manual_upload_modal())
})

#' @details load the corems files
observeEvent(input$corems_files, {
  fpaths <- input$corems_files$datapath
  fnames <- input$corems_files$name

  tryCatch({
    modalmessage <- store_corems(fpaths, fnames)
    removeModal()
  }, error = function(e) {
    modalmessage <<- div(sprintf(info_text[["COREMS_UPLOAD_ERROR"]], e))
  })

  showModal(corems_upload_success_modal(modalmessage))
})

#' @details Insert the original e_data/e_meta upload tab
observeEvent(input$upload_type_modal_single, {
  insertTab(
    "top_page",
    target = "Welcome",
    tab = upload_tab(),
    position = "after"
  )
  removeModal()
})