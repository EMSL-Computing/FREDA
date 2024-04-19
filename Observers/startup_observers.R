#' @details Parse and store header parameters.  If 'corems-prefix' is passed, load
#' the files and display a different tab for 'upload'.
observe({
  query <- parseQueryString(session$clientData$url_search)

  # establish minio connection if we are pulling cloud resources
  if (any(names(query) %in% VALID_MINIO_HEADER_PARAMS)) {
    isolate({
      # store header params in a reactive variable
      for (key in names(query)) {
        header_params[[key]] <- query[[key]]
        message(sprintf("INFO: stored parameter %s: %s", key, query[[key]]))
      }
      
      if ('corems-prefix' %in% names(query)) {
        # get the appropriate minio config for retrieving CoreMS files
        cfg_location = if (Sys.getenv("COREMS_MINIO_CONFIG_PATH") == "") "./cfg/minio_config_corems.yml" else Sys.getenv("COREMS_MINIO_CONFIG_PATH")
        cms_minio_con <<- mapDataAccess::map_data_connection(cfg_location)
        
        html(selector = "#loading-gray-overlay > div", html = "Loading Core-MS data...")
        
        uris <- reticulate::iterate(
          cms_minio_con$client$list_objects(
            cms_minio_con$bucket,
            prefix = header_params[['corems-prefix']],
            recursive = TRUE),
          function(x) x$object_name
        )
        
        if (length(uris) > 0) {
          corems_upload_success <- tryCatch({
            fpaths <- lapply(uris, function(uri) {
              mapDataAccess::get_file(
                cms_minio_con, id = uri, filename = file.path(tempfile(), basename(uri)),
                use_dir = FALSE
              )
            })
            
            modalmessage <- store_corems(fpaths)
            TRUE
            
          }, error = function(e) {
            modalmessage <<- div(sprintf(info_text[["COREMS_UPLOAD_ERROR"]], e))
            FALSE
          })
        } else {
          corems_upload_success <- FALSE
          modalmessage <- div(info_text[["COREMS_UPLOAD_NOSAMPS"]])
        }
        
        if (corems_upload_success) {
          # defined in srv_ui_elements/corems_UI.R
          showModal(corems_upload_success_modal(modalmessage)) 
        } else {
          showNotification(
            modalmessage,
            duration = NULL,
            type = "error"
          )
        }
      } else if ('map-object' %in% names(query)) {
        # get the appropriate minio config for retrieving CoreMS files
        cfg_location = if (Sys.getenv("MAP_MINIO_CONFIG_PATH") == "") "./cfg/minio_config_map.yml" else Sys.getenv("MAP_MINIO_CONFIG_PATH")
        map_minio_con <<- mapDataAccess::map_data_connection(cfg_location)
        
        html(selector = "#loading-gray-overlay > div", html = "Loading data from MAP...")
        
        ftms_obj <- tryCatch({
          mapDataAccess::get_data(map_minio_con, header_params[['map-object']])
        }, error = function(e) {
          NULL
        })
        
        valid_obj_types = c("project omic")
        
        if (!inherits(ftms_obj, valid_obj_types)) {
          showNotification(
            HTML(sprintf("Something went wrong retrieving your data from MAP.  Your MAP object was not of the appropriate type.  Please confirm that the object is of types:  [%s]", valid_obj_types)),
            duration = NULL,
            type = "error"
          )
        } else {
          # store in some reactive variable that e_data and e_meta....essentially check exists and use if so.
          revals$map_project <- ftms_obj 
        }
      }
    })
    
  } else {
    showNotification(
      sprintf("No valid header parameters found in query string.  Found parameters: %s.  Valid header parameters: %s", names(query), VALID_MINIO_HEADER_PARAMS),
      duration = NULL,
      type = 'error'
    )
  }

  on.exit({
    Sys.sleep(1)
    hide("loading-gray-overlay")
  })

  isolate({
    # if we're not coming from minio (CoreMS files or MAP project object), ask whether they have multiple CoreMS files to upload or a single unified file.
    if ((length(corems_revals[['combined_tables']]) == 0) && is.null(revals$map_project)) {
      showModal(upload_type_modal())
    } else if (!is.null(revals$map_project)) {
      insertTab(
        "top_page",
        target = "Welcome",
        tab = upload_tab(),
        position = "after"
      )
      showModal(map_upload_modal())
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

#' Modal which informs the user that a MAP object was uploaded correctly (or incorrectly)
map_upload_modal <- function() {
  modalDialog(
    title = "Data Uploaded from MAP",
    tags$p("Your data has been uploaded from the Multi-Omics Analysis Portal.  Your data will be pre-populated on the Upload tab"),
    footer = tagList(
      actionButton("map_modal_goto_upload", "Go to Upload tab"),
      modalButton("Dismiss")
    )
  )
}

#' observer for above modal actionbutton
observeEvent(input$map_modal_goto_upload, {
  updateTabsetPanel(inputId = "top_page", selected="Upload")
  removeModal()
})

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