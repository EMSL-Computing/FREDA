###########
# Most elements of server.R are sourced from files in Observers/ and srv_ui_elements/
# This makes it easier to work with elements on a single tab by having the corresponding .R files open rather than one enourmous server file
# CMD-shift-F is your friend here
###########

# Modify maximum file size (currently 250 mb)
options(shiny.maxRequestSize=250*1024^2, ch.dir = TRUE) 

# Uncomment for error checking on server
#options(shiny.sanitize.errors = FALSE)

shinyServer(function(session, input, output) {
  # onStop(function() rm(revals$peakData2, pos = 1))
  Sys.setenv(R_ZIPCMD="/usr/bin/zip")
  
  # source error handling file if exists, will be a script with observers that 
  # store objects that will show up the workspace after disconnect, like so:
  
  # reactive values, including peakdata objects
  # observeEvent(reactiveValuesToList(revals),{
  #   revals$uploaded_emeta <- Emeta()
  #   revals_postmortem <<- reactiveValuesToList(revals)
  # })

  tryCatch({
    source('untracked_resources/store_postmortem_objects.R', local = TRUE)
  }, error = function(e) message('Not storing postmortem objects'))
  
  # Source all scripts
  for (f in Sys.glob("./helper_functions/*.R")) source(f, local = TRUE)
  for (f in Sys.glob("./Reactive_Variables/*.R")) source(f, local = TRUE)
  for (f in Sys.glob("./Observers/*.R")) source(f, local = TRUE)
  for (f in Sys.glob("./srv_ui_elements/*.R")) source(f, local = TRUE)  
  for (f in Sys.glob("./tab_factories/*.R")) source(f, local = TRUE) 
  
  #'@details Store any values passed in the URL
  header_params = reactiveValues()
  
  #'@details General unorganized reactiveValues
  revals <-
    reactiveValues(
      ntables = 0,
      makeplot = 1,
      color_by_choices = NULL,
      axes_choices = NULL,
      redraw_largedata = FALSE,
      react_largedata = FALSE,
      plot_data_export = NULL,
      peakData_export = NULL,
      redraw_filter_plot = TRUE,
      reac_filter_plot = TRUE,
      group_1 = NULL,
      group_2 = NULL,
      single_group = NULL,
      single_sample = NULL,
      whichSample1 = NULL,
      whichSample2 = NULL,
      warningmessage_upload = list(upload = "style = 'color:deepskyblue'>Upload data and molecular identification files described in 'Data Requirements' on the previous page."),
      reset_counter = 0,
      chooseplots = NULL,
      filter_click_disable = list(init = TRUE),
      peakData2 = NULL,
      groups_list = list(),
      removed_samples = list()
    )
  
  plots <-
    reactiveValues(
      last_plot = NULL,
      plot_list = list(),
      plot_data = list(),
      linked_plots = list(),
      plot_table = data.frame(
        "File Name" = character(0),
        'Download?' = character(0),
        "Plot Type" = character(0),
        "Sample Type" = character(0),
        "Group 1 Samples" = character(0),
        "Group 2 Samples" = character(0),
        "Boundary Set" = character(0),
        "Color By Variable" = character(0),
        "X Variable" = character(0),
        "Y Variable" = character(0),
        "Presence Threshold" = character(0),
        "Absence Threshold" = character(0),
        "P-Value" = character(0),
        "Comparisons Method" = character(0),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  
  tables <-
    reactiveValues(
      mapping_tables = list(),
      saved_db_info = data.frame(
        'Tables' = character(0),
        'No. Rows' = character(0),
        'Column Names' = character(0),
        check.names = F,
        stringsAsFactors = F
      )
    )
  
  #' @details core-ms files loaded through a header parameter that points to a
  #' 'folder' in minio containing all files.
  corems_samples <- reactiveValues()
  
  # Reload objects for debugging if they exist
  observeEvent(input$debug_reload,{
    if(exists('RELOAD_POSTMORTEM_PLOTS__')){
      lapply(names(RELOAD_POSTMORTEM_PLOTS__), function(x){
        plots[[x]] <<- RELOAD_POSTMORTEM_PLOTS__[[x]]
      })
    }
    
    if(exists('RELOAD_POSTMORTEM_OBJECTS__')){
      lapply(names(RELOAD_POSTMORTEM_OBJECTS__), function(x){
        revals[[x]] <<- RELOAD_POSTMORTEM_OBJECTS__[[x]]
      })
    }
    
    if(exists('RELOAD_POSTMORTEM_TABLES__')){
      lapply(names(RELOAD_POSTMORTEM_TABLES__), function(x){
        tables[[x]] <<- RELOAD_POSTMORTEM_TABLES__[[x]]
      })
    }
  })

  exportTestValues(plot_data = revals$plot_data_export, peakData = revals$peakData_export, color_choices = revals$color_by_choices)
  
  ##############################
  ######## Welcome Tab #########
  ##############################

  #############
  output$downloadData <- downloadHandler(
    filename = "FREDA_Example_Data.zip",
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(fname) {
      tmpdir <- tempdir()
      print(tempdir())
      fs <- c("example12T_edata.csv", "example12T_emeta.csv")
      write_csv(example_edata, path = file.path(tmpdir, "example12T_edata.csv"))
      write_csv(example_emeta, path = file.path(tmpdir, "example12T_emeta.csv"))      
      print(fs)
      zip(zipfile=fname, files=file.path(tmpdir, fs), flags = "-j")
    },
    contentType = "application/zip"
  )

  #---- processed data download --------#
  output$download_processed_data <- downloadHandler(
    filename = paste("FREDA_Output_",proc.time()[1],".zip", sep = ""),
    content = function(fname){
      zip(zipfile=fname, files=revals$fs, flags = "-j")
      if (file.exists(paste0(fname,".zip"))){file.rename(paste0(fname,".zip"),fname)}
      
    },
    contentType = "application/zip"
  )
  
  # UI objects to be rendered when hidden
  lapply(c('colorpal_out'), function(name){
    outputOptions(output, name, suspendWhenHidden = FALSE)
  })
  
})
