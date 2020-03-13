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
  # static objects
  dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
  dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'
  
  # onStop(function() rm(revals$peakData2, pos = 1))
  Sys.setenv(R_ZIPCMD="/usr/bin/zip")
  
  # source error handling file if exists, will be a script with observers that store objects that will show up the workspace after disconnect, like so:
  # observeEvent(c(objects$omicsData, objects$omicsData_2),{
  #   omicsData_postmortem <<- objects$omicsData
  #   omicsData_2_postmortem <<- objects$omicsData_2
  # })
  tryCatch({
    source('untracked_resources/store_postmortem_objects.R', local = TRUE)
  }, error = function(e) message('Not storing postmortem objects'))
  
  # Source various helper functions
  source('helper_functions/selection_addons.R')
  source('helper_functions/summaryFilter.R') 
  source('helper_functions/summaryPreprocess.R')
  source('helper_functions/database_utils.R')
  
  # observers and UI elements which operate across tabs
  source("Observers/global_observers.R", local = TRUE)
  source('srv_ui_elements/global_UI.R', local = TRUE)
  
  # Misc Reactive Values:
  # peakData2_dim(), uploaded_data_dim(). The number of cells in e_data of the respective objects
  source('Reactive_Variables/misc_revals.R', local = TRUE)
  
  revals <- reactiveValues(ntables = 0, makeplot = 1, color_by_choices = NULL, axes_choices = NULL, redraw_largedata = FALSE, react_largedata = FALSE,
                           plot_data_export = NULL, peakData_export = NULL, redraw_filter_plot = TRUE, reac_filter_plot = TRUE,
                           group_1 = NULL, group_2 = NULL, single_group = NULL, single_sample = NULL, whichSample1 = NULL, whichSample2 = NULL, 
                           warningmessage_upload = list(upload = "style = 'color:deepskyblue'>Upload data and molecular identification files described in 'Data Requirements' on the previous page."),
                           reset_counter = 0, chooseplots = NULL, filter_click_disable = list(init = TRUE), peakData2 = NULL, 
                           groups_list = list(), removed_samples = list())
  
  plots <- reactiveValues(last_plot = NULL, plot_list = list(), plot_data = list(), linked_plots = list(),
                          plot_table = data.frame("File Name" = character(0), 'Download?' = character(0), "Plot Type" = character(0), "Sample Type" = character(0), "Group 1 Samples" = character(0), 
                                                  "Group 2 Samples" = character(0), "Boundary Set" = character(0), "Color By Variable" = character(0), "X Variable" = character(0), 
                                                  "Y Variable" = character(0), "Presence Threshold" = character(0), "Absence Threshold" = character(0), "P-Value" = character(0),
                                                   "Comparisons Method" = character(0), check.names = FALSE, stringsAsFactors = FALSE))
  
  tables <- reactiveValues(mapping_tables = list(), saved_db_info = data.frame('Tables' = character(0), 'No. Rows' = character(0), 'Column Names' = character(0), check.names = F, stringsAsFactors = F))
  
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

  exportTestValues(plot_data = revals$plot_data_export, peakData = revals$peakData_export, color_choices = revals$color_by_choices)
  
  ##############################
  ######## Welcome Tab #########
  ##############################
  
  #------ Download Example Data ---------#
  example_edata <- read_csv('Data/example12T_edata.csv') %>% as.data.frame(stringsAsFactors = FALSE)
  example_emeta <- read_csv('Data/example12T_emeta.csv') %>% as.data.frame(stringsAsFactors = FALSE)
  calc_opts <- read_csv('calculation_options.csv') %>% as.data.frame(stringsAsFactors = FALSE)
  calc_vars <- read_csv('calculation_variables.csv') %>% as.data.frame(stringsAsFactors = FALSE)
  # determines when 'large data' options are triggered
  max_cells <- 2000000
  
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
  ##############################
  ######## Upload Tab ##########
  ##############################
  
  # Upload Tab Reactive Values:
  # Edata():  The uploaded data file
  # edata_cnames():  All column names from from Edata()
  # Emeta():  The uploaded e_meta file
  # emeta_cnames():  All column names from Emeta()
  # sample_names(): Sample names from uploaded data (all columns from Edata() minus mass column)
  # fdata():  dummy f_data object created from sample_names().  Needed for input to as.peakData.  CONTAINS NO GROUPING INFORMATION
  source("Reactive_Variables/upload_revals.R", local = TRUE)

  ### Upload Observers:  Contains conditional dropdown behavior, shinyjs functionality.
  source("Observers/upload_observers.R", local = TRUE)
  ###
  
  #### Main Panel (Upload Tab) ####
  # Minor upload UI Elements (output$<name>)
  # num_peaks, num_samples
  # emeta_text, edata_text, success_upload
  source('srv_ui_elements/upload_UI_mainpanel.R', local = TRUE)
  
  #### Sidebar Panel (Upload Tab) ####
  # element selection and C13 sidebar elements
  source('srv_ui_elements/upload_UI_sidebar.R', local = TRUE)
  
  #########################
  ####### Groups Tab ######
  #########################

  source("Observers/groups_observers.R", local = TRUE)
  source("srv_ui_elements/groups_UI.R", local = TRUE)
  
  # Groups tab reactive variables:
  # groupstab_df():  reactive table that displays groups and their filtered/non filtered samples
  source("Reactive_Variables/groups_revals.R", local = TRUE)
  
  ##############################
  ####### Preprocess Tab #######
  ##############################
  
  # Preprocess Tab reactive variables:
  # emeta_display_choices():  Columns of emeta minus mass column, isotopic information column, and categorical columns with greater than 12 categories
  source("Reactive_Variables/preprocess_revals.R", local = TRUE)
  
  source("Observers/preprocess_observers.R", local = TRUE)
  source('srv_ui_elements/preprocess_UI.R', local = TRUE)
  
  ######################################
  ############## QC tab ################
  ######################################
  
  source('Observers/qc_observers.R', local = TRUE)
  source('srv_ui_elements/qc_UI.R', local = TRUE)

  ########################################
  ############## Filter tab ##############
  ########################################
  
  ### Filter Observers.  Contains much of the dropdown behavior and helper button functionality.
  source("Observers/filter_observers.R", local = TRUE)
  ###
  
  # Filter UI Elements
  source('srv_ui_elements/filter_UI.R', local = TRUE)
  
  ## Filter tab reactive values
  source("Reactive_Variables/filter_revals.R", local = TRUE)

  #############################
  ####### Visualize Tab #######
  #############################
  
  #Visualize Tab reactive variables:
  # plot_data(): Plotting dataframe that is passed to output$FxnPlot.  This object triggers an important observer (in server.R) which controls dropdown selection
  # plot_defaults():  defaults arguments to plot axes/title values
  # numeric_selected():  keeps track of whether the column selected by input$vk_colors is numeric or categorical
  # g1_samples() and g2_samples()  These store the samples that will be compared during group/sample comparison plots
  source("Reactive_Variables/visualize_revals.R", local = TRUE)
  
  # Viztab observers.  Help Button. Dropdown choices, plot clearing, and shinyjs helper functionality###
  source("Observers/visualize_observers.R", local = TRUE)
  #
  
  # Minor UI Elements
  # Label adjustment and plot download buttons (sidebar)
  source('srv_ui_elements/visualize_UI_main_and_plot_opts.R', local = TRUE)
  # plot, sample type, and comparison options
  source('srv_ui_elements/visualize_UI_sidebar.R', local = TRUE)
  # warnings_visualize: displays warning messages in revals$warningmessage_visualize
  # chooseplots_icon, axlabs_icon, saveplots_icon: icons for collapse panels
  source('srv_ui_elements/visualize_UI_misc.R', local = TRUE)
  
  ## Linked plots Sub-tab
  source('srv_ui_elements/visualize_linked_plots_UI.R', local = TRUE)
  source('Observers/linked_plot_observers.R', local = TRUE)
  source('Reactive_Variables/linked_plot_revals.R', local = TRUE)
  
  ############################
  ####### Database Tab #######
  ############################
  
  source('./Observers/database_observers.R', local = TRUE)
  source('./srv_ui_elements/database_UI.R', local = TRUE)
  
  ############################
  ####### Download Tab #######
  ############################
  
  source("Observers/download_observers.R", local = TRUE)
  source("helper_functions/report.R", local = TRUE)
  source('srv_ui_elements/download_UI.R', local = TRUE)
  
  #---- processed data download --------#
  output$download_processed_data <- downloadHandler(
    filename = paste("FREDA_Output_",proc.time()[1],".zip", sep = ""),
    content = function(fname){
      zip(zipfile=fname, files=revals$fs, flags = "-j")
      if (file.exists(paste0(fname,".zip"))){file.rename(paste0(fname,".zip"),fname)}
      
    },
    contentType = "application/zip"
  )
})
