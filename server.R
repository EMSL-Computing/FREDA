
# Modify maximum file size (currently 250 mb)
options(shiny.maxRequestSize=250*1024^2, ch.dir = TRUE) 

# Uncomment for error checking on server
#options(shiny.sanitize.errors = FALSE)

library(shiny)
library(ftmsRanalysis)
library(ggplot2)
library(reshape2)
library(webshot)
library(htmlwidgets)
library(dplyr)
library(raster)
library(magick)
library(purrr)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(pander)
library(readr)
library(plotly)

shinyServer(function(session, input, output) {
  
  # onStop(function() rm(revals$peakData2, pos = 1))
  Sys.setenv(R_ZIPCMD="/usr/bin/zip")
  # Source files for 'summaryFilt' and 'summaryPreprocess'
  source('helper_functions/selection_addons.R')
  source('helper_functions/summaryFilter.R') 
  source('helper_functions/summaryPreprocess.R')
  source("helper_functions/renderDownloadPlots.R")
  source("Observers/misc_observers.R", local = TRUE)
  # Misc Reactive Values:
  # peakData_dim(), peakData2_dim(), uploaded_data_dim(). The number of cells in e_data of the respective objects
  source('Reactive_Variables/misc_revals.R', local = TRUE)
  
  revals <- reactiveValues(ntables = 0, makeplot = 1, color_by_choices = NULL, axes_choices = NULL, redraw_largedata = FALSE, react_largedata = FALSE,
                           plot_data_export = NULL, peakData_export = NULL, redraw_filter_plot = TRUE, reac_filter_plot = TRUE,
                           group_1 = NULL, group_2 = NULL, single_group = NULL, single_sample = NULL, whichSample1 = NULL, whichSample2 = NULL, 
                           warningmessage_upload = list(upload = "style = 'color:deepskyblue'>Upload data and molecular identification files described in 'Data Requirements' on the previous page."),
                           warningmessage_visualize = list(), warningmessage_filter = list(), warningmessage_preprocess = list(), warningmessage_groups = list(), warningmessage_qc = list(),
                           current_plot = NULL, plot_list = list(), plot_data = list(), reset_counter = 0, current_qc_boxplot = NULL,
                           chooseplots = NULL, filter_click_disable = list(init = TRUE), peakData2 = NULL, 
                           groups_list = list(), removed_samples = list())
  
  exportTestValues(plot_data = revals$plot_data_export, peakData = revals$peakData_export, color_choices = revals$color_by_choices)
  ######## Welcome Tab #############
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
  ######## Upload Tab ##############
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
  
  ### Minor Upload UI Elements (output$<name>)
  # num_peaks, num_samples
  # emeta_text, edata_text, success_upload
  source('srv_ui_elements/upload_UI_misc.R', local = TRUE)
  # element selection and C13 sidebar elements
  source('srv_ui_elements/upload_UI_elements.R', local = TRUE)
  
  #### Sidebar Panel (Upload Tab) ####
  
  # Drop down list: Get edata unique identifier
  output$edata_id <- renderUI({
    # Drop down list with options from column names
    selectInput("edata_id_col", "Choose column with IDs",
                choices  = c('Select one', edata_cnames()))
  }) # End edata_id #
  
  # Drop-down lists: Choose formula column
  output$f_column <- renderUI({
    selectInput("f_column", "Choose formula column",
                choices = c('Select one', emeta_cnames()))
  }) # End f_column #
  
  #### Action Button Reactions (Upload Tab) ####
  
  # Object: Create peakData when Upload Button clicked
  observeEvent(c(input$upload_click, input$preprocess_click), {
    if(input$top_page == 'Upload'){
      shinyjs::disable('upload_click')
      shinyjs::show('upload_waiting', anim=T)
      on.exit({
        shinyjs::enable('upload_click')
        shinyjs::hide('upload_waiting', anim=T)
        })
      # Error handling: unique identifier chosen
      validate(need(input$edata_id_col != 'Select one', 'Please select a unique identifier column'),
               need(input$edata_id_col %in% edata_cnames() & input$edata_id_col %in% emeta_cnames(),
                    message = "The chosen ID column does not exist in one or both of the Data/Molecular Identification"))
      
      validate(         
        need(input$select != 0, 'Please select either Formula or Elemental columns'),
        need(input$isotope_yn != 0, 'Please select yes or no on information for isotopes'),
        need(sum(!(Edata()[,input$edata_id_col] %in% Emeta()[,input$edata_id_col])) == 0, 
             'Not all peaks in data file are present in molecular identification file, please add/remove these peaks to emeta / from edata')
        
      ) # End error handling #
      
      ## If formula column chosen
      if (input$select == 1) {
        
        # Error handling: f_column chosen and  (if chosen) is of class 'character'
        validate(
          need((input$f_column != 'Select one'),
               'Please select a formula column'),
          need({
            if (input$f_column != 'Select one') 
              is.character(Emeta()[,input$f_column])
            else 
              FALSE
          }, # End 'need'
          
          'Formula column is not a character vector. Please select another.')
          
        ) # End error handling #
        if (input$isotope_yn == 1 & isTRUE(input$iso_info_filter == 1)) { # If there's C13 # 
          
          # Error handling: entered isotopic notation must exist in the isotope information column
          validate(
            need(input$iso_info_column != "0", message = "Please choose a column of isotopic information"),
            need(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol),
                 'The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise.')
          ) # End error handling
          
          res <- as.peakData(e_data = Edata(), f_data = fdata(),
                                e_meta = Emeta(), edata_cname = input$edata_id_col, 
                                fdata_cname = 'SampleId', mass_cname = input$edata_id_col, 
                                instrument_type = input$instrument,
                                mf_cname = input$f_column,
                                isotopic_cname = input$iso_info_column,
                                isotopic_notation = as.character(input$iso_symbol),
                                check_rows = TRUE, data_scale = input$data_scale)
          
        } # End C13 / no C13 if statement
        
        if (input$isotope_yn == 2 | isTRUE(input$iso_info_filter) != 1) { #no C13
          # Calculate peakDataData with formula column
          res <- as.peakData(e_data = Edata(), f_data = fdata(),
                                e_meta = Emeta(), edata_cname = input$edata_id_col, 
                                fdata_cname = 'SampleId', mass_cname = input$edata_id_col, 
                                instrument_type = input$instrument, mf_cname = input$f_column,
                                check_rows = TRUE, data_scale = input$data_scale)
        } 
      }
      
      # If elemental columns chosen
      if (input$select == 2){
        
        ## Error handling: all drop down columns nonempty and of class 'numeric'
        
        # first check that H and C columns are specified and numeric...
        validate(
          need({(input$c_column != 'Select a column') & 
              (input$h_column != 'Select a column')
          }, 
          'Hydrogen/carbon columns are required. Please double-check drop-down options.')
        )
        validate(
          need({
            all(is.numeric(Emeta()[,input$c_column])) &
              all(is.numeric(Emeta()[,input$h_column]))
          }, 
          'One or more elemental columns are non-numeric.')
        )
        # ...then check that -if- other columns are selected, they are numeric
        for(col in c('n_column', 'o_column', 's_column', 'p_column')){
         if(input[[col]] != 'Select a column'){
           validate(need(is.numeric(Emeta()[,input[[col]]]), 'One or more elemental columns are non-numeric.'))
         } 
        }# End error handling #
        
        # If no C13
        if (input$isotope_yn == 2 | isTRUE(input$iso_info_filter == 2)) {
          # Create peakData object
          res <- as.peakData(e_data = Edata(), f_data = fdata(),
                                e_meta = Emeta(), edata_cname = input$edata_id_col, 
                                fdata_cname = 'SampleId', mass_cname = input$edata_id_col,
                                instrument_type = input$instrument,
                                c_cname = input$c_column, h_cname = input$h_column, 
                                n_cname = if(input$n_column == 'Select a column') NULL else input$n_column,
                                o_cname = if(input$o_column == 'Select a column') NULL else input$o_column, 
                                s_cname = if(input$s_column == 'Select a column') NULL else input$s_column, 
                                p_cname = if(input$p_column == 'Select a column') NULL else input$p_column,
                                check_rows = TRUE, data_scale = input$data_scale)
          
        }
        if (input$isotope_yn == 1 & isTRUE(input$iso_info_filter == 1)) { # If there's C13 # 
          
          # Error handling: entered isotopic notation must exist in the isotope information column
          validate(need(input$iso_info_column != "0", message = "Please choose a column of isotopic information"))
          validate(need(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol),
                        'The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise.')
          ) # End error handling
          
          res <- as.peakData(e_data = Edata(), f_data = fdata(),
                                e_meta = Emeta(), edata_cname = input$edata_id_col, 
                                fdata_cname = 'SampleId', mass_cname = input$edata_id_col, 
                                instrument_type = input$instrument,
                                c_cname = input$c_column, h_cname = input$h_column, 
                                n_cname = if(input$n_column == 'Select a column') NULL else input$n_column,
                                o_cname = if(input$o_column == 'Select a column') NULL else input$o_column, 
                                s_cname = if(input$s_column == 'Select a column') NULL else input$s_column, 
                                p_cname = if(input$p_column == 'Select a column') NULL else input$p_column,
                                isotopic_cname = input$iso_info_column,
                                isotopic_notation = as.character(input$iso_symbol),
                                check_rows = TRUE, data_scale = input$data_scale)
          
        } # End C13 / no C13 if statement
        
      } # End elemental column if statement
      
      shinyjs::show('upload_success')
      
      # create reactive peakData2 object
      revals$peakData2 <- res
      
      # store peakdata for post-mortem testing
      # test_peakData <<- res
      
      # reset 'removed samples' reval
      revals$removed_samples <- list()
      revals$groups_list <- list()
      updateCollapse(session, 'upload_collapse', close = c('file_upload', 'column_info'))
      shinyjs::show('ok_idcols')
      
      revals$uploaded_data <- res
    }
    else if(input$top_page == 'Preprocess'){
      # Keep a reactive copy of the pre-filtered data in case of a filter reset event
      req(revals$uploaded_data, isolate(input$tests))
      
      temp <- revals$uploaded_data
      
      for(el in isolate(input$tests)){
        if(grepl("assign_class", el)){
          foo <- strsplit(el, ";")[[1]]
          f <- get(foo[1], envir=asNamespace("ftmsRanalysis"), mode="function")
          temp <- f(temp, foo[2])
          temp$e_meta[paste0(foo[2], "_class")] <- gsub(";.*", "", temp$e_meta[,paste0(foo[2], "_class")])
          
        }
        else{
          f <- get(el, envir=asNamespace("ftmsRanalysis"), mode="function")
          temp <- f(temp)
        }
      }
      
      revals$uploaded_data <- temp
    }
    
  }) # End peakData creation

  #### Main Panel (Upload Tab) ####

  # Summary: Display number of peaks with formulas
  output$num_peaks_formula <- renderText({
    
    # Error handling: Require e_meta and others
    req(Emeta())
    req(input$select != 0)
    
    # Scope: Set up num_rows_formula to edit in if statements
    num_rows_formula = nrow(Edata())
    
    # If f_columns have been identified
    if (input$select == 1){
      
      # Error handling: need formula column
      req(input$f_column)
      req(input$f_column != 'Select one')
      req(is.character(Emeta()[,input$f_column]))
      
      # Count all non-NA columns
      f_column <- Emeta()[,input$f_column]
      
      # Count all nonempty and non-NA entries
      num_rows_formula <- length(which((!is.na(f_column)) & (f_column != "")))
      
    } else if (input$select == 2) { # If elemental columns have been identified
      
      # Error handling: drop down columns must exist and be numeric
      req({
        (input$c_column != 'Select a column') && 
          (input$h_column != 'Select a column') && 
          (input$n_column != 'Select a column') &&
          (input$o_column != 'Select a column') &&
          (input$s_column != 'Select a column') &&
          (input$p_column != 'Select a column') &&
          all(is.numeric(Emeta()[,input$c_column])) &&
          all(is.numeric(Emeta()[,input$h_column])) &&
          all(is.numeric(Emeta()[,input$n_column])) &&
          all(is.numeric(Emeta()[,input$o_column])) &&
          all(is.numeric(Emeta()[,input$s_column])) &&
          all(is.numeric(Emeta()[,input$p_column]))
      }) # End error handling for elemental columns #
      
      # Set up list of column names
      elem_cnames <- c(input$c_column, input$h_column, 
                       input$n_column, input$o_column, 
                       input$s_column, input$p_column)
      
      # Create data frame of all elemental columns to sum across
      elem_columns <- data.frame(Emeta()[,elem_cnames])
      req(input$isotope_yn)
      req(input$iso_info_filter)
      # If isotopic information is included and matching entered notation, filter out where isotopes = denoted symbol
      if (input$isotope_yn == 1 & input$iso_info_filter == 1) {
        req(input$iso_info_column)
        validate(need(input$iso_info_column != 0, message = "Please choose a column of isotopic information"))
        if (any(Emeta()[,input$iso_info_column] %in% input$iso_symbol)) {
    iso <- Emeta()[,input$iso_info_column]
    elem_columns <- elem_columns[-(which(as.character(iso) == as.character(input$iso_symbol))),]
  }
      }# End if isotopic information is chosen and correctly denoted#
      
      # Count all remaining rows with nonzero sums
      num_rows_formula <- length(which(rowSums(elem_columns) > 0))
      
    } # End elemental columns option
    
    validate(
      need(!is.null(revals$uploaded_data), message = "")
    )
    # Display number of peaks/rows with formula assigned
    c('Number of peaks with formulas: ', num_rows_formula)
    
    
  }) # End num_peaks_formula in summary panel
  
  ####### Groups Tab ######
  
  # sample selection input, depends on sample names in the UI
  source("Observers/groups_observers.R", local = TRUE)
  
  # Groups tab reactive variables:
  # groupstab_df():  reactive table that displays groups and their filtered/non filtered samples
  source("Reactive_Variables/groups_revals.R", local = TRUE)
  
  # sample names selector based on the sample names of revals$uploaded_data
  output$group_samples <- renderUI({
    validate(need(sample_names(), message = "Upload data before defining groups"))
    pickerInput("group_samples", "Samples to include in this group:", choices = sample_names(), 
                options =  pickerOptions(dropupAuto = FALSE, actionsBox = TRUE), multiple = TRUE)
  })
  
  # table which displays stored groups
  output$group_table <- DT::renderDataTable(groupstab_df(),
                                            selection = 'single',
                                            options = list(scrollX = TRUE))
  
  output$warnings_groups <- renderUI({
    HTML(paste(revals$warningmessage_groups, collapse = ""))
  })
  
  ####### Preprocess Tab #######
  
  # Preprocess Tab reactive variables:
  # emeta_display_choices():  Columns of emeta minus mass column, isotopic information column, and categorical columns with greater than 12 categories
  source("Reactive_Variables/preprocess_revals.R", local = TRUE)
  
  source("Observers/preprocess_observers.R", local = TRUE)
  
  # Populate List from CSV File calculation_options.csv
  output$which_calcs <- renderUI({
    choices <- calc_opts$Function
    names(choices) <- calc_opts$DisplayName
      tooltip_checkbox("tests", "What Values Should be Calculated?", choices, selected = c("calc_element_ratios", "calc_kendrick"),
                       extensions = lapply(1:length(choices), function(i){
                         div(style = "color:deepskyblue;display:inline-block",
                          tipify(icon("question-sign", lib = "glyphicon"), title = calc_opts$Info[i], placement = "top", trigger = 'hover')
                         )
                       })
    )
  })
  
  # Warnings for preprocess tab
  output$warnings_preprocess <- renderUI({
    HTML(paste(revals$warningmessage_preprocess, collapse = ""))
  })
  
  #### Action Button reactions ####
  
  ## Action button: Apply calculation functions When action button is clicked
  # Depends on: revals$peakData2, input$tests
  observeEvent(input$preprocess_click, {
    validate(need(input$tests, message = "Please choose at least one test to calculate"))
    disable('preprocess_click')
    shinyjs::show('preprocess_waiting', anim=T)
    on.exit({
      enable('preprocess_click')
      shinyjs::hide('preprocess_waiting', anim=T)
      })
    
    # If columns have already been calculated, start over from uploaded data
    if (any(attr(revals$peakData2, "cnames") %in% calc_vars$ColumnName)){
      revals$peakData2 <- revals$uploaded_data
    }
    
    # Apply all relevant functions
    withProgress(message = "Calculating Values....",{
      for(el in input$tests){
        if(grepl("assign_class", el)){
          foo <- strsplit(el, ";")[[1]]
          f <- get(foo[1], envir=asNamespace("ftmsRanalysis"), mode="function")
          revals$peakData2 <- f(revals$peakData2, foo[2])
          revals$peakData2$e_meta[paste0(foo[2], "_class")] <<- gsub(";.*", "", revals$peakData2$e_meta[,paste0(foo[2], "_class")])
          
        }
        else{
          f <- get(el, envir=asNamespace("ftmsRanalysis"), mode="function")
          revals$peakData2 <- f(revals$peakData2)
        }
        
        incProgress(1/length(input$tests))
      }
    })
    
    # post mortem test object
    # test_uploaded_data <<- revals$peakData2 
    
    # reset 'removed samples' reval
    revals$removed_samples <- list()
    
    if (isTRUE(getOption("shiny.testmode"))) {
      exportTestValues(peakData2 = revals$peakData2)
    }
    
  }, priority = 10) # End action button event
  
  # Creates two reactive variables for continuous and categorical variables which are used to display separate tables
  # Note: dependent on preprocess click and the user-specified calculations
  observeEvent(input$preprocess_click, {
    # Error handling: revals$peakData2 must have a non-NULL Kendrick Mass column name
    #req(!is.null(attr(revals$peakData2, 'cnames')$kmass_cname))
    req(input$tests)
    
    # Get csv file of all possible calculation column names
    possible_calc_cnames <- read_csv("calculation_variables.csv") %>% as.data.frame(stringsAsFactors = FALSE)
    
    # Get column names from revals$peakData2's e_meta
    actual_cnames <- colnames(revals$peakData2$e_meta)
    
    # Find all columns with names that match names for calculated columns
    v_index <- which(possible_calc_cnames[,1] %in% actual_cnames)
    
    # Save calculation column names from above and their display names 
    intersect <- possible_calc_cnames[v_index,]
    
    # get numeric columns
    numeric_cols <- revals$peakData2$e_meta %>% 
      dplyr::select(which(sapply(.[intersect[,1]], is.numeric))) %>% 
      names()
    
    # get categorical columns
    categorical_cols <- revals$peakData2$e_meta %>% 
      dplyr::select(which(!sapply(.[intersect[,1]], is.numeric))) %>%
      names() 
    
    #set reactive variables for observers
    revals$numeric_cols <- intersect %>% filter(ColumnName %in% numeric_cols)
    revals$categorical_cols <- intersect %>% filter(ColumnName %in% categorical_cols)

  }) 
  
  #### Main Panel ####
  
  # Drop down list: potential histogram options
  observeEvent(input$preprocess_dismiss, {
    output$which_hist_out <- renderUI({
        # Error handling: input csv of calculations variables required
        req(calc_vars, revals$numeric_cols, revals$categorical_cols)
        
        tagList(
          hr(),
          tags$p('I would like to see a histogram/bar-chart across all values of:'),
          selectInput('which_hist', NULL,
                      choices = emeta_display_choices(),
                      selected = colnames(revals$peakData2$e_meta)[ncol(revals$uploaded_data$e_meta) + 1])
        )
      }) 
  })# End which_hist
  
  # Plot the histogram chosen above
  # Depends on: which_hist
  output$preprocess_hist <- renderPlotly({
    
    # Error handling: Require some columns to be selected
    req(input$which_hist)
    
    isolate({
      # Save column name for later display
      columnName <- input$which_hist
      
      # set display name
      displayName <- calc_vars %>% filter(ColumnName == columnName) %>%
        pluck("DisplayName")
      
      # Plot histogram using plotly
      p <- plot_ly(x = revals$peakData2$e_meta[,columnName], type = 'histogram') %>%
        layout( title = paste('Observed distribution of', displayName),
                scene = list(
                  xaxis = list(title = displayName),
                  yaxis = list(title = 'Frequency')))
      p$elementId <- NULL
      
      #____test export_____
      exportTestValues(preprocess_hist = p, hist_attrs = p$x$attrs[[p$x$cur_data]], hist_layout = p$x$layout, hist_visdat = p$x$visdat[[p$x$cur_data]]())
      
      return(p)
    })
    
  }) # End process_hist
  
  ############## QC tab ################
  source('Observers/qc_observers.R', local = TRUE)
  
  # Y axis scale select for boxplots
  output$qc_plot_scale <- renderUI({
    validate(need(!is.null(revals$peakData2), message = "No data object found, please verify you have successfully uploaded data"))
    pickerInput("qc_plot_scale", "Plot on scale:", 
                choices = list('Log base 2' = 'log2', 'Log base 10'='log10', 'Natural log'='log', 
                               'Presence/absence' = 'pres', 'Raw intensity'='abundance'), 
                selected = 'pres')
    
  })
  
  # Group selection
  output$qc_select_groups <- renderUI({
    pickerInput("qc_select_groups", "Select groups:", 
                choices = names(revals$groups_list),
                multiple = TRUE
    )
  })
  
  # Boxplots
  output$qc_boxplots <- renderPlotly({
    input$update_boxplot_axes
    req(!is.null(revals$peakData2))
    req(!is.null(input$qc_plot_scale))
    if(uploaded_data_dim() > max_cells) isolate(on.exit(revals$redraw_largedata <- FALSE))
    req(isolate(revals$redraw_largedata))
    
    color_by <- if(isTRUE(input$qc_plot_scale %in% c('log2', 'log10', 'log', 'abundance'))) 'groups' else 'molform'
    ds = attributes(revals$peakData2)$data_info$data_scale
    
    # subset the data if a group is selected
    if(isTRUE(all(input$qc_select_groups %in% names(revals$groups_list)) & !is.null(input$qc_select_groups))){
      # get set of unique samples in all selected groups
      samples <- revals$groups_list[input$qc_select_groups] %>% unlist() %>% unique() %>% setdiff(revals$removed_samples)
      temp_peakData2 <- subset(revals$peakData2, samples = samples)
    } 
    else temp_peakData2 <- revals$peakData2
    
    # if their data scale selection does not match the object's data scale, transform before plotting
    if(isTRUE(ds != input$qc_plot_scale)){
      p <- plot(edata_transform(temp_peakData2, input$qc_plot_scale), 
             xlabel=isolate(input$qc_boxplot_xlab), ylabel=isolate(input$qc_boxplot_ylab), 
             title = isolate(input$qc_boxplot_title),colorBy = color_by) %>% layout(margin = list(b = 100), xaxis = list(tickangle = 45))
    }
    else p <- plot(temp_peakData2, xlabel=isolate(input$qc_boxplot_xlab), ylabel=isolate(input$qc_boxplot_ylab), 
                title = isolate(input$qc_boxplot_title), colorBy=color_by) %>% layout(margin = list(b = 100), xaxis = list(tickangle = 45))
    
    revals$current_qc_boxplot <- p
    enable('add_qc_boxplot')
    shinyjs::hide('qc_download_ok')
    
    p
  })
  
  # qc warnings
  output$warnings_qc <- renderUI({
    HTML(lapply(revals$warningmessage_qc, function(el){
      paste0("<p ", el, "</p>")
    }) %>%
      paste(collapse = "")
    )
  })

  ############## Filter tab ##############
  
  ### Filter Observers.  Contains much of the dropdown behavior and helper button functionality.
  source("Observers/filter_observers.R", local = TRUE)
  ###
  
  # Filter minor UI Elements
  source('srv_ui_elements/filter_UI_misc.R', local = TRUE)
  
  ## Filter tab reactive values
  source("Reactive_Variables/filter_revals.R", local = TRUE)
  
  # ----- Filter Reset Setup -----# 
  
  # Allow a button click to undo filtering
  f <- reactiveValues(clearFilters = FALSE)
  observeEvent(input$clear_filters_yes, {
    f$clearFilters <- TRUE
  }, priority = 10)
  
  observeEvent(input$filter_click, {
    f$clearFilters <- FALSE
  }, priority = 10)
  
  #### Sidebar Panel (Filter Tab) ####
  
  # Drop down list: Minimum Number of observations
  # Depends on edata_cnames()
  output$minobs <- renderUI({
     selectInput('minobs', "Minimum number observed", choices = seq(1, max(length(input$keep_samples),1), 1), selected = 2)
  }) # End minobs
  
  output$filter_samples <- renderUI({
    selectInput('keep_samples', NULL, choices = revals$uploaded_data$f_data[,getFDataColName(revals$uploaded_data)], selected = revals$uploaded_data$f_data[,getFDataColName(revals$uploaded_data)], multiple = TRUE)
  })
  #### Action Button Reactions (Filter Tab) ####
  
  # Event: Create filtered nonreactive revals$peakData2 when action button clicked
  # Depends on action button 'filter_click'
  observeEvent(input$filter_click, {
    shinyjs::show('calc_filter', anim = T)
    shinyjs::disable('filter_click')
    on.exit({ 
      shinyjs::enable('filter_click')
      shinyjs::hide('calc_filter', anim = T)
      })
    
    # if the data is already filtered start over from the uploaded data
    if (any(c("moleculeFilt", "massFilt", "formulaFilt") %in% names(attributes(revals$peakData2)$filters)) | any(grepl("emetaFilt", names(attributes(revals$peakData2)$filters))) | !all(colnames(revals$peakData2$e_data) %in% colnames(revals$uploaded_data$e_data))){
      revals$peakData2 <- revals$uploaded_data
    }
    
    n_filters = sum(sapply(list(input$massfilter, input$molfilter, input$samplefilter, input$formfilter, input$custom1, input$custom2, input$custom3), isTRUE))
    
    withProgress(message = "Applying filters....",{
      # Apply sample filter
      if(input$samplefilter){
        req(length(input$keep_samples) > 0)
        revals$peakData2 <- subset(revals$peakData2, samples = input$keep_samples, check_rows = TRUE)
        revals$removed_samples <- c(revals$removed_samples, setdiff(sample_names(), input$keep_samples))
        
        # remove empty lists
        if(length(revals$groups_list) > 0){
            
            # get indices of now empty groups
            inds <- sapply(revals$groups_list, function(el){
              length(intersect(el, input$keep_samples)) == 0
            })
            
            revals$groups_list[inds] <- NULL
        }
        incProgress(1/n_filters, detail = 'Sample filter done.')
      }else revals$removed_samples <- list()
      
      # Apply mass filter
      if (input$massfilter){
        
        # Error handling: Min mass less than max mass, but greater than 0
        req(input$min_mass < input$max_mass)
        req(input$min_mass > 0)
        
        # Create and apply mass filter to nonreactive peakData object
        filterMass <- mass_filter(revals$peakData2)
        revals$peakData2 <- applyFilt(filterMass, revals$peakData2, min_mass = as.numeric(input$min_mass), 
                               max_mass = as.numeric(input$max_mass))
        rm(filterMass)
        incProgress(1/n_filters, detail = 'Mass filter done.')
      }
      
      # Apply molecule filter
      if (input$molfilter) {
        
        # Create and apply molecule filter to nonreactive peakData object
        filterMols <- molecule_filter(revals$peakData2)
        revals$peakData2 <- applyFilt(filterMols, revals$peakData2, min_num = as.integer(input$minobs))
        rm(filterMols)
        incProgress(1/n_filters, detail = 'Molecule filter done.')
      } # End molecule filter if statement
      
      # Apply formula filter
      if (input$formfilter){
        filterForm <- formula_filter(revals$peakData2)
        revals$peakData2 <- applyFilt(filterForm, revals$peakData2)
        rm(filterForm)
        incProgress(1/n_filters, detail = 'Formula filter done.')
      }
      
      # Apply custom filters
      if (input$customfilterz){
        
          #apply the filter for each input
          for(i in 1:3){
            
            #require that a selection has been made for filter i
            if (input[[paste0("custom",i)]] == "Select item") return(NULL)
            
            #make the filter based on selection
            filter <- emeta_filter(revals$peakData2, input[[paste0("custom",i)]])
            
            # if numeric, apply filter with specified max and min values
            if (is.numeric(revals$peakData2$e_meta[,input[[paste0("custom",i)]]])){
              req(input[[paste0("minimum_custom",i)]], input[[paste0("maximum_custom", i)]])
              revals$peakData2 <- applyFilt(filter, revals$peakData2,
                                     min_val = input[[paste0("minimum_custom",i)]], 
                                     max_val = input[[paste0("maximum_custom", i)]], 
                                     na.rm = !input[[paste0("na_custom",i)]])
              
            }
            # else apply with selected categories
            else if (!is.numeric(revals$peakData2$e_meta[,input[[paste0("custom",i)]]])){
              req(input[[paste0("categorical_custom",i)]])
              revals$peakData2 <- applyFilt(filter, revals$peakData2, 
                                     cats = input[[paste0("categorical_custom",i)]], 
                                     na.rm = !input[[paste0("na_custom",i)]])
            }
            
            rm(filter)
            incProgress(1/n_filters, detail = sprintf('Custom filter %s done', i))
          }
          
        }
    })
    # display success modal
    showModal(
      modalDialog(title = "Filter Success",
                  fluidRow(
                    column(10, align = "center", offset = 1,
                           HTML('<h4 style= "color:#1A5276">Your data has been filtered.</h4>
                              <h4 style= "color:#1A5276">The filtered data is stored and will be reset if you re-upload or re-process data.</h4>'),
                           hr(),
                           actionButton("filter_dismiss", "Review Results", width = '75%'),
                           br(),
                           br(),
                           actionButton("goto_viz", "Continue to Visualization", width = '75%')
                    )
                  )
                  ,footer = NULL)
    )
    
    #__test-export__
    exportTestValues(peakData2 = revals$peakData2)
  
  }) # End creating revals$peakData2
  
  #### Main Panel (Filter Tab) ####

  # Show table from summaryFilt
  # Depends on: summaryFilterDataFrame
  output$summary_filter <- renderTable({
    
    # Set default results: NA if no filters selected
    afterResults <- c(NA, NA, NA, NA)
    last_filt_ind <- max(which(summaryFilterDataFrame()[,'dispText'] != 'NA'))
    afterResults <- unlist(summaryFilterDataFrame()[last_filt_ind, c('sum_peaks', 'assigned', 'min_mass', 'max_mass')])
    
    # Find which row in summaryFilterDataFrame represents the Unfiltered information
    rowNum = which(summaryFilterDataFrame()$data_state == 'Unfiltered')
    
    # Create a dataframe out of Before and After results from summaryFilterDataFrame
    summary_table <- data.frame('Before' = as.numeric(unlist(summaryFilterDataFrame()[rowNum, c('sum_peaks', 'assigned', 
                                                                                     'min_mass', 'max_mass')])),
                                'After' = as.numeric(afterResults),
                                row.names = c('Number of peaks',
                                              'Number of peaks assigned a formula', 
                                              'Minimum mass observed', 
                                              'Maximum Mass observed'), stringsAsFactors = FALSE)
    
    # Format the last two rows of this table to have decimal places and the first two rows to have a comma
    # this requires converting the table to a string, keep two copies in case the string changes
    display_table <- summary_table
    
    display_table[1:2, 1] <- formatC(round(summary_table[1:2,1]), big.mark = ",", format = "d")
    display_table[1:2, 2] <- formatC(round(summary_table[1:2,2]), big.mark = ",", format = "d")
    display_table[3:4, 1] <- formatC(round(summary_table[3:4, 1], digits = 4), format = "f", big.mark = ",")
    display_table[3:4, 2] <- formatC(round(summary_table[3:4, 2], digits = 4), format = "f", big.mark = ",")
    
    #___test-export___
    exportTestValues(rem_peaks = as.numeric(summaryFilterDataFrame()[last_filt_ind, 'sum_peaks']))
    
    return(display_table)
  }, # End code portion of summary_filter
  
  # Options: include rownames, no decimal places
  rownames = TRUE
  ) # End summary_filter
  
  # Plot bar chart
  # Depends on: summaryFilterDataFrame
  output$barplot_filter <- renderPlot({
    req(isolate(revals$redraw_filter_plot) == TRUE | (isolate(uploaded_data_dim()) > max_cells))
    
    filter_inds <- c(TRUE, isolate(input$samplefilter) & length(isolate(input$keep_samples)) > 0, isolate(input$massfilter), isolate(input$molfilter), isolate(input$formfilter), 
                     any(c(isolate(input$custom1), isolate(input$custom2), isolate(input$custom3)) != "Select item") & isolate(input$customfilterz))
    
    which_filts <- c("Unfiltered", "After Sample Filter", "After Mass Filter", "After Molecule Filter", "After Formula Filter", "After Custom Filters")[filter_inds]
    
    # Melt dataframe into 2 objects
    ggdata_barplot <- melt(summaryFilterDataFrame()[,c('data_state', 'assigned', 'unassigned')]) %>% filter(data_state %in% which_filts)
    ggdata_text <- summaryFilterDataFrame()[, c('data_state', 'sum_peaks', 'dispText')] %>% filter(data_state %in% which_filts)
    
    # Aesthetic purposes: get max height, divide by 30, use as offset in geom_text
    num_displaced <- round(ggdata_text[1, 2] / 35, digits = -1)
    
    shinyjs::hide('draw_large_filter_plot')
    
    # Plot using ggplot2
    ggplot() + geom_bar(aes(x = data_state, 
                            y = value, 
                            fill = variable), data = ggdata_barplot, stat = 'identity') +
      theme_bw(base_size = 16) + geom_text(data = ggdata_text, 
                                           aes(x = data_state, 
                                               y = sum_peaks + num_displaced, 
                                               label = dispText), size = 6) + 
      scale_fill_brewer(name = 'Peak Type', labels = c('Formulae Assigned', 
                                                       'Formulae Unassigned'), palette="Blues") + 
      labs(x = 'Data State', y = 'Number of peaks') 
      
  }) # End barplot_filter #
  
  
  
  ####### Visualize Tab #######
  
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
  source('srv_ui_elements/visualize_UI_plot_opts.R', local = TRUE)
  # warnings_visualize: displays warning messages in revals$warningmessage_visualize
  # chooseplots_icon, axlabs_icon, saveplots_icon: icons for collapse panels
  source('srv_ui_elements/visualize_UI_misc.R', local = TRUE)
  #
  
  ## Sidebar Panel ##
  # Plot options, with selections removed if the necessary columns in e_meta are not present.
  output$plot_type <- renderUI({
    input$top_page
    validate(need(revals$peakData2, "A peakData object was not found, please check that you have successfully uploaded data"))
    req(input$top_page == "Visualize")
    
    choices <- c('Van Krevelen Plot', 'Kendrick Plot', 'Density Plot', 'Custom Scatter Plot', 'PCOA Plot')
    
    # disallow kendrick plots if either kmass or kdefect not calculated/present in emeta
    if (is.null(attr(revals$peakData2, "cnames")$kmass_cname) | is.null(attr(revals$peakData2, "cnames")$kdefect_cname)){
      choices <- choices[choices != "Kendrick Plot"]
    }
    
    # disallow vk plots if o:c or h:c ratios not calculated/present in emeta or only contain zeros/NA's
    if (is.null(attr(revals$peakData2, "cnames")$o2c_cname) | is.null(attr(revals$peakData2, "cnames")$h2c_cname)){
      choices <- choices[choices != "Van Krevelen Plot"]
    }
    else if(any(all(revals$peakData2$e_meta[[attr(revals$peakData2, "cnames")$o2c_cname]] %in% c(0,NA)),
            all(revals$peakData2$e_meta[[attr(revals$peakData2, "cnames")$h2c_cname]] %in% c(0,NA)))){
      choices <- choices[choices != "Van Krevelen Plot"]
    }
    
    # disallow density plots if there are no numeric columns
    if (!any(sapply(revals$peakData2$e_meta %>% dplyr::select(-one_of(getEDataColName(revals$peakData2))), is.numeric))){
      choices <- choices[choices != c("Density Plot", "Custom Scatter Plot")]
    }
    
    # disallow custom scatter plot if we have large data
    if (peakData2_dim() > max_cells){
      choices <- choices[choices != 'Custom Scatter Plot']
    }
    
    # disallow pcoa plot for 1 sample data
    if (nrow(revals$peakData2$f_data) < 2){
      choices <- choices[choices != 'PCOA Plot']
    }
    
    #if everything is disallowed, give warning and silently stop execution.
    if (length(choices) == 0) return(tags$p("There is not enough information in the molecular identification file to produce any plots.  Choose more variables to calculate in the preprocess tab or append some metadata to the molecular identification file prior to uploading", style = "color:gray"))
    
    selectInput('chooseplots', 'Choose a plot type',
                  choices = choices, 
                  selected = 0
    )
  })
  
  # Logic to force single sample selection in the case where only 1 sample is present
  output$plotUI <- renderUI({
    req(!is.null(input$chooseplots))
    if (nrow(revals$peakData2$f_data) == 1 | (input$chooseplots %in% c('Custom Scatter Plot', 'PCOA Plot'))){
      return(tagList(
        tags$div(class = 'grey_out',
          hidden(selectInput('choose_single', 'I want to plot using:',
                    choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples by group' = 2, 
                                'A comparison of groups' = 3, 'A comparison of two samples' = 4),
                    selected = 1))
          )
        #tags$p("No grouping options for custom scatter plots and single sample datasets.", style = "color:gray;font-size:small;margin-top:3px")
      ))
    }
    else {
      return(selectInput('choose_single', 'I want to plot using:',
                         choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples' = 2,
                                     'A comparison of groups' = 3, 'A comparison of two samples' = 4),
                         selected = 0))
    }
  })
  
  # pcoa distance metric dropdown
  output$pcoa_dist <- renderUI({
    req(input$chooseplots=='PCOA Plot')
    dist_choices = c('manhattan', 'euclidean', 'canberra', 'clark', 'bray', 'kulczynski', 
                    'jaccard', 'gower', 'altGower', 'morisita', 'horn', 'mountford', 'raup', 
                    'binomial', 'cao','mahalanobis')
    
    selectInput('choose_dist', "Choose a distance metric", choices = dist_choices, selected = 'bray')
  })
  
  # select groups to color by
  output$viztab_select_groups <- renderUI({
    req(input$chooseplots=='PCOA Plot')
    pickerInput("viztab_select_groups", "Select groups:", 
                choices = names(revals$groups_list),
                multiple = TRUE
    )
  })
  
  ## UI outputs for group/sample comparisons ##
  ## conditional display depending on whether comparing two samples or two groups ## 
  output$plotUI_comparison_1 <- renderUI({
    req(input$choose_single != 0, !is.null(input$chooseplots))
    if(input$choose_single == 3){
      choice_diff <- setdiff(names(revals$groups_list), isolate(input$whichGroups2))
      name_label = HTML("<input type = 'text' id = 'group1_name' placeholder = 'Group 1' style = 'border-style:solid;border-width:1px;'/>")
      pickerInput('whichGroups1', name_label,
              choices = choice_diff,
              selected = if(is.null(isolate(revals$group_1))) choice_diff[1] else isolate(revals$group_1),
              options =  pickerOptions(dropupAuto = FALSE, actionsBox = TRUE))
    }
    else if(input$choose_single == 4){
      choice_diff <- setdiff(colnames(revals$peakData2$e_data[-which(colnames(revals$peakData2$e_data) == getEDataColName(revals$peakData2))]), isolate(input$whichSample2))
      pickerInput('whichSample1', "Sample 1:",
                  choices = choice_diff,
                  selected = if(is.null(isolate(revals$whichSample1))) choice_diff[1] else isolate(revals$whichSample1),
                  options =  pickerOptions(dropupAuto = FALSE))
    }
  })
    
  output$plotUI_comparison_2 <- renderUI({
    req(input$choose_single != 0, !is.null(input$chooseplots))
    if(input$choose_single == 3){
      choice_diff <- setdiff(names(revals$groups_list), isolate(input$whichGroups1))
      name_label = HTML("<input type = 'text' id = 'group2_name' placeholder = 'Group 2' style = 'border-style:solid;border-width:1px;'/>")
      pickerInput("whichGroups2", name_label, 
              choices = setdiff(names(revals$groups_list), isolate(input$whichGroups1)),
              selected = if(is.null(isolate(revals$group_2))) choice_diff[2] else isolate(revals$group_2),
              options =  pickerOptions(dropupAuto = FALSE, actionsBox = TRUE))
    }
    else if(input$choose_single == 4){
      choice_diff <- setdiff(colnames(revals$peakData2$e_data[-which(colnames(revals$peakData2$e_data) == getEDataColName(revals$peakData2))]), isolate(input$whichSample1))
      pickerInput("whichSample2", "Sample 2:", 
                  choices = choice_diff,
                  selected = if(is.null(isolate(revals$whichSample2))) choice_diff[2] else isolate(revals$whichSample2),
                  options =  pickerOptions(dropupAuto = FALSE))
    }
  })
  
  ##
  
  # UI output for single sample or single group
  output$plotUI_single <- renderUI({
    req(input$choose_single != 0, !is.null(input$chooseplots), input$chooseplots != 'PCOA Plot')
    if(input$choose_single == 2){
      tagList(
          div(id = "js_whichSamples",
              pickerInput('whichSamples', 'Grouped Samples',
                      choices = colnames(revals$peakData2$e_data %>% dplyr::select(-one_of(getEDataColName(revals$peakData2)))),
                      multiple = TRUE, selected = isolate(revals$single_group), 
                      options =  pickerOptions(dropupAuto = FALSE, actionsBox = TRUE))),
          conditionalPanel(
            condition = 'input.whichSamples.length < 2',
            tags$p("Please select at least 2 samples", style = "color:gray")
          )# End conditional output multiple samples#
        )
    }
    else return(div(id = "js_whichSamples", selectInput('whichSamples', 'Sample', 
                                                        choices = colnames(revals$peakData2$e_data %>% dplyr::select(-one_of(getEDataColName(revals$peakData2)))), 
                                                        selected = revals$single_sample)))
  })
  
  # selector for summary funcion
  output$summary_fxn_out <- renderUI({
    text_pres_fn <- "For a given peak, should the count or proportion of nonmissing values across samples in a group be used to determine whether or not that peak is present/absent within the group"
    text_test <- HTML("<p>Should a G-test or presence absence thresholds be used to determine whether a sample is unique to a particular group?</p><p>Depending on your selection, you will be asked for a presence threshold and a p-value (G-test) or a presence AND absence threshold<p/>") 
    
    # density plot has group summary options disabled
    if (isTRUE(input$chooseplots == "Density Plot")){
      summary_dropdown <- tags$div(class = "grey_out",
                                   tags$p("No summary functions for comparison density plots", style = "color:gray;font-size:small;margin-top:3px;font-weight:bold"),
                                   hidden(
                                     radioButtons("pres_fn", 
                                                  div("Determine presence/absence by:", div(style = "display:inline-block;right:5px", tipify(icon("question-sign", lib = "glyphicon"), title = text_pres_fn, placement = "top", trigger = 'hover'))), 
                                                  choices = c("No. of Samples Present" = "nsamps", "Proportion of Samples Present" = "prop"), inline = TRUE, selected = "nsamps")
                                    ),
                                   
                                   hidden(selectInput("summary_fxn", 
                                                        div("Determine uniqueness using:", div(style = "display:inline-block", tipify(icon("question-sign", lib = "glyphicon"), title = text_test, placement = "top", trigger = 'hover'))), 
                                                        choices = c("Select one" = "select_none", "G test" = "uniqueness_gtest", "Presence/absence thresholds" = "uniqueness_nsamps"), selected = "select_none")),
                                   hidden(numericInput("pres_thresh", "Presence threshold", value = 1, step = 0.1)),
                                   hidden(numericInput("absn_thresh", "Absence threshold", value = 0, step = 0.1)),
                                   hidden(numericInput("pval", "p-value", min = 0, max = 1, value = 0.05, step = 0.1))
      )
     # non-density plots 
    }else{ 
      summary_dropdown <- tagList(
        hr(style = 'margin-top:2px;height:3px;background-color:#1A5276'),
        radioButtons("pres_fn", 
                     div("Determine presence/absence by:", div(style = "color:deepskyblue;display:inline-block", tipify(icon("question-sign", lib = "glyphicon"), title = text_pres_fn, placement = "top", trigger = 'hover'))), 
                     choices = c("No. of Samples Present" = "nsamps", "Proportion of Samples Present" = "prop"), inline = TRUE, selected = "nsamps"),
        
        hr(style = "margin-top:2px"),
        
        div(id = "js_summary_fxn", selectInput("summary_fxn", 
                                              div("Determine uniqueness using:", div(style = "color:deepskyblue;display:inline-block", tipify(icon("question-sign", lib = "glyphicon"), title = text_test, placement = "top", trigger = 'hover'))), 
                                              choices = c("Select one" = "select_none", "G test" = "uniqueness_gtest", "Presence/absence thresholds" = "uniqueness_nsamps"))),
        
        splitLayout(class = "squeezesplitlayout", 
          div(id = "js_pres_thresh", numericInput("pres_thresh", "Presence threshold", value = 1, step = 0.1)),
          div(id = "js_absn_thresh", numericInput("absn_thresh", "Absence threshold", value = 0, step = 0.1)),
          div(id ="js_pval", numericInput("pval", "p-value", min = 0, max = 1, value = 0.05, step = 0.1))
        )
      )}
    return(summary_dropdown)
  })
  
  #### Main Panel (Visualize Tab) ####
  
  # When plot_data() is recalculated, repopulate the dropdowns under the plot.  Specifically vk_colors and custom scatterplot options.
  observeEvent(plot_data(),{
    
    # store test value
    if (isTRUE(getOption("shiny.testmode"))) {
      revals$plot_data_export <- plot_data()
    }
    
    ## ifelse block determines how to populate vk_colors dropdown
    
    # Density plots care not for choose_single!!!!
    if (input$chooseplots == "Density Plot"){
      numeric_cols <- which(sapply(plot_data()$e_meta %>% 
                                     dplyr::select(one_of(emeta_display_choices())), is.numeric))
      color_by_choices <- emeta_display_choices()[numeric_cols]
    }
    else if (input$choose_single == 1){
      color_by_choices <- emeta_display_choices()
      
      if (input$chooseplots == "Van Krevelen Plot"){
        color_by_choices <- switch(input$vkbounds,
                                   'bs1' = c('Van Krevelen Boundary Set' = 'bs1', color_by_choices),
                                   'bs2' = c('Van Krevelen Boundary Set' = 'bs2', color_by_choices),
                                   "0" = c('Van Krevelen Boundary Set 1' = 'bs1', 'Van Krevelen Boundary Set 2' = 'bs2', color_by_choices))
      }
    }
    else if (input$choose_single == 2) {
      
      # create vector of color choices by combining unique elements from e_data and e_meta
      edata_colors <- plot_data()$e_data %>% dplyr::select(-one_of(getEDataColName(plot_data()))) %>% colnames()
      color_by_choices <- c(edata_colors[!(edata_colors %in% emeta_display_choices())], emeta_display_choices())
      
    } else if (input$choose_single %in% c(3,4)) {
      color_by_choices <- c("Group membership" = input$summary_fxn)
    }
    
    # Give default names to unnamed choices
    names(color_by_choices) <- sapply(1:length(color_by_choices), function(i){
      ifelse(names(color_by_choices[i]) == "" | is.null(names(color_by_choices[i])),
             yes = color_by_choices[i],
             no = names(color_by_choices[i]))
    })
    
    # if statements which prevent plot from resetting colors/axes when plot is redrawn.
    selected = color_by_choices[1]
    
    if (input$vk_colors %in% color_by_choices){
      selected <- input$vk_colors
    }
    
    selected_x = color_by_choices[color_by_choices != selected][1]
    selected_y = color_by_choices[!(color_by_choices %in% c(selected, selected_x))][1]
    
    if ((input$scatter_x %in% color_by_choices) & (input$scatter_y %in% color_by_choices)){
      selected_x <- input$scatter_x
      selected_y <- input$scatter_y
    }
    
    # Density Colors
    if (input$chooseplots == 'Density Plot') {
      updateSelectInput(session, 'vk_colors', 'Plot Distribution of Variable:', 
                        choices = color_by_choices,
                        selected = selected)
    }
    
    # Kendrick Colors
    if (input$chooseplots == 'Kendrick Plot') {
      updateSelectInput(session, 'vk_colors', 'Color by:',
                        choices = color_by_choices,
                        selected = selected)
    }
    
    # Van Krevelen Colors
    if (input$chooseplots == 'Van Krevelen Plot') {
      updateSelectInput(session, 'vk_colors', 'Color by:',
                        choices = color_by_choices,
                        selected = selected)
    }
    
    if (input$chooseplots %in% c('Custom Scatter Plot', 'PCOA Plot')) {
      # allow only numeric columns for the axes but keep categorical coloring options
      numeric_cols <- which(sapply(full_join(plot_data()$e_meta, plot_data()$e_data) %>% dplyr::select(color_by_choices), is.numeric))
      
      # maintain exclusivity of colorby and x-y variables only in scatterplot
      if(input$chooseplots == 'Custom Scatter Plot'){
        axes_choices <- revals$axes_choices <- color_by_choices[numeric_cols]
        
        updateSelectInput(session, 'scatter_x', "Horizontal axis variable:",
                          choices = axes_choices[!(axes_choices %in% c(input$scatter_y, input$vk_colors))],
                          selected = selected_x)
        updateSelectInput(session, "scatter_y", "Vertical axis variable:",
                          choices = axes_choices[!(axes_choices %in% c(input$scatter_x, input$vk_colors))],
                          selected = selected_y)
        updateSelectInput(session, 'vk_colors', 'Color  by:',
                          choices = color_by_choices[!(color_by_choices %in% c(input$scatter_x, input$scatter_y))],
                          selected = selected)
      }
      else if(input$chooseplots == 'PCOA Plot'){
        axes_choices <- 1:min(5, ncol(revals$peakData2$e_data)-2)
        names(axes_choices) <- paste0('PC', axes_choices)
        selected_x <- 1
        selected_y <- 2
      }
    }
    
    revals$color_by_choices <- color_by_choices
    
    # The dropdown value will not be updated if this if statement's condition is true, force re-execution of plotting in this case with a reactive var
    if (input$vk_colors %in% color_by_choices){
      revals$makeplot <- -revals$makeplot
    }
    
  }, priority = 9)
  
  # Main plotting output #
  output$FxnPlot <- renderPlotly({
    req(!is.null(input$chooseplots))
    
    # reactive dependencies
    input$update_axes
    input$vk_colors
    input$colorpal
    revals$makeplot #in case vk_colors does not change we still want to redraw the plot.
    
    disable('plot_submit')
    on.exit({
      enable('plot_submit')
      enable("add_plot")
      })
    
    # for testing if plot actually got updated in test mode
    exportTestValues(plot = NULL, plot_attrs = NULL)
    isolate({  
      if (v$clearPlot){
        return(NULL)
      } 
      else {
        # Make sure a plot stype selection has been chosen
        validate(need(input$choose_single != 0, message = "Please select plotting criteria"))
        
        revals$legendTitle = ifelse(is.null(input$legend_title_input) || (input$legend_title_input == ""),
                             yes = names(revals$color_by_choices[revals$color_by_choices == input$vk_colors]),
                             no = input$legend_title_input
        )
        
        # Apply custom color scale if numeric is selected
        if (numeric_selected() & !(input$vk_colors %in% c("bs1", "bs2"))){
          diverging_options = c("RdYlGn")
          pal <- RColorBrewer::brewer.pal(n = 9, input$colorpal)
          
          # diverging_options specify color palletes that look weird if they are truncated: [3:9], only truncate the 'normal' ones
          if (!(input$colorpal %in% diverging_options)){
            pal <- RColorBrewer::brewer.pal(n = 9, input$colorpal)[3:9]
          }
          
          # flip the color scale on button click
          if (input$flip_colors %% 2 != 0){
            pal <- rev(pal)
          }
          
          # get domain and obtain color pallette function
          domain = range(plot_data()$e_meta[,input$vk_colors], na.rm = TRUE)
          colorPal <- scales::col_numeric(pal, domain)
          #revals$colorPal <- paste(paste(pal, collapse = ","), paste(domain, collapse = ","), sep = ":")
        }
        else if(!(input$choose_single %in% c(3,4)) & !(input$vk_colors %in% c("bs1", "bs2"))){
          # if there are too many categories, warn user and provide color palette
          if(length(unique(plot_data()$e_meta[, input$vk_colors])) > 12){
            ramp <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))
            pal <- ramp(length(unique(plot_data()$e_meta[, input$vk_colors])))
            colorPal <- scales::col_factor(pal, domain = unique(plot_data()$e_meta[, input$vk_colors]))
          }
          else colorPal <- NA
          #revals$colorPal <- NA
        }
        else if(input$choose_single %in% c(3,4)){
          pal = switch(input$colorpal,
                       'default' = c("#7fa453", "#a16db8", "#cb674a"), 'bpr' = c("#0175ee", '#7030A0', "#fd003d"),
                       'neutral' = c("#FC8D59", '#7030A0', "#91CF60"), 'bpg' = c('#8377cb', '#c95798', '#60a862'),
                       'rblkgn' = c('red', 'black', 'green'))
          
          # still allow color_inversion, even though it looks weird
          if (input$flip_colors %% 2 != 0){
            pal <- rev(pal)
          }
          
          pal <- pal[c(1,3,2)] # dont ask
          
         domain <- unique(plot_data()$e_data[,which(grepl('^uniqueness', colnames(plot_data()$e_data)))])
         domain <- domain[which(!is.na(domain))]
         colorPal <- scales::col_factor(pal, domain = domain)
        }
        else colorPal <- NA # just in case....
        
        #----------- Single sample plots ------------#
        #-------Kendrick Plot-----------# 
        if (input$chooseplots == 'Kendrick Plot') {
          validate(need(!is.null(input$whichSamples) | !(is.null(g1_samples()) & is.null(g2_samples())), message = "Please select at least 1 sample"))
          p <- kendrickPlot(plot_data(), colorCName = input$vk_colors, colorPal = colorPal,
                            xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                            title = input$title_input,legendTitle = revals$legendTitle)
          
          if (input$vk_colors %in% c('bs1', 'bs2')) {
            p <- kendrickPlot(plot_data(), vkBoundarySet = input$vk_colors,
                              xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                              title = input$title_input,legendTitle = revals$legendTitle)
          } else {
            # if color selection doesn't belong to a boundary, color by test
            p <- kendrickPlot(plot_data(), colorCName = input$vk_colors, colorPal = colorPal,
                              xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                              title = input$title_input,legendTitle = revals$legendTitle)
          }
        }
        #-------VanKrevelen Plot--------#
        if (input$chooseplots == 'Van Krevelen Plot') {
          validate(need(!is.null(input$whichSamples) | !(is.null(g1_samples()) & is.null(g2_samples())), message = "Please select at least 1 sample"))
          if (input$vkbounds == 0) { #no bounds
            # if no boundary lines, leave the option to color by boundary
            if (input$vk_colors %in% c('bs1', 'bs2')) {
              p <- vanKrevelenPlot(plot_data(), showVKBounds = FALSE, vkBoundarySet = input$vk_colors,
                                   xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                   title = input$title_input,legendTitle = revals$legendTitle)
            } else {
              # if no boundary lines and color selection doesn't belong to a boundary, color by test
              p <- vanKrevelenPlot(plot_data(), showVKBounds = FALSE, colorCName = input$vk_colors, colorPal = colorPal,
                                   xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                   title = input$title_input,legendTitle = revals$legendTitle)
            }
          } else {
            # if boundary lines, allow a color by boundary class 
            if (input$vk_colors %in% c('bs1', 'bs2')) {
              p <- vanKrevelenPlot(plot_data(), vkBoundarySet = input$vkbounds, showVKBounds = TRUE,
                                   xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                   title = input$title_input,legendTitle = revals$legendTitle)
            } else {
              # if boundary lines and color isn't a boundary class
              p <- vanKrevelenPlot(plot_data(), vkBoundarySet = input$vkbounds, showVKBounds = TRUE, 
                                   colorCName = input$vk_colors, colorPal = colorPal,
                                   xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                   title = input$title_input,legendTitle = revals$legendTitle)
            }
          }
        }
        
        #--------- Density Plot --------#
        if (input$chooseplots == 'Density Plot') {
          validate(need(!is.null(input$whichSamples) | !(is.null(g1_samples()) & is.null(g2_samples())), message = "Please select at least 1 sample"),
                   need(!is.na(input$vk_colors), message = "Please select a variable")
          )
          
          # sample/group inputs depending on whether or not we are doing a comparison of groups
          if (input$choose_single %in% c(3,4)){
            samples = FALSE
            groups = unique(attr(plot_data(), "group_DF")$Group)
          }
          else if (input$choose_single == 2){
            samples = input$whichSamples
            groups = "Group"
          }
          else if (input$choose_single == 1){
            samples = input$whichSamples
            groups = FALSE
          }
          
          # if x axis input field is empty, get the display name of the color_by_choices vector index that equals vk_colors, otherwise use what the user typed
          xlabel = ifelse(is.null(input$x_axis_input) || input$x_axis_input == "",
                          yes = names(revals$color_by_choices[revals$color_by_choices == input$vk_colors]),
                          no = input$x_axis_input)
          
          p <- densityPlot(plot_data(),variable = input$vk_colors, samples = samples, groups = groups,
                           plot_hist = ifelse(input$choose_single == 1, TRUE, FALSE), 
                           xlabel = xlabel, ylabel = input$y_axis_input, title = input$title_input)
        }
        
        #---------- Custom Scatter Plot --------#
        if (input$chooseplots == 'Custom Scatter Plot'){
          validate(need(!is.null(input$whichSamples), message = "Please select at least 1 sample"),
                   need(!is.na(input$vk_colors), message = "Please select a variable to color by"))
          req(!is.null(input$scatter_x), !is.null(input$scatter_y), !("" %in% c(input$scatter_x, input$scatter_y)))
          
          p <- scatterPlot(plot_data(), input$scatter_x, input$scatter_y, colorCName = input$vk_colors, colorPal = colorPal,
                           xlabel = ifelse(is.null(input$x_axis_input) | (input$x_axis_input == ""), 
                                                   yes = names(revals$color_by_choices[revals$color_by_choices == input$scatter_x]), 
                                                   no = input$x_axis_input), 
                           ylabel = ifelse(is.null(input$y_axis_input) | (input$y_axis_input == ""), 
                                                   yes = names(revals$color_by_choices[revals$color_by_choices == input$scatter_y]), 
                                                   no = input$y_axis_input),
                           title = input$title_input, legendTitle = revals$legendTitle)
          
        }
        #----------- PCOA Plot ----------#
        if(input$chooseplots==('PCOA Plot')){
          # maximum of 5 pcs or the number of samples - 2 (#columns - ID column - 1)
          xlabel = ifelse(is.null(input$x_axis_input) | (input$x_axis_input == ""), 
                                  yes = paste0('PC ', input$scatter_x), 
                                  no = input$x_axis_input)
          ylabel = ifelse(is.null(input$y_axis_input) | (input$y_axis_input == ""), 
                                  yes = paste0('PC ', input$scatter_y), 
                                  no = input$y_axis_input)
          
          pcs <- getPrincipalCoordinates(plot_data(), n_dims = min(5, ncol(plot_data()$e_data)-2), dist_metric = input$choose_dist)
          p <- plotPrincipalCoordinates(pcs, title = input$title_input, x=as.numeric(input$scatter_x), y=as.numeric(input$scatter_y), 
                                        xlabel = xlabel, ylabel=ylabel,
                                        ftmsObj = plot_data(), size = 10)
        }
      }
    })
    # Axes Options
    f <- list(family = "Arial", size = 18, color = "#7f7f7f")
    
    x <- y <- list(titlefont = f)
  
    p <- p %>% layout(xaxis = x, yaxis = y, titlefont = f)
    
    # Null assignment bypasses plotly bug
    p$elementId <- NULL
  
    # ___test-export___
    exportTestValues(plot = p, plot_attrs = p$x$attrs[[p$x$cur_data]], plot_layout = p$x$layout, plot_visdat = p$x$visdat[[p$x$cur_data]]())
    
    # inspect <<- p
    
    revals$current_plot <- p
    
    # use webGL for large plots
    if(peakData2_dim() > max_cells & !(input$chooseplots == 'Density Plot')){
      p <- p %>% layout(margin = list(b = 50, l = 75)) # I dont know why but webGL crops axes titles, must reset
      p <- toWebGL(p)
    }
    
    return(p)
  })
  # END FXNPLOT
  
  #-------- create a table that stores plotting information -------#
  # the table needs to grow with each click of the download button
  parmTable <- reactiveValues()
  # need to initialize the table and fill in values
  parmTable$parms <- data.frame("File Name" = NA, "Plot Type" = NA, "Sample Type" = NA, "Group 1 Samples" = NA, "Group 2 Samples" = NA, "Boundary Set" = NA,
                                "Color By Variable" = NA, "X Variable" = NA, "Y Variable" = NA, "Presence Threshold" = NA, "Absence Threshold" = NA, "P-Value" = NA,
                                "Comparisons Method" = NA, check.names = FALSE)
  
  observeEvent(input$add_plot, {

    # counter which begins at 1 even if a filter reset has occurred.
    ind <- input$add_plot + input$add_qc_boxplot - revals$reset_counter
    
    # initialize a new line
    newLine <- data.frame(FileName = NA, PlotType = NA, SampleType = NA, Group_1_Samples = NA,  Group_2_Samples = NA, BoundarySet = NA,
                          ColorBy = NA, x_var = NA, y_var = NA, pres_thresh = NA, absn_thresh = NA, pval = NA, compfn = NA)
                          
    # fill values to a position depending on input$add_plot
    
    # which type of plot
    newLine$FileName <- ifelse(is.na(input$title_input) | input$title_input == '', paste0('Plot_', ind), paste0('Plot_', ind, '_', input$title_input))
    newLine$PlotType <- input$chooseplots
    # Single or Multiple Samples
    newLine$SampleType <- ifelse(input$chooseplots == "PCOA Plot", 'None',
                             switch(as.character(input$choose_single), '1' = 'Single Sample', '2' = 'Single Group of Samples', '3' = 'Comparison of Two Groups', '4' = 'Comparison of Two Samples')
                          )
    # Sample(s) in The first group (depends on input$choose_single to decide if this is a single or multiple sample list)
    newLine$Group_1_Samples <- ifelse(input$choose_single %in% c(1,2), yes = paste(input$whichSamples, collapse = ','), no = paste(g1_samples(), collapse = ','))
    # Sample(s) in the second group. Automatically NA if input$choose_single is single sample or single group
    newLine$Group_2_Samples <- ifelse(input$choose_single %in% c(3,4), yes =  paste(g2_samples() , collapse = ','), no = 'None')
    # Boundary set borders to use (NA for non-Van Krevelen plots)
    newLine$BoundarySet <- ifelse(input$chooseplots == 'Van Krevelen Plot', yes = ifelse(input$vkbounds == 0, 'None', input$vkbounds), no = 'None')
    newLine$ColorBy <- ifelse(input$chooseplots == 'PCOA Plot', 'None', input$vk_colors)
    newLine$x_var <- input$scatter_x
    newLine$y_var <- input$scatter_y
    
    newLine$x_var <- switch(input$chooseplots, 'Van Krevelen Plot' = 'O:C Ratio', 'Kendrick Plot' = 'Kendrick Mass', 
                                               'Density Plot' = input$vk_colors, 'Custom Scatter Plot' = input$scatter_x,
                                               'PCOA Plot' = paste0('Principal Component ', input$scatter_x))
    newLine$y_var <- switch(input$chooseplots, 'Van Krevelen Plot' = 'H:C Ratio', 'Kendrick Plot' = 'Kendrick Defect', 
                                               'Density Plot' = 'Density', 'Custom Scatter Plot' = input$scatter_y,
                                               'PCOA Plot' = paste0('Principal Component ', input$scatter_y))
    
    
    newLine$compfn <- ifelse(isTRUE(input$choose_single %in% c(3,4)) & isTRUE(input$summary_fxn != ""), 
                             switch(input$summary_fxn,
                                    "select_none" = "None", 
                                    "uniqueness_gtest" = "G test", 
                                    "uniqueness_nsamps" = "Presence/absence thresholds",
                                    "uniqueness_prop" = "Presence/absence thresholds"),
                             no = "None")
    
    # special storage options for single and two-group plots
    if (input$choose_single == 2){
      # store edata_result of summarizeGroups()
      revals$plot_data[[ind]] <- plot_data()$e_data 
    }
    
    if (input$choose_single %in% c(3,4)){
      # store edata result of summarizeGroupComparisons()
      revals$plot_data[[ind]] <- plot_data()$e_data 
      
      # parameters specific to group comparison plots
      newLine$pres_thresh <- input$pres_thresh
      newLine$absn_thresh <- input$absn_thresh
      newLine$pval <- input$pval
    }
    
    # Prettified colnames
    colnames(newLine) <- c("File Name", "Plot Type", "Sample Type", "Group 1 Samples", "Group 2 Samples", "Boundary Set",
    "Color By Variable", "X Variable", "Y Variable", "Presence Threshold", "Absence Threshold", "P-Value", "Comparisons Method")
    
    if (ind == 1) {
      # replace the existing line on the first click
      parmTable$parms[ind, ] <- newLine
      exportTestValues(parmTable_1 = parmTable$parms)
    } else {
      # concat every new line after
      parmTable$parms <- rbind(parmTable$parms, newLine)
      exportTestValues(parmTable_1 = parmTable$parms)
    }
    
    # store the current plot in a reactiveValue for later download
    revals$plot_list[[ind]] <- revals$current_plot
    
    # dont save the plot twice
    disable('add_plot')
    
  }, priority = 7)
  
  # render the above table
  output$parmsTable <- renderDataTable(parmTable$parms, options = list(scrollX = TRUE))
  
  # End Visualize tab #
  
  
  ####### Download Tab #######
  
  source("Observers/download_observers.R", local = TRUE)
  source("helper_functions/report.R", local = TRUE)
  # copy the table from the visualize tab so as not to confuse javascript
  output$parmsTable2 <- DT::renderDataTable(parmTable$parms,
                                            options = list(scrollX = TRUE),
                                            server = TRUE)
  
  # print the selected indices
  output$x4 = renderPrint({
    s = input$parmsTable2_rows_selected    
    
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
  # observer which write files to temp directory in preparation for download
  # this needs to be outside the downloadhandler to get around the server timing out on long downloads
  observeEvent(input$makezipfile,{
    disable('makezipfile')
    on.exit({
      enable('makezipfile')
    })
    
    print(tempdir())
    fs <- vector()
    
    # ____test functionality____
    # if in testmode just select all rows since shinytest doesn't recognize row selection for parmsTable2
    if (isTRUE(getOption("shiny.testmode"))){
      rows <- 1:nrow(parmTable$parms)
    }
    else rows <- input$parmsTable2_rows_selected
    #
    total_files <- sum(c(input$report_selection, length(input$download_selection), length(rows))) + 1
    
    withProgress(message = "Writing files: ",{
      # option to choose report output format?  need to change inputs in report.R.
      if (input$report_selection == TRUE & !is.null(revals$peakData2)){
        fs <- c(fs, file.path(tempdir(), "report.html"))
        report(revals$uploaded_data, revals$peakData2, Emeta(), output_file = file.path(tempdir(), "report.html"), output_format = "html_document", 
               C13_ID = input$iso_symbol, groups_list = revals$groups_list)
        incProgress(1/total_files, detail = 'HTML report done..')
      }
      
      if ("separate" %in% input$download_selection & !is.null(revals$peakData2)){
        fs <- c(fs, file.path(tempdir(), "FREDA_processed_e_data.csv"), file.path(tempdir(), "FREDA_processed_e_meta.csv"))
        write_csv(revals$peakData2$e_data, path = file.path(tempdir(), "FREDA_processed_e_data.csv"))
        write_csv(revals$peakData2$e_meta, path = file.path(tempdir(), "FREDA_processed_e_meta.csv"))
        incProgress(1/total_files, detail = 'Data and molecular identification file done..')
      }
      if ("merged" %in% input$download_selection & !is.null(revals$peakData2)){
        fs <- c(fs, file.path(tempdir(), "FREDA_processed_merged_data.csv"))
        merged_data <- merge(revals$peakData2$e_data, revals$peakData2$e_meta)
        write_csv(merged_data, path = file.path(tempdir(), "FREDA_processed_merged_data.csv"))
        incProgress(1/total_files, detail = 'Merged file done..')
      }
      if ("group_data" %in% input$download_selection){
        if(length(revals$plot_data) != 0){
          for(i in 1:length(revals$plot_data)){
            if (!is.null(revals$plot_data[[i]])){
              path <- file.path(tempdir(), paste0("FREDA_group_data_summary_", gsub("/", "-", parmTable$parms[["File Name"]][i]),".csv"))
              fs <- c(fs, path)
              write_csv(revals$plot_data[[i]], path = path) 
            }
          }
        }
        incProgress(1/total_files, detail = 'Group plot summaries done..')
      }
      
      if (length(rows) > 0) {
        bitmaps <- list()
        for (i in rows) {
          path <- paste(tempdir(), "/",gsub("/", "-", parmTable$parms[["File Name"]][i]), ".", input$image_format, sep = "") #create a plot name
          fs <- c(fs, path) # append the new plot to the old plots
          export(revals$plot_list[[i]],
                 file = paste(tempdir(), "plot",i,".png", sep = ""), zoom = 2) # use webshot to export a screenshot to the opened pdf
          #r <- brick(file.path(getwd(), paste("plot",i,".png", sep = ""))) # create a raster of the screenshot
          img <- magick::image_read(paste(tempdir(), "plot",i,".png", sep = ""))#attr(r,"file")@name) #turn the raster into an image of selected format
          
          if (isTRUE(getOption("shiny.testmode"))) bitmaps[[i]] <- as.raster(img)
          
          image_write(img, path = path, format = input$image_format) #write the image
          incProgress(1/total_files, detail = sprintf('Plot %s done..', i))
          #rsvg::rsvg_svg(img, file = path)
        }
        
        # ___test-export___
        exportTestValues(images_out = digest::digest(bitmaps))
        
        fs <- c(fs, file.path(tempdir(), "Plot_key.csv"))
        outtable <- parmTable$parms[rows,]
        write_csv( outtable, path = file.path(tempdir(), "Plot_key.csv"))
      }
      print(fs)
      revals$fs <- fs
      
      # setwd(orig_wd)
    })
  })
  
  #---- processed data download --------#
  output$download_processed_data <- downloadHandler(
    filename = paste("FREDA_Output_",proc.time()[1],".zip", sep = ""),
    content = function(fname){
      # orig_wd <- getwd()
      # setwd(tempdir())
      zip(zipfile=fname, files=revals$fs, flags = "-j")
      if (file.exists(paste0(fname,".zip"))){file.rename(paste0(fname,".zip"),fname)}
      
    },
    contentType = "application/zip"
  )
})
