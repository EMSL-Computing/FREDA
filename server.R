
# Modify maximum file size (currently 250 mb)
options(shiny.maxRequestSize=250*1024^2, ch.dir = TRUE) 
library(shiny)
library(fticRanalysis)
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


#peakIcr2 <- NULL #when finished developing, uncomment this to clear the workspace on exit

shinyServer(function(session, input, output) {
  
  Sys.setenv(R_ZIPCMD="/usr/bin/zip")
  # Source files for 'summaryFilt' and 'summaryPreprocess'
  source('helper_functions/selection_addons.R')
  source('helper_functions/summaryFilter.R') 
  source('helper_functions/summaryPreprocess.R')
  source("helper_functions/renderDownloadPlots.R")
  
  revals <- reactiveValues(ntables = 0, makeplot = 1, color_by_choices = NULL, axes_choices = NULL,
                           plot_data_export = NULL, peakICR_export = NULL, redraw_filter_plot = TRUE, reac_filter_plot = TRUE, 
                           warningmessage = list(upload = "<p style = 'color:deepskyblue'>Upload data and molecular identification files described in 'Data Requirements' on the previous page."))
  
  exportTestValues(plot_data = revals$plot_data_export, peakICR = revals$peakICR_export, color_choices = revals$color_by_choices)
  ######## Welcome Tab #############
  #------ Download Example Data ---------#
  example_edata <- read.csv('Data/example12T_edata.csv')
  example_emeta <- read.csv('Data/example12T_emeta.csv')
  calc_opts <- read.csv('calculation_options.csv', stringsAsFactors = FALSE)
  calc_vars <- read.csv('calculation_variables.csv', stringsAsFactors = FALSE)
  #### in case we want a preview rendered #####
  # output$example_data_table <- DT::renderDataTable({
  #   example_edata
  # })
  # 
  # output$example_meta_table <- DT::renderDataTable({
  #   example_emeta
  # })
  #############
  output$downloadData <- downloadHandler(
    filename = "FREDA_Example_Data.zip",
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      fs <- c("example12T_edata.csv", "example12T_emeta.csv")
      write.csv(example_edata, row.names = FALSE, file = "example12T_edata.csv")
      write.csv(example_emeta, row.names = FALSE, file = "example12T_emeta.csv")      
      print(fs)
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  ######## Upload Tab ##############
  
  #### Sidebar Panel (Upload Tab) ####
  
  # Object: Get e_data from file input
  Edata <- reactive({
    # Error handling: Need file_edata path
    req(input$file_edata$datapath)
    
    # Load file
    filename <- input$file_edata$datapath
    
    exportTestValues(e_data = read.csv(filename, stringsAsFactors = FALSE))
    read.csv(filename, stringsAsFactors = FALSE)
    
  }) # End Edata #
  
  # Object: Get list of column names of Edata
  # Note: created when e_data is uploaded
  edata_cnames <- reactive({
    
    # Get column names
    names(Edata())
    
  }) # End edata_cnames #
  
  # Drop down list: Get edata unique identifier
  output$edata_id <- renderUI({
    
    # Drop down list with options from column names
    selectInput("edata_id_col", "Choose column with IDs",
                choices  = c('Select one', edata_cnames()))
    
  }) # End edata_id #
  
  # Object: Get e_meta from file input
  Emeta <- reactive({
    
    # Error handling: Need file_emeta to be valid
    req(input$file_emeta$datapath)
    
    # Load file
    filename <- input$file_emeta$datapath
    
    exportTestValues(e_meta = read.csv(filename, stringsAsFactors = FALSE))
    read.csv(filename, stringsAsFactors = FALSE)
    
  }) # End Emeta #
  
  # Object: Emeta column names 
  # Note: created when emeta is loaded/updated
  emeta_cnames <- reactive({
    
    names(Emeta())
    
  }) # End emeta_cnames #
  
  # Object: Sample names from e_data
  # Note: This object is created when e_data and edata_id are entered
  sample_names <- reactive({
    setdiff(edata_cnames(), input$edata_id_col)
    
  }) # End sample_names #
  
  # Create reactive fake f_data (used when action button creates peakICR())
  fdata <- reactive({
    
    col2 <- rep(NA, length(sample_names()))
    data.frame('SampleId' = sample_names(), 'Var1' = col2)
    
  }) # End fdata #
  
  # Drop-down lists: Choose formula column
  output$f_column <- renderUI({
    
    selectInput("f_column", "Choose formula column",
                choices = c('Select one', emeta_cnames()))
    
  }) # End f_column #
  
  ### C H N O S P C13 ###
  # Drop-down lists: Select which column represents C / H / N / O / etc
  # First try to locate the column name with a grepl
  # Note: All require emeta_cnames()
  output$c_column <- renderUI({
    
    selectInput("c_column", "Choose column for C",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^c$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^c$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  })
  
  output$h_column <- renderUI({
    
    selectInput("h_column", "Choose column for H",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^h$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^h$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  })
  
  output$n_column <- renderUI({
    
    selectInput("n_column", "Choose column for N",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^n$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^n$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  })
  
  output$o_column <- renderUI({
    
    selectInput("o_column", "Choose column for O",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^o$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^o$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  }) 
  
  output$s_column <- renderUI({
    
    selectInput("s_column", "Choose column for S",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^s$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^s$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
    
  })
  
  output$p_column <- renderUI({
    
    selectInput("p_column", "Choose column for P",
                choices  = c('Select a column', emeta_cnames()),
                selected = ifelse(grepl("^p$", tolower(emeta_cnames())),
                                  yes = emeta_cnames()[grepl("^p$", tolower(emeta_cnames()))][1],
                                  no = 'Select a column'))
  })
  
  output$iso_info_filter_out <- renderUI({
    selectInput(inputId = "iso_info_filter", label = "Filter isotopic peaks?", 
                choices = list('Yes' = 1,
                               'No' = 2),
                selected = 'Yes'
    )
  })
  output$iso_info_column_out <- renderUI({
    req(input$iso_info_filter )
    if (input$iso_info_filter == 1) {
      selectInput("iso_info_column", "Which column contains isotopic information?",
                  choices  = c('Select a column' = 0, emeta_cnames()))
    } else (return(NULL))
  })
  
  output$iso_symbol_out <- renderUI({
    req(input$iso_info_filter )
    if (input$iso_info_filter == 1) {
      textInput("iso_symbol", label = "Enter a symbol denoting isotopic notation:",
                value = "1")
    } else (return(NULL))
  })
  
  observeEvent(input$iso_info_filter,{
    if (input$iso_info_filter == 2)
    showModal(
      modalDialog("",h4("Warning!", style = "color:Orange; font-weight:bold"),
                  HTML("<p style = color:Orange; font-weight:bold> Leaving isotopic peaks in the data may confound analysis/visualization results. We recommend filtering isotopic peaks.")
                    )
                  )
  })
  ### END of CHNOSP DROP DOWN LISTS ###
  
  
  #### Action Button Reactions (Upload Tab) ####
  
  # Object: Create peakICR when Upload Button clicked
  peakICR <- eventReactive(input$upload_click, {
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
        
        res <- as.peakIcrData(e_data = Edata(), f_data = fdata(),
                              e_meta = Emeta(), edata_cname = input$edata_id_col, 
                              fdata_cname = 'SampleId', mass_cname = input$edata_id_col, 
                              instrument_type = input$instrument,
                              mf_cname = input$f_column,
                              isotopic_cname = input$iso_info_column,
                              isotopic_notation = as.character(input$iso_symbol))
        
      } # End C13 / no C13 if statement
      
      if (input$isotope_yn == 2 | isTRUE(input$iso_info_filter) != 1) { #no C13
        # Calculate peakIcrData with formula column
        res <- as.peakIcrData(e_data = Edata(), f_data = fdata(),
                              e_meta = Emeta(), edata_cname = input$edata_id_col, 
                              fdata_cname = 'SampleId', mass_cname = input$edata_id_col, 
                              instrument_type = input$instrument, mf_cname = input$f_column)
      } 
    }
    
    # If elemental columns chosen
    if (input$select == 2){
      
      ## Error handling: all drop down columns nonempty and of class 'numeric'
      validate(
        need({(input$c_column != 'Select a column') & 
            (input$h_column != 'Select a column') &
            (input$n_column != 'Select a column') &
            (input$o_column != 'Select a column') &
            (input$s_column != 'Select a column') &
            (input$p_column != 'Select a column')
        }, 
        'Missing column information. Please double-check drop-down options.')
      )
      validate(
        need({
          all(is.numeric(Emeta()[,input$c_column])) &
            all(is.numeric(Emeta()[,input$h_column])) &
            all(is.numeric(Emeta()[,input$n_column])) &
            all(is.numeric(Emeta()[,input$o_column])) &
            all(is.numeric(Emeta()[,input$s_column])) &
            all(is.numeric(Emeta()[,input$p_column]))
        }, 
        'One or more elemental columns are non-numeric.')
        
      ) # End error handling #
      # If no C13
      if (input$isotope_yn == 2 | isTRUE(input$iso_info_filter == 2)) {
        # Create peakICR object
        res <- as.peakIcrData(e_data = Edata(), f_data = fdata(),
                              e_meta = Emeta(), edata_cname = input$edata_id_col, 
                              fdata_cname = 'SampleId', mass_cname = input$edata_id_col,
                              instrument_type = input$instrument,
                              c_cname = input$c_column, h_cname = input$h_column, 
                              n_cname = input$n_column, o_cname = input$o_column, 
                              s_cname = input$s_column, p_cname = input$p_column)
        
      }
      if (input$isotope_yn == 1 & isTRUE(input$iso_info_filter == 1)) { # If there's C13 # 
        
        # Error handling: entered isotopic notation must exist in the isotope information column
        validate(need(input$iso_info_column != "0", message = "Please choose a column of isotopic information"))
        validate(need(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol),
                      'The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise.')
        ) # End error handling
        
        res <- as.peakIcrData(e_data = Edata(), f_data = fdata(),
                              e_meta = Emeta(), edata_cname = input$edata_id_col, 
                              fdata_cname = 'SampleId', mass_cname = input$edata_id_col, 
                              instrument_type = input$instrument,
                              c_cname = input$c_column, h_cname = input$h_column, 
                              n_cname = input$n_column, o_cname = input$o_column, 
                              s_cname = input$s_column, p_cname = input$p_column, 
                              isotopic_cname = input$iso_info_column,
                              isotopic_notation = as.character(input$iso_symbol))
        
      } # End C13 / no C13 if statement
      
    } # End elemental column if statement
    
    # create nonreactive peakIcr object
    peakIcr2 <<- res
    
    return(res)
    
  }) # End peakICR creation
  
  #### shinyjs observers ####
  
  # display list of warnings pasted on separate lines
  output$warnings <- renderUI({
    HTML(lapply(revals$warningmessage, function(el){
      paste0("<p ", el, "</p>")
      }) %>%
      paste(collapse = "")
      )
    #HTML(paste("<p,", paste(revals$warningmessage, collapse = "<br>"), "</p>"))
  })
  
  source("Observers/upload_observers.R", local = TRUE)

  #### Main Panel (Upload Tab) ####
  
  # Display success message OR display errors
  output$success_upload <- renderUI({
    
    # Error handling: peakICR() must exist
    req(peakICR())
    
    # If no errors, show Success message
    HTML('<h4 style= "color:#1A5276">You may proceed to data filtering</h4>')
    
  }) # End success #
  
  # create 
  observeEvent(peakICR(),{
    #Error handling: peakICR() must exist
    req(peakICR())
    
    #___test-export___
    if (isTRUE(getOption("shiny.testmode"))) {
      revals$peakICR_export <- peakICR()
    }
    
    showModal(
      modalDialog(
        title = "Upload Success",
        fluidRow(
          column(10, align = "center", offset = 1,
                 HTML('<h4 style= "color:#1A5276">Your data has been successfully uploaded. 
                      You may proceed to the subsequent tabs for analysis.</h4>'),
                 hr(),
                 actionButton("upload_dismiss", "Review results.", width = '75%'),
                 br(),
                 br(),
                 actionButton("goto_preprocess", "Continue to preprocessing", width = '75%')
                 )
        )
        ,footer = NULL)
    )
    
    # enable inputs that should only be available if data is sucessfully uploaded
    disabled_inputs <- c("preprocess_click", "filter_click", "reset_filters", "plot_submit", "update_axes")
    lapply(disabled_inputs, enable)
    
  })
  
  # modal dialog behavior
  observeEvent(input$upload_dismiss,{removeModal()})
  observeEvent(input$goto_preprocess, {
    updateTabsetPanel(session, "top_page", selected = "Preprocess")
    removeModal()
  })
  
  # Summary: Display number of peaks and samples
  output$num_peaks <- renderText({
    peakICR()
    c('Number of peaks: ', nrow(peakICR()$e_data))
    
  }) # End num_peaks
  
  output$num_samples <- renderText({
    peakICR()
    c('Number of samples: ', (length(edata_cnames()) - 1))
    
  }) # End num_samples # 
  
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
      need(!is.null(peakICR()), message = "")
    )
    # Display number of peaks/rows with formula assigned
    c('Number of peaks with formulas: ', num_rows_formula)
    
    
  }) # End num_peaks_formula in summary panel
  
  # Display explanation above e_data
  output$edata_text <- renderUI({
    
    # Error handling: Edata() must exist
    req(Edata())
    
    HTML('<h4>Displaying Uploaded Data File</h4>')
    
  }) # End edata_text
  
  # Display preview for edata
  # output$head_edata <- renderDataTable({
  #   
  #   if (dim(Edata())[2] > 6)
  #     head(Edata())[,1:6]
  #   else
  #     head(Edata())
  #   
  # }, # End code portion of head_edata
  # 
  #   
  #   # Options for renderDataTable
  #   options = list(dom = 't', searching = FALSE)
  #   
  #   ) # End head_edata
  output$head_edata <- DT::renderDT(Edata(),
                                    options = list(scrollX = TRUE))
  
  # e_meta display
  output$head_emeta <- DT::renderDataTable(expr = Emeta(),
                                           options = list(scrollX = TRUE))
  
  # Display explanation for e_meta
  output$emeta_text <- renderUI({
    
    req(Emeta())
    HTML('<h4>Displaying Uploaded Molecular Identification File</h4>')
    
  })# End emeta_text
  
  
  
  ####### Preprocess Tab #######
  
  #### Populate List from CSV File ####
  
  observeEvent(input$preprocess_help,{
    showModal(
      modalDialog("",
                  tags$p("Here you can tell FREDA to compute certain values based on your input data.  The result of these calculations will be appended
                         to your molecular identification file and can be used as filtering variables in the next tab.\n",
                         style = "color:CornFlowerBlue"),
                  br(),
                  HTML("<p style = color:CornFlowerBlue> Check boxes to select which values you want calculated and then hit 'Process Data'.  
                       <span style = font-weight:bold>Element ratios are selected by default as they are required to produce Van-Krevelen
                       and Kendrick plots.</span>  \n Table summaries and an interactive histogram/bar chart of the values you selected will be generated.<p>")
                  )
                  )
  })
  
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
  
  #### Action Button reactions ####
  
  ## Action button: Apply calculation functions When action button is clicked
  # Depends on: peakIcr2, input$tests
  observeEvent(input$preprocess_click, {
    validate(need(input$tests, message = "Please choose at least one test to calculate"))
    
    ## Include functionality to possibly reset 
    
    # peakIcr2$e_meta <<- peakIcr2$e_meta %>% dplyr::select(-one_of(calc_vars$ColumnName))
    
    # Apply all relevant functions
    for(el in input$tests){
      # set f to the function that is named in the ith element of compound_calcs # 
      f <- get(el, envir=asNamespace("fticRanalysis"), mode="function")
      peakIcr2 <<- f(peakIcr2)
    }
    
    if (isTRUE(getOption("shiny.testmode"))) {
      exportTestValues(peakIcr2 = peakIcr2)
    }
    
  }, priority = 10) # End action button event
  
  # Creates two reactive variables for continuous and categorical variables
  # Note: dependent on preprocess click and the user-specified calculations
  observeEvent(input$preprocess_click, {
    # Error handling: peakIcr2 must have a non-NULL Kendrick Mass column name
    #req(!is.null(attr(peakIcr2, 'cnames')$kmass_cname))
    
    # Get csv file of all possible calculation column names
    possible_calc_cnames <- read.csv("calculation_variables.csv", header = TRUE, stringsAsFactors = FALSE)
    
    # Get column names from peakIcr2's e_meta
    actual_cnames <- colnames(peakIcr2$e_meta)
    
    # Find all columns with names that match names for calculated columns
    v_index <- which(possible_calc_cnames[,1] %in% actual_cnames)
    
    # Save calculation column names from above and their display names 
    intersect <- possible_calc_cnames[v_index,]
    
    # get numeric columns
    numeric_cols <- peakIcr2$e_meta %>% 
      dplyr::select(which(sapply(.[intersect[,1]], is.numeric))) %>% 
      names()
    
    # get categorical columns
    categorical_cols <- peakIcr2$e_meta %>% 
      dplyr::select(which(!sapply(.[intersect[,1]], is.numeric))) %>%
      names() 
    
    #set reactive variables for observers
    revals$numeric_cols <- intersect %>% filter(ColumnName %in% numeric_cols)
    revals$categorical_cols <- intersect %>% filter(ColumnName %in% categorical_cols)
    
    
    
  }) 
  
  #### Main Panel ####
  
  # Summary Panel: Display table summaries of numeric and categorical columns in e_meta
  
  # For numeric columns:
  observe({
    
    req(nrow(revals$numeric_cols) > 0)
    
    # Create Table Output
    output$numeric_summary <- DT::renderDataTable({
      columns <- summaryPreprocess(peakIcr2, revals$numeric_cols) %>% colnames()
      
      summaryPreprocess(peakIcr2, revals$numeric_cols) %>%
        datatable(options = list(dom = "t", pageLength = nrow(.))) %>% 
        formatRound(columns, digits = 2)
    }) 
    
    # Summary Header
    output$numeric_header <- renderUI(tags$p("Summary Statistics for Numeric Variables"))
    
  })
  
  # For Categorical Columns
  
  # This observer assigns renderTable calls to various output ID's and passes them to the renderUI call immediately below
  observe({
    req(nrow(revals$categorical_cols) > 0) 
    
    # List of tables which will be passed to renderTable()
    table_list <- summaryPreprocess(peakIcr2, revals$categorical_cols, categorical = TRUE)
    
    # Reactive variable which lets lapply know how many output ID's to generate depending on number of categorical variables selected
    revals$ntables <- length(table_list)
    
    # Call renderTable on each table and assign it to an output ID
    lapply(1:length(table_list), function(i){
      output[[paste0('Table_',i)]] <- DT::renderDataTable({table_list[[i]]}, options = list(scrollX = TRUE, dom = "t"))
    })
    
    # Summary Header
    output$cat_header <- renderUI(tags$p("Mode and Counts for Categorical Variables"))
  })
  
  # The renderUI call that takes input from the above observer
  output$categorical_summary <- renderUI({
    tagList(lapply(1:revals$ntables, function(i){
      DT::dataTableOutput(paste0('Table_',i))
    })
    )
  })
  
  ## END TABLE SUMMARY SECTION ##
  
  # Drop down list: potential histogram options
  output$which_hist_out <- renderUI({
    
    # Error handling: input csv of calculations variables required
    req(calc_vars, revals$numeric_cols, revals$categorical_cols)
    
    tagList(
      br(),
      br(),
      tags$p('I would like to see a histogram/bar-chart across all values of:'),
      selectInput('which_hist', NULL,
                  choices = isolate(emeta_display_choices()))
    )
  }) # End which_hist
  
  # Plot the histogram chosen above
  # Depends on: which_hist
  output$preprocess_hist <- renderPlotly({
    
    # Error handling: Require some columns to be selected
    req(input$which_hist)
    
    # Save column name for later display
    columnName <- input$which_hist
    
    # set display name
    displayName <- calc_vars %>% filter(ColumnName == columnName) %>%
      pluck("DisplayName")
    
    # Plot histogram using plotly
    p <- plot_ly(x = peakIcr2$e_meta[,columnName], type = 'histogram') %>%
      layout( title = paste('Observed distribution of', displayName),
              scene = list(
                xaxis = list(title = displayName),
                yaxis = list(title = 'Frequency')))
    p$elementId <- NULL
    
    #____test export_____
    exportTestValues(preprocess_hist = p, hist_attrs = p$x$attrs[[p$x$cur_data]])
    
    return(p)
    
  }) # End process_hist
  
  observeEvent(input$preprocess_click,{
    req(peakIcr2)
    
    showModal(
      modalDialog(title = "Preprocess Success",
                  fluidRow(
                    column(10, align = "center", offset = 1,
                           HTML('<h4 style= "color:#1A5276">Your data has been preprocessed.  Calculated variables have been added to the molecular identification file and can be used in subsequent filtering and visualization.</h4>'),
                           hr(),
                           actionButton("preprocess_dismiss", "Review results.", width = '75%'),
                           br(),
                           br(),
                           actionButton("goto_filter", "Continue to filtering", width = '75%')
                    )
                  )
                  ,footer = NULL)
    ) 
  }) # End successMessage
  observeEvent(input$preprocess_dismiss,{removeModal()})
  observeEvent(input$goto_filter,{
    updateTabsetPanel(session, "top_page", selected = "Filter")
    removeModal()
  })
  
  ############## Filter tab ##############
  
  source("Observers/filter_observers.R", local = TRUE)
  
  # ----- Filter Reset Setup -----# 
  # Keep a reactive copy of the pre-filtered data in case of a filter reset event
  uploaded_data <- reactive({
    req(peakICR(), input$tests)
    
    temp <- peakICR()
    
    for(el in input$tests){
      # set f to the function that is named in the ith element of compound_calcs # 
      f <- get(el, envir=asNamespace("fticRanalysis"), mode="function")
      temp <- f(temp)
    }
    
    return(temp)
  })
  
  observeEvent(input$filter_help,{
    showModal(
      modalDialog("",
                  tags$p("This page allows you to filter the data by various metrics.\n 
                         The default options are to:", style = "color:CornFlowerBlue"),
                  tags$ul(
                    tags$li("Retain peaks within a mass range (Mass Filter)"),
                    tags$li("Retain peaks that appear a minimum number of times across all samples (Molecule Filter)"),
                    tags$li("Retain peaks that have elemental information - either elemental columns or a full formula column (Formula Filter)"),
                    style = "color:CornFlowerBlue"
                  ),
                  tags$p("Additionally, one can filter by up to three variables contained in the molecular identification file.\n
                         As you select options, a plot will update showing the remaining observations after the application of each filter.\n",
                         style = "color:CornFlowerBlue"),
                  tags$p("Check boxes to select which filters to apply, specify filtering criteria by a range for numeric data or a selection of values for categorical data and then click 'Filter Data'",
                         style = "color:CornFlowerBlue"))
      )
  })
  
  #get display name choices for dropdown
  emeta_display_choices <- reactive({
    drop_cols <- c(input$f_column, input$o_column, input$h_column, input$n_column,
                   input$s_column, input$p_column, input$c_column, attr(peakIcr2, "cnames")$mass_cname,
                   input$iso_info_column)
    
    column_choices <- peakIcr2$e_meta %>% 
      dplyr::select(-one_of(drop_cols)) %>%
      dplyr::select(which(sapply(., function(col){ length(unique(col)) < 20 } ) | sapply(., is.numeric))) %>% #dont include columns with too many categories
      colnames() 
    
    #columns included in calculation_options.csv get their prettified names, everything else gets the column name
    names(column_choices) <- lapply(column_choices, function(x){
      if(x %in% calc_vars$ColumnName){
        calc_vars %>% filter(ColumnName == x) %>% pluck("DisplayName")
      }
      else x
    }) %>% unlist()
    
    #____test export_____
    exportTestValues(display_names = column_choices)
    
    column_choices
    
  })
  
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
    
    #tags$h()
    selectInput('minobs', "Minimum number of times observed", 
                choices = seq(1, (length(edata_cnames()) - 1), 1), selected = 2)
    
  }) # End minobs
  
  #### Action Button Reactions (Filter Tab) ####
  
  # Event: Create filtered nonreactive peakIcr2 when action button clicked
  # Depends on action button 'filter_click'
  observeEvent(input$filter_click, {
    
    # if the data is already filtered start over from the uploaded data
    if(any(c("moleculeFilt", "massFilt", "formulaFilt") %in% names(attributes(peakIcr2)$filters)) | any(grepl("emetaFilt", names(attributes(peakIcr2)$filters)))){
      peakIcr2 <<- uploaded_data()
    }
    
    # If mass filtering is checked
    if (input$massfilter){
      
      # Error handling: Min mass less than max mass, but greater than 0
      req(input$min_mass < input$max_mass)
      req(input$min_mass > 0)
      
      # Create and apply mass filter to nonreactive peakICR object
      filterMass <- mass_filter(peakIcr2)
      peakIcr2 <<- applyFilt(filterMass, peakIcr2, min_mass = as.numeric(input$min_mass), 
                             max_mass = as.numeric(input$max_mass))
    }
    
    # If molecule filtering is checked
    if (input$molfilter) {
      
      # Create and apply molecule filter to nonreactive peakICR object
      filterMols <- molecule_filter(peakIcr2)
      peakIcr2 <<- applyFilt(filterMols, peakIcr2, min_num = as.integer(input$minobs))
      
    } # End molecule filter if statement
    
    if (input$formfilter){
      filterForm <- formula_filter(peakIcr2)
      peakIcr2 <<- applyFilt(filterForm, peakIcr2)
      
    }
    
    # custom filters
    if(input$customfilterz){
      
        #apply the filter for each input
        lapply(1:3, function(i){
          
          #require that a selection has been made for filter i
          req(input[[paste0("custom",i)]] != "Select item")
          
          #make the filter based on selection
          filter <- emeta_filter(peakIcr2, input[[paste0("custom",i)]])
          
          # if numeric, apply filter with specified max and min values
          if(is.numeric(peakIcr2$e_meta[,input[[paste0("custom",i)]]])){
            req(input[[paste0("minimum_custom",i)]], input[[paste0("maximum_custom", i)]])
            peakIcr2 <<- applyFilt(filter, peakIcr2,
                                   min_val = input[[paste0("minimum_custom",i)]], 
                                   max_val = input[[paste0("maximum_custom", i)]], 
                                   na.rm = input[[paste0("na_custom",i)]])
            
          }
          # else apply with selected categories
          else if(!is.numeric(peakIcr2$e_meta[,input[[paste0("custom",i)]]])){
            req(input[[paste0("categorical_custom",i)]])
            peakIcr2 <<- applyFilt(filter, peakIcr2, 
                                   cats = input[[paste0("categorical_custom",i)]], 
                                   na.rm = input[[paste0("na_custom",i)]])
          }
        })
        
      }

    #__test-export__
    exportTestValues(peakIcr2 = peakIcr2)
    
    
  }) # End creating peakIcr2
  
  #### Main Panel (Filter Tab) ####
  
  # Object: Create 'Success' message if everything works out, show errors if not
  # Note: Created when Filter action button is clicked
  successMessage <- eventReactive(input$filter_click, {
    
    # If mass filter is checked
    if (input$massfilter) {
      
      # Error handling: need 0 < minMass < maxMass
      validate(
        need((input$min_mass < input$max_mass), 
             'Minimum mass must be less than maximum mass'), 
        
        need((input$min_mass > 0), 
             'Minimum mass must be greater than 0'),
        
        need((input$min_mass && input$max_mass), 
             'Both minimum and maximum mass required to filter')
        
      ) # End error handling
    }
    showModal(
      modalDialog(title = "Filter Success",
                  fluidRow(
                    column(10, align = "center", offset = 1,
                           HTML('<h4 style= "color:#1A5276">Your data has been filtered using mass and/or minimum observations. 
                                You may proceed to the next tabs for subsequnt analysis.</h4>'),
                           hr(),
                           actionButton("filter_dismiss", "Review results.", width = '75%'),
                           br(),
                           br(),
                           actionButton("goto_viz", "Continue to Visualization", width = '75%')
                           )
                  )
                  ,footer = NULL)
    )
    
    HTML('<h4 style= "color:#1A5276">You may now proceed to preprocessing and visualization</h4>')
    
  }) # End successMessage
  observeEvent(input$filter_dismiss,{removeModal()})
  observeEvent(input$goto_viz,{
    updateTabsetPanel(session, "top_page", selected = "Visualize")
    removeModal()
  })
  
  # Display successMessage
  # Depends on: successMessage
  output$filterTest <- renderUI({
    
    successMessage()
    
  })
  
  # Reactive value for each filter:
  massfilter_ids <- eventReactive(c(input$massfilter, input$min_mass, input$max_mass),{
    revals$redraw_filter_plot <- FALSE
    if(input$massfilter){
      mass_filter(uploaded_data()) %>% 
        filter(Mass <= input$max_mass, Mass >= input$min_mass) %>%
        pluck(1)
    }
    else NULL
  })
  
  molfilter_ids <- eventReactive(c(input$minobs, input$molfilter), {
    if(input$molfilter){
      molecule_filter(uploaded_data()) %>%
        filter(Num_Observations >= input$minobs) %>%
        pluck(1)
    }
    else NULL
  })
  
  formfilter_ids <- eventReactive(input$formfilter, {
    if(input$formfilter){
      formula_filter(uploaded_data()) %>%
        filter(Formula_Assigned == TRUE) %>%
        pluck(1)
    }
    else NULL
  })
  
  # Object: Get data frame from summaryFilt
  # Depends on: peakIcr2, checkboxes for filters, and inputs for filters
  summaryFilterDataFrame <- eventReactive(revals$reac_filter_plot, {
    req(input$top_page == "Filter")
    
      # determine which, if any, custom filters to apply
      conds <- c(!is.null(revals$custom1_ids), !is.null(revals$custom2_ids), !is.null(revals$custom3_ids))
      
      if(any(conds) & isolate(input$customfilterz)){
        customids_to_keep <- data.frame(ids = c(revals$custom1_ids, revals$custom2_ids, revals$custom3_ids), stringsAsFactors = FALSE) %>%
          group_by(ids) %>%
          mutate(n = n()) %>%
          filter(n == sum(conds)) %>%
          pluck(1)
      }
      else customids_to_keep <- NULL
      
      # Get summary table from sourced file 'summaryFilter.R'
      summaryFilt(peakICR(), massfilter_ids(), molfilter_ids(), formfilter_ids(), customids_to_keep)
    
    
  }, ignoreInit = TRUE) # End summaryFilterDataFrame
  
  
  # Show table from summaryFilt
  # Depends on: summaryFilterDataFrame
  output$summary_filter <- renderTable({
    
    # Set default results: NA if no filters selected
    afterResults <- c(NA, NA, NA, NA)
    
    # If mass filter checked
    if (input$massfilter) {
      
      # Get which row has the row name 'After Mass Filter'
      rowNum <- which(summaryFilterDataFrame()$data_state == 'After Mass Filter')
      
      # Get relevant columns out of summaryFilterDataFrame
      afterResults <- unlist(summaryFilterDataFrame()[rowNum, c('sum_peaks', 'assigned', 
                                                                'min_mass', 'max_mass')])
    }
    # If molecule filter checked
    if (input$molfilter) {
      
      # Get which row has the row name 'After Mass Filter'
      rowNum <- which(summaryFilterDataFrame()$data_state == 'After Molecule Filter')
      
      # Get relevant columns out of summaryFilterDataFrame
      afterResults <- unlist(summaryFilterDataFrame()[rowNum, c('sum_peaks', 'assigned', 
                                                                'min_mass', 'max_mass')])
    }
    
    if (input$formfilter) {
      
      # Get which row has the row name 'After Mass Filter'
      rowNum <- which(summaryFilterDataFrame()$data_state == 'After Formula Filter')
      
      # Get relevant columns out of summaryFilterDataFrame
      afterResults <- unlist(summaryFilterDataFrame()[rowNum, c('sum_peaks', 'assigned', 
                                                                'min_mass', 'max_mass')])
    }
    
    if (input$customfilterz) {
      
      # Get which row has the row name 'After Mass Filter'
      rowNum <- which(summaryFilterDataFrame()$data_state == 'After Custom Filters')
      
      # Get relevant columns out of summaryFilterDataFrame
      afterResults <- unlist(summaryFilterDataFrame()[rowNum, c('sum_peaks', 'assigned', 
                                                                'min_mass', 'max_mass')])
    }
    
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
    
    return(display_table)
  }, # End code portion of summary_filter
  
  # Options: include rownames, no decimal places
  rownames = TRUE
  ) # End summary_filter
  
  # Plot bar chart
  # Depends on: summaryFilterDataFrame
  output$barplot_filter <- renderPlot({
    
    req(isolate(revals$redraw_filter_plot) == TRUE)
    
    filter_inds <- c(TRUE, isolate(input$massfilter), isolate(input$molfilter), isolate(input$formfilter), 
                     any(c(isolate(input$custom1), isolate(input$custom2), isolate(input$custom3)) != "Select item") & isolate(input$customfilterz))
    
    which_filts <- c("Unfiltered", "After Mass Filter", "After Molecule Filter", "After Formula Filter", "After Custom Filters")[filter_inds]
    
    # Melt dataframe into 2 objects
    ggdata_barplot <- melt(summaryFilterDataFrame()[,c('data_state', 'assigned', 'unassigned')]) %>% filter(data_state %in% which_filts)
    ggdata_text <- summaryFilterDataFrame()[, c('data_state', 'sum_peaks', 'dispText')] %>% filter(data_state %in% which_filts)
    
    # Aesthetic purposes: get max height, divide by 30, use as offset in geom_text
    num_displaced <- round(ggdata_text[1, 2] / 35, digits = -1)
    
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
  
  #-------- Reset Activity -------#
  # Allow a 'reset' that restores the uploaded object and unchecks the filter
  # boxes.  Will display a popup that warns the user of plot erasure and gives 
  # the option to reset or to go back without clearing filters.
  
  observeEvent(input$reset_filters,{
    showModal(modalDialog(
      
      ##### There is probably a better way to code the display behavior of this dialog -DC
      
      fluidPage(
        fluidRow(
          column(10, align = "center", offset = 1,
                 tags$p("Caution:  If you reset filters all plots made using filtered data will be lost.", style = "color:red;font:bold", align = "center"),
                 actionButton("clear_filters_yes", "Yes, clear filters without saving plots.", width = '100%'),
                 br(),
                 br(),
                 br(),
                 actionButton("clear_filters_no", "No, take me back.", width = '100%')
          ))),
      footer = NULL
    )
    )
  })
  
  # if they click no, just exit the modal dialog
  observeEvent(input$clear_filters_no,{
    removeModal()
  })
  
  # if they click yes, reset data and exit modal dialog
  observeEvent(input$clear_filters_yes, {
    if (f$clearFilters) {
      updateCheckboxInput(session = session, inputId = "massfilter", value = FALSE)
      updateCheckboxInput(session = session, inputId = "molfilter", value = FALSE)
      updateCheckboxInput(session = session, inputId = "formfilter", value = FALSE)
      updateCheckboxInput(session = session, inputId = "customfilterz", value = FALSE)
    }
    peakIcr2 <<- uploaded_data()
    removeModal()
  })
  
  
  ####### Visualize Tab #######
  
  ## Sidebar Panel ##
  
  #### Viztab observers.  Help Button. Dropdown choices, plot clearing, and shinyjs helper functionality###
  source("Observers/visualize_observers.R", local = TRUE)
  #####
  
  # Plot options, with selections removed if the necessary columns in e_meta are not present.
  output$plot_type <- renderUI({
    choices <- c('Van Krevelen Plot', 'Kendrick Plot', 'Density Plot', 'Custom Scatter Plot', 'Select an Option' = 0)
    
    #disallow kendrick plots if either kmass or kdefect not calculated/present in emeta
    if(is.null(attr(peakIcr2, "cnames")$kmass_cname) | is.null(attr(peakIcr2, "cnames")$kdefect_cname)){
      choices <- choices[choices != "Kendrick Plot"]
    }
    
    #disallow vk plots if o:c or h:c ratios not calculated/present in emeta
    if(is.null(attr(peakIcr2, "cnames")$o2c_cname) | is.null(attr(peakIcr2, "cnames")$h2c_cname)){
      choices <- choices[choices != "Van Krevelen Plot"]
    }
    
    #disallow density plots if at least 1 
    if(!any(sapply(peakIcr2$e_meta %>% dplyr::select(-one_of(getEDataColName(peakIcr2))), is.numeric))){
      choices <- choices[choices != c("Density Plot", "Custom Scatter Plot")]
    }
    
    #if everything is disallowed, give warning and silently stop execution.
    if(all(choices == 0)) return(tags$p("There is not enough information in the molecular identification file to produce any plots.  Choose more variables to calculate in the preprocess tab or append some metadata to the molecular identification file prior to uploading", style = "color:gray"))
    
    selectInput('chooseplots', 'I want to plot a', 
                  choices = choices,
                  selected = 0
    )
  })
  
  # Logic to force single sample selection in the case where only 1 sample is present
  output$plotUI <- renderUI({
    validate(
      need(input$chooseplots != 0, message = "Please select plotting criteria")
    )
    if(nrow(peakIcr2$f_data) == 1 | input$chooseplots == "Custom Scatter Plot"){
      return(tagList(
        tags$div(class = "grey_out",
          disabled(selectInput('choose_single', 'I want to plot using:',
                                                choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples by group' = 2, 'A comparison of groups' = 3),
                                                selected = 1))
          ),
        tags$p("Grouping options hidden for custom scatter plots and single sample datasets.", style = "color:gray;font-size:small;margin-top:3px")
      ))
    }
    if (input$chooseplots == "Density Plot") {
      return(selectInput('choose_single', 'I want to plot using:',
                         choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples by group' = 2, 'A comparison of groups' = 3),
                         selected = 0))
    }
    else{
      return(selectInput('choose_single', 'I want to plot using:',
                         choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples by group' = 2, 'A comparison of groups' = 3),
                         selected = 0))
    }
  })
  
  # Conditional dropdowns based on grouping criteria (choose_single)
  output$plotUI_cond <- renderUI({
    req(input$choose_single != 0, input$chooseplots != 0)
    if(input$choose_single == 3){
      return(tagList(
        div(id = "js_whichGroups1", 
            selectInput('whichGroups1', 'Group 1',
                    choices = setdiff(sample_names(), isolate(input$whichGroups2)),
                    multiple = TRUE)),
        div(id = "js_whichGroups2",
            selectInput("whichGroups2", "Group 2", 
                    choices = setdiff(sample_names(), isolate(input$whichGroups1)), 
                    multiple = TRUE))
      ))
    }
    else if(input$choose_single == 2){
      return(tagList(
        div(id = "js_whichSamples",
            selectInput('whichSamples', 'Grouped Samples',
                    choices = sample_names(),
                    multiple = TRUE)),
        conditionalPanel(
          condition = 'input.whichSamples.length < 2',
          tags$p("Please select at least 2 samples", style = "color:gray")
        )# End conditional output multiple samples#
      ))
    }
    else if(input$choose_single == 1){
      return(div(id = "js_whichSamples", selectInput('whichSamples', 'Sample', choices = sample_names())))
    }
    else return(NULL)
  })
  
  # selector for summary funcion
  output$summary_fxn_out <- renderUI({
    if(input$chooseplots == "Density Plot"){
      summary_dropdown <- tags$div(class = "grey_out",
                                   disabled(selectInput("summary_fxn", "Summary Function", choices = "uniqueness_gtest")),
                                   tags$p("No summary functions for comparison density plots", style = "color:gray;font-size:small;margin-top:3px")
      )
      
    }else summary_dropdown <- selectInput("summary_fxn", "Summary Function", choices = "uniqueness_gtest")
    return(summary_dropdown)
  })

  #### Main Panel (Visualize Tab) ####
  
  # color pallete selection
  output$colorpal_out <- renderUI({
    choices = c("YlOrRd", "YlGnBu", "YlGn", "RdYlGn")
    
    extensions <- lapply(choices, function(choice){
      tags$img(src = paste0(choice, ".png"), width = "100px", height = "25px")
    })
    
    if(isTRUE(input$chooseplots == "Density Plot")){
      addClass("js_colorpal", "grey_out")
      disabled(colored_radiobuttons(inputId = "colorpal", label = "Pick a coloring scheme", inline = TRUE,
                                         choices = choices, extensions = extensions))
    }else{
      removeClass("js_colorpal", "grey_out")
      colored_radiobuttons(inputId = "colorpal", label = "Pick a coloring scheme", inline = TRUE,
                               choices = choices, extensions = extensions)
    }
  })
  
  # Create plotting dataframe to be passed to FxnPlot
  plot_data <- eventReactive(input$plot_submit,{
    
    req(calc_vars)
    validate(need(input$chooseplots != 0 & input$choose_single !=0, message = "Please select plotting criteria"))
    if(is.null(input$choose_single)){ # corresponds to data with a single sample
      return(peakIcr2) # no need to subset
    }
    if (input$choose_single == 1) { # single sample -selected- but multiple samples present
      validate(need(!is.null(input$whichSamples), message = "Please select a sample to plot"))
      return(subset(peakIcr2, input$whichSamples))
      #key_name <- paste(attributes(peakIcr2)$cnames$fdata_cname, "=", input$whichSamples, sep = "")
    }
    #---------- Group Plots ------------#
    else if (input$choose_single == 2) { # single group'
      
      validate(need(!is.null(input$whichSamples), message = "Please select samples for grouping"))
      validate(need(length(input$whichSamples) > 1, message = "Please select at least 2 samples"))
      
      temp_group_df <- data.frame(input$whichSamples, "Group")
      colnames(temp_group_df) <- c(getFDataColName(peakIcr2), "Group")
      
      temp_data <- peakIcr2 %>% 
        subset(input$whichSamples)
      
      temp_data <- fticRanalysis:::setGroupDF(temp_data, temp_group_df)
      
      # no need to summarize for density plot function
      if(input$chooseplots == "Density Plot"){
        return(temp_data)
      }

      temp_data <- summarizeGroups(temp_data, summary_functions = getGroupSummaryFunctionNames())
      temp_data$e_meta <- cbind(temp_data$e_meta, temp_data$e_data %>% dplyr::select(-one_of(getEDataColName(temp_data))))
      
      return(temp_data)
      
    } else if (isolate(input$choose_single) == 3) {# two groups 
      # Make sure at least one test has been calculated
      validate(need(!is.null(isolate(input$whichGroups1)), message = "Please select samples for first grouping"))
      validate(need(length(input$whichGroups1) > 0, message = "Please select at least 1 sample"))
      validate(need(!is.null(isolate(input$whichGroups2)), message = "Please select samples for second grouping"))
      validate(need(length(input$whichGroups2) > 0, message = "Please select at least 1 sample"))
      
      group1_samples <- isolate(input$whichGroups1)
      group2_samples <- isolate(input$whichGroups2)
      temp_group_df <- data.frame(c(group1_samples, group2_samples), c(rep("Group1", times=length(group1_samples)), rep("Group2", length(group2_samples))))
      colnames(temp_group_df) <- c(getFDataColName(peakIcr2), "Group")
      
      temp_data <- peakIcr2 %>%
        subset(samples=c(group1_samples, group2_samples))
      
      temp_data <- fticRanalysis:::setGroupDF(temp_data, temp_group_df)
      
      # no need to summarize for density plot function
      if(input$chooseplots == "Density Plot"){
        return(temp_data)
      }
      
      grpComparisonsObj <- divideByGroupComparisons(temp_data, comparisons = "all")[[1]]$value
      summaryObj <- summarizeGroupComparisons(grpComparisonsObj, summary_functions = input$summary_fxn, 
                                              summary_function_params = list(uniqueness_gtest = list(pres_fn = "prop", pres_thresh = 0.2)))
      
      return(summaryObj)
      
    }
  })
  
  # When plot_data() is recalculated, repopulate the dropdowns under the plot.  Specifically vk_colors and custom scatterplot options.
  observeEvent(plot_data(),{
    
    # store test value
    if (isTRUE(getOption("shiny.testmode"))) {
      revals$plot_data_export <- plot_data()
    }
    
    ## ifelse block determines how to populate vk_colors dropdown
    
    # Density plots care not for choose_single!!!!
    if(input$chooseplots == "Density Plot"){
      numeric_cols <- which(sapply(plot_data()$e_meta %>% 
                                     dplyr::select(one_of(emeta_display_choices())), is.numeric))
      color_by_choices <- emeta_display_choices()[numeric_cols]
    }
    else if (input$choose_single == 1){
      color_by_choices <- emeta_display_choices()
      
      if (input$chooseplots == "Van Krevelen Plot"){
        color_by_choices <- switch(input$vkbounds,
                                   'bs1' = c('Van Krevelen Boundary Set 1' = 'bs1', color_by_choices),
                                   'bs2' = c('Van Krevelen Boundary Set 2' = 'bs2', color_by_choices),
                                   "0" = c('Van Krevelen Boundary Set 1' = 'bs1', 'Van Krevelen Boundary Set 2' = 'bs2', color_by_choices))
      }
    }
    else if (input$choose_single == 2) {
      
      # create vector of color choices by combining unique elements from e_data and e_meta
      edata_colors <- plot_data()$e_data %>% dplyr::select(-one_of(getEDataColName(plot_data()))) %>% colnames()
      color_by_choices <- c(edata_colors[!(edata_colors %in% emeta_display_choices())], emeta_display_choices())
      
    } else if (input$choose_single == 3) {
      color_by_choices <- c("Unique GTest" = "uniqueness_gtest")
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
    
    if (input$chooseplots == 'Custom Scatter Plot') {
      # allow only numeric columns for the axes but keep categorical coloring options
      numeric_cols <- which(sapply(full_join(plot_data()$e_meta, plot_data()$e_data) %>% 
                                     dplyr::select(color_by_choices), 
                                   is.numeric))
      
      axes_choices <- revals$axes_choices <- color_by_choices[numeric_cols]
      
      updateSelectInput(session, 'scatter_x', "Horizontal Axis Variable:",
                        choices = axes_choices[!(axes_choices %in% c(input$scatter_y, input$vk_colors))],
                        selected = selected_x)
      updateSelectInput(session, "scatter_y", "Vertical Axis Variable:",
                        choices = axes_choices[!(axes_choices %in% c(input$scatter_x, input$vk_colors))],
                        selected = selected_y)
      updateSelectInput(session, 'vk_colors', 'Color  by:',
                        choices = color_by_choices[!(color_by_choices %in% c(input$scatter_x, input$scatter_y))],
                        selected = selected)
    }
    
    revals$color_by_choices <- color_by_choices
    
    # The dropdown value will not be updated if this is true, force re-execution of plotting in this case with a reactive var
    if(input$vk_colors %in% color_by_choices){
      revals$makeplot <- -revals$makeplot
    }
    
  }, priority = 9)
  
  # Main plotting output #
  output$FxnPlot <- renderPlotly({
    req(input$chooseplots != 0)
    
    # reactive dependencies
    input$update_axes
    input$vk_colors
    input$colorpal
    revals$makeplot #in case vk_colors does not change we still want to redraw the plot.
    
    # for testing if plot actually got updated in test mode
    exportTestValues(plot = NULL, plot_attrs = NULL)
    
    if (isolate(v$clearPlot)){
      return(NULL)
    } else {
      # Make sure a plot stype selection has been chosen
      validate(need(input$choose_single != 0, message = "Please select plotting criteria"))
      
      revals$legendTitle = ifelse(isolate(is.null(input$legend_title_input) || (input$legend_title_input == "")),
                           yes = names(isolate(revals$color_by_choices[revals$color_by_choices == input$vk_colors])),
                           no = isolate(input$legend_title_input)
      )
      
      # Apply custom color scale if numeric is selected
      if(isolate(numeric_selected()) & !(input$vk_colors %in% c("bs1", "bs2"))){
        diverging_options = c("RdYlGn")
        pal <- RColorBrewer::brewer.pal(n = 9, input$colorpal)
        
        if(!(input$colorpal %in% diverging_options)){
          pal <- RColorBrewer::brewer.pal(n = 9, input$colorpal)[3:9]
        }
        
        if(input$flip_colors%%2 != 0){
          pal <- rev(pal)
        }
        
        domain = range(plot_data()$e_meta[,input$vk_colors], na.rm = TRUE)
        colorPal <- scales::col_numeric(pal, domain)
        revals$colorPal <- paste(paste(pal, collapse = ","), paste(domain, collapse = ","), sep = ":")
      }
      else{
        colorPal <- revals$colorPal <- NA
        
      }
      
      
      #----------- Single sample plots ------------#
      #-------Kendrick Plot-----------# 
      if (input$chooseplots == 'Kendrick Plot') {
        validate(need(!is.null(input$whichSamples) | !(is.null(isolate(input$whichGroups1)) & is.null(isolate(input$whichGroups2))), message = "Please select at least 1 sample"))
        p <- kendrickPlot(isolate(plot_data()), colorCName = input$vk_colors, colorPal = colorPal,
                          xlabel = isolate(input$x_axis_input), ylabel = isolate(input$y_axis_input),
                          title = isolate(input$title_input),legendTitle = revals$legendTitle)
        
        if (input$vk_colors %in% c('bs1', 'bs2')) {
          p <- kendrickPlot(isolate(plot_data()), vkBoundarySet = input$vk_colors,
                            xlabel = isolate(input$x_axis_input), ylabel = isolate(input$y_axis_input),
                            title = isolate(input$title_input),legendTitle = revals$legendTitle)
        } else {
          # if color selection doesn't belong to a boundary, color by test
          p <- kendrickPlot(isolate(plot_data()), colorCName = input$vk_colors, colorPal = colorPal,
                            xlabel = isolate(input$x_axis_input), ylabel = isolate(input$y_axis_input),
                            title = isolate(input$title_input),legendTitle = revals$legendTitle)
        }
      }
      #-------VanKrevelen Plot--------#
      if (input$chooseplots == 'Van Krevelen Plot') {
        validate(need(!is.null(input$whichSamples) | !(is.null(isolate(input$whichGroups1)) & is.null(isolate(input$whichGroups2))), message = "Please select at least 1 sample"))
        if (input$vkbounds == 0) { #no bounds
          # if no boundary lines, leave the option to color by boundary
          if (input$vk_colors %in% c('bs1', 'bs2')) {
            p <- vanKrevelenPlot(isolate(plot_data()), showVKBounds = FALSE, vkBoundarySet = input$vk_colors,
                                 xlabel = isolate(input$x_axis_input), ylabel = isolate(input$y_axis_input),
                                 title = isolate(input$title_input),legendTitle = revals$legendTitle)
          } else {
            # if no boundary lines and color selection doesn't belong to a boundary, color by test
            p <- vanKrevelenPlot(isolate(plot_data()), showVKBounds = FALSE, colorCName = input$vk_colors, colorPal = colorPal,
                                 xlabel = isolate(input$x_axis_input), ylabel = isolate(input$y_axis_input),
                                 title = isolate(input$title_input),legendTitle = revals$legendTitle)
          }
        } else {
          # if boundary lines, allow a color by boundary class 
          if (input$vk_colors %in% c('bs1', 'bs2')) {
            p <- vanKrevelenPlot(isolate(plot_data()), vkBoundarySet = input$vkbounds, showVKBounds = TRUE,
                                 xlabel = isolate(input$x_axis_input), ylabel = isolate(input$y_axis_input),
                                 title = isolate(input$title_input),legendTitle = revals$legendTitle)
          } else {
            # if boundary lines and color isn't a boundary class
            p <- vanKrevelenPlot(isolate(plot_data()), vkBoundarySet = input$vkbounds, showVKBounds = TRUE, 
                                 colorCName = input$vk_colors, colorPal = colorPal,
                                 xlabel = isolate(input$x_axis_input), ylabel = isolate(input$y_axis_input),
                                 title = isolate(input$title_input),legendTitle = revals$legendTitle)
          }
        }
      }
      
      #--------- Density Plot --------#
      if (input$chooseplots == 'Density Plot') {
        validate(need(!is.null(input$whichSamples) | !(is.null(isolate(input$whichGroups1)) & is.null(isolate(input$whichGroups2))), message = "Please select at least 1 sample"),
                 need(!is.na(input$vk_colors), message = "Please select a variable to color by")
        )
        
        # sample/group inputs depending on whether or not we are doing a comparison of groups
        if(input$choose_single == 3){
          samples = FALSE
          groups = c("Group1", "Group2")
        }
        else if(input$choose_single == 2){
          samples = input$whichSamples
          groups = "Group"
        }
        else if(input$choose_single == 1){
          samples = input$whichSamples
          groups = FALSE
        }
        
        # if x axis input field is empty, get the display name of the color_by_choices vector index that equals vk_colors, otherwise use what the user typed
        xlabel = ifelse(isolate(is.null(input$x_axis_input) || input$x_axis_input == ""),
                        yes = names(revals$color_by_choices[revals$color_by_choices == input$vk_colors]),
                        no = isolate(input$x_axis_input))
        
        p <- densityPlot(isolate(plot_data()),variable = input$vk_colors, samples = samples, groups = groups,
                         plot_hist = ifelse(input$choose_single == 1, TRUE, FALSE), 
                         xlabel = xlabel, ylabel = isolate(input$y_axis_input), title = isolate(input$title_input))
        
        
        # p <- densityPlot(isolate(plot_data()), variable = input$vk_colors,
        #                  xlabel = ifelse(isolate(is.null(input$x_axis_input) || input$x_axis_input == ""),
        #                                  yes = names(revals$color_by_choices[revals$color_by_choices == input$vk_colors]),
        #                                  no = isolate(input$x_axis_input)),
        #                  ylabel = isolate(input$y_axis_input),
        #                  title = isolate(input$title_input))
        
      }
      
      #---------- Custom Scatter Plot --------#
      if(input$chooseplots == 'Custom Scatter Plot'){
        validate(need(!is.null(input$whichSamples), message = "Please select at least 1 sample"),
                 need(!is.na(input$vk_colors), message = "Please select a variable to color by"))
        req(!is.null(input$scatter_x), !is.null(input$scatter_y), !("" %in% c(input$scatter_x, input$scatter_y)))
        
        p <- scatterPlot(isolate(plot_data()), input$scatter_x, input$scatter_y, colorCName = input$vk_colors, colorPal = colorPal,
                         xlabel = isolate(ifelse(is.null(input$x_axis_input) | (input$x_axis_input == ""), 
                                                 yes = names(revals$color_by_choices[revals$color_by_choices == input$scatter_x]), 
                                                 no = input$x_axis_input)), 
                         ylabel = isolate(ifelse(is.null(input$y_axis_input) | (input$y_axis_input == ""), 
                                                 yes = names(revals$color_by_choices[revals$color_by_choices == input$scatter_y]), 
                                                 no = input$y_axis_input)),
                         title = isolate(input$title_input), legendTitle = revals$legendTitle)
        
      }
    }
    
    # Axes Options
    f <- list(family = "Arial", size = 18, color = "#7f7f7f")
    
    x <- y <- list(titlefont = f)
    
    # ___test-export___
    exportTestValues(plot = p, plot_attrs = p$x$attrs[[p$x$cur_data]])
    
    p <- p %>% layout(xaxis = x, yaxis = y, titlefont = f, margin = list(t = 50))
    
    # Null assignment bypasses plotly bug
    p$elementId <- NULL
    enable("add_plot")
    return(p)
    
  })
  
  
  #------ plot axes and titles options ------#
  # use the 'formals' argument to figure out default chart labels
  plot_defaults <- reactive({
    validate((
      need(input$chooseplots != 0, message = "")
    ))
    if (input$chooseplots == 'Van Krevelen Plot') {
      defs <- formals(vanKrevelenPlot)
      #defs$legendTitle = names(emeta_display_choices())[emeta_display_choices() == input$vk_colors]
    } else if(input$chooseplots == "Custom Scatter Plot"){
      defs <- formals(scatterPlot)
      defs$ylabel = NULL
      defs$xlabel = NULL
    } else if (input$chooseplots == 'Kendrick Plot') {
      defs <- formals(kendrickPlot)
      #defs$legendTitle = input$vk_colors
    } else if (input$chooseplots == 'Density Plot') {
      defs <- formals(densityPlot)
      defs$ylabel = "Density"
      defs$xlabel = NULL
    }
    return(defs)
  })
  
  # Axis and title label input menus
  output$title_out <- renderUI({
    
    validate(
      need(input$chooseplots != 0, message = "")
    )
    textInput(inputId = "title_input", label = "Plot Title", value = "")
  })
  output$x_axis_out <- renderUI({
    validate(
      need(input$chooseplots != 0, message = "")
    )
    textInput(inputId = "x_axis_input", label = "X Axis Label", value = plot_defaults()$xlabel)
  })
  output$y_axis_out <- renderUI({
    validate(
      need(input$chooseplots != 0, message = "")
    )
    textInput(inputId = "y_axis_input", label = "Y Axis Label", value = plot_defaults()$ylabel)
  })
  
  # legend input
  output$legend_title_out <- renderUI({
    validate(
      need(input$chooseplots != 0, message = "")
    )
    if(input$chooseplots == "Density Plot"){
      addCssClass("js_legend_title_input", "grey_out")
      disabled(textInput(inputId = "legend_title_input", label = "Legend Label", value = ""))
    }
    else{
      removeCssClass("js_legend_title_input", "grey_out")
      textInput(inputId = "legend_title_input", label = "Legend Label", value = "")
    }
    
  })
  
  # reactive variable that keeps track of whether the selected column is numeric or categorical.
  numeric_selected <- eventReactive(c(input$vk_colors, plot_data()),{
    if(input$vk_colors %in% (plot_data()$e_data %>% colnames())){
      (is.numeric(plot_data()$e_data %>% pluck(input$vk_colors)) & !is_integer(plot_data()$e_data %>% pluck(input$vk_colors)))
    }else if(input$vk_colors %in% (plot_data()$e_meta %>% colnames())){
      (is.numeric(plot_data()$e_meta %>% pluck(input$vk_colors)) & !is_integer(plot_data()$e_meta %>% pluck(input$vk_colors)))
    }else if(input$vk_colors %in% c("bs1", "bs2")){
      FALSE
    }else TRUE
  }, ignoreNULL = FALSE)
  
 
  
  
  #-------- create a table that stores plotting information -------#
  # the table needs to grow with each click of the download button
  parmTable <- reactiveValues()
  # need to initialize the table and fill in values
  parmTable$parms <- data.frame(FileName = NA, PlotType = NA, SampleType = NA, G1 = NA, G2 = NA, BoundarySet = NA,
                                ColorBy = NA, ContinuousVariable = NA, UniqueCommon = NA, x_var = NA, y_var = NA,
                                UniqueCommonParameters = NA, ChartTitle = NA, XaxisTitle = NA,
                                YaxisTitle = NA, LegendTitle = NA, ColorPalette = NA, HiddenPalette = NA)
  
  observeEvent(input$add_plot, {
    
    # initialize a new line
    newLine <- data.frame(FileName = NA, PlotType = input$chooseplots, SampleType = NA, G1 = NA, G2 = NA, BoundarySet = NA,
                          ColorBy = NA, ContinuousVariable = NA, UniqueCommon = NA, x_var = NA, y_var = NA,
                          UniqueCommonParameters = NA, ChartTitle = NA, XaxisTitle = NA,
                          YaxisTitle = NA, LegendTitle = NA, ColorPalette = NA, HiddenPalette = NA)
    # fill values to a position depending on input$add_plot
    # which type of plot
    newLine$FileName <- paste("Plot", input$add_plot, sep = "")
    newLine$PlotType <- input$chooseplots
    # Single or Multiple Samples
    newLine$SampleType <- ifelse(input$choose_single == 1, yes = "Single Sample", no = "Multiple Samples")
    # Sample(s) in The first group (depends on input$choose_single to decide if this is a single or multiple sample list)
    newLine$G1 <- ifelse(input$choose_single %in% c(1,2), yes = paste(input$whichSamples, collapse = ","), no = paste(input$whichGroups1, collapse = ","))
    # Sample(s) in the second group. Automatically NA if input$choose_single is single sample or single group
    newLine$G2 <- ifelse(input$choose_single == 3, yes =  paste(input$whichGroups2, collapse = ","), no = NA)
    # Boundary set borders to use (NA for non-Van Krevelen plots)
    newLine$BoundarySet <- ifelse(input$chooseplots == "Van Krevelen Plot", yes = ifelse(input$vkbounds == 0, NA, input$vkbounds), no = "NA")
    # Color By
    newLine$ColorBy <- input$vk_colors
    newLine$UniqueCommon <- ifelse(input$choose_single == 3, yes = input$summary_fxn, no = "NA")
    newLine$ChartTitle <- input$title_input
    # special density plot treatment
    ylab <- input$y_axis_input
    if (input$chooseplots == 'Density Plot') {
      # if x axis input field is empty, get the display name of the color_by_choices vector index that equals vk_colors, otherwise use what the user typed
      xlab <- ifelse(isolate(is.null(input$x_axis_input) || input$x_axis_input == ""),
                      yes = names(revals$color_by_choices[revals$color_by_choices == input$vk_colors]),
                      no = isolate(input$x_axis_input))
    } else if (input$chooseplots == 'Custom Scatter Plot') {
      xlab <- isolate(ifelse(is.null(input$x_axis_input) | (input$x_axis_input == ""), 
                     yes = names(revals$color_by_choices[revals$color_by_choices == input$scatter_x]), 
                     no = input$x_axis_input))
      ylab <- isolate(ifelse(is.null(input$y_axis_input) | (input$y_axis_input == ""), 
                              yes = names(revals$color_by_choices[revals$color_by_choices == input$scatter_y]), 
                              no = input$y_axis_input))
      } else {
      xlab <- input$x_axis_input
    }
    newLine$XaxisTitle <- ifelse(is.na(xlab), yes = "default", no = xlab)
    newLine$YaxisTitle <- ifelse(is.na(ylab), yes = "default", no = ylab)
    newLine$LegendTitle <- ifelse((input$chooseplots == 'Density Plot') | is.null(input$legend_title_input), yes = "default", no = revals$legendTitle)
    newLine$x_var <- input$scatter_x
    newLine$y_var <- input$scatter_y
    if(!is.na(revals$colorPal)){
      newLine$ColorPalette <- strsplit(revals$colorPal, split = ":")[[1]][1]
      newLine$HiddenPalette <- revals$colorPal
    }
    else newLine$ColorPalette <- newLine$HiddenPalette <- NA
    
    if (input$add_plot == 1) {
      # replace the existing line on the first click
      parmTable$parms[input$add_plot, ] <- newLine
      exportTestValues(parmTable_1 = parmTable$parms)
    } else {
      # concat every new line after
      parmTable$parms <- rbind(parmTable$parms, newLine)
      exportTestValues(parmTable_1 = parmTable$parms)
    }
    
    
    #}
    
  }, priority = 7)
  
  
  output$parmsTable <- renderDataTable(parmTable$parms[,-18],
                                       options = list(scrollX = TRUE))
  
  # End Visualize tab #
  
  
  ####### Download Tab #######
  
  # copy the table from the visualize tab so as not to confuse javascript
  output$parmsTable2 <- DT::renderDataTable(parmTable$parms[,-18],
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
  
  #---- processed data download --------#
  output$download_processed_data <- downloadHandler(
    filename = paste("FREDA_Output_",proc.time(),".zip", sep = ""),
    content = function(fname){
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      fs <- vector()
      
      # ____test functionality____
      # if in testmode just select all rows since shinytest doesn't recognize row selection for parmsTable2
      if(isTRUE(getOption("shiny.testmode"))){
        rows <- 1:nrow(parmTable$parms)
      }
      else rows <- input$parmsTable2_rows_selected
      #
      
      if ("separate" %in% input$download_selection){
        fs <- c(fs, "FREDA_processed_e_data.csv", "FREDA_processed_e_meta.csv")
        write.csv(peakIcr2$e_data, file = "FREDA_processed_e_data.csv", row.names = FALSE)
        write.csv(peakIcr2$e_meta, file = "FREDA_processed_e_meta.csv", row.names = FALSE)
      }
      if ("merged" %in% input$download_selection){
        fs <- c(fs, "FREDA_processed_merged_data.csv")
        merged_data <- merge(peakIcr2$e_data, peakIcr2$e_meta)
        write.csv(merged_data, file = "FREDA_processed_merged_data.csv", row.names = FALSE)
      }
      
      if (length(rows) > 0) {
        bitmaps <- list()
        for (i in rows) {
          path <- paste(parmTable$parms$FileName[i],".", input$image_format, sep = "") #create a plot name
          fs <- c(fs, path) # append the new plot to the old plots
          export(renderDownloadPlots(parmTable = parmTable$parms[i,], peakIcr2),
                 file = paste("plot",i,".png", sep = ""), zoom = 2) # use webshot to export a screenshot to the opened pdf
          #r <- brick(file.path(getwd(), paste("plot",i,".png", sep = ""))) # create a raster of the screenshot
          img <- magick::image_read(paste("plot",i,".png", sep = ""))#attr(r,"file")@name) #turn the raster into an image of selected format
          
          if(isTRUE(getOption("shiny.testmode"))) bitmaps[[i]] <- as.raster(img)
          
          image_write(img, path = path, format = input$image_format) #write the image
          #rsvg::rsvg_svg(img, file = path)
        }
        
        # ___test-export___
        exportTestValues(images_out = digest::digest(bitmaps))
        
        fs <- c(fs, "Plot_key.csv")
        outtable <- parmTable$parms[rows, -18]
        write.csv( outtable, row.names = FALSE, file = "Plot_key.csv")
      }
      print(fs)
      
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname,".zip"))){file.rename(paste0(fname,".zip"),fname)}
    },
    contentType = "application/zip"
  )
  
  #----------- plot download ---------#
  # 
  #   output$download_plots <- downloadHandler(
  #     filename = 'pdfs.zip', #this creates a directory to store the pdfs...not sure why it's not zipping
  #     content = function(fname) { #write a function to create the content populating said directory
  #       fs <- c()
  #       tmpdir <- tempdir() # render the images in a temporary environment
  #       setwd(tempdir())
  #       print(tempdir())
  # 
  #       print(fs) #print all the pdfs to file
  #       zip(zipfile=fname, files=fs) #zip  it up (this isn't working for some reason!)
  #       if(file.exists(paste0(fname,".zip"))){file.rename(paste0(fname,".zip"),fname)} #bug workaround see https://groups.google.com/forum/?utm_medium=email&utm_source=footer#!msg/shiny-discuss/D5F2nqrIhiM/ZshRutFpiVQJ
  #     },
  #     contentType = "application/zip"
  #   )
  ####### Glossary Tab #######
  
  
  
  })
