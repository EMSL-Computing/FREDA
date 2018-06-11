
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

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
#peakIcr2 <- NULL #when finished developing, uncomment this to clear the workspace on exit

shinyServer(function(session, input, output) {
  Sys.setenv(R_ZIPCMD="/usr/bin/zip")
  # Source files for 'summaryFilt' and 'summaryPreprocess'
  source('tooltip_checkbox.R')
  source('summaryFilter.R') 
  source('summaryPreprocess.R')
  source("renderDownloadPlots.R")
  
  revals <- reactiveValues(ntables = 0)
  
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
    
    selectInput("c_column", "Choose column representing C",
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
  
  output$iso_info_column <- renderUI({
    selectInput("iso_info_column", "Which column contains isotope information?",
                choices  = c('Select a column' = 0, emeta_cnames()))
    
  })
  
  output$iso_symbol <- renderUI({
    textInput("iso_symbol", label = "Enter a symbol denoting isotopic notation:",
              value = "1")
  })
  
  ### END of CHNOSP DROP DOWN LISTS ###
  
  
  #### Action Button Reactions (Upload Tab) ####
  
  # Object: Create peakICR when Upload Button clicked
  peakICR <- eventReactive(input$upload_click, {
    # Error handling: unique identifier chosen
    validate(
      need(input$edata_id_col != 'Select one', 
           'Please select a unique identifier column'), 
      need(input$select != 0, 
           'Please select either Formula or Elemental columns'),
      need(input$isotope_yn != 0,
           'Please select yes or no on information for isotopes')
      
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
      if (input$isotope_yn == 1) { # If there's C13 # 
        
        # Error handling: entered isotopic notation must exist in the isotope information column
        validate(
          need(input$iso_info_column != 0, message = "Please choose a column of isotopic information"),
          need(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol),
               'The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise.')
        ) # End error handling
        
        return(as.peakIcrData(e_data = Edata(), f_data = fdata(),
                              e_meta = Emeta(), edata_cname = input$edata_id_col, 
                              fdata_cname = 'SampleId', mass_cname = input$edata_id_col, 
                              instrument_type = input$instrument,
                              mf_cname = input$f_column,
                              isotopic_cname = input$iso_info_column,
                              isotopic_notation = as.character(input$iso_symbol)))
        
      } # End C13 / no C13 if statement
      
      if (input$isotope_yn == 2) { #no C13
        # Calculate peakIcrData with formula column
        return(as.peakIcrData(e_data = Edata(), f_data = fdata(),
                              e_meta = Emeta(), edata_cname = input$edata_id_col, 
                              fdata_cname = 'SampleId', mass_cname = input$edata_id_col, 
                              instrument_type = input$instrument, mf_cname = input$f_column))
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
      if (input$isotope_yn == 2) {
        # Create peakICR object
        return(as.peakIcrData(e_data = Edata(), f_data = fdata(),
                              e_meta = Emeta(), edata_cname = input$edata_id_col, 
                              fdata_cname = 'SampleId', mass_cname = input$edata_id_col,
                              instrument_type = input$instrument,
                              c_cname = input$c_column, h_cname = input$h_column, 
                              n_cname = input$n_column, o_cname = input$o_column, 
                              s_cname = input$s_column, p_cname = input$p_column))
        
      }
      if (input$isotope_yn == 1) { # If there's C13 # 
        
        # Error handling: entered isotopic notation must exist in the isotope information column
        validate(
          need(input$iso_info_column != 0, message = "Please choose a column of isotopic information"),
          need(any(Emeta()[,input$iso_info_column] %in% input$iso_symbol),
               'The entered isotopic notation does not match the entries in the chosen isotope information column. Please revise.')
        ) # End error handling
        
        return(as.peakIcrData(e_data = Edata(), f_data = fdata(),
                              e_meta = Emeta(), edata_cname = input$edata_id_col, 
                              fdata_cname = 'SampleId', mass_cname = input$edata_id_col, 
                              instrument_type = input$instrument,
                              c_cname = input$c_column, h_cname = input$h_column, 
                              n_cname = input$n_column, o_cname = input$o_column, 
                              s_cname = input$s_column, p_cname = input$p_column, 
                              isotopic_cname = input$iso_info_column,
                              isotopic_notation = as.character(input$iso_symbol)))
        
      } # End C13 / no C13 if statement
      
    } # End elemental column if statement
    
  }) # End peakICR creation
  
  
  #### Main Panel (Upload Tab) ####
  
  # Display success message OR display errors
  output$success_upload <- renderUI({
    
    # Error handling: peakICR() must exist
    req(peakICR())
    
    # Create nonreactive peakICR object
    peakIcr2 <<- peakICR()
    
    # Create fake dependency on peakICR()
    test1 <- peakICR()$e_data #why is this here?
    
    # If no errors, show Success message
    HTML('<h4 style= "color:#1A5276">You may proceed to data filtering</h4>')
    
  }) # End success #
  
  observeEvent(peakICR(),
               # # Error handling: peakICR() must exist
               # req(peakICR())
               showModal(
                 modalDialog(
                   title = "Upload message",
                   HTML('<h4 style= "color:#1A5276">Your data has been successfully uploaded. 
               You may proceed to the subsequent tabs for analysis.</h4>')
                 )
               )
  )
  
  # Summary: Display number of peaks and samples
  output$num_peaks <- renderText({
    validate(
      need(!is.null(peakICR()), message = "")
    )
    c('Number of peaks: ', nrow(peakICR()$e_data))
    
  }) # End num_peaks
  
  output$num_samples <- renderText({
    validate(
      need(!is.null(peakICR()), message = "")
    )
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
      # If isotopic information is included and matching entered notation, filter out where isotopes = denoted symbol
      if ((!input$isotope_yn %in% c("0","2") ) && (any(Emeta()[,input$iso_info_column] %in% input$iso_symbol))) {
        
        iso <- Emeta()[,input$iso_info_column]
        elem_columns <- elem_columns[-(which(as.character(iso) == as.character(input$iso_symbol))),]
        
      } # End if isotopic information is chosen and correctly denoted#
      
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
    
    HTML('<h4>Displaying uploaded Data File</h4>')
    
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
  # Display explanation for e_meta
  output$emeta_text <- renderUI({
    
    req(Emeta())
    HTML('<h4>Displaying uploaded Molecular Identification File</h4>')
    
  })# End emeta_text
  
  ####### Preprocess Tab #######
  
  
  
  #### Populate List from CSV File ####
  
  output$which_calcs <- renderUI({
    choices <- calc_opts$Function
    names(choices) <- calc_opts$DisplayName
    
    title0 <-  paste0("Cox Gibbs Free Energy. Calculated as:",
                   "60.3 - 28.5*NOSC ",
                   tags$a(href = "https://www.sciencedirect.com/science/article/pii/S0016703711000378", "[LaRowe & Van Cappellen, 2011]"))
    
    tooltip_checkbox("tests", "What Values Should be Calculated?", choices, selected = c("calc_element_ratios", "calc_kendrick"),
                     extensions = lapply(1:length(choices), function(i){  
                       tipify(icon("question-sign", lib = "glyphicon"), title = title0, placement = "top", trigger = 'hover')
                       # tipify(bsButton(paste0("Option", as.character(i)), "", icon = icon("question-sign", lib = "glyphicon"), size = "extra-small"),
                       #        title = "", placement = "top", trigger = 'click')
                     }))
    
    #checkboxGroupInput("tests", "What Values should be Calculated?", choices, selected = c("calc_element_ratios", "calc_kendrick"))
  })
  
  #### Action Button reactions ####
  
  ## Action button: Apply calculation functions When action button is clicked
  # Depends on: peakIcr2, input$tests
  observeEvent(input$preprocess_click, {
    validate(need(input$tests, message = "Please choose at least one test to calculate"))
    
    ## Include functionality to possibly reset 
    
    #peakIcr2$e_meta <<- peakIcr2$e_meta %>% select(-one_of(calc_vars$ColumnName))   
    
    # Apply all relevant functions
    for(el in input$tests){
      # set f to the function that is named in the ith element of compound_calcs # 
      f <- get(el, envir=asNamespace("fticRanalysis"), mode="function")
      peakIcr2 <<- f(peakIcr2)
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
  output$which_hist <- renderUI({
    
    # Error handling: input csv of calculations variables required
    req(calc_vars, revals$numeric_cols)

    # Create named list with potential histogram options
    hist_choices <- intersect(calc_vars$ColumnName, peakIcr2$e_meta %>% colnames())
    names(hist_choices) <- calc_vars %>% filter(ColumnName %in% hist_choices) %>% pluck("DisplayName")
    # Drop down list 
    tagList(
      br(),
      br(),
      tags$p('I would like to see a histogram/bar-chart across all values of:'),
      selectInput('which_hist', NULL,
                  choices = hist_choices)
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
    return(p)
    
  }) # End process_hist
  
  ############## Filter tab ##############
  # ----- Filter Reset Setup -----# 
  # Keep a
  # reactive copy of the pre-filtered data in case of a filter reset event
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
    
    selectInput('minobs', "Minimum number of times observed", 
                choices = seq(1, (length(edata_cnames()) - 1), 1), selected = 2)
    
  }) # End minobs
  output$head_emeta <- DT::renderDataTable(expr = Emeta(),
                                           options = list(scrollX = TRUE))
  #### Action Button Reactions (Filter Tab) ####
  
  # Event: Create filtered nonreactive peakIcr2 when action button clicked
  # Depends on action button 'filter_click'
  observeEvent(input$filter_click, {
    
    # if the data is already filtered start over from the uploaded data
    if(any(c("moleculeFilt", "massFilt") %in% names(attributes(peakIcr2)$filters))){
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
    if (input$molfilter){
  
      # Create and apply molecule filter to nonreactive peakICR object
      filterMols <- molecule_filter(peakIcr2)
      peakIcr2 <<- applyFilt(filterMols, peakIcr2, min_num = as.integer(input$minobs))
      
    } # End molecule filter if statement
    
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
                  HTML('<h4 style= "color:#1A5276">Your data has been filtered using mass and/or minimum observations. 
                       You may proceed to the next tabs for subsequnt analysis.</h4>'))
    )
    HTML('<h4 style= "color:#1A5276">You may now proceed to preprocessing and visualization</h4>')
    
  }) # End successMessage
  
  # Display successMessage
  # Depends on: successMessage
  output$filterTest <- renderUI({
    
    successMessage()
    
  })
  
  # Object: Get data frame from summaryFilt
  # Depends on: peakIcr2, checkboxes for filters, and inputs for filters
  summaryFilterDataFrame <- reactive({
    # TODO: Improve this logic so I don't have to hard code min_mass and max_mass defaults
    min_mass <- 200
    max_mass <- 900
    
    # If mass filter checked
    if(input$massfilter) {
      
      # Error handling: Require min_mass and max_mass to exist
      req(input$min_mass)
      req(input$max_mass)
      req(input$min_mass < input$max_mass)
      
      # Set min_mass and max_mass to actual entries
      min_mass <- input$min_mass
      max_mass <- input$max_mass
    }
    
    # Get summary table from sourced file 'summaryFilter.R'
    summaryFilt(peakICR(), input$massfilter, min_mass, 
                max_mass, input$molfilter, input$minobs)
    
  }) # End summaryFilterDataFrame
  
  
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
    
    # Find which row in summaryFilterDataFrame represents the Unfiltered information
    rowNum = which(summaryFilterDataFrame()$data_state == 'Unfiltered')
    
    # Create a dataframe out of Before and After results from summaryFilterDataFrame
    summary_table <- data.frame('Before' = unlist(summaryFilterDataFrame()[rowNum, c('sum_peaks', 'assigned', 
                                                                                     'min_mass', 'max_mass')]),
                                'After' = afterResults,
                                row.names = c('Number of peaks',
                                              'Number of peaks assigned a formula', 
                                              'Minimum mass observed', 
                                              'Maximum Mass observed'))
    
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
    # Melt dataframe into 2 objects
    ggdata_barplot <- melt(summaryFilterDataFrame()[,c('data_state', 'assigned', 'unassigned')])
    ggdata_text <- summaryFilterDataFrame()[, c('data_state', 'sum_peaks', 'dispText')]
    
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
    }
    peakIcr2 <<- uploaded_data()
    removeModal()
  })
  
  ####### Visualize Tab #######
  #### Sidebar Panel ####
  # choose ui to render depending on which plot is chosen from input$chooseplot
  output$plotUI <- renderUI({
    validate(
      need(input$chooseplots != 0, message = "Please select plotting criteria")
    )
    #------ Van Krevelen Sidebar Options ---------#
    if (input$chooseplots == 'Van Krevelen Plot') {
      return(tagList(
        # Drop down list: single samples or multiple?
        selectInput('choose_single', 'I want to plot using:',
                    choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples by group' = 2, 'A comparison of groups' = 3),
                    selected = 0), 
        
        # (Conditional on choose_single) If Multiple: show options for grouping
        conditionalPanel(
          condition = 'input.choose_single == 2',
          
          fluidRow(
            ######### MAKE GROUPS MUTUALLY EXCLUSIVE ##########
            # Column with width 6: which samples are in Group 1?
            column(12,
                   selectInput('whichGroups1', 'Group 1',
                               choices = sample_names(),
                               multiple = TRUE)
            )#,
            
            # Column with width 6: which samples are in Group 2?
            # column(6,
            #        uiOutput('whichGroups2')
            # )
          )
          
        ), # End conditional output multiple samples#
        
        # (Conditional on choose_single) If single: choose sample
        conditionalPanel(
          condition = 'input.choose_single == 1',
          
          selectInput('whichSample', 'Sample',
                      choices = sample_names())
        ) # End conditional output, single sample #
      ))
    }
    #------ Kendrick Sidebar Options ---------#
    if (input$chooseplots == 'Kendrick Plot') {
      return(tagList(
        # Drop down list: single samples or multiple?
        selectInput('choose_single', 'I want to plot using:',
                    choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples by group' = 2, 'A comparison of groups' = 3),
                    selected = 0), 
        
        # (Conditional on choose_single) If Multiple: show options for grouping
        conditionalPanel(
          condition = 'input.choose_single == 2',
          
          fluidRow(
            ######### MAKE GROUPS MUTUALLY EXCLUSIVE ##########
            # Column with width 6: which samples are in Group 1?
            column(12,
                   selectInput('whichGroups1', 'Group 1',
                               choices = sample_names(),
                               multiple = TRUE)
            )#,
            
            # Column with width 6: which samples are in Group 2?
            # column(6,
            #        uiOutput('whichGroups2')
            # )
          )
          
        ), # End conditional output multiple samples#
        
        # (Conditional on choose_single) If single: choose sample
        conditionalPanel(
          condition = 'input.choose_single == 1',
          
          selectInput('whichSample', 'Sample',
                      choices = sample_names())
        ) # End conditional output, single sample #
      ))
    }
    #------ Density Sidebar Options ---------#
    if (input$chooseplots == 'Density Plot') {
      return(tagList(
        # Drop down list: single samples or multiple?
        selectInput('choose_single', 'I want to plot using:',
                    choices = c('Make a selection' = 0, 'A single sample' = 1, 'Multiple samples by group' = 2, 'A comparison of groups' = 3),
                    selected = 0), 
        
        # (Conditional on choose_single) If Multiple: show options for grouping
        conditionalPanel(
          condition = 'input.choose_single == 2',
          
          fluidRow(
            ######### MAKE GROUPS MUTUALLY EXCLUSIVE ##########
            # Column with width 6: which samples are in Group 1?
            column(12,
                   selectInput('whichGroups1', 'Group 1',
                               choices = sample_names(),
                               multiple = TRUE)
            )#,
            
            # Column with width 6: which samples are in Group 2?
            # column(6,
            #        uiOutput('whichGroups2')
            # )
          )
          
        ), # End conditional output multiple samples#
        
        # (Conditional on choose_single) If single: choose sample
        conditionalPanel(
          condition = 'input.choose_single == 1',
          
          selectInput('whichSample', 'Sample',
                      choices = sample_names())
        )# End conditional output, single sample #
      ))
    }
    
  })
  output$vkbounds <- renderUI({
    validate(
      need(input$choose_single != 0, message = "Please select sample(s)"),
      need(input$chooseplots != 0, message = "")
    )
    return(selectInput('vkbounds', 'Use Van Krevelen boundary set:',
                       choices = c('BS1' = 'bs1', 'BS2' = 'bs2', 'None' = 0),
                       selected = 'bs1'))
  })
  
  display_name_choices <- reactive({
    req(calc_vars)
    validate(
      need(input$chooseplots != 0 & input$choose_single !=0, message = "Please select plotting criteria")
    )
    # Create named list with potential histogram options
    hist_choices <- intersect(calc_vars$ColumnName, peakIcr2$e_meta %>% colnames())
    names(hist_choices) <- calc_vars %>% filter(ColumnName %in% hist_choices) %>% pluck("DisplayName")
    # append the prettified calculation options with any other column in emeta
    meta_hist_choices <- colnames(peakIcr2$e_meta)[!(colnames(peakIcr2$e_meta) %in% hist_choices)]
    names(meta_hist_choices) <- meta_hist_choices
    # combine prettified names and not prettified names
    hist_choices <- c(meta_hist_choices, hist_choices)
    # don't allow columns of all unique values (e.g. MolForm)
    all_unique <- unlist(lapply(peakIcr2$e_meta, function(x) length(unique(x)) > 20 & !is.numeric(x)))
    hist_choices <- hist_choices[!all_unique]
  })
  output$vk_colors <- renderUI({
    req(input$chooseplots)
    # (Conditional on vkbounds):
    # Error handling: input csv required
    req(calc_vars)
    validate(
      need(input$chooseplots != 0 & input$choose_single != 0, message = "Please select plotting criteria")
    )
    # Create named list with potential histogram options
    # hist_choices <- intersect(calc_vars$ColumnName, peakIcr2$e_meta %>% colnames())
    # names(hist_choices) <- calc_vars %>% filter(ColumnName %in% hist_choices) %>% pluck("DisplayName")
    # # append the prettified calculation options with any other column in emeta
    # meta_hist_choices <- colnames(peakIcr2$e_meta)[!(colnames(peakIcr2$e_meta) %in% hist_choices)]
    # names(meta_hist_choices) <- meta_hist_choices
    # # combine prettified names and not prettified names
    hist_choices <- display_name_choices()#c(hist_choices, meta_hist_choices)
    #----- group summary color choices -------#
    if (input$choose_single == 2) {
      hist_choices <- getGroupSummaryFunctionNames()
      return(selectInput('vk_colors', 'Color by:', 
                         choices = c(hist_choices),
                         selected = hist_choices[1]))
    }
    #------- density plot color choices --------#
    if (input$chooseplots == 'Density Plot') {
      return(selectInput('vk_colors', 'Color by:', 
                         choices = c(hist_choices),
                         selected = hist_choices[1]))
    }
    # Kendrick Colors
    if (input$chooseplots == 'Kendrick Plot') {
      return(selectInput('vk_colors', 'Color by:', 
                         choices = c('Van Krevelen Boundary Set 1' = 'bs1',
                                     'Van Krevelen Boundary Set 2' = 'bs2', 
                                     hist_choices),
                         selected = 'bs1'))  
    }
    if (input$chooseplots == 'Van Krevelen Plot') {
      if (input$vkbounds == 0) {#no boundaries
        if (input$choose_single == 2) {
          hist_choices <- getGroupSummaryFunctionNames()
          return(selectInput('vk_colors', 'Color by:', 
                             choices = c(hist_choices),
                             selected = hist_choices[1]))
        } else {
          return(selectInput('vk_colors', 'Color by:', 
                             choices = c('Van Krevelen Boundary Set 1' = 'bs1',
                                         'Van Krevelen Boundary Set 2' = 'bs2', 
                                         hist_choices),
                             selected = 'bs1'))  
        }
      } else if (input$vkbounds == 'bs1') { #only allow bs1 boundary colors
        if (input$choose_single == 2) {
          return(selectInput('vk_colors', 'Color by:', 
                             choices = c(hist_choices),
                             selected = hist_choices[1]))
        } else {
          return(selectInput('vk_colors', 'Color by:', 
                             choices = c('Van Krevelen Boundary Set 1' = 'bs1',
                                         hist_choices),
                             selected = 'bs1'))
        }
      } else if (input$vkbounds == 'bs2') { #only allow bs2 boundary colors
        if (input$choose_single == 2) {
          return(selectInput('vk_colors', 'Color by:', 
                             choices = c(hist_choices),
                             selected = hist_choices[1]))
        } else {
          selectInput('vk_colors', 'Color by:', 
                      choices = c('Van Krevelen Boundary Set 2' = 'bs2', 
                                  hist_choices),
                      selected = 'bs2')
        }
      } 
    }
  })
  
  #### Main Panel ####
  v <- reactiveValues(clearPlot = TRUE)
  observeEvent(c(input$chooseplots, input$choose_single, input$whichGroups1, input$whichSample), {
    v$clearPlot <- TRUE
  }, priority = 10)
  observeEvent(input$plot_submit, {
    v$clearPlot <- FALSE
  }, priority = 10)
  
  output$FxnPlot <- renderPlotly({
    if (v$clearPlot){
      return(NULL)
    } else {
      # Make sure a plot stype selection has been chosen
      validate(need(input$choose_single != 0, message = "Please select plotting criteria"))
      if (input$choose_single == 1) { #single sample
        # Make sure at least one test has been calculated
        division_data <- subset(peakIcr2, input$whichSample)
        #key_name <- paste(attributes(peakIcr2)$cnames$fdata_cname, "=", input$whichSample, sep = "")
      }
      #---------- Group Plots ------------#
      else if (input$choose_single == 2) {# single group
        # Make sure at least one test has been calculated
       # browser()
        validate(need(!is.null(input$whichGroups1), message = "Please select samples for grouping"))
        division_data <- subset(peakIcr2, input$whichGroups1)
        summarized_data <- summarizeGroups(division_data, summary_functions = input$vk_colors)
        #-------Kendrick Plot-----------# 
        if (input$chooseplots == 'Kendrick Plot') {
            p <- groupKendrickPlot(summarized_data, colorCName = input$vk_colors,
                                   xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                   title = input$title_input, legendTitle = input$legend_title_input)
        }
      }
      #----------- Single sample plots ------------#
      #-------Kendrick Plot-----------# 
      if (input$chooseplots == 'Kendrick Plot') {
        if (input$choose_single == 2) {
            p <- groupKendrickPlot(summarized_data, colorCName = input$vk_colors)
        } else if (input$choose_single == 1) {
            validate(need(!is.null(input$whichSample) | !is.null(input$whichGroups1),
                          message = "Please choose a sample below"))
            if (input$vk_colors %in% c('bs1', 'bs2')) {
              p <- kendrickPlot(division_data, vkBoundarySet = input$vk_colors,
                                xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                title = input$title_input,legendTitle = input$legend_title_input)
            } else {
              # if color selection doesn't belong to a boundary, color by test
              p <- kendrickPlot(division_data, colorCName = input$vk_colors,
                                xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                title = input$title_input,legendTitle = input$legend_title_input)
            }
        }
      }
      #-------VanKrevelen Plot--------#
      if (input$chooseplots == 'Van Krevelen Plot') {
        if (input$choose_single == 2) {
          if (input$vkbounds == 0) {
              p <- groupVanKrevelenPlot(summarized_data, colorCName = input$vk_colors, showVKBounds = FALSE,
                                        xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                        title = input$title_input,legendTitle = input$legend_title_input)
          } else {
              p <- groupVanKrevelenPlot(summarized_data, colorCName = input$vk_colors, vkBoundarySet = input$vkbounds,
                                        xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                        title = input$title_input,legendTitle = input$legend_title_input)
          }
          
        } else if (input$choose_single == 1) {
            validate(need(!is.null(input$whichSample) | !is.null(input$whichGroups1),
                          message = "Please choose a sample below"))
            #-----boundary line logic------#
            if (input$vkbounds == 0) { #no bounds
              # if no boundary lines, leave the option to color by boundary
              if (input$vk_colors %in% c('bs1', 'bs2')) {
                p <- vanKrevelenPlot(division_data, showVKBounds = FALSE, vkBoundarySet = input$vk_colors,
                                     xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                     title = input$title_input,legendTitle = input$legend_title_input)
              } else {
                # if no boundary lines and color selection doesn't belong to a boundary, color by test
                p <- vanKrevelenPlot(division_data, showVKBounds = FALSE, colorCName = input$vk_colors,
                                     xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                     title = input$title_input,legendTitle = input$legend_title_input)
              }
            } else {
              # if boundary lines, allow a color by boundary class 
              if (input$vk_colors %in% c('bs1', 'bs2')) {
                p <- vanKrevelenPlot(division_data, vkBoundarySet = input$vkbounds, showVKBounds = TRUE,
                                     xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                     title = input$title_input,legendTitle = input$legend_title_input)
              } else {
                # if boundary lines and color isn't a boundary class
                p <- vanKrevelenPlot(division_data, vkBoundarySet = input$vkbounds, showVKBounds = TRUE, colorCName = input$vk_colors,
                                     xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                                     title = input$title_input,legendTitle = input$legend_title_input)
                
              }
            }
        }
        
        # p <- p %>% layout(xaxis = list(scaleanchor = "y", constraintoward = "left")) # SQUARE SCALING
        
        p
      }
      
      #--------- Density Plot --------#
      if (input$chooseplots == 'Density Plot') {
        #return({
          input$vk_colors
          validate(
            need(!is.null(input$whichSample) | !is.null(input$whichGroups1),
                 message = "Please choose a sample below"),
            need(!is.na(input$vk_colors), message = "Please select a variable to color by")
          )
          p <- densityPlot(division_data, variable = input$vk_colors,
                           xlabel = input$x_axis_input, ylabel = input$y_axis_input,
                           title = input$title_input)
        #})
      }
    }
    
    x <- list(
      titlefont = f
    )
    y <- list(
      titlefont = f
    )
    p$elementId <- NULL
    layout(p, xaxis = x, yaxis = y)
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
      validate(need(!is.null(input$vk_colors), message = "select a color"))
      #defs$legendTitle = names(display_name_choices())[display_name_choices() == input$vk_colors]
    } else if (input$chooseplots == 'Kendrick Plot') {
      defs <- formals(kendrickPlot)
      #defs$legendTitle = input$vk_colors
    } else if (input$chooseplots == 'Density Plot') {
      defs <- formals(densityPlot)
      defs$ylabel = "Density"
      defs$xlabel = input$vk_colors
    }
    return(defs)
  })
  output$title_input <- renderUI({
    validate(
      need(input$chooseplots != 0, message = "")
    )
    textInput(inputId = "title_input", label = "Plot Title", value = plot_defaults()$title)
  })
  output$x_axis_input <- renderUI({
    validate(
      need(input$chooseplots != 0, message = "")
    )
    textInput(inputId = "x_axis_input", label = "X Axis Label", value = plot_defaults()$xlabel)
  })
  output$y_axis_input <- renderUI({
    validate(
      need(input$chooseplots != 0, message = "")
    )
    textInput(inputId = "y_axis_input", label = "Y Axis Label", value = plot_defaults()$ylabel)
  })
  
  # output$legend_title_input <- renderUI({
  #   if (input$chooseplots == 'Density Plot') {
  #     return(NULL)
  #   } else{
  #     textInput(inputId = "legend_title_input", label = "Legend Label", value = plot_defaults()$legendTitle)
  #   }
  # })
  
  #-------- create a table that stores plotting information -------#
  # the table needs to grow with each click of the download button
  parmTable <- reactiveValues()
  # need to initialize the table and fill in values
  parmTable$parms <- data.frame(PlotType = NA, SampleType = NA, G1 = NA, G2 = NA, BoundarySet = NA,
                                ColorBy = NA, ContinuousVariable = NA, UniqueCommon = NA,
                                UniqueCommonParameters = NA,FileName = NA, ChartTitle = NA, XaxisTitle = NA,
                                YaxisTitle = NA)#, LegendTitle = NA)
  
  observeEvent(input$add_plot, {
    # initialize a new line
    newLine <- data.frame(PlotType = input$chooseplots, SampleType = NA, G1 = NA, G2 = NA, BoundarySet = NA,
                          ColorBy = NA, ContinuousVariable = NA, UniqueCommon = NA,
                          UniqueCommonParameters = NA,FileName = NA, ChartTitle = NA, XaxisTitle = NA,
                          YaxisTitle = NA)#, LegendTitle = NA)
    # fill values to a position depending on input$add_plot
    # which type of plot
    newLine$PlotType <- input$chooseplots
    # Single or Multiple Samples
    newLine$SampleType <- ifelse(input$choose_single == 1, yes = "Single Sample", no = "Multiple Samples")
    # Sample(s) in The first group (depends on input$choose_single to decide if this is a single or multiple sample list)
    newLine$G1 <- ifelse(input$choose_single == 1, yes = input$whichSample, no = paste(input$whichGroups1, collapse = ","))
    # Sample(s) in the second group. Automatically NA if input$choose_single is single sample or single group
    newLine$G2 <- ifelse(input$choose_single %in% c(1,2), yes = "NA", no = "not yet available")
    # Boundary set borders to use (NA for non-Van Krevelen plots)
    newLine$BoundarySet <- ifelse(input$chooseplots == "Van Krevelen Plot", yes = input$vkbounds, no = "NA")
    # Color By
    newLine$ColorBy <- input$vk_colors
    newLine$ChartTitle <- input$title_input
    newLine$XaxisTitle <- ifelse(is.na(input$x_axis_input), yes = "default", no = input$x_axis_input)
    newLine$YaxisTitle <- ifelse(is.na(input$y_axis_input), yes = "default", no = input$y_axis_input)
    newLine$FileName <- paste("Plot", input$add_plot, ".pdf", sep = "")
    # Nope, plotly doesn't title legends on categorical vars
    # so don't allow this option anymorenewLine$LegendTitle <- ifelse(input$chooseplots == 'Density Plot', yes = "default", no = input$legend_title_input)
    
    if (input$add_plot == 1) {
      # replace the existing line on the first click
      parmTable$parms[input$add_plot, ] <- newLine
    } else {
      # concat every new line after
      parmTable$parms <- rbind(parmTable$parms, newLine)
    }
    
    
    #}
    
  }, priority = 7)
  
  
  output$parmsTable <- renderDataTable(parmTable$parms,
                                       options = list(scrollX = TRUE))
  
  # End Visualize tab
  ####### Download Tab #######
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
  
  #---- processed data download --------#
  output$download_processed_data <- downloadHandler(
    filename = paste("FREDA_Output_",proc.time(),".zip", sep = ""),
    content = function(fname){
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      fs <- vector()
      
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
      
      if (length(input$parmsTable2_rows_selected) > 0) {
        for (i in input$parmsTable2_rows_selected) {
          path <- parmTable$parms$FileName[i] #create a plot name
          fs <- c(fs, path) # append the new plot to the old plots
          export(renderDownloadPlots(parmTable = parmTable$parms[i,], peakIcr2),
                 file = paste("plot",i,".png", sep = ""), zoom = 2) # use webshot to export a screenshot to the opened pdf
          r <- brick(file.path(getwd(), paste("plot",i,".png", sep = ""))) # create a raster of the screenshot
          img <- magick::image_read(attr(r,"file")@name) #turn the raster into an image of selected format
          image_write(img, path = path, format = "pdf") #write the image
        }
        fs <- c(fs, "Plot_key.csv")
        outtable <- parmTable$parms[input$parmsTable2_rows_selected, ]
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
