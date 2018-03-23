
# Modify maximum file size (currently 250 mb)
options(shiny.maxRequestSize=250*1024^2, ch.dir = TRUE) 
library(shiny)
library(fticRanalysis)
library(ggplot2)
library(reshape2)


shinyServer(function(session, input, output) {
  
  # Source files for 'summaryFilt' and 'summaryPreprocess'
  source('summaryFilter.R') 
  source('summaryPreprocess.R')
  
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
                choices  = c('Select a column', emeta_cnames()))
    
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
           'Please select either Formula or Elemental columns')
      
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
      
      if (input$isotope_yn %in% c(0,2)) {
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
      if (input$isotope_yn %in% c(0,2)) {
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
    HTML('<h4 style= "color:#1A5276">Your data has been successfully uploaded. 
               You may proceed to the subsequent tabs for analysis.</h4>')
    
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
    
    HTML('<h4>Displaying uploaded e_data</h4>')
    
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
    HTML('<h4>Displaying uploaded e_meta</h4>')
    
  })# End emeta_text
  
  # Display preview for emeta
  # output$head_emeta <- renderDataTable({
  #   
  #   if (dim(Emeta())[2] > 6)
  #     head(Emeta())[,1:6]
  #   else
  #     head(Emeta())
  # }, # End code portion of head_emeta
  # 
  # # Options
  # options = list(dom = 't', searching = FALSE)
  # 
  # ) # End head_emeta
  # 
  ############## Filter tab ###############
  
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
    
    # # Create nonreactive peakICR object
    # peakIcr2 <<- peakICR()
    
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
    HTML('<h4 style= "color:#1A5276">Your data has been filtered using mass and/or minimum observations. 
         You may proceed to the next tabs for subsequnt analysis.</h4>')
    
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
  
  
  ####### Preprocess Tab #######
  
  #### Action Button reactions ####
  
  ## Action button: Apply calculation functions When action button is clicked
  # Depends on: peakIcr2, input$tests
  observeEvent(input$preprocess_click, {
    
    validate(need(input$tests, message = "Please choose at least one test to calculate"))
    # Apply all relevant functions
    peakIcr2 <<- compound_calcs(peakIcr2, calc_fns = c(input$tests))
  }) # End action button event
  
  # Object: Create dataframe of possible calculations to show in summary/histogram
  # Note: dependent on preprocess click and the user-specified calculations
  test_names <- eventReactive(input$preprocess_click, {
    
    # Error handling: peakIcr2 must have a non-NULL Kendrick Mass column name
    req(!is.null(attr(peakIcr2, 'cnames')$kmass_cname))
    
    # Get csv file of all possible calculation column names
    possible_calc_cnames <- read.csv("processedCols.csv", 
                                     header = FALSE, stringsAsFactors = FALSE)
    
    # Get column names from peakIcr2's e_meta
    actual_cnames <- colnames(peakIcr2$e_meta)
    
    # Find all columns with names that match names for calculated columns
    v_index <- which(possible_calc_cnames[,1] %in% actual_cnames)
    
    # Save calculation column names from above and their display names 
    possible_calc_cnames[v_index,]
    
  }) # End test_names
  
  #### Main Panel ####
  
  # Summary Panel: Display output of summaryPreprocess
  # Depends on: test_names
  output$summary_preprocess <- renderTable({
    
    # Error handling
    req(test_names())
    
    # Call summaryPreprocess
    summaryPreprocess(peakIcr2, test_names())
    
  }, # End code for summary_preprocess
  
  # Options for renderTable
  rownames = TRUE, 
  digits = 2 # This maybe needs to change?
  
  ) # End summary_preprocess
  
  # Drop down list: potential histogram options
  # Depends on: test_names
  output$which_hist <- renderUI({
    
    # Error handling: test_names required
    req(test_names())
    
    # Create named list with potential histogram options
    hist_choices <- unlist(test_names()[,1])
    names(hist_choices) <- test_names()[,2]
    
    # Drop down list 
    selectInput('which_hist', 'I would like to see the histogram across all values of...',
                choices = hist_choices)
    
  }) # End which_hist
  
  # Plot the histogram chosen above
  # Depends on: which_hist (above) and test_names
  output$preprocess_hist <- renderPlotly({
    
    # Error handling: Require which_hist and test_names (for display)
    req(input$which_hist)
    req(test_names())
    
    # Save column name for later display
    columnName <- input$which_hist
    
    # Get 'display name' from test_names
    displayName <- test_names()[which(test_names()[,1] == columnName), 2]
    
    # Plot histogram using plotly
    plot_ly(x = peakIcr2$e_meta[,columnName], type = 'histogram') %>%
      layout( title = paste('Histogram of ', displayName),
              scene = list(
                xaxis = list(title = displayName),
                yaxis = list(title = 'Frequency')))
    
  }) # End process_hist
  
  ####### Visualize Tab #######
  #### Sidebar Panel ####
  # choose ui to render depending on which plot is chosen from input$chooseplot
  output$plotUI <- renderUI({
    if (input$chooseplots == 0) {
      return(NULL)
    }
    if (input$chooseplots == 1) {
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
        ), # End conditional output, single sample #
        actionButton("plot_submit", label = "Sumbit")
      ))
    }
    if (input$chooseplots == 2) {
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
        ), # End conditional output, single sample #
        actionButton("plot_submit", label = "Sumbit")
      ))
    }
    
  })
  
  output$vk_colors <- renderUI({
    # (Conditional on vkbounds):
    # Error handling: test_names required
    req(test_names())
    
    # Create named list with potential histogram options
    hist_choices <- unlist(test_names()[,1])
    names(hist_choices) <- test_names()[,2]
    
    if(input$vkbounds == 0) {#no boundaries
      return(selectInput('vk_colors', 'Color by:', 
                         choices = c('Van Krevelen Boundary Set 1' = 'bs1',
                                     'Van Krevelen Boundary Set 2' = 'bs2', 
                                     hist_choices),
                         selected = 'bs1'))  
    } else if (input$vkbounds == 'bs1'){ #only allow bs1 boundary colors
      return(selectInput('vk_colors', 'Color by:', 
                         choices = c('Van Krevelen Boundary Set 1' = 'bs1',
                                     hist_choices),
                         selected = 'bs1'))
      
    } else if (input$vkbounds == 'bs2') { #only allow bs2 boundary colors
      selectInput('vk_colors', 'Color by:', 
                  choices = c('Van Krevelen Boundary Set 2' = 'bs2', 
                              hist_choices),
                  selected = 'bs2')
    }
  })
  
  #### Main Panel ####
  observeEvent(input$plot_submit, {
    # Make sure a plot stype selection has been chosen
    validate(need(input$choose_single != 0, message = "Please select plotting criteria"))
    if (input$choose_single == 1) {
      division_data <- subset(peakIcr2, input$whichSample)
      #key_name <- paste(attributes(peakIcr2)$cnames$fdata_cname, "=", input$whichSample, sep = "")
    }
    if (input$choose_single == 2) {
      division_data <- subset(peakIcr2, input$whichGroups1)
    }
    
    #-------Kendrick Plot-----------# 
    if (input$chooseplots %in% c(1,2)) {
      if (input$chooseplots == 1) plotFxn <- kendrickPlot
      if (input$chooseplots == 2) plotFxn <- vanKrevelenPlot
      output$FxnPlot <- renderPlotly({
        validate(need(!is.null(input$whichSample) | !is.null(input$whichGroups1),
                      message = "Please choose a sample below"))
        #-----boundary line logic------#
        if (input$vkbounds == 0) { #no bounds
          # if no boundary lines, leave the option to color by boundary
          if (input$vk_colors %in% c('bs1', 'bs2')) {
            return(plotFxn(division_data, showVKBounds = FALSE, vkBoundarySet = input$vk_colors))
          } else {
            # if no boundary lines and color selection doesn't belong to a boundary, color by test
            return(plotFxn(division_data, showVKBounds = FALSE, colorCName = input$vk_colors))
          }
        } else {
          # if boundary lines, allow a color by boundary class 
          if (input$vk_colors %in% c('bs1', 'bs2')) {
            return(plotFxn(division_data, vkBoundarySet = input$vkbounds, showVKBounds = TRUE))
          } else {
            # if boundary lines and color isn't a boundary class
            return(plotFxn(division_data, vkBoundarySet = input$vkbounds, showVKBounds = TRUE, colorCName = input$vk_colors))
            
          }
        }
      })
    }
  })
  
  # # Stubs: Kendrick and Van Krevelen plots
  # output$kendrick <- renderPlotly({
  #   # if (is.null(attr(peakIcr2, "cnames")$mf_cname)) {
  #   #   peakIcr2 <<- assign_mf(peakIcr2)
  #   # }
  #   if (input$choose_single == 1) {
  #     validate(need(!is.null(input$whichSample), message = "Please choose a sample below"))
  #     return(kendrickPlot(division_data()[[input$whichSample]]$value))
  #   }
  # })
  # 
  # output$vankrev <- renderPlotly({
  #   if (input$choose_single == 1) {
  #     validate(need(!is.null(input$whichSample), message = "Please choose a sample below"))
  #     return(vanKrevelenPlot(division_data()[[input$whichSample]]$value))
  #   }
  #   
  # })
  
  ####### Download Tab #######
  
  ####### Glossary Tab #######
  
  ####### Download Tab #######
  
  # at the end of the session, remove the peakIcr2 global object
  # TODO: this doesn't yet behave the way I expect it to
  if ("peakIcr2" %in% ls()) {
    on.exit(rm(list = list(peakIcr2)))
  }
  
})
