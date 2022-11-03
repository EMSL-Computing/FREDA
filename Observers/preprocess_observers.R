## Action button: Apply calculation functions When action button is clicked
# Depends on: revals$uploaded_data, input$tests
observeEvent(input$preprocess_click, {
  validate(need(input$tests, message = "Please choose at least one test to calculate"))
  req(!is.null(revals$uploaded_data))
  
  disable('preprocess_click')
  shinyjs::show('preprocess_waiting', anim=T)
  on.exit({
    enable('preprocess_click')
    shinyjs::hide('preprocess_waiting', anim=T)
  })
  
  # Apply all relevant functions
  withProgress(message = "Calculating Values....",{
    
    temp <- revals$uploaded_data
    
    ### construct params
    arguments <- list()
    choices <- calc_opts$Function
    
    tryCatch({ 
      for(x in choices) {
        if(x == 'calc_kendrick'){
          if(!is.null(input$base_unit)){
            arguments[[x]] <- list(base_compounds = input$base_unit)
          }
        }
        else if(x == 'calc_dbe'){
          if(!is.null(input$dbe_valences) & isTRUE(input$dbe_valences != '')){
            # input argument to calc_dbe is a dataframe with valences fore each element
            valence_df <-  data.frame('C' = numeric(0), 'H' = numeric(0), 'N' = numeric(0), 'O' = numeric(0), 'S' = numeric(0), 'P' = numeric(0))
            
            valence_list <- strsplit(input$dbe_valences, ';')[[1]]
            valence_list <- lapply(valence_list, function(el){
              V <- strsplit(el, '(?<=.)(?=(C|H|N|O|S|P)[0-9]*)', perl=T)[[1]] # Stare not into the void that is regex.  I have looked and seen the terror within; the terror that consumes the souls of men and deposits their husks like sand dollars on the beach of eternity.
              counts <- lapply(V, function(x){
                  if(x == ''){
                    2 # if they didnt specify, assume the valence that doesnt contribute to the dbe
                  }
                  else if(!grepl('[0-9]', x)){
                    1 # specified without number, assume they mean one, i.e. H = H1
                  }
                  else as.numeric(gsub("[A-Z]", '', x))
                })
                Vnames <- sapply(V, function(x) gsub("[0-9]", '', x))
                names(counts) <- Vnames
                counts
              })
            
            # populate the empty dataframe
            for(i in 1:length(valence_list)){
              V <- valence_list[[i]]
              for(name in names(V)){
                valence_df[i,name] <- V[[name]]
              }
            }
            
            arguments[[x]] <- list(valences = valence_df)
          }
        } # end DBE
      } # end for
    },
    error = function(e){
      msg <<- sprintf('Something went wrong applying extra options, double check your input.  System error:  %s', e)
      revals$warningmessage_preprocess$get_args <<- sprintf("<p style = 'color:red'>%s</p>", msg)
      NULL
    })
    
    tryCatch({
      revals$warningmessage_preprocess$makeobject_error <- NULL
      for(el in isolate(input$tests)){
        args <- arguments[[el]]
        
        if(grepl("assign_class", el)){
          foo <- strsplit(el, ";")[[1]]
          f <- get(foo[1], envir=asNamespace("ftmsRanalysis"), mode="function")
          temp <- f(temp, foo[2])
          temp$e_meta[paste0(foo[2], "_class")] <- gsub(";.*", "", temp$e_meta[,paste0(foo[2], "_class")])
        }
        else{
          f <- get(el, envir=asNamespace("ftmsRanalysis"), mode="function")
          temp <- if(is.null(arguments[[el]])) f(temp) else do.call(f, c(list(temp), arguments[[el]]))
        }
        
        incProgress(1/length(input$tests))
      }
    },
    error = function(e){
      msg = paste0('Error calculating some of your variables: \n System error: ', e)
      revals$warningmessage_preprocess$makeobject_error <<- sprintf("<p style = 'color:red'>%s</p>", msg)
    })
    
    if(!exists('msg')) {
      revals$uploaded_data <- temp
      exportTestValues(uploaded_data_processed = revals$uploaded_data)
    }
  })
  
}, priority = 10) # End action button event

# Creates two reactive variables for continuous and categorical variables which are used to display separate tables
# Note: dependent on preprocess click and the user-specified calculations
observeEvent(input$preprocess_click, {
  req(input$tests)
  
  # Get column names from revals$uploaded_data's e_meta
  actual_cnames <- colnames(revals$uploaded_data$e_meta)
  
  # Find all columns with names that match names for calculated columns
  v_index <- which(names(PREPROCESS_CALC_VARS) %in% actual_cnames)
  
  # Save calculation column names from above and their display names 
  present_vars <- PREPROCESS_CALC_VARS[v_index]
  
  # get numeric columns
  numeric_cols <- revals$uploaded_data$e_meta %>% 
    dplyr::select(which(sapply(.[,names(present_vars)], is.numeric))) %>% 
    names()
  
  # get categorical columns
  categorical_cols <- revals$uploaded_data$e_meta %>% 
    dplyr::select(which(!sapply(.[names(present_vars)], is.numeric))) %>%
    names() 
  
  #set reactive variables for observers
  revals$numeric_cols <- present_vars[names(present_vars) %in% numeric_cols]
  revals$categorical_cols <- present_vars[names(present_vars) %in% categorical_cols]
  
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
                  selected = colnames(revals$uploaded_data$e_meta)[ncol(revals$uploaded_data$e_meta) + 1])
    )
  }) 
})# End which_hist

### Summary Panel: Display table summaries of numeric and categorical columns in e_meta ###

observeEvent(input$preprocess_dismiss,{
  
  req(c(revals$numeric_cols, revals$categorical_cols))
  
  if(isTRUE(length(revals$numeric_cols) > 0)){
    tmp_num_table <- summaryPreprocess(isolate(revals$uploaded_data), revals$numeric_cols)
    
    revals$preprocess_tables$numeric <- tmp_num_table %>%
                                          datatable(options = list(dom = "t", pageLength = nrow(.))) %>% 
                                          formatRound(colnames(tmp_num_table), digits = 2)
    
    # __SHINYTEST__
    exportTestValues(numeric_table = tmp_num_table)
  }

  if(isTRUE(length(revals$categorical_cols) > 0)){
    revals$preprocess_tables$categorical <- summaryPreprocess(revals$uploaded_data, revals$categorical_cols, categorical = TRUE)
    
    # __SHINYTEST__
    exportTestValues(categorical_table = revals$preprocess_tables$categorical)
  }

})

# For numeric columns:
observeEvent(input$preprocess_dismiss,{
  
  req(revals$preprocess_tables)
  
  if(length(revals$preprocess_tables$numeric) > 0){
    # Create Table Output
    output$numeric_summary <- DT::renderDataTable({revals$preprocess_tables$numeric}) 
    
    # Summary Header
    output$numeric_header <- renderUI(tags$p("Summary Statistics for Numeric Variables"))
  }

  if(length(revals$preprocess_tables$categorical) > 0){
    lapply(1:length(revals$preprocess_tables$categorical), function(i){
      output[[paste0('Table_',i)]] <- DT::renderDataTable({revals$preprocess_tables$categorical[[i]]}, options = list(scrollX = TRUE, dom = "t"))
      Sys.sleep(0.5)
    })
    
    revals$ntables <- length(revals$preprocess_tables$categorical)
    
    output$cat_header <- renderUI(tags$p("Counts for Categorical Variables"))
  }
})

# The renderUI call that takes input from the above observer
output$categorical_summary <- renderUI({
  req(revals$ntables)
  
  if(isTRUE(nrow(isolate(revals$categorical_cols)) == 0)) NULL
  else{
    tagList(lapply(1:revals$ntables, function(i){
      DT::dataTableOutput(paste0('Table_',i))
                })
    )
  }
})

## END TABLE SUMMARY SECTION ##

# display warning if nothing selected
observeEvent(input$tests, {
  revals$warningmessage_preprocess$no_selection <- if(!isTRUE(length(input$tests) > 0)) "<p style = 'color:deepskyblue'>Select at least one test to calculate</p>" else NULL
}, ignoreNULL = FALSE)

# Success dialogs
observeEvent(revals$uploaded_data,{
  req(revals$uploaded_data, input$top_page == 'Preprocess')
  validate(need(input$tests, message = "Please choose at least one test to calculate"))
  
  if(!is.null(attributes(revals$peakData2)$filters)){
    msg <- "<p style = 'color:red'>You returned to this page after performing filtering, Re-apply filters to update your data</p>"
  }
  else msg <- NULL
  
  showModal(
    modalDialog(title = "Preprocess Success",
                fluidRow(
                  column(10, align = "center", offset = 1,
                         HTML('<h4 style= "color:#1A5276">Your data has been preprocessed.  Calculated variables have been added to the molecular identification file and can be used in subsequent filtering and visualization.</h4>'),
                         HTML(msg),
                         hr(),
                         actionButton('preprocess_dismiss', 'Review results', width = '75%'),
                         actionButton('goto_qc', 'Go to the QC tab to see some boxplots', style = 'width:75%;margin:5px'),
                         actionButton('goto_filter', 'Continue to filtering', width = '75%')
                  )
                )
                ,footer = NULL)
  ) 
}) # End successMessage

observeEvent(input$preprocess_dismiss,{
  removeModal()
  }, priority = 10)

observeEvent(input$goto_filter,{
  updateTabsetPanel(session, "top_page", selected = "Filter")
  removeModal()
})

observeEvent(input$goto_qc,{
  updateTabsetPanel(session, 'top_page', selected = 'Quality Control')
  removeModal()
})