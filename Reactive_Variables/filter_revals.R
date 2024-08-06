# Object: Get data frame from summaryFilt
# Depends on a reactive variable that has delayed invalidation (revals$reac_filter_plot)
summaryFilterDataFrame <- eventReactive(revals$reac_filter_plot, {
  req(input$top_page == "Filter")
  req(revals$redraw_largedata, cancelOutput = TRUE)
  print('recalculating summaryfilter dataframe...')

  if (uploaded_data_dim() > max_cells) {
    on.exit(revals$redraw_largedata <- FALSE)
  }

  # determine which, if any, custom filters to apply
  conds <- c(!is.null(revals$custom1_ids), !is.null(revals$custom2_ids), !is.null(revals$custom3_ids))

  if (any(conds) & isolate(input$customfilterz)) {
    customids_to_keep <- data.frame(ids = c(revals$custom1_ids, revals$custom2_ids, revals$custom3_ids), stringsAsFactors = FALSE) %>%
      group_by(ids) %>%
      mutate(n = n()) %>%
      filter(n == sum(conds)) %>%
      pluck(1)
  }
  else customids_to_keep <- NULL
  # Get summary table from sourced file 'summaryFilter.R'
  df <- summaryFilt(revals$uploaded_data, sampfilter_ids(), massfilter_ids(), molfilter_ids(), formfilter_ids(), customids_to_keep)

  return(df)

}, ignoreInit = TRUE) # End summaryFilterDataFrame

### ids which are passed to summaryFilt() to calculate the remaining peaks for table and bar plot
# removed sample filter ids
sampfilter_ids <- eventReactive(c(input$keep_samples, input$samplefilter, input$top_page, revals$react_largedata), {
  req(!is.null(revals$peakData2))
  if (!revals$redraw_largedata) {
    return(NULL)
  }
  else if (input$samplefilter) {
    if (length(input$keep_samples) == 0) { # no samples kept
      return(NULL)
    }
    else if (length(intersect(colnames(revals$peakData2$e_data), input$keep_samples)) == 0) { # selected samples not in data
      return(NULL)
    }
    else {
      revals$uploaded_data %>%
        subset(samples = input$keep_samples, check_rows = TRUE) %>%
        {.$e_data} %>%
        pluck(getMassColName(revals$peakData2))
    }
  }
  else NULL
})

# removed mass filter filter ids
massfilter_ids <- eventReactive(c(input$massfilter, input$min_mass, input$max_mass, input$top_page, revals$react_largedata), {
  req(!is.null(revals$peakData2), input$top_page == "Filter")
  if (!revals$redraw_largedata) {
    return(NULL)
  }
  revals$redraw_filter_plot <- FALSE
  if (input$massfilter) {
    req(length(input$max_mass) > 0, length(input$min_mass) > 0)
    mass_filter(revals$uploaded_data) %>%
      dplyr::filter(`ID__` <= input$max_mass, `ID__` >= input$min_mass) %>%
      purrr::pluck('ID__')
  }
  else NULL
})

# removed molecule filter ids
molfilter_ids <- eventReactive(c(input$minobs, input$molfilter, input$keep_samples, input$samplefilter, input$top_page, revals$react_largedata), {
  req(!is.null(revals$peakData2))
  if (!revals$redraw_largedata) {
    return(NULL)
  }
  else if (input$molfilter) {
    req(length(input$minobs) > 0)
    # if we are subsampling and samples are selected and the selected samples are in the data
    if (input$samplefilter & length(input$keep_samples) > 0 & length(intersect(colnames(revals$peakData2$e_data), input$keep_samples)) != 0) {
      revals$uploaded_data %>%
        subset(samples = input$keep_samples, check_rows = TRUE) %>%
        molecule_filter() %>%
        dplyr::filter(Num_Observations >= as.integer(input$minobs)) %>%
        pluck(getMassColName(revals$peakData2))
    }
    else {
      molecule_filter(revals$uploaded_data) %>%
        dplyr::filter(Num_Observations >= as.integer(input$minobs)) %>%
        pluck(getMassColName(revals$peakData2))
    }
  }
  else NULL
})

# removed formula filter ids
formfilter_ids <- eventReactive(c(input$formfilter, input$top_page, revals$react_largedata), {
  req(!is.null(revals$peakData2))
  if (!revals$redraw_largedata) {
    return(NULL)
  }
  else if (input$formfilter) {
    formula_filter(revals$uploaded_data) %>%
      dplyr::filter(Formula_Assigned == TRUE) %>%
      pluck(getMassColName(revals$peakData2))
  }
  else NULL
})
### end filter ids ###
