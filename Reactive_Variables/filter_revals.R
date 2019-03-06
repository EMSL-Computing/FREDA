# Object: Get data frame from summaryFilt
# Depends on a reactive variable that has delayed invalidation (revals$reac_filter_plot)
summaryFilterDataFrame <- eventReactive(revals$reac_filter_plot, {
  req(input$top_page == "Filter")
  
  # determine which, if any, custom filters to apply
  conds <- c(!is.null(revals$custom1_ids), !is.null(revals$custom2_ids), !is.null(revals$custom3_ids))
  
  if (any(conds) & isolate(input$customfilterz)){
    customids_to_keep <- data.frame(ids = c(revals$custom1_ids, revals$custom2_ids, revals$custom3_ids), stringsAsFactors = FALSE) %>%
      group_by(ids) %>%
      mutate(n = n()) %>%
      filter(n == sum(conds)) %>%
      pluck(1)
  }
  else customids_to_keep <- NULL
  # Get summary table from sourced file 'summaryFilter.R'
  summaryFilt(peakICR(), sampfilter_ids(), massfilter_ids(), molfilter_ids(), formfilter_ids(), customids_to_keep)
  
  
}, ignoreInit = TRUE) # End summaryFilterDataFrame

### ids which are passed to summaryFilt() to calculate the remaining peaks for table and bar plot
# removed sample filter ids
sampfilter_ids <- eventReactive(c(input$keep_samples, input$samplefilter, input$top_page),{
  if(input$samplefilter){
    if(length(input$keep_samples) == 0) NULL
    else{
      uploaded_data() %>% 
        subset(samples = input$keep_samples, check_rows = TRUE) %>%
        {.$e_data} %>%
        pluck(getMassColName(peakIcr2))
    }
  }
  else NULL
})

# removed mass filter filter ids
massfilter_ids <- eventReactive(c(input$massfilter, input$min_mass, input$max_mass, input$top_page),{
  revals$redraw_filter_plot <- FALSE
  if (input$massfilter){
    mass_filter(uploaded_data()) %>% 
      dplyr::filter(!!sym(getMassColName(peakIcr2)) <= input$max_mass, !!sym(getMassColName(peakIcr2)) >= input$min_mass) %>%
      pluck(getMassColName(peakIcr2))
  }
  else NULL
})

# removed molecule filter ids
molfilter_ids <- eventReactive(c(input$minobs, input$molfilter, input$keep_samples, input$samplefilter, input$top_page), {
  if (input$molfilter){
    if(input$samplefilter & length(input$keep_samples) > 0){
      uploaded_data() %>% 
        subset(samples = input$keep_samples, check_rows = TRUE) %>%
        molecule_filter() %>% 
        dplyr::filter(Num_Observations >= as.integer(input$minobs)) %>%
        pluck(getMassColName(peakIcr2))
    }
    else{
      molecule_filter(uploaded_data()) %>%
        dplyr::filter(Num_Observations >= as.integer(input$minobs)) %>%
        pluck(getMassColName(peakIcr2))
    }
  }
  else NULL
})

# removed formula filter ids
formfilter_ids <- eventReactive(c(input$formfilter, input$top_page), {
  if (input$formfilter){
    formula_filter(uploaded_data()) %>%
      dplyr::filter(Formula_Assigned == TRUE) %>%
      pluck(getMassColName(peakIcr2))
  }
  else NULL
})
### end filter ids ###
