# Object: plot axes and titles options
# use the 'formals' argument to figure out default chart labels
plot_defaults <- reactive({
  validate((
    need(!is.null(input$chooseplots), message = "")
  ))
  if (input$chooseplots == 'Van Krevelen Plot') {
    defs <- formals(vanKrevelenPlot)
    #defs$legendTitle = names(emeta_display_choices())[emeta_display_choices() == input$vk_colors]
  } else if (input$chooseplots == "Custom Scatter Plot"){
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
  } else if (input$chooseplots == 'PCOA Plot') {
    defs <- formals(plotPrincipalCoordinates)
    defs$xlabel = 'PC1'
    defs$ylabel = 'PC2'
  }
  return(defs)
})

# Object:  reactive variable that keeps track of whether the selected column is numeric or categorical.
numeric_selected <- eventReactive(c(input$vk_colors, plot_data()),{
  edata_col <- plot_data()$e_data %>% pluck(input$vk_colors)
  emeta_col <- plot_data()$e_meta %>% pluck(input$vk_colors)
  if (input$vk_colors %in% (plot_data()$e_data %>% colnames())){
    (is.numeric(edata_col) & !(is_integer(edata_col) & length(unique(edata_col)) < 12))
  }else if (input$vk_colors %in% (plot_data()$e_meta %>% colnames())){
    (is.numeric(emeta_col) & !(is_integer(emeta_col) & length(unique(emeta_col)) < 12))
  }else if (input$vk_colors %in% c("bs1", "bs2")){
    FALSE
  }else TRUE
}, ignoreNULL = FALSE)

# Objects: vectors of sample names.  Depend on group/sample selection dropdown when doing a comparison
g1_samples <- eventReactive(c(input$whichGroups1, input$whichSample1, input$choose_single, input$top_page),{
  if(is.null(isolate(input$whichGroups1)) & is.null(isolate(input$whichSample1))) NULL 
  else if(isolate(input$choose_single == 3)) setdiff(unique(unlist(revals$groups_list[isolate(input$whichGroups1)])), revals$removed_samples)
  else if(isolate(input$choose_single == 4)) isolate(input$whichSample1)
})

g2_samples <- eventReactive(c(input$whichGroups2, input$whichSample2, input$choose_single, input$top_page),{
  if(is.null(isolate(input$whichGroups2)) & is.null(isolate(input$whichSample2))) NULL 
  else if(isolate(input$choose_single == 3)) setdiff(unique(unlist(revals$groups_list[isolate(input$whichGroups2)])), revals$removed_samples)
  else if(isolate(input$choose_single == 4)) isolate(input$whichSample2)
})
#

# Object:  Plotting dataframe to be passed to output$FxnPlot
plot_data <- eventReactive(input$plot_submit,{
  
  req(calc_vars)
  validate(need(!is.null(input$chooseplots) & input$choose_single !=0, message = "Please select plot type"))
  
  if(input$chooseplots=='PCOA Plot'){
    req(exists("peakIcr2", where = 1))
    validate(need(length(sample_names()>0), "No data found, or only 1 sample"))
    
    samples <- setdiff(sample_names(), revals$removed_samples)
    
    # for each sample create a string indicating each group it belongs to
    if(!is.null(input$viztab_select_groups)){
      groups <- sapply(samples, function(sampname){
        tempgroup = NULL
        for(grp in names(revals$groups_list[input$viztab_select_groups])){
          if(isTRUE(sampname %in% revals$groups_list[[grp]])) tempgroup[length(tempgroup)+1] <- grp
        }
        
        if(is.null(tempgroup)){
          return("None")
        }
        else return(paste(tempgroup, collapse="&"))
      })
      
      group_DF <- data.frame(samples, groups)
      colnames(group_DF) <- c(getFDataColName(peakIcr2), "Group")
    }
    else group_DF <- NULL
    
    temp_data <- fticRanalysis:::setGroupDF(peakIcr2, group_DF)
    return(temp_data)
  }
  if (is.null(input$choose_single)){ # corresponds to data with a single sample
    return(peakIcr2) # no need to subset
  }
  if (input$choose_single == 1) { # single sample -selected- but multiple samples present
    validate(need(!is.null(input$whichSamples), message = "Please select a sample to plot"))
    return(subset(peakIcr2, input$whichSamples))
    #key_name <- paste(attributes(peakIcr2)$cnames$fdata_cname, "=", input$whichSamples, sep = "")
  }
  if (input$chooseplots == "Custom Scatter Plot") req(input$scatter_x != input$scatter_y)
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
    if (input$chooseplots == "Density Plot"){
      return(temp_data)
    }
    
    temp_data <- summarizeGroups(temp_data, summary_functions = getGroupSummaryFunctionNames())
    temp_data$e_meta <- cbind(temp_data$e_meta, temp_data$e_data %>% dplyr::select(-one_of(getEDataColName(temp_data))))
    
    return(temp_data)
    
  } else if (isolate(input$choose_single) %in% c(3,4)) {# two groups 
    # Make sure at least one test has been calculated
    validate(need(!is.null(g1_samples()), message = "Please select samples for first grouping"))
    # validate(need(length(g1_samples) > 1, message = "Please select at least 1 sample"))
    validate(need(!is.null(g2_samples()), message = "Please select samples for second grouping"))
    # validate(need(length(g2_samples) > 1, message = "Please select at least 1 sample"))
    
    group1 <- ifelse(is.null(input$group1_name) | isTRUE(input$group1_name == ""), "Group 1", input$group1_name)
    group2 <- ifelse(is.null(input$group2_name) | isTRUE(input$group2_name == ""), "Group 2", input$group2_name)
    
    # assign a group DF to the data with a level for each of the two groups
    temp_group_df <- data.frame(c(g1_samples(), g2_samples()), c(rep(group1, times=length(g1_samples())), rep(group2, length(g2_samples()))))
    colnames(temp_group_df) <- c(getFDataColName(peakIcr2), "Group")

    temp_data <- peakIcr2 %>%
      subset(samples=c(g1_samples(), g2_samples()))
    
    temp_data <- fticRanalysis:::setGroupDF(temp_data, temp_group_df)
    
    # no need to summarize for density plot function
    if (input$chooseplots == "Density Plot"){
      return(temp_data)
    }
    
    # error checking after passing density plots
    validate(need(input$summary_fxn %in% fticRanalysis:::getGroupComparisonSummaryFunctionNames(), "Please select a summary function"))
    
    # get the value of the single pairwise comparison         
    grpComparisonsObj <- divideByGroupComparisons(temp_data, comparisons = "all")[[1]]$value
    
    # paramaters specific to uniqueness_gtest()
    if (input$summary_fxn == "uniqueness_gtest"){
      validate(need(isTRUE(input$pval < 1 & input$pval > 0) & is.numeric(input$pval), message = "Specify a p-value between 0 and 1"))
      gtest_parms <- list(pres_fn = input$pres_fn, pvalue_thresh = input$pval)
    }
    else {
      gtest_parms <- list(absn_thresh = input$absn_thresh)
    }
    
    # conditional error checking depending on nsamps and proportion
    if (input$pres_fn == "nsamps"){
      validate(need(input$pres_thresh <= min(length(g1_samples()), length(g2_samples())), "Maximum threshold is above the minimum number of samples in a group"),
               need(is.numeric(input$pres_thresh), "Please enter a numeric value for threshold to determine presence"),
               need(input$absn_thresh < input$pres_thresh & input$absn_thresh >= 0, "absence threshold must be non-negative and lower than presence threshold"))
    }
    else if (input$pres_fn == "prop"){
      validate(need(input$pres_thresh <= 1 & input$pres_thresh > 0, "Proportion threshold is not in the interval (0,1]"),
               need(is.numeric(input$pres_thresh), "Please enter a numeric proportion threshold to determine presence"),
               need(input$absn_thresh < input$pres_thresh & input$absn_thresh >= 0, "absence threshold must be non-negative and lower than presence threshold"))
    }
    
    # populate a list of args to pass to summarizeGroupComparisons()
    parms <- list()
    parms[[input$summary_fxn]] <- c(list(pres_thresh = input$pres_thresh), gtest_parms)
    
    # create the group comparisons object, passing the user specified function and its (user specified) list of args.
    summaryObj <- summarizeGroupComparisons(grpComparisonsObj, summary_functions = input$summary_fxn, 
                                            summary_function_params = parms)
    
    return(summaryObj)
    
  }
})

