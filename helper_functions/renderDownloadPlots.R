renderDownloadPlots <- function(parmTable, peakData2){
  # first parse out the color palette if there is one
  if (!is.na(parmTable$HiddenPalette)) {
    splitpal <- strsplit(parmTable$HiddenPalette,split = ":")
    pal <- strsplit(splitpal[[1]][1], split = ",")[[1]]
    domain <- as.numeric(strsplit(splitpal[[1]][2], split = ",")[[1]])
    colorPal <- scales::col_numeric(pal, domain)
  }
  else colorPal <- NA
  if (parmTable$SampleType == "Single Sample") { #single sample
    # Make sure at least one test has been calculated
    plot_data <- subset(peakData2, parmTable$G1)
    #key_name <- paste(attributes(peakData2)$cnames$fdata_cname, "=", isolate(input$whichSample), sep = "")
  }
  #---------- Group Plots ------------#
  else if (parmTable$SampleType == "Multiple Samples") {# single group
    # Make sure at least one test has been calculated
    
    # check for one or two groups
    if (is.na(parmTable$G2)) { #one group
      samples <- strsplit(parmTable$G1, split = ",")[[1]]
      temp_group_df <- data.frame(samples, "Group")
      colnames(temp_group_df) <- c(getFDataColName(peakData2), "Group")
      
      temp_data <- peakData2 %>% 
        subset(samples)
      
      temp_data <- ftmsRanalysis:::setGroupDF(temp_data, temp_group_df)
      
      # Density plots dont need summarizegroups object 
      if (parmTable$PlotType == "Density Plot"){
        plot_data <- temp_data
      }
      else {
        temp_data <- summarizeGroups(temp_data, summary_functions = getGroupSummaryFunctionNames())
        temp_data$e_meta <- cbind(temp_data$e_meta, temp_data$e_data %>% dplyr::select(-one_of(getEDataColName(temp_data))))
        
        plot_data <- temp_data
      }
      
    } else {#two groups
      group1_samples <- strsplit(parmTable$G1, split = ",")[[1]]
      group2_samples <- strsplit(parmTable$G2, split = ",")[[1]]
      temp_group_df <- data.frame(c(group1_samples, group2_samples), c(rep("Group1", times=length(group1_samples)), rep("Group2", length(group2_samples))))
      colnames(temp_group_df) <- c(getFDataColName(peakData2), "Group")
      
      temp_data <- peakData2 %>%
        subset(samples = c(group1_samples, group2_samples))
      
      temp_data <- ftmsRanalysis:::setGroupDF(temp_data, temp_group_df)
      
      # Density plots dont need summarizegroups object 
      if (parmTable$PlotType == "Density Plot"){
        plot_data <- temp_data
      } else {
        grpComparisonsObj <- divideByGroupComparisons(temp_data, comparisons = "all")[[1]]$value
        plot_data <- summarizeGroupComparisons(grpComparisonsObj, summary_functions = parmTable$UniqueCommon, 
                                                summary_function_params = list(uniqueness_gtest = list(pres_fn = "prop", pres_thresh = 0.2)))
      }
    }
    
  }
    #----------- Single sample plots ------------#
    #-------Kendrick Plot-----------# 
  #-------Kendrick Plot-----------# 
  if (parmTable$PlotType == 'Kendrick Plot') {
    return( kendrickPlot(plot_data, colorCName = parmTable$ColorBy, colorPal = colorPal,
                      xlabel = parmTable$XaxisTitle, ylabel = parmTable$YaxisTitle,
                      title = parmTable$ChartTitle,legendTitle = parmTable$LegendTitle))
    
    if (parmTable$ColorBy %in% c('bs1', 'bs2')) {
      return( kendrickPlot(plot_data, vkBoundarySet = parmTable$BoundarySet,
                        xlabel =parmTable$XaxisTitle, ylabel = parmTable$YaxisTitle,
                        title = parmTable$ChartTitle,legendTitle = parmTable$LegendTitle))
    } else {
      # if color selection doesn't belong to a boundary, color by test
      return( kendrickPlot(plot_data, colorCName = parmTable$ColorBy, colorPal = colorPal,
                        xlabel = parmTable$XaxisTitle, ylabel = parmTable$YaxisTitle,
                        title = parmTable$ChartTitle,legendTitle = parmTable$LegendTitle))
    }
  }
    #-------VanKrevelen Plot--------#
  if (parmTable$PlotType == 'Van Krevelen Plot') {
    if (is.na(parmTable$BoundarySet)) { #no bounds
      # if no boundary lines, leave the option to color by boundary
      if (parmTable$ColorBy %in% c('bs1', 'bs2')) {
        return( vanKrevelenPlot(plot_data, showVKBounds = FALSE, vkBoundarySet = parmTable$ColorBy,
                             xlabel = parmTable$XaxisTitle, ylabel = parmTable$YaxisTitle,
                             title = parmTable$ChartTitle,legendTitle = parmTable$LegendTitle))
      } else {
        # if no boundary lines and color selection doesn't belong to a boundary, color by test
        return( vanKrevelenPlot(plot_data, showVKBounds = FALSE, colorCName = input$vk_colors, colorPal = colorPal,
                             xlabel = parmTable$XaxisTitle, ylabel = parmTable$YaxisTitle,
                             title = parmTable$ChartTitle,legendTitle = parmTable$LegendTitle))
      }
    } else {
      # if boundary lines, allow a color by boundary class 
      if (parmTable$ColorBy %in% c('bs1', 'bs2')) {
        return( vanKrevelenPlot(plot_data, vkBoundarySet = parmTable$BoundarySet, showVKBounds = TRUE,
                             xlabel = parmTable$XaxisTitle, ylabel = parmTable$YaxisTitle,
                             title = parmTable$ChartTitle,legendTitle = parmTable$LegendTitle))
      } else {
        # if boundary lines and color isn't a boundary class
        return( vanKrevelenPlot(plot_data, vkBoundarySet = parmTable$BoundarySet, showVKBounds = TRUE, 
                             colorCName = parmTable$ColorBy, colorPal = colorPal,
                             xlabel = parmTable$XaxisTitle, ylabel = parmTable$YaxisTitle,
                             title = parmTable$ChartTitle,legendTitle = parmTable$LegendTitle))
      }
    }
  }
  
    #--------- Density Plot --------#
    if (parmTable$PlotType == 'Density Plot') {
      
      # set parameters depending on single/multiple/groupcomparison
      if(parmTable$SampleType == "Multiple Samples"){
        if(is.na(parmTable$G2)){
          samples <- strsplit(parmTable$G1, split = ",")[[1]]
          groups = FALSE
            
        }
        else{
          samples = FALSE
          groups = c("Group1", "Group2")
        }
      }
      else if(parmTable$SampleType == "Single Sample"){
        samples = parmTable$G1
        groups = FALSE
      }
      
      return({
        densityPlot(plot_data, variable = parmTable$ColorBy, samples = samples, groups = groups,
                         plot_hist = ifelse(parmTable$SampleType == "Single Sample", TRUE, FALSE),
                    title = parmTable$ChartTitle, ylabel = parmTable$YaxisTitle, xlabel = parmTable$XaxisTitle)
        
      })
    }
  
    #---------- Custom Scatter Plot --------#
    if(parmTable$PlotType == 'Custom Scatter Plot'){
      return({
        scatterPlot(plot_data, parmTable$x_var, parmTable$y_var, colorCName = parmTable$ColorBy)
        scatterPlot(plot_data, parmTable$x_var, parmTable$y_var, colorCName = parmTable$ColorBy, colorPal = colorPal,
                    xlabel = parmTable$XaxisTitle, 
                    ylabel = parmTable$YaxisTitle,
                    title = parmTable$ChartTitle, legendTitle = parmTable$LegendTitle)    
        })
      
    }   
} 