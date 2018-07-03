renderDownloadPlots <- function(parmTable, peakIcr2){
  
  if (parmTable$SampleType == "Single Sample") { #single sample
    # Make sure at least one test has been calculated
    plot_data <- subset(peakIcr2, parmTable$G1)
    #key_name <- paste(attributes(peakIcr2)$cnames$fdata_cname, "=", isolate(input$whichSample), sep = "")
  }
  #---------- Group Plots ------------#
  else if (parmTable$SampleType == "Multiple Samples") {# single group
    # Make sure at least one test has been calculated
    
    # check for one or two groups
    if (is.na(parmTable$G2)) { #one group
      samples <- strsplit(parmTable$G1, split = ",")[[1]]
      temp_group_df <- data.frame(samples, "Group")
      colnames(temp_group_df) <- c(getFDataColName(peakIcr2), "Group")
      
      temp_data <- peakIcr2 %>% 
        subset(samples)
      
      attr(temp_data, "group_DF") <- temp_group_df
      plot_data <- summarizeGroups(temp_data, summary_functions = getGroupSummaryFunctionNames())
    } else {#two groups
      group1_samples <- strsplit(parmTable$G1, split = ",")[[1]]
      group2_samples <- strsplit(parmTable$G2, split = ",")[[1]]
      temp_group_df <- data.frame(c(group1_samples, group2_samples), c(rep("Group1", times=length(group1_samples)), rep("Group2", length(group2_samples))))
      colnames(temp_group_df) <- c(getFDataColName(peakIcr2), "Group")
      
      temp_data <- peakIcr2 %>%
        subset(samples = c(group1_samples, group2_samples))
      
      temp_data <- fticRanalysis:::setGroupDF(temp_data, temp_group_df)
      grpComparisonsObj <- divideByGroupComparisons(temp_data, comparisons = "all")[[1]]$value
      plot_data <- summarizeComparisons(grpComparisonsObj, summary_functions = parmTable$UniqueCommon)
    }
    
  }
    #----------- Single sample plots ------------#
    #-------Kendrick Plot-----------# 
    if (parmTable$PlotType == 'Kendrick Plot') {
      if (parmTable$SampleType == "Multiple Samples") {
        return({
          kendrickPlot(plot_data, colorCName = parmTable$ColorBy)
        })
      } else if (parmTable$SampleType == "Single Sample") {
        return({
          if (parmTable$ColorBy %in% c('bs1', 'bs2')) {
            return(kendrickPlot(plot_data, vkBoundarySet = parmTable$ColorBy))
          } else {
            # if color selection doesn't belong to a boundary, color by test
            return(kendrickPlot(plot_data, colorCName = parmTable$ColorBy))
          }
        })
      }
    }
    #-------VanKrevelen Plot--------#
    if (parmTable$PlotType == 'Van Krevelen Plot') {
      if (parmTable$SampleType == "Multiple Samples") {
        if (is.na(parmTable$BoundarySet)) {
          return({
            vanKrevelenPlot(plot_data, colorCName = parmTable$ColorBy, showVKBounds = FALSE)
          })
        }
        else return({
          vanKrevelenPlot(plot_data, colorCName = parmTable$ColorBy, showVKBounds = TRUE)
        })
        
      } else if (parmTable$SampleType == "Single Sample") {
        return({
          #-----boundary line logic------#
          if (is.na(parmTable$BoundarySet)) { #no bounds
            # if no boundary lines, leave the option to color by boundary
            if (parmTable$ColorBy %in% c('bs1', 'bs2')) {
              return(vanKrevelenPlot(plot_data, showVKBounds = FALSE, vkBoundarySet = parmTable$ColorBy))
            } else {
              # if no boundary lines and color selection doesn't belong to a boundary, color by test
              return(vanKrevelenPlot(plot_data, showVKBounds = FALSE, colorCName = parmTable$ColorBy))
            }
          } else {
            # if boundary lines, allow a color by boundary class 
            if (parmTable$ColorBy %in% c('bs1', 'bs2')) {
              return(vanKrevelenPlot(plot_data, vkBoundarySet = parmTable$BoundarySet, showVKBounds = TRUE))
            } else {
              # if boundary lines and color isn't a boundary class
              return(vanKrevelenPlot(plot_data, vkBoundarySet = parmTable$BoundarySet, showVKBounds = TRUE, colorCName = parmTable$ColorBy))
              
            }
          }
        })
      }
    }
    
    #--------- Density Plot --------#
    if (parmTable$PlotType == 'Density Plot') {
      return({
        densityPlot(plot_data, variable = parmTable$ColorBy)
      })
    }
} 