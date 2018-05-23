renderDownloadPlots <- function(parmTable, peakIcr2){
    if (parmTable$SampleType == "Single Sample") { #single sample
      # Make sure at least one test has been calculated
      division_data <- subset(peakIcr2, parmTable$G1)
    }
    #---------- Group Plots ------------#
    else if (parmTable$SampleType == "Multiple Samples") {# single group
      #parse the pasted string back into a vector
      parmTable$G1 <- strsplit(parmTable$G1, split = ",")[[1]]
      #TODO: Make sure at least one test has been calculated
      division_data <- subset(peakIcr2, parmTable$G1)
      summarized_data <- summarizeGroups(division_data, summary_functions = parmTable$ColorBy)
      #-------Kendrick Plot-----------# 
      if (parmTable$PlotType == 'Kendrick Plot') {
        return({
          groupKendrickPlot(summarized_data, colorCName = parmTable$ColorBy)
        })
      }
    }
    #----------- Single sample plots ------------#
    #-------Kendrick Plot-----------# 
    if (parmTable$PlotType == 'Kendrick Plot') {
      if (parmTable$SampleType == "Multiple Samples") {
        return({
          groupKendrickPlot(summarized_data, colorCName = parmTable$ColorBy)
        })
      } else if (parmTable$SampleType == "Single Sample") {
        return({
          if (parmTable$ColorBy %in% c('bs1', 'bs2')) {
            return(kendrickPlot(division_data, vkBoundarySet = parmTable$ColorBy))
          } else {
            # if color selection doesn't belong to a boundary, color by test
            return(kendrickPlot(division_data, colorCName = parmTable$ColorBy))
          }
        })
      }
    }
    #-------VanKrevelen Plot--------#
    if (parmTable$PlotType == 'Van Krevelen Plot') {
      if (parmTable$SampleType == "Multiple Samples") {
        if (is.na(parmTable$BoundarySet)) {
          return({
            groupVanKrevelenPlot(summarized_data, colorCName = parmTable$ColorBy)
          })
        } else {
          return({
            groupVanKrevelenPlot(summarized_data, colorCName = parmTable$ColorBy, vkBoundarySet = parmTable$BoundarySet)
          })
        }
        
      } else if (parmTable$SampleType == "Single Sample") {
        return({
          #-----boundary line logic------#
          if (is.na(parmTable$BoundarySet)) { #no bounds
            # if no boundary lines, leave the option to color by boundary
            if (parmTable$ColorBy %in% c('bs1', 'bs2')) {
              return(vanKrevelenPlot(division_data, showVKBounds = FALSE, vkBoundarySet = parmTable$ColorBy))
            } else {
              # if no boundary lines and color selection doesn't belong to a boundary, color by test
              return(vanKrevelenPlot(division_data, showVKBounds = FALSE, colorCName = parmTable$ColorBy))
            }
          } else {
            # if boundary lines, allow a color by boundary class 
            if (parmTable$ColorBy %in% c('bs1', 'bs2')) {
              return(vanKrevelenPlot(division_data, vkBoundarySet = parmTable$BoundarySet, showVKBounds = TRUE))
            } else {
              # if boundary lines and color isn't a boundary class
              return(vanKrevelenPlot(division_data, vkBoundarySet = parmTable$BoundarySet, showVKBounds = TRUE, colorCName = parmTable$ColorBy))
              
            }
          }
        })
      }
    }
    
    #--------- Density Plot --------#
    if (parmTable$PlotType == 'Density Plot') {
      return({
        densityPlot(division_data, variable = parmTable$ColorBy)
      })
    }
} 