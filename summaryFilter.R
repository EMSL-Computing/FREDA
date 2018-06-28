#' Create a Summary of the Results from Filtering a peakICR object
#' 
#' This function takes a peakICR object and determines the summary figure if certain filters should be applied.
#' 
#' @param icRobject A peakICR object, nonfiltered.
#' @param which_filts A logical vector specifying which of (in order) Mass, Molecule, Formula, Custom 1/2/3 filters to apply
#' @param minMass The minimum value filtered for mass - defaults to 200
#' @param maxMass The maximum value allowed for mass - defaults to 200
#' @param isMolChecked
#' @param minMol
#' 
#' @return a list with two data frames, including 
#' 

summaryFilt <- function(icRobject, which_filts, minMass = 200, maxMass = 900, 
                        minMol = 2) {
  
  # Checks: minMass < molMass
  if (minMass > maxMass) {
    stop('Error: Minimum mass must be greater than maximum mass')
  }
  # TODO: minMol is <= the total number of samples
  
  # Create 3-by-5 dataframe to store all data
  data_state <- c('Unfiltered', 'After Mass Filter', 'After Molecule Filter', "After Formula Filter")
  nfilters <- length(data_state)
  summaryTable <- data.frame(data_state = factor(data_state, levels = data_state), 
                             assigned = numeric(nfilters), 
                             unassigned = numeric(nfilters), 
                             min_mass = numeric(nfilters), 
                             max_mass = numeric(nfilters),
                             sum_peaks = numeric(nfilters), 
                             dispText = 'NA', 
                             stringsAsFactors = FALSE)
  
  # Assign unfiltered sum and the display text
  summaryTable[1, 'sum_peaks'] <- dim(icRobject$e_data)[1]
  summaryTable[1, 'dispText'] <- as.character(summaryTable[1, 'sum_peaks'])
  
  # Scope: Create fcol to store T/F Assigned/Unassigned list
  fcol <- NULL
  
  # If f_column exists
  if(!is.null(attr(icRobject, 'cnames')$mf_cname)) {
    fcol <- formula_filter(icRobject)
    
  }else{fcol <- assign_mf(icRobject) %>% formula_filter}
  
  # Find assigned and unassigned for unfiltered
  summaryTable[1,'assigned'] <- sum(fcol$Formula_Assigned)
  summaryTable[1, 'unassigned'] <- summaryTable[1, 'sum_peaks'] - summaryTable[1, 'assigned']
  
  # Find minimum and maximum mass
  summaryTable[1, 'min_mass'] <- min(as.numeric(icRobject$e_data[, attr(icRobject, 'cnames')$mass_cname]))
  summaryTable[1, 'max_mass'] <- max(as.numeric(icRobject$e_data[, attr(icRobject, 'cnames')$mass_cname]))
  
  filteredICR <- icRobject$e_data[,attr(icRobject, 'cnames')$edata_cname]
  
  # Mass filter #
  if (which_filts[1]) {
    
    # Get masses and unique identifiers
    massfilt <- mass_filter(icRobject)
    
    # Get rows with masses within given min/max
    rowNums <- which((massfilt[,2] >= minMass) & (massfilt[,2] <= maxMass))
    
    # Get unique identifiers of valid rows
    filteredICR <- massfilt[rowNums, 1]
    
    # Find total number of peaks / rows
    summaryTable[2, 'sum_peaks'] <- length(filteredICR)
    summaryTable[2, 'dispText'] <- as.character(summaryTable[2, 'sum_peaks'])
    
    # Find assigned and unassigned
    summaryTable[2, 'assigned'] <- sum(fcol %>% 
                                         dplyr::filter(fcol[,getEDataColName(icRobject)] %in% filteredICR) %>% 
                                         pluck("Formula_Assigned"))
    summaryTable[2, 'unassigned'] <- summaryTable[2, 'sum_peaks'] - summaryTable[2, 'assigned']
    
    # Find minimum and maximum mass
    summaryTable[2, 'min_mass'] <- min(massfilt[rowNums, 2])
    summaryTable[2, 'max_mass'] <- max(massfilt[rowNums, 2])
  }
  
  # Molecule filter #
  if (which_filts[2]) {
    
    molfilter <- molecule_filter(icRobject)
    vmolfilter <<- molfilter
    # Find the name of the ID column
    id_col <- which(colnames(molfilter) != 'Num_Observations')
    
    # Sort based on the ID column
    filteredICR <- intersect(filteredICR, 
                             molfilter[which(molfilter[,2] >= as.numeric(minMol)), 1])
    
    # Find sum
    summaryTable[3, 'sum_peaks'] <- length(filteredICR)
    summaryTable[3, 'dispText'] <- as.character(summaryTable[3, 'sum_peaks'])
    
    # Find row numbers for filtered peaks/rows in e_data
    rowNums <- which(icRobject$e_meta[, attr(icRobject, 'cnames')$edata_cname] %in% filteredICR)
    
    # Find assigned and unassigned 
    summaryTable[3, 'assigned'] <- sum(fcol %>% 
                                         dplyr::filter(fcol[,getEDataColName(icRobject)] %in% filteredICR) %>% 
                                         pluck("Formula_Assigned"))
    
    summaryTable[3, 'unassigned'] <- summaryTable[3, 'sum_peaks'] - summaryTable[3, 'assigned']
    
    # Find minimum and maximum mass
    summaryTable[3, 'min_mass'] <- min(as.numeric(icRobject$e_data[rowNums,
                                                                   attr(icRobject, 'cnames')$mass_cname]))
    summaryTable[3, 'max_mass'] <- 
      max(as.numeric(icRobject$e_data[rowNums,
                                      attr(icRobject, 'cnames')$mass_cname]))
  }
  
  if(which_filts[3]){
    
    filteredICR <- intersect(filteredICR, 
                             fcol %>% 
                               dplyr::filter(Formula_Assigned == TRUE) %>% 
                               pluck(getEDataColName(icRobject))
    )
    
    # Find sum
    summaryTable[4, 'sum_peaks'] <- length(filteredICR)
    summaryTable[4, 'dispText'] <- as.character(summaryTable[4, 'sum_peaks'])
    
    # Find row numbers for filtered peaks/rows in e_data
    rowNums <- which(icRobject$e_meta[, attr(icRobject, 'cnames')$edata_cname] %in% filteredICR)
    
    # Find assigned and unassigned 
    summaryTable[4, 'assigned'] <- sum(fcol %>% 
                                         filter(fcol[,getEDataColName(icRobject)] %in% filteredICR) %>% 
                                         pluck("Formula_Assigned"))
    
    summaryTable[4, 'unassigned'] <- summaryTable[4, 'sum_peaks'] - summaryTable[4, 'assigned']
    
    # Find minimum and maximum mass
    summaryTable[4, 'min_mass'] <- min(as.numeric(icRobject$e_data[rowNums,
                                                                   attr(icRobject, 'cnames')$mass_cname]))
    summaryTable[4, 'max_mass'] <- 
      max(as.numeric(icRobject$e_data[rowNums,
                                      attr(icRobject, 'cnames')$mass_cname]))
  }
  
  return(summaryTable)
}
