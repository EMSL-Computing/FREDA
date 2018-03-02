#' Create a Summary of the Results from Filtering a peakICR object
#' 
#' This function takes a peakICR object and determines the summary figure if certain filters should be applied.
#' 
#' @param icRobject A peakICR object, nonfiltered.
#' @param isMassChecked Is mass filtering requested?
#' @param minMass The minimum value filtered for mass - defaults to 200
#' @param maxMass The maximum value allowed for mass - defaults to 200
#' @param isMolChecked
#' @param minMol
#' 
#' @return a list with two data frames, including 
#' 

summaryFilt <- function(icRobject, isMassChecked, minMass = 200, maxMass = 900, 
                        isMolChecked, minMol = 2) {
  # Checks: minMass < molMass
  if (minMass > maxMass) {
    stop('Error: Minimum mass must be greater than maximum mass')
  }
  # TODO: minMol is <= the total number of samples
  
  # Create 3-by-5 dataframe to store all data
  data_state <- c('Unfiltered', 'After Mass Filter', 'After Molecule Filter')
  summaryTable <- data.frame(data_state = factor(data_state, levels = data_state), 
                             assigned = numeric(length = 3), 
                             unassigned = numeric(length = 3), 
                             min_mass = numeric(length = 3), 
                             max_mass = numeric(length = 3),
                             sum_peaks = numeric(length = 3), 
                             dispText = rep('NA', 3), 
                             stringsAsFactors = FALSE)
  
  # Assign unfiltered sum and the display text
  summaryTable[1, 'sum_peaks'] <- dim(icRobject$e_data)[1]
  summaryTable[1, 'dispText'] <- as.character(summaryTable[1, 'sum_peaks'])
  
  # Scope: Create fcol to store T/F Assigned/Unassigned list
  fcol <- NULL
  
  # If f_column exists
  if(!is.null(attr(icRobject, 'cnames')$mf_cname)) {
    fcol <- icRobject$e_meta[,attr(icRobject, 'cnames')$mf_cname]
    fcol <- ((!is.na(fcol)) & (fcol != ""))
  
  }else{ # If elemental columns exist #
    
    # Set up list of column names
    elem_cols <- c(attr(icRobject, 'cnames')$c_cname, attr(icRobject, 'cnames')$h_cname,
                   attr(icRobject, 'cnames')$n_cname, attr(icRobject, 'cnames')$o_cname,
                   attr(icRobject, 'cnames')$s_cname, attr(icRobject, 'cnames')$p_cname)
    
    elem_mat <- data.frame(icRobject$e_meta[, elem_cols])
    
    # If c13 is chosen, filter out ones where c13 = 1
    c13 <- rep(TRUE, dim(icRobject$e_meta)[1])
    
    if (!is.null(attr(icRobject, 'cnames')$c13_cname)) {
      c13 <- (icRobject$e_meta[,attr(icRobject, 'cnames')$c13_cname] == 0)
    }
    
    # Count all remaining nonzero rows
    fcol <- rowSums(elem_mat) > 0
    
    # Assigned: Has a nonzero rowSum AND c13 is 0
    fcol <- (fcol & c13)
  }
  
  # Find assigned and unassigned for unfiltered
  summaryTable[1,'assigned'] <- sum(fcol)
  summaryTable[1, 'unassigned'] <- summaryTable[1, 'sum_peaks'] - summaryTable[1, 'assigned']
  
  # Find minimum and maximum mass
  summaryTable[1, 'min_mass'] <- min(as.numeric(icRobject$e_data[, attr(icRobject, 'cnames')$mass_cname]))
  summaryTable[1, 'max_mass'] <- max(as.numeric(icRobject$e_data[, attr(icRobject, 'cnames')$mass_cname]))
  
  filteredICR <- icRobject$e_data[,attr(icRobject, 'cnames')$edata_cname]
  
  # Mass filter #
  if (isMassChecked) {
    
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
    summaryTable[2, 'assigned'] <- sum(fcol[rowNums])
    summaryTable[2, 'unassigned'] <- summaryTable[2, 'sum_peaks'] - summaryTable[2, 'assigned']

    # Find minimum and maximum mass
    summaryTable[2, 'min_mass'] <- min(massfilt[rowNums, 2])
    summaryTable[2, 'max_mass'] <- max(massfilt[rowNums, 2])
  }

  # Molecule filter #
  if (isMolChecked) {
    
    molfilter <- molecule_filter(icRobject)

    # Find the name of the ID column
    id_col <- which(colnames(molfilter) != 'Num_Observations')

    # Sort based on the ID column
    filteredICR <- intersect(filteredICR, 
                             molfilter[which(molfilter[,2] >= as.numeric(minMol)), 1])

    # Find sum
    summaryTable[3, 'sum_peaks'] <- length(filteredICR)
    summaryTable[3, 'dispText'] <- as.character(summaryTable[3, 'sum_peaks'])
    
    # Find row numbers for filtered peaks/rows in e_data
    rowNums <- which(icRobject$e_data[, attr(icRobject, 'cnames')$edata_cname] %in% filteredICR)
    
    # Find assigned and unassigned 
    summaryTable[3, 'assigned'] <- sum(fcol[rowNums])
    summaryTable[3, 'unassigned'] <- summaryTable[3, 'sum_peaks'] - summaryTable[3, 'assigned']

    # Find minimum and maximum mass
    summaryTable[3, 'min_mass'] <- min(as.numeric(icRobject$e_data[rowNums,
                                                                   attr(icRobject, 'cnames')$mass_cname]))
    summaryTable[3, 'max_mass'] <- 
      max(as.numeric(icRobject$e_data[rowNums,
                                      attr(icRobject, 'cnames')$mass_cname]))
  }

  
  return(summaryTable)
}
