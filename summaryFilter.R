#' Create a Summary of the Results from Filtering a peakICR object
#' 
#' This function takes a peakICR object and determines the summary figure if certain filters should be applied.
#' 
#' @param icRobject A peakICR object, nonfiltered.
#' @param massfilt_ids,molfilt_ids,formfilt_ids vectors containing the retained peak ID's for any applied mass, molecule, or formula filters.
#' @param customfilt_ids vectors containing the retained peak ID's resulting from the application of ALL OF up to 3 custom filters
#' 
#' @return a dataframe containing filter display names, and the remaining assigned and unassigned 
#' 

summaryFilt <- function(icRobject, massfilt_ids = NULL, molfilt_ids = NULL, formfilt_ids = NULL, customfilt_ids = NULL) {
  
  # Checks: minMass < molMass
  # if (minMass > maxMass) {
  #   stop('Error: Minimum mass must be greater than maximum mass')
  # }
  # TODO: minMol is <= the total number of samples
  
  mass_cname <- attr(icRobject, 'cnames')$mass_cname
  edata_cname <- attr(icRobject, 'cnames')$edata_cname
  
  # Create 3-by-5 dataframe to store all data
  data_state <- c('Unfiltered', 'After Mass Filter', 'After Molecule Filter', "After Formula Filter", "After Custom Filters")
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
  summaryTable[1, 'min_mass'] <- min(as.numeric(icRobject$e_data[, mass_cname]))
  summaryTable[1, 'max_mass'] <- max(as.numeric(icRobject$e_data[, mass_cname]))
  
  filteredICR <- icRobject$e_data %>% 
    dplyr::select(attr(icRobject, 'cnames')$edata_cname, attr(icRobject, 'cnames')$mass_cname) %>% 
    left_join(fcol)

  # Mass filter #
  if (!is.null(massfilt_ids)) {
    
    filteredICR <- filteredICR %>% filter(!!sym(edata_cname) %in% massfilt_ids)
    
    # Find total number of peaks / rows
    summaryTable[2, 'sum_peaks'] <- nrow(filteredICR)
    summaryTable[2, 'dispText'] <- as.character(summaryTable[2, 'sum_peaks'])
    
    # Find assigned and unassigned
    summaryTable[2, 'assigned'] <- nrow(filteredICR %>% filter(Formula_Assigned == TRUE))
                                        
    summaryTable[2, 'unassigned'] <- summaryTable[2, 'sum_peaks'] - summaryTable[2, 'assigned']
    
    # Find minimum and maximum mass
    summaryTable[2, 'min_mass'] <- min(filteredICR %>% pluck(mass_cname))
    summaryTable[2, 'max_mass'] <- max(filteredICR %>% pluck(mass_cname))
  }
  
  # Molecule filter #
  if (!is.null(molfilt_ids)) {
    
    filteredICR <- filteredICR %>% filter(!!sym(edata_cname) %in% molfilt_ids)
    
    # Find sum
    summaryTable[3, 'sum_peaks'] <- nrow(filteredICR)
    summaryTable[3, 'dispText'] <- as.character(summaryTable[3, 'sum_peaks'])
    
    # Find assigned and unassigned 
    summaryTable[3, 'assigned'] <- nrow(filteredICR %>% filter(Formula_Assigned == TRUE))
    
    summaryTable[3, 'unassigned'] <- summaryTable[3, 'sum_peaks'] - summaryTable[3, 'assigned']
    
    # Find minimum and maximum mass
    summaryTable[3, 'min_mass'] <- min(filteredICR %>% pluck(mass_cname))
    summaryTable[3, 'max_mass'] <- max(filteredICR %>% pluck(mass_cname))
  }
  
  if(!is.null(formfilt_ids)){
    
    filteredICR <- filteredICR %>% filter(!!sym(edata_cname) %in% formfilt_ids)
    
    # Find sum
    summaryTable[4, 'sum_peaks'] <- nrow(filteredICR)
    summaryTable[4, 'dispText'] <- as.character(summaryTable[4, 'sum_peaks'])
    
    # Find assigned and unassigned 
    summaryTable[4, 'assigned'] <- nrow(filteredICR %>% filter(Formula_Assigned == TRUE))
    
    summaryTable[4, 'unassigned'] <- summaryTable[4, 'sum_peaks'] - summaryTable[4, 'assigned']
    
    # Find minimum and maximum mass
    summaryTable[4, 'min_mass'] <- min(filteredICR %>% pluck(mass_cname))
    summaryTable[4, 'max_mass'] <- max(filteredICR %>% pluck(mass_cname))
  }
  
  if(!is.null(customfilt_ids)){
    filteredICR <- filteredICR %>% filter(!!sym(edata_cname) %in% customfilt_ids)
    
    # Find sum
    summaryTable[5, 'sum_peaks'] <- nrow(filteredICR)
    summaryTable[5, 'dispText'] <- as.character(summaryTable[5, 'sum_peaks'])
    
    # Find assigned and unassigned 
    summaryTable[5, 'assigned'] <- nrow(filteredICR %>% filter(Formula_Assigned == TRUE))
    
    summaryTable[5, 'unassigned'] <- summaryTable[5, 'sum_peaks'] - summaryTable[5, 'assigned']
    
    # Find minimum and maximum mass
    summaryTable[5, 'min_mass'] <- min(filteredICR %>% pluck(mass_cname))
    summaryTable[5, 'max_mass'] <- max(filteredICR %>% pluck(mass_cname))
  }
  
  return(summaryTable)
}
