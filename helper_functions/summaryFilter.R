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

summaryFilt <- function(icRobject, sampfilt_ids = NULL, massfilt_ids = NULL, molfilt_ids = NULL, formfilt_ids = NULL, customfilt_ids = NULL) {
  
  # Checks: minMass < molMass
  # if (minMass > maxMass) {
  #   stop('Error: Minimum mass must be greater than maximum mass')
  # }
  # TODO: minMol is <= the total number of samples
  
  mass_cname <- attr(icRobject, 'cnames')$mass_cname
  edata_cname <- attr(icRobject, 'cnames')$edata_cname
  
  # Create 3-by-5 dataframe to store all data
  data_state <- list('Unfiltered' = NULL, 'After Sample Filter' = sampfilt_ids, 'After Mass Filter' = massfilt_ids, 'After Molecule Filter' = molfilt_ids, 
                     "After Formula Filter" = formfilt_ids, "After Custom Filters" = customfilt_ids)
  nfilters <- length(data_state)
  summaryTable <- data.frame(data_state = factor(names(data_state), levels = names(data_state)), 
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
  
  for(el in names(data_state[-1])){
    if(!is.null(data_state[[el]])){
      filteredICR <- filteredICR %>% filter(!!sym(edata_cname) %in% data_state[[el]])
      
      row = which(summaryTable$data_state == el)
      
      # Find total number of peaks / rows
      summaryTable[row, 'sum_peaks'] <- nrow(filteredICR)
      summaryTable[row, 'dispText'] <- as.character(summaryTable[row, 'sum_peaks'])
      
      # Find assigned and unassigned
      summaryTable[row, 'assigned'] <- nrow(filteredICR %>% filter(Formula_Assigned == TRUE))
      
      summaryTable[row, 'unassigned'] <- summaryTable[row, 'sum_peaks'] - summaryTable[row, 'assigned']
      
      # Find minimum and maximum mass
      summaryTable[row, 'min_mass'] <- min(filteredICR %>% pluck(mass_cname))
      summaryTable[row, 'max_mass'] <- max(filteredICR %>% pluck(mass_cname))
    }
  }
  
  return(summaryTable)
}
