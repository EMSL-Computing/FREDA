#' Create a Summary For the Information created On Preprocess Tab
#' 
#' This function takes as input a preprocessed peakICR object (created when the 
#' Preprocess Data button is clicked) and what tests have been applied, and then finds the min, 
#' mean, median, and max for the summary table.
#' 
#'  @param icRobject an object of class 'peakICR', with the default calculations already applied
#'  @param testsSelected A two-column data frame with real names of the test columns in one column 
#'  and the display names in the second
#'  @return A table of information

summaryPreprocess <- function(icRobject, testsSelected) {

  # TODO: Add checks (cnames not null, etc)

  # Set up rownames for use in table
  rowNames <- testsSelected[,2]
  rowNum <- length(rowNames)
  
  # Set up data frame (finally!)
  summaryTable <- data.frame('Min' = numeric(length = rowNum), 
                             'Mean' = numeric(length = rowNum), 
                             'Median' = numeric(length = rowNum), 
                             'Max' = numeric(length = rowNum))
  
  row.names(summaryTable) <- rowNames
  
  # Call summary and extract info
  allCols <- icRobject$e_meta[,testsSelected[,1]]
  
  # NOT YET WORKING: Sapply the summary
  # summaryTable <- t(sapply(allCols, function(x) unname(summary(x)[c('Min.', 'Mean', 'Median', 'Max.')])))
  
  # For loop alternative: because sometimes sapply is unreadable
  for (i in 1:rowNum) {
    # Get summary
    summary_tests <- summary(allCols[,i])

    # Put into summaryTable
    summaryTable[i, ] <- unname(summary_tests[c('Min.', 'Mean', 'Median', 'Max.')])
  }
  return (summaryTable)
}
