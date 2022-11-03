#' Create a Summary For the Information created On Preprocess Tab
#' 
#' This function takes as input a preprocessed peakData object (created when the 
#' Preprocess Data button is clicked) and what tests have been applied, and then finds the min, 
#' mean, median, and max for the summary table.
#' 
#'  @param ftmsRobject an object of class 'peakData', with the default calculations already applied
#'  @param testsSelected A two-column data frame with real names of the test columns in one column 
#'  and the display names in the second
#'  @param categorial set to TRUE if the tests selected are on categorical variables
#'  @param split_chars Character that divides categories for observations that belong to more than one category
#'  @return A table of information

summaryPreprocess <- function(ftmsRobject, testsSelected, categorical = FALSE, split_chars = ";") {

  ## Categorical variable handling block
  if(categorical){
    
    #Apply to each column name
    lapply(names(testsSelected), function(colname){
      
      # split any values in the column that have 2 categories
      split_col <- strsplit(ftmsRobject$e_meta[,colname], split_chars) %>% unlist()
     
      tab_na <- table(split_col, useNA = "ifany")
      tab <- table(split_col)
      
      # get levels/number of levels
      cats <- names(tab_na)
      N_cats <- length(cats)
      
      # make a dummy dataframe that will contain a column for the mode and counts of each category
      df <- data.frame(matrix(ncol = N_cats, nrow = 0)) %>%
        {`colnames<-`(., cats)}
      
      df[1,] <- tab_na # store counts
      rownames(df) <- testsSelected[[colname]] # set rowname for display purposes
    
      return(df)
    })
  }
  
  ## Numeric variable handling block
  
  else{
  # Set up rownames for use in table
    rowNames <- testsSelected
    rowNum <- length(rowNames)
    
    # Set up data frame (finally!)
    summaryTable <- data.frame('Min' = numeric(length = rowNum), 
                               'Mean' = numeric(length = rowNum), 
                               'Median' = numeric(length = rowNum), 
                               'Max' = numeric(length = rowNum))
    
    row.names(summaryTable) <- rowNames
    
    # Call summary and extract info
    allCols <- ftmsRobject$e_meta %>% dplyr::select(names(testsSelected))

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
}
