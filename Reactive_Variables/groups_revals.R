groupstab_df <- eventReactive(c(revals$groups_list, revals$removed_samples),{
  
  # names of the groups
  names <- names(revals$groups_list)
  
  # samples in each group
  samples <- sapply(revals$groups_list, function(x){
    diff <- setdiff(x, revals$removed_samples)
    paste(diff, collapse = ";")})
  
  # samples removed in filtering step
  removed_samples <- sapply(revals$groups_list, function(x){
    intersect <- intersect(x, revals$removed_samples)
    paste(intersect, collapse = " ")})
  
  # make a dataframe with number of rows equal to the number of groups
  rows <- length(names)
  groupstab_df = data.frame("Group Name" = character(rows), "Group Samples" = character(rows), "Filtered Samples" = character(rows),
                            stringsAsFactors = FALSE, check.names = FALSE)
  
  if(rows == 0){
    return(groupstab_df)
  }
  
  # populate the table
  groupstab_df["Group Name"] <- names
  groupstab_df["Group Samples"] <- samples
  groupstab_df["Filtered Samples"] <- removed_samples
  
  groupstab_df
  
})