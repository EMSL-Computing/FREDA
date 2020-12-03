##Function which creates info hover icons for preprocessing checkboxes.
##Thanks to @K. Rohde on stackoverflow for this.  https://stackoverflow.com/questions/36670065/tooltip-in-shiny-ui-for-help-text

tooltip_checkbox <- function(..., extensions, options){
  output <- checkboxGroupInput(...)
  
  #number of checkboxes
  n <- length(output$children[[2]]$children[[1]])
  
  # output$children[[2]]$children[[1]][[i]] references the i-th row of the checkbox group ...
  # ... from there, each list element identifies a 'column' of that row.
  lapply(1:n, function(i){
    #second column gets checkboxes
    output$children[[2]]$children[[1]][[i]]$children[[2]] <<- output$children[[2]]$children[[1]][[i]]$children[[1]]
    #first column which previously held checkboxes gets tipify icons
    output$children[[2]]$children[[1]][[i]]$children[[1]] <<- extensions[[i]]
    
    if(!is.null(options[[i]])){
      output$children[[2]]$children[[1]][[i]] <<- div(output$children[[2]]$children[[1]][[i]], options[[i]])
    }
  })
  
  output
}

##Function which creates info hover icons for viztab radio buttons

colored_radiobuttons <- function(..., extensions){
  output <- radioButtons(...)

  #number of checkboxes
  n <- length(output$children[[2]]$children[[1]])
  
  lapply(1:n, function(i){
    #second column gets checkboxes
    output$children[[2]]$children[[1]][[i]]$children[[2]] <<- extensions[[i]]
    
  })
  
  output
}

