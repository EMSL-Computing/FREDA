##Function which creates info hover icons for preprocessing checkboxes.

tooltip_checkbox <- function(..., extensions){
  output <- checkboxGroupInput(...)
  
  #number of checkboxes
  n <- length(output$children[[2]]$children[[1]])
  
  lapply(1:n, function(i){
    #second column gets checkboxes
    output$children[[2]]$children[[1]][[i]]$children[[2]] <<- output$children[[2]]$children[[1]][[i]]$children[[1]]
    #first column which previously held checkboxes gets tipify icons
    output$children[[2]]$children[[1]][[i]]$children[[1]] <<- extensions[[i]]
  })
  
  output
}

##Function which creates info hover icons for preprocessing checkboxes.

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