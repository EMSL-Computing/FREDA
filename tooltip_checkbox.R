tooltip_checkbox <- function(..., extensions){
  output <- checkboxGroupInput(...)
  
  n <- length(output$children[[2]]$children[[1]])
  
  lapply(1:n, function(i){
    output$children[[2]]$children[[1]][[i]]$children[[2]] <<- output$children[[2]]$children[[1]][[i]]$children[[1]]
    output$children[[2]]$children[[1]][[i]]$children[[1]] <<- extensions[[i]]
  })
  
  output
}