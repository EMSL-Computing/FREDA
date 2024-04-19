#' Change a FileInput input widget to look like something has been uploaded.  Nothing will actually be 'uploaded', in that input$fileinputid will be NULL.  Depends on a css class 'progress-visible', see details.
#' 
#' @param id The id of the fileInput widget
#' @param content The content to display in the progress bar.
#' @param disable_id The id of a wrapper div around the fileInput widget for disabling purposes.  Defaults to the fileInput widget with js_ prepended.  Set to FALSE to prevent disabling.
#' 
#' @details
#' 
#' The below css class will be applied to the appropriate widgets, it makes the progress bar visible and gives it 100% width.  The !important tag is set, overriding some dynamic css within the shiny fileInput widget.
#' 
#' .progress-visible {
#'  visibility:visible !important;
#' }
#' 
#' .progress-visible .progress-bar {
#'   width:100% !important;
#' }
#' 
#' @return Nothing, simply edits the widget.
#'  
mimic_fileinput_upload <- function(id, progress_content, placeholder_content = NULL, disable_id=NULL, placeholder_id=NULL) {
  progress_bar_id = sprintf("%s_progress", id)
  removeCssClass(progress_bar_id, "active")
  addCssClass(progress_bar_id, class = 'progress-visible')
  
  progress_js = sprintf("$('#%s .progress-bar')[0].innerHTML = '%s'",progress_bar_id, progress_content)
  shinyjs::runjs(progress_js)
  
  if(isTRUE(placeholder_id == FALSE)) {
    invisible()
  } else {
    if (is.null(placeholder_id)) {
      placeholder_id = paste0("js_", id)
    }
    
    if (is.null(placeholder_content)) {
      placeholder_content = "File Uploaded"
    }
    
    placeholder_js =  sprintf("$('#%s input[type=\x22text\x22]')[0].placeholder = '%s'", placeholder_id, placeholder_content)
    
    shinyjs::runjs(placeholder_js)
  }
  
  
  if (isTRUE(disable_id == FALSE)) {
    return()
  } 
  
  if (is.null(disable_id)) {
    disable_id = paste0("js_", id)
  }
  
  shinyjs::disable(disable_id)
  shinyjs::disable(selector = sprintf("#%s .btn", disable_id))
}