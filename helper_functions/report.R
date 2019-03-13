#' Creates a report for peakData objects
#'
#' This function generates a .docx report for a peakData object
#'
#' @param peakData a list containing peakData objects, including at least one data object (of the class 'peakData' created by \code{\link{as.seqData}} and any other peakData objects to include in the report.
#'
#' @details This function generates a .docx report for a peakData data object.  The report includes attributes of the original data, calculated metadata, and filtering procedures applied. 
#'
#' @return A .docx report of the preprocessing and filtering performed on the data
#'
#' @examples
#' \dontrun{
#' library(mintJansson)
#' data(cDNA_hiseq_data)
#' mycdnadata <- group_designation(omicsData = cDNA_hiseq_data, main_effects = c("treatment"), time_course=NULL)
#' mycdnadata_norm <- normalize_data(omicsData = mycdnadata, norm_fn = "percentile")
#' mycdnadata_results <- countSTAT(omicsData = mycdnadata_norm, comparisons = "all", control = NULL, test = c("dw", "eq", "el", "ef"), pval_adjust = "none", pval_thresh = 0.05)
#' report(omicsData = list(Norm=mycdnadata_norm, Statistics = mycdnadata_results), output_file = "cDNAdata_Report.docx")
#' }
#'
#' @author Allison Thompson
#'
#' @export
report <- function(uploaded_data, processed_data, output_file=NULL, output_format = 'html_document', ...){
  library(rmarkdown)

  if(!all(inherits(uploaded_data, "peakData"), inherits(processed_data, "peakData"))){
    stop("One or both of the input data objects are not of class 'peakData'")
  }

  params <- list(upload=uploaded_data, processed = processed_data, ...)

  render("peakData_Report.Rmd", output_file=output_file, output_format = output_format, params=params, envir = new.env())

}

