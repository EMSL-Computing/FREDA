#' Creates a report for peakIcr objects
#'
#' This function generates a .docx report for a peakIcr object
#'
#' @param peakIcrData a list containing peakIcrData objects, including at least one data object (of the class 'peakIcrData' created by \code{\link{as.seqData}} and any other peakIcrData objects to include in the report.
#'
#' @details This function generates a .docx report for a peakIcrData data object.  The report includes attributes of the original data, calculated metadata, and filtering procedures applied. 
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

  if(!is.list(peakIcrData)){
    stop("peakIcrData must be a list of peakIcr objects")
  }
  if(!all(inherits(uploaded_data, "peakIcrData"), inherits(processed_data, "peakIcrData"))){
    stop("One or both of the input data objects are not of class 'peakIcrData'")
  }

  params <- list(upload=uploaded_data, processed = processed_data, ...)

  render("peakIcrData_Report.Rmd", output_file=output_file, output_format = output_format, params=params, envir = new.env())

}

