#' @details store corems data files in the appropriate reactiveValues and
#' return a success or error message for use in a modal
#' 
#' @param fpaths character vector of file paths, either retrieved using mapDataAccess
#' or from a fileInput (e.g. input$corems_files$datapath)
#' @param fnames character vector of file names, if NULL, will be generated from
#' fpaths.  This will otherwise come from the fileInput (e.g. input$corems_files$name)
#' 
#' @return modalmessage character vector of message to be used in a modal
#' 
store_corems <- function(fpaths, fnames = NULL) {
    if (is.null(fnames)) {
        fnames <- sapply(fpaths, function(x) basename(tools::file_path_sans_ext(x))) %>%
            make.unique()
    }

    names(fpaths) <- fnames

    corems_revals[['combined_tables']] <- ftmsRanalysis::read_CoreMS_data(
        unlist(fpaths),
        sample_names = names(fpaths)
    )

    for (name in names(fpaths)) {
        corems_revals[['tables']][[name]] <- read_csv(fpaths[[name]])
        corems_revals[['fpaths']][[name]] <- fpaths[[name]]
    }

    modalmessage <- div(class = "column-scroll-sm",
        HTML(info_text[["COREMS_UPLOAD_SUCCESS"]]),
        HTML(paste(names(fpaths), collapse = "<br>"))
    )

    return(modalmessage)
}