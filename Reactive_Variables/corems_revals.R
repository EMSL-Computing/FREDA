#'@details The columns of the table resulting from reading in the multiple 
#'corems files
corems_cols <- reactive({
  req(corems_revals[['combined_tables']])
  colnames(corems_revals[['combined_tables']])
})

# create CoreMSData object upon button click
cms_data <- eventReactive(input$make_cmsdata, {
  req(corems_revals[['combined_tables']])
  if (input$c13_cname == "Column not present") {c13 <- NULL} else {c13 <- input$c13_cname}
  if (input$o18_cname == "Column not present") {o18 <- NULL} else {o18 <- input$o18_cname}
  if (input$n15_cname == "Column not present") {n15 <- NULL} else {n15 <- input$n15_cname}
  if (input$s34_cname == "Column not present") {s34 <- NULL} else {s34 <- input$s34_cname}
  cms_dat <- as.CoreMSData(corems_revals[['combined_tables']],
                           c13_cname = c13,
                           o18_cname = o18,
                           n15_cname = n15,
                           s34_cname = s34)
  return(cms_dat)
})

#'@details create conf_filt object for confidence filtering panel
conf_filt_obj <- reactive({
  conf_filter(cms_data())
})

#' @details The filtered corems data.
cms_data_filtered <- eventReactive(input$apply_conf_filter, {
  applyFilt(conf_filt_obj(), cms_data(), min_conf = input$min_conf)
})

########## Unique MF Assignment Tab ##########
cms_dat_unq_mf <- eventReactive(input$unique_mf, {  
  if (input$unq_mf_method == "Confidence score") {method <- "confidence"}
  if (input$unq_mf_method == "Peak height") {method <- "peak_intensity"}
  
  unq_dat <- unique_mf_assignment(cms_data_filtered(), method)
  return(unq_dat)
})