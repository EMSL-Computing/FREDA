##
#' Dropdowns for arguments to as.CoreMSData, all are named as 
#' output$<as.coreMSData argument name>
#' 

mutually_exclusive_dropdown <- function(id, title, selected = NULL) {
  renderUI({
    choices = union(
      input[[id]], 
      coreMS_remaining_choices()
    ) %>% setdiff(NULLSELECT__)
    
    choices = c("Select one" = NULLSELECT__, choices)
    
    if(any(!(selected %in% choices),isTRUE(input[[id]] != NULLSELECT__))) {
      selected = input[[id]]
    }
      
    pickerInput(id,
                title,
                choices = choices,
                selected = selected
    )
  }) 
}

output$index_cname <- mutually_exclusive_dropdown(
  "index_cname", "Index Column:", "Index"
)

# output$index_cname <- renderUI({
#   selectInput("index_cname",
#               "Index Column:",
#               choices = union(
#                 c("Select one", input$index_cname), 
#                 coreMS_remaining_choices()
#               ),
#               selected = input$index_cname
#               )
# })

output$obs_mass_cname <- mutually_exclusive_dropdown(
  "obs_mass_cname", "Observed Mass Column:", "m/z"
)
# output$obs_mass_cname <- renderUI({
#   selectInput("obs_mass_cname", "Observed Mass Column:",
#               choices = c("Select one", corems_cols()))
# })

output$calc_mass_cname <- mutually_exclusive_dropdown(
  "calc_mass_cname", "Calculated Mass Column:", "Calculated m/z"
)

# output$calc_mass_cname <- renderUI({
#   selectInput("calc_mass_cname", "Calculated Mass Column:",
#               choices = c("Select one", corems_cols()))
# })

output$pheight_cname <- mutually_exclusive_dropdown(
  "pheight_cname", "Peak Height Column:", "Peak Height"
)

# output$pheight_cname <- renderUI({
#   selectInput("pheight_cname", "Peak Height Column:",
#               choices = c("Select one", corems_cols()))
# })

output$error_cname <- mutually_exclusive_dropdown(
  "error_cname", "Mass Error Column:", "Mass Error (ppm)"
)
# 
# output$error_cname <- renderUI({
#   selectInput("error_cname", "Mass Error Column:",
#               choices = c("Select one", corems_cols()))
# })


output$conf_cname <- mutually_exclusive_dropdown(
  "conf_cname", "Confidence Score Column:", "Confidence Score"
)

# output$conf_cname <- renderUI({
#   selectInput("conf_cname", "Confidence Score Column:",
#               choices = c("Select one", corems_cols()))
# })

output$file_cname <- mutually_exclusive_dropdown(
  "file_cname", "Filename/Sample Column:", "Filename"
)

# output$file_cname <- renderUI({
#   selectInput("file_cname", "Filename/Sample Column:",
#               choices = c("Select one", corems_cols()))
# })

output$mono_index_cname <- mutually_exclusive_dropdown(
  "mono_index_cname", "Mono Isotopic Index Column:", "Mono Isotopic Index"
)

# output$mono_index_cname <- renderUI({
#   selectInput("mono_index_cname", "Mono Isotopic Index Column:",
#               choices = c("Select one", corems_cols()))
# })

output$mf_cname <- mutually_exclusive_dropdown(
  "mf_cname", "Molecular Formula Column:", "Molecular Formula"
)

# output$mf_cname <- renderUI({
#   selectInput("mf_cname", "Molecular Formula Column:",
#               choices = c("Select one", corems_cols()))
# })

output$c13_cname <- mutually_exclusive_dropdown(
  "c13_cname", "C13 Column:", "13C"
)

# output$c13_cname <- renderUI({
#   selectInput("c13_cname", "C13 Column:",
#               choices = c("Select one", "Column not present", corems_cols()))
# })

output$o18_cname <- mutually_exclusive_dropdown(
  "o18_cname", "O18 Column:", "18O"
)

# output$o18_cname <- renderUI({
#   selectInput("o18_cname", "O18 Column:",
#               choices = c("Select one", "Column not present", corems_cols()))
# })

output$n15_cname <- mutually_exclusive_dropdown(
  "n15_cname", "N15 Column:", "15N"
)

# output$n15_cname <- renderUI({
#   selectInput("n15_cname", "N15 Column:",
#               choices = c("Select one", "Column not present", corems_cols()))
# })

output$s34_cname <- mutually_exclusive_dropdown(
  "s34_cname", "S34 Column:", "34S"
)

# output$s34_cname <- renderUI({
#   selectInput("s34_cname", "S34 Column:",
#               choices = c("Select one", "Column not present", corems_cols()))
# })

output$cms_raw_data <- DT::renderDT(
  corems_revals[['combined_tables']],
  options = list(dom = 'ftp',
                 scrollX = TRUE)
)

#'@details display plot of unique masses per sample
#'@app_location CoreMS Creation Tab
output$cmsdat_plot <- renderPlotly({
  req(cms_data())
  plot(cms_data())
})

#'@details data table with kept/removed peaks
#'@app_location Confidence Filtering Tab
output$filt_peaks_dt <- DT::renderDT(
  ftmsRanalysis:::conf_filter_dt(cms_data(), input$min_conf),
  options = list(dom = 't')
)

#'@details Plot of filtered corems data
#'@app_location Confidence Filtering Tab
output$cms_filt_plot <- renderPlotly({
  validate(need(cms_data_filtered(), "Create your filtered data to view filter plot"))
  plot(cms_data_filtered())
})

#'@details display mass error plot with min_conf slider values
#'@app_location Confidence Filtering Tab
output$me_plot <- renderPlotly({
  mass_error_plot(cms_data(), min_conf = input$min_conf)
})

#'@details Molecular formula plot
#'@app_location Unique molecular formula assignment tab
output$mf_plot <- renderPlotly({
  validate(need(cms_dat_unq_mf(), "Please assign molecular formulae to your CoreMS data"))
  plot(cms_dat_unq_mf())
})

#'@details Button to convert corems data to ftmsRanalysis peakData
#'@app_location Convert to peakdata tab
output$corems_to_peakdata_UI <- renderUI({
  validate(need(cms_dat_unq_mf(), "Please assign molecular formulae to your CoreMS data"))
  actionButton("corems_to_peakdata", "Convert to peak data")
})