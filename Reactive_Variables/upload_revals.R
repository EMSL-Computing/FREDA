# Object: Get e_data from file input
Edata <- reactive({
  # Handle scenario where we made edata from another source.
  if (!is.null(revals$uploaded_data) & is.null(input$file_edata$datapath)) {
    return(revals$uploaded_data$e_data)
  }

  # If we uploaded from a MAP file
  if (!is.null(revals$map_project)) {
    return(revals$map_project$Data$e_data)
  }
  
  # Error handling: Need file_edata path
  req(input$file_edata$datapath)

  # Load file
  filename <- input$file_edata$datapath

  exportTestValues(e_data = read_csv(filename) %>% as.data.frame(stringsAsFactors = FALSE))
  read_csv(filename) %>% as.data.frame(stringsAsFactors = FALSE)

}) # End Edata #

# Object: Get list of column names of Edata
# Note: created when e_data is uploaded
edata_cnames <- reactive({
  # Get column names
  names(Edata())
}) # End edata_cnames #

# Object: Get e_meta from file input
Emeta <- reactive({
  # If we uploaded from a MAP file
  if (!is.null(revals$map_project)) {
    return(revals$map_project$Data$e_meta)
  }
  # Error handling: Need file_emeta to be valid
  req(input$file_emeta$datapath)
  # Load file
  filename <- input$file_emeta$datapath
  exportTestValues(e_meta = read_csv(filename) %>% as.data.frame(stringsAsFactors = FALSE))
  read_csv(filename) %>% as.data.frame(stringsAsFactors = FALSE)
}) # End Emeta #

# Object: Emeta column names
emeta_cnames <- reactive({names(Emeta())})

# Object: Sample names from e_data
sample_names <- reactive({
  if (!is.null(revals$uploaded_data) & is.null(input$file_edata$datapath)) {
    setdiff(edata_cnames(), attr(revals$uploaded_data, "cnames")$edata_cname)
  } else {
    setdiff(edata_cnames(), input$edata_id_col) 
  }
})

# Create reactive fake f_data (used when action button creates peakData())
fdata <- reactive({
  col2 <- rep(NA, length(sample_names()))
  data.frame('SampleId' = sample_names(), 'Var1' = col2)

}) # End fdata #

extra_elements <- reactiveVal(value = list(), label = "extra_elements")
