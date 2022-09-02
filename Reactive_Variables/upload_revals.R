# Object: Get e_data from file input
Edata <- reactive({
  if(!is.null(revals$uploaded_data)) {
    return(revals$uploaded_data$e_data %>%
             dplyr::select(-dplyr::one_of(
               ftmsRanalysis::getEDataColName(revals$uploaded_data)
             )))
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
  setdiff(edata_cnames(), input$edata_id_col)
}) 

# Create reactive fake f_data (used when action button creates peakData())
fdata <- reactive({
  col2 <- rep(NA, length(sample_names()))
  data.frame('SampleId' = sample_names(), 'Var1' = col2)
  
}) # End fdata #