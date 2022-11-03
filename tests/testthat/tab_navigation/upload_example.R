#'Upload example data and fill in basic information on the uplaods tab
#'
upload_basic <- function(app) {
  app$set_inputs(filter_sidebar = c("samplefilt_collapse", "massfilt_collapse", "molfilt_collapse", 
                                    "formfilt_collapse"))
  app$set_inputs(select = character(0))
  app$set_inputs(isotope_yn = character(0))
  app$set_inputs(data_scale = "abundance")
  app$set_inputs(upload_collapse = "file_upload")
  app$set_inputs(file_edata = character(0))
  app$set_inputs(file_emeta = character(0))
  app$click("element_dropdown")
  app$click("upload_click")
  app$set_inputs(top_page = "data_requirements")
  app$set_inputs(top_page = "Resources/Contact")
  app$set_inputs(top_page = "Upload")
  app$upload_file(file_edata = "../../Data/example12T_edata.csv")
  app$wait_for_value(input="edata_id_col")
  app$set_inputs(edata_id_col = "Mass")
  app$upload_file(file_emeta = "../../Data/example12T_emeta.csv")
  app$set_inputs(upload_collapse = "column_info")
  app$set_inputs(select = "2")
  app$click("element_dropdown")
  app$set_inputs(element_dropdown_state = TRUE)
  app$set_inputs(element_dropdown_state = FALSE)
  app$set_inputs(isotope_yn = "2")
  app$click("upload_click")
  
  btn_value = app$wait_for_value(input = "upload_dismiss")
  
  return(app)
}