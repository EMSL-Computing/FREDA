library(shinytest2)

context("basic-workflow")

test_that("Basic Tests", {
  app <- AppDriver$new(name = "FREDA", height = 1199, width = 1299)
  app <- upload_basic(app)

  ### Make Groups
  message("Testing groups tab...")
  
  app <- groups_basic(app)
  
  groups_list <- app$get_value(export = "groups_list")
  expect_setequal(groups_list$G1, c("EM0011_sample", "EM0013_sample", "EM0015_sample",
                                      "EM0017_sample", "EM0019_sample", "EM0061_sample",
                                      "EM0063_sample", "EM0065_sample", "EM0067_sample",
                                      "EM0069_sample"))
  expect_setequal(groups_list$G2, c("EW0111_sample", "EW0113_sample", "EW0115_sample",
                                      "EW0117_sample", "EW0119_sample", "EW0161_sample",
                                      "EW0163_sample", "EW0165_sample", "EW0167_sample",
                                      "EW0169_sample"))
  
  message("Testing preprocess tab...")
  ### Preprocessing
  app$click("goto_preprocess_main")
  app$wait_for_value(input = "tests")
  app$set_inputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_nosc"))
  app$set_inputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_nosc", "calc_gibbs"))
  app$set_inputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_nosc", "calc_gibbs",
      "calc_aroma"))
  app$set_inputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_nosc", "calc_gibbs",
      "calc_aroma", "calc_dbe"))
  app$set_inputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_nosc", "calc_gibbs",
      "calc_aroma", "calc_dbe", "assign_elemental_composition"))
  app$set_inputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_nosc", "calc_gibbs",
      "calc_aroma", "calc_dbe", "assign_elemental_composition", "assign_class;bs1"))
  app$set_inputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_nosc", "calc_gibbs",
      "calc_aroma", "calc_dbe", "assign_elemental_composition", "assign_class;bs1",
      "assign_class;bs2"))
  app$set_inputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_nosc", "calc_gibbs", 
      "calc_aroma", "calc_dbe", "assign_elemental_composition", "assign_class;bs1", 
      "assign_class;bs2", "assign_class;bs3"))
  
  app$wait_for_value(input = "base_unit")
  app$set_inputs(base_unit = c("CH2", "CO2"))
  app$set_inputs(base_unit = c("CH2", "CO2", "H2"))
  app$set_inputs(base_unit = c("CH2", "CO2", "H2", "H2O"))
  app$set_inputs(base_unit = c("CH2", "CO2", "H2", "H2O", "CHO"))
  app$click("preprocess_click")
  
  app$wait_for_value(input = "preprocess_dismiss")
  app$click("preprocess_dismiss")
  
  app$wait_for_value(output = "preprocess_hist")

  uploaded_data <- app$get_value(export = "uploaded_data_processed")
  expected_cols = c('Mass', 'C', 'H', 'O', 'N', 'C13', 'S', 'P', 'Error', 
                    'NeutralMass', 'MolForm', 'OtoC_ratio', 'HtoC_ratio', 
                    'NtoC_ratio', 'PtoC_ratio', 'NtoP_ratio', 'kmass.CH2', 
                    'kdefect.CH2', 'NOSC', 'GFE', 'AI', 'AI_Mod', 'DBE_1', 
                    'DBE_O', 'DBE_AI', 'ElComposition', 'bs1_class', 
                    'bs2_class', 'bs3_class', 'kmass.CO2', 'kmass.H2', 
                    'kmass.H2O', 'kmass.CHO', 'kdefect.CO2', 'kdefect.H2', 
                    'kdefect.H2O', 'kdefect.CHO')
  
  expect_setequal(colnames(uploaded_data$e_meta), expected_cols)
  
  numtable <- app$get_value(export = "numeric_table")
  cattable <- app$get_value(export = "categorical_table")
  
  expect_equal(digest::digest(numtable), "1d8dc9e8fe23e5579e8fda9d60fb92ca")
  expect_equal(digest::digest(cattable), "78029809dbe2eeafe986ec9d68efb3f5")
  
  message("Testing filter tab...")
  ### Filter
  app$set_inputs(top_page = "Filter")
  app$set_inputs(samplefilter = TRUE)
  app$set_inputs(max_mass = 870)
  app$set_inputs(massfilter = TRUE)
  app$set_inputs(molfilter = TRUE)
  app$set_inputs(formfilter = TRUE)
  app$set_inputs(filter_sidebar = c("samplefilt_collapse", "massfilt_collapse", "molfilt_collapse", 
      "formfilt_collapse", "customfilt_collapse"))
  app$set_inputs(customfilterz = TRUE)
  app$set_inputs(filter_sidebar = c("samplefilt_collapse", "massfilt_collapse", "molfilt_collapse", 
      "formfilt_collapse", "customfilt_collapse", "customfilt_collapse"))
  app$set_inputs(custom1 = "C")
  
  app$click("filter_click")
  
  btn_value = app$wait_for_value(input = "filter_dismiss")
  app$click("filter_dismiss")
  
  message("Testing visualization tab...")
  ### Visualization
  app$click("goto_viz")
  app$set_inputs(choose_single = "1")
  app$click("plot_submit")
  app$click("saveplot")
  app$set_inputs(choose_single = "3")
  app$set_inputs(summary_fxn = "uniqueness_gtest")
  app$click("plot_submit")
  app$click("saveplot")
  app$set_inputs(summary_fxn = "uniqueness_nsamps")
  app$click("plot_submit")
  app$click("saveplot")
  app$set_inputs(chooseplots = "Kendrick Plot")
  app$set_inputs(choose_single = "2")
  app$set_inputs(viz_sidebar = c("axlabs", "peakplots"))
  app$set_inputs(whichSamples = "EM0011_sample")
  app$set_inputs(whichSamples = c("EM0011_sample", "EM0013_sample"))
  app$set_inputs(viz_sidebar = "axlabs")
  app$set_inputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample"))
  app$set_inputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", 
      "EM0067_sample"))
  app$set_inputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", 
      "EM0067_sample", "EW0111_sample"))
  app$click("plot_submit")
  app$click("saveplot")
  app$set_inputs(choose_single = "1")
  app$click("plot_submit")
  app$set_inputs(top_page = "Glossary")
  app$set_inputs(top_page = "Visualize")
  app$set_inputs(chooseplots = "Kendrick Plot")
  app$set_inputs(choose_single = "3")
  app$set_inputs(summary_fxn = "uniqueness_gtest")
  app$click("plot_submit")
  app$click("saveplot")
  app$set_inputs(chooseplots = "Custom Scatter Plot")
  app$set_inputs(whichSamples = "EM0019_sample")
  app$set_inputs(viz_sidebar = c("axlabs", "reactive_plot_opts"))
  app$set_inputs(vk_colors = "uniqueness_gtest")
  app$set_inputs(scatter_x = "N")
  app$set_inputs(scatter_y = "HtoC_ratio")
  app$click("plot_submit")
  app$click("saveplot")
  app$set_inputs(chooseplots = "PCOA Plot")
  app$set_inputs(choose_dist = "gower")
  app$set_inputs(title_input = "My")
  app$set_inputs(title_input = "MyTitle")
  app$click("plot_submit")
  app$click("saveplot")
  
  ### Download
  app$set_inputs(top_page = "Download")
  app$set_inputs(download_selection = c("separate", "merged", "group_data"))
  app$set_inputs(download_mappings = TRUE)
  app$set_inputs(download_plot_table_rows_selected="1", allow_no_input_binding=TRUE)
  app$click("mark_plot_download")
  app$set_inputs(download_plot_table_rows_selected="3", allow_no_input_binding=TRUE)
  app$click("mark_plot_download")
  app$set_inputs(download_plot_table_rows_selected="4", allow_no_input_binding=TRUE)
  app$click("mark_plot_download")
  app$set_inputs(download_plot_table_rows_selected="5", allow_no_input_binding=TRUE)
  app$click("mark_plot_download")
  
  app$set_inputs(download_img_width = 1500)
  app$click("makezipfile")
})
