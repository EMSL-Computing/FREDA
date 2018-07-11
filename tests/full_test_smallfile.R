app <- ShinyDriver$new("../")
app$snapshotInit("full_test_smallfile")
library(testthat)
library(fticRanalysis)

##### UPLOAD TAB ######
app$setInputs(top_page = "Upload")
app$uploadFile(file_edata = "test_edata.csv")
app$setInputs(edata_id_col = "Mass")
app$uploadFile(file_emeta = "test_emeta.csv")
app$setInputs(isotope_yn = "1")
app$setInputs(iso_info_column = "C13")
app$setInputs(select = "2")
app$snapshot(items = list(input = TRUE, output = c("head_emeta", "head_edata")))
app$setInputs(upload_click = "click")
app$setInputs(upload_dismiss = "click")

vals <- app$getAllValues()
test_that("check uploaded data",{
  expect_true(inherits(vals$export$peakICR, "icrData"))
  expect_true(nrow(vals$export$peakICR$e_data) <= nrow(vals$export$e_data))
  expect_true(nrow(vals$export$peakICR$e_meta) <= nrow(vals$export$e_meta))
  expect_equal(colnames(vals$export$peakICR$e_data), colnames(vals$export$e_data))
  expect_true(all(colnames(vals$export$e_meta) %in% colnames(vals$export$peakICR$e_meta)))
})
print("Upload tests passed.  Moving to preprocess tab....")
app$takeScreenshot("screenshots/upload_end.png")


##### PREPROCESS TAB ######
app$setInputs(top_page = "Preprocess")

# select everything but NOSC and click(arbitrarily excluded)
app$setInputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_gibbs"))
app$setInputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_gibbs", "calc_aroma"))
app$setInputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_gibbs", "calc_aroma", "calc_dbe"))
app$setInputs(tests = c("calc_element_ratios", "calc_kendrick", "calc_gibbs", "calc_aroma", "calc_dbe", "assign_elemental_composition"))
app$setInputs(preprocess_click = "click")

# snapshot table values
app$setInputs(which_hist = "NtoC_ratio")
app$snapshot(list(output = c("Table_1", "numeric_summary"), export = "hist_attrs"))

# test that dataframe was correctly constructed
vals <- app$getAllValues()
test_that("preprocess tests",{
  expect_true(is.character(vals$export$display_names))
  expect_true(all(vals$export$display_names %in% colnames(vals$export$peakIcr2$e_meta)))
  expect_true(inherits(vals$export$preprocess_hist, "plotly"))
})
app$takeScreenshot("screenshots/preprocess_end.png")
print("Preprocess tests passed.  Moving to filter tab.....")


#### FILTER TAB #####

app$setInputs(top_page = "Filter")
app$setInputs(massfilter = TRUE)
Sys.sleep(0.5)
app$snapshot(list(output = c("summary_filter", "barplot_filter")))
app$setInputs(molfilter = TRUE)
Sys.sleep(0.5)
app$snapshot(list(output = c("summary_filter", "barplot_filter")))
app$takeScreenshot()
app$setInputs(filter_click = "click")
app$setInputs(filter_dismiss = "click")

vals <- app$getAllValues()
test_that("filtered peakICR object tests",{
  if(vals$input$massfilter){
    expect_false(is.null(attr(vals$export$peakIcr2, "filters")$massFilt))
  }
  if(vals$input$molfilter){
    expect_false(is.null(attr(vals$export$peakIcr2, "filters")$moleculeFilt))
  }
  
  expect_true(inherits(vals$export$peakIcr2, "icrData"))
  
})
app$takeScreenshot("screenshots/filter_end.png")
print("Filter tests passed.  Moving to visualize tab.....")

###### VISUALIZE TAB ######

# Single sample VK plot
app$setInputs(top_page = "Visualize")
app$setInputs(chooseplots = "Van Krevelen Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "1")
Sys.sleep(0.5)
app$setInputs(whichSamples = "CB_40_Oct2016")
app$setInputs(plot_submit = "click")
app$setInputs(vkbounds = "bs2")
app$setInputs(vkbounds = "0")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(plot_submit = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
test_that("Single VK plot produced", {
  
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_true(inherits(vals$export$plot, "plotly"))
  expect_equal(samp_names, vals$input$whichSamples)
  
})
app$takeScreenshot("screenshots/vk_single.png")
app$snapshot(items = list(export = "plot_attrs"))
print("Single sample VK plot tests passed. Moving to multi-sample VK plots....")



# Multi-sample VK plot
Sys.sleep(0.5)
app$setInputs(choose_single = "2")
Sys.sleep(0.5)
app$setInputs(whichSamples = "CB_40_Oct2016")
app$setInputs(whichSamples = c("CB_40_Oct2016", "CB_50_Oct2016"))
app$setInputs(whichSamples = c("CB_40_Oct2016", "CB_50_Oct2016", "CB_60_Oct2016"))
app$setInputs(whichSamples = c("CB_40_Oct2016", "CB_50_Oct2016", "CB_60_Oct2016", "PB_00_Oct2016"))
app$setInputs(plot_submit = "click")
app$setInputs(vkbounds = "bs2")
app$setInputs(vkbounds = "0")
app$setInputs(vk_colors = "Group_prop_present")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(plot_submit = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()

test_that("Multiple Sample VK plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$takeScreenshot("screenshots/vk_multiple.png")
app$snapshot(items = list(export = "plot_attrs"))
print("Multi sample VK plot tests passed. Moving to single sample kendrick plots....")

#Single sample Kendrick plot
app$setInputs(chooseplots = "Kendrick Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "1")
Sys.sleep(0.5)
app$setInputs(whichSamples = "CB_40_Oct2016")
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
app$snapshot(items = list(export = "plot_attrs"))
test_that("Single sample Kendrick plot produced", {
  
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_true(inherits(vals$export$plot, "plotly"))
  expect_equal(samp_names, vals$input$whichSamples)
  
})
app$takeScreenshot("screenshots/kendrick_single.png")
print("Single sample Kendrick plot tests passed. Moving to multiple sample kendrick plots....")

#Multiple sample Kendrick plot
Sys.sleep(0.5)
app$setInputs(choose_single = "2")
Sys.sleep(0.5)
app$setInputs(whichSamples = "CB_40_Oct2016")
app$setInputs(whichSamples = c("CB_40_Oct2016", "CB_50_Oct2016"))
app$setInputs(whichSamples = c("CB_40_Oct2016", "CB_50_Oct2016", "CB_60_Oct2016"))
app$setInputs(whichSamples = c("CB_40_Oct2016", "CB_50_Oct2016", "CB_60_Oct2016", "PB_00_Oct2016"))
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "Group_prop_present")
app$setInputs(title_input = "3")
app$setInputs(x_axis_input = "4")
app$setInputs(y_axis_input = "5")
app$setInputs(plot_submit = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()

test_that("Multiple sample Kendrick plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$takeScreenshot("screenshots/kendrick_multiple.png")
app$snapshot(items = list(export = "plot_attrs"))
print("Multiple sample Kendrick plot tests passed. Moving to single sample density plots....")

# Single Sample Density Plot
app$setInputs(chooseplots = "Density Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "1")
Sys.sleep(0.5)
app$setInputs(whichSamples = "CB_40_Oct2016")
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(plot_submit = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
test_that("Single sample density plot produced", {
  
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_true(inherits(vals$export$plot, "plotly"))
  expect_equal(samp_names, vals$input$whichSamples)
  
})
app$takeScreenshot("screenshots/density_single.png")
app$snapshot(items = list(output = "parmsTable", export = "plot_attrs"))
print("Single sample density plot tests passed. Moving to DOWNLOAD TAB....")

app$setInputs(top_page = "Download")
app$snapshotDownload("download_processed_data", filename = "images.zip")
app$snapshot(items = list(export = "images_out"))
