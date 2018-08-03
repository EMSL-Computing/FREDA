app <- ShinyDriver$new("../")
app$snapshotInit("full_test_largefile")
library(testthat)
library(fticRanalysis)

##### UPLOAD TAB ######
app$setInputs(top_page = "Upload")
app$uploadFile(file_edata = "example12T_edata.csv")
app$setInputs(edata_id_col = "Mass")
app$uploadFile(file_emeta = "example12T_emeta.csv")
app$setInputs(isotope_yn = "1")
app$setInputs(iso_info_filter = "1")
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