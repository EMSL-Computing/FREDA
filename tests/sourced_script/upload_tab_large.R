print("Testing the UPLOAD TAB")

##### UPLOAD TAB ######
app$setInputs(top_page = "Upload")
app$uploadFile(file_edata = "example12T_edata.csv")
app$setInputs(edata_id_col = "Mass", wait_ = FALSE, values_ = FALSE)
app$uploadFile(file_emeta = "example12T_emeta.csv")
app$setInputs(isotope_yn = "1")
app$setInputs(iso_info_filter = "1")
app$setInputs(iso_info_column = "C13",
              select = "2")
app$snapshot(items = list(input = TRUE, output = c("head_emeta", "head_edata")))
app$setInputs(upload_click = "click")
app$setInputs(upload_dismiss = "click", wait_ = FALSE, values_ = FALSE)

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