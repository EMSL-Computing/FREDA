print("Testing the FILTER TAB, must pass preprocess tab tests first...")
source("sourced_script/preprocess_tab_large.R", local = TRUE)
#### FILTER TAB #####

app$setInputs(top_page = "Filter")
app$setInputs(massfilter = TRUE)
Sys.sleep(0.5)
app$snapshot(list(output = c("summary_filter", "barplot_filter")))
app$setInputs(molfilter = TRUE)
Sys.sleep(0.5)
app$snapshot(list(output = c("summary_filter", "barplot_filter")))
app$snapshot(list(output = c("summary_filter", "barplot_filter")))
app$setInputs(formfilter= TRUE)
Sys.sleep(0.5)
app$setInputs(customfilterz = TRUE)
app$setInputs(custom1 = "HtoC_ratio")
app$setInputs(minimum_custom1 = 0.4)
app$setInputs(maximum_custom1 = 2)
app$setInputs(custom2 = "ElComposition")
app$setInputs(categorical_custom2 = c("CHOS", "CHO", "CHOP", "CHNOS", "CHNOSP", "CHOSP"))

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