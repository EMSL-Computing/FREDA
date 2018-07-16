print("Testing the PREPROCESS TAB, need to pass upload tab tests first...")
source("sourced_script/upload_tab_small.R", local = TRUE)

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