print("Performing more detailed test of group comparison plots, must pass filter tab tests first")
source("sourced_script/filter_tab_large.R", local = TRUE )

##### Group Comparison VK Plots
print("VK plots.  Testing using G-test")
app$setInputs(top_page = "Visualize")
app$setInputs(chooseplots = "Van Krevelen Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "3")
Sys.sleep(0.5)
app$setInputs(whichGroups1 = c("EM0011_sample", "EM0013_sample", "EM0015_sample"))
app$setInputs(whichGroups2 = c("EM0017_sample", "EM0019_sample", "EM0061_sample"))
app$setInputs(summary_fxn = "uniqueness_gtest")
app$setInputs(pval = 0.1)
app$setInputs(pres_thresh = 1)
app$setInputs(plot_submit = "click")
app$setInputs(vkbounds = "bs2")
app$setInputs(vkbounds = "0")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(plot_submit = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
})

test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

# New Parameters (proportion)
print("Testing uniqueness_prop....")
app$setInputs(pres_fn = "prop")
app$setInputs(summary_fxn = "uniqueness_prop")
app$setInputs(pres_thresh = 0.8)
app$setInputs(absn_thresh = 0.1)
app$setInputs(plot_submit = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
})
test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(export = "plot_attrs"))

# New Parameters (number of samples)
print("Testing uniqueness_nsamps....")
app$setInputs(pres_fn = "nsamps")
app$setInputs(summary_fxn = "uniqueness_nsamps")
app$setInputs(pres_thresh = 2)
app$setInputs(absn_thresh = 1)
app$setInputs(plot_submit = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
})
test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(export = "plot_attrs"))
app$takeScreenshot("screenshots/vk_groupcomparison.png")

print("VK groupcomparison tests passed, moving to Kendrick Plots")

# Group comparison kendrick plots
app$setInputs(chooseplots = "Kendrick Plot")
print("Testing using G-test")
Sys.sleep(0.5)
app$setInputs(choose_single = "3")
Sys.sleep(0.5)
app$setInputs(whichGroups1 = c("EM0011_sample", "EM0013_sample", "EM0015_sample"))
app$setInputs(whichGroups2 = c("EM0017_sample", "EM0019_sample", "EM0061_sample"))
app$setInputs(summary_fxn = "uniqueness_gtest")
app$setInputs(pval = 0.1)
app$setInputs(pres_thresh = 1)
app$setInputs(plot_submit = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(update_axes = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c(allcolnames)))
})

test_that("Group kendrick plots produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(export = "plot_attrs"))

# New Parameters (proportion)
print("Testing uniqueness_prop....")
app$setInputs(pres_fn = "prop")
app$setInputs(summary_fxn = "uniqueness_prop")
app$setInputs(pres_thresh = 0.8)
app$setInputs(absn_thresh = 0.1)
app$setInputs(plot_submit = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
})
test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(export = "plot_attrs"))

# New Parameters (number of samples)
print("Testing uniqueness_nsamps....")
app$setInputs(pres_fn = "nsamps")
app$setInputs(summary_fxn = "uniqueness_nsamps")
app$setInputs(pres_thresh = 2)
app$setInputs(absn_thresh = 1)
app$setInputs(plot_submit = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% allcolnames))
})
test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(export = "plot_attrs"))
app$takeScreenshot("screenshots/kendrick_groupcomparison.png")

print("Kendrick plot tests passed, moving to group density plots")

# Group Comparison Density Plots
app$setInputs(chooseplots = "Density Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "3")
Sys.sleep(0.5)
app$setInputs(whichGroups1 = c("EM0011_sample", "EM0013_sample", "EM0015_sample"))
app$setInputs(whichGroups2 = c("EM0017_sample", "EM0019_sample", "EM0061_sample"))
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(update_axes = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
test_that("Group density plot produced", {
  
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_true(inherits(vals$export$plot, "plotly"))
  expect_true(c(vals$input$whichGroups1, vals$input$whichGroups2) %in% samp_names)
  
})
app$takeScreenshot("screenshots/density_groupcomparisons.png")
app$snapshot(items = list(output = "parmsTable", export = "plot_attrs"))
print("Group comparison density plot tests passed, Moving to custom scatter plots....")