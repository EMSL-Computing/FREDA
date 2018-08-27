print("Testing VISUALIZE TAB, must pass filter tab tests first")
source("sourced_script/filter_tab_large.R", local = TRUE )

###### VISUALIZE TAB ######

# Single sample VK plot
app$setInputs(top_page = "Visualize")
app$setInputs(chooseplots = "Van Krevelen Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "1")
Sys.sleep(0.5)
app$setInputs(whichSamples = "EM0019_sample")
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

allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("check plotting dataframe",{
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_equal(samp_names, vals$input$whichSamples)
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "peakIcrData"))
})

test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/vk_single.png")
print("Single sample VK plot tests passed. Moving to multi-sample VK plots....")



# Multi-sample VK plot
Sys.sleep(0.5)
app$setInputs(choose_single = "2")
Sys.sleep(0.5)
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", "EM0017_sample", "EM0019_sample", "EM0061_sample"))
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
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "groupSummary"))
  expect_true(!is.null(vals$export$plot_data %>% attr("group_DF")))
})

test_that("Multiple Sample VK plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/vk_multiple.png")
print("Multi sample VK plot tests passed. Moving to group comparison VK plots....")

# Group Comparison VK Plots
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

test_that("check plotting dataframe",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "comparisonSummary"))
  expect_true(!is.null(vals$export$plot_data %>% attr("group_DF")))
})

test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/vk_groupcomparison.png")
print("Group comparison vk plot tests passed.  Moving to single sample kendrick plots....")

#Single sample Kendrick plot
app$setInputs(chooseplots = "Kendrick Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "1")
Sys.sleep(0.5)
app$setInputs(whichSamples = "EM0017_sample")
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()

allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("check plotting dataframe",{
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_equal(samp_names, vals$input$whichSamples)
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "peakIcrData"))
})

test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/kendrick_single.png")
print("Single sample Kendrick plot tests passed. Moving to multiple sample kendrick plots....")

#Multiple sample Kendrick plot
Sys.sleep(0.5)
app$setInputs(choose_single = "2")
Sys.sleep(0.5)
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", "EM0017_sample", "EM0019_sample", "EM0061_sample"))
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "Group_prop_present")
app$setInputs(title_input = "3")
app$setInputs(x_axis_input = "4")
app$setInputs(y_axis_input = "5")
app$setInputs(plot_submit = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "groupSummary"))
  expect_true(!is.null(vals$export$plot_data %>% attr("group_DF")))
})

test_that("Multiple Sample VK plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/kendrick_multiple.png")
print("Multiple sample Kendrick plot tests passed. Moving to group comparison kendrick plots....")

# Group comparison kendrick plots
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

test_that("check plotting dataframe",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "comparisonSummary"))
  expect_true(!is.null(vals$export$plot_data %>% attr("group_DF")))
})

test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/kendrick_groupcomparison.png")
print("Group comparison kendrick plot tests passed.  Moving to single sample density plots....")

# Single Sample Density Plot
app$setInputs(chooseplots = "Density Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "1")
Sys.sleep(0.5)
app$setInputs(whichSamples = "EM0019_sample")
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(plot_submit = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()

allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("check plotting dataframe",{
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "peakIcrData"))
  expect_equal(samp_names, vals$input$whichSamples)
})

test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/density_single.png")

print("Single sample density plot tests passed. Moving to multi-sample density plots....")

# Multi Sample Density Plots
app$setInputs(chooseplots = "Density Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "2")
Sys.sleep(0.5)
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", "EM0017_sample", "EM0019_sample", "EM0061_sample"))
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(update_axes = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("check plotting dataframe",{
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_equal(samp_names, vals$input$whichSamples)
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "peakIcrData"))
  expect_true(!is.null(vals$export$plot_data %>% attr("group_DF")))
})

test_that("Single sample density plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/density_multiple.png")
print("Multi-sample density plot tests passed.  Moving to group comparison density plots....")

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
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("check plotting dataframe",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "peakIcrData"))
  expect_true(!is.null(vals$export$plot_data %>% attr("group_DF")))
})

test_that("Single sample density plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/density_groupcomparisons.png")
print("Group comparison density plot tests passed, Moving to custom scatter plots....")


# Custom Scatter Plot
app$setInputs(chooseplots = "Custom Scatter Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "1")
Sys.sleep(0.5)
app$setInputs(whichSamples = "EM0019_sample")
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(scatter_x = "DBE")
app$setInputs(scatter_y = "kdefect")
app$setInputs(plot_submit = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames())

test_that("check plotting dataframe",{
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "peakIcrData"))
  expect_equal(samp_names, vals$input$whichSamples)
})

test_that("Single sample density plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$takeScreenshot("screenshots/scatter_single.png")
app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))
print("Custom scatterplot tests passed. Moving to DOWNLOAD TAB....")