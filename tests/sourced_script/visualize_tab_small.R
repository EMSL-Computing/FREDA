print("Testing the VISUALIZE TAB, must pass filter tab tests first")
source("sourced_script/filter_tab_small.R", local = TRUE)

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
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
})

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
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
})

test_that("Multiple Sample VK plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$takeScreenshot("screenshots/vk_multiple.png")
app$snapshot(items = list(export = "plot_attrs"))
print("Multi sample VK plot tests passed. Moving to group comparison VK plots....")

# Group Comparison VK Plots
Sys.sleep(0.5)
app$setInputs(choose_single = "3")
Sys.sleep(0.5)
app$setInputs(whichGroups1 = c("CB_40_Oct2016", "CB_50_Oct2016"))
app$setInputs(whichGroups2 = c("CB_60_Oct2016", "PB_00_Oct2016"))
app$setInputs(plot_submit = "click")
app$setInputs(vkbounds = "bs2")
app$setInputs(vkbounds = "0")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(plot_submit = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

app$takeScreenshot("screenshots/vk_groupcomparison.png")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
})

test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$takeScreenshot("screenshots/vk_groupcomparison.png")
app$snapshot(items = list(export = "plot_attrs"))
print("Group comparison vk plot tests passed.  Moving to single sample kendrick plots....")

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
app$setInputs(whichSamples = c("CB_40_Oct2016", "CB_50_Oct2016", "CB_60_Oct2016", "PB_00_Oct2016"))
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "Group_prop_present")
app$setInputs(title_input = "3")
app$setInputs(x_axis_input = "4")
app$setInputs(y_axis_input = "5")
app$setInputs(update_axes = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()

test_that("Multiple sample Kendrick plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$takeScreenshot("screenshots/kendrick_multiple.png")
app$snapshot(items = list(export = "plot_attrs"))
print("Multiple sample Kendrick plot tests passed. Moving to group comparison kendrick plots....")

# Group Comparison Kendrick Plots
Sys.sleep(0.5)
app$setInputs(choose_single = "3")
Sys.sleep(0.5)
app$setInputs(whichGroups1 = c("CB_40_Oct2016", "CB_50_Oct2016"))
app$setInputs(whichGroups2 = c("CB_60_Oct2016", "PB_00_Oct2016"))
app$setInputs(plot_submit = "click")
app$setInputs(vkbounds = "bs2")
app$setInputs(vkbounds = "0")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(update_axes = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
})

test_that("Group kendrick plots produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$takeScreenshot("screenshots/kendrick_groupcomparison.png")
app$snapshot(items = list(export = "plot_attrs"))
print("Group comparison kendrick plot tests passed.  Moving to single sample density plots....")

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
app$setInputs(update_axes = "click")
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

print("Single sample density plot tests passed. Moving to multi-sample density plots....")

# Multi Sample Density Plots
app$setInputs(chooseplots = "Density Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "2")
Sys.sleep(0.5)
app$setInputs(whichSamples = c("CB_40_Oct2016", "CB_50_Oct2016", "CB_60_Oct2016", "PB_00_Oct2016"))
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(update_axes = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
test_that("Single sample density plot produced", {
  
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_true(inherits(vals$export$plot, "plotly"))
  expect_equal(samp_names, vals$input$whichSamples)
  
})
app$takeScreenshot("screenshots/density_multiple.png")
app$snapshot(items = list(output = "parmsTable", export = "plot_attrs"))
print("Multi-sample density plot tests passed.  Moving to group comparison density plots....")

# Group Comparison Density Plots
app$setInputs(chooseplots = "Density Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "3")
Sys.sleep(0.5)
app$setInputs(whichGroups1 = c("CB_40_Oct2016", "CB_50_Oct2016"))
app$setInputs(whichGroups2 = c("CB_60_Oct2016", "PB_00_Oct2016"))
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(x_axis_input = "2")
app$setInputs(y_axis_input = "3")
app$setInputs(update_axes = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
test_that("Single sample density plot produced", {
  
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_true(inherits(vals$export$plot, "plotly"))
  expect_equal(samp_names, vals$input$whichSamples)
  
})
app$takeScreenshot("screenshots/density_groupcomparisons.png")
app$snapshot(items = list(output = "parmsTable", export = "plot_attrs"))
print("Group comparison density plot tests passed, Moving to custom scatter plots....")

# Custom Scatter Plot
app$setInputs(chooseplots = "Custom Scatter Plot")
Sys.sleep(0.5)
app$setInputs(choose_single = "1")
Sys.sleep(0.5)
app$setInputs(whichSamples = "CB_40_Oct2016")
app$setInputs(plot_submit = "click")
app$setInputs(vk_colors = "kmass")
app$setInputs(title_input = "1")
app$setInputs(scatter_x = "DBE")
app$setInputs(scatter_y = "kdefect")
app$setInputs(update_axes = "click")
#app$snapshot(list(output = "FxnPlot"))
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
test_that("Scatter plot produced", {

  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()

  expect_true(inherits(vals$export$plot, "plotly"))
  expect_equal(samp_names, vals$input$whichSamples)

})
app$takeScreenshot("screenshots/scatter_single.png")
app$snapshot(items = list(output = "parmsTable", export = "plot_attrs"))
print("Custom scatterplot tests passed. Moving to DOWNLOAD TAB....")

