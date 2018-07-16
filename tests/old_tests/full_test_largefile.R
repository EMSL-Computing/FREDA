source("sourced_script/load_and_process_largefile.R", local = TRUE )

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
app$setInputs(whichSamples = "EM0011_sample")
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample"))
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample"))
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", "EM0017_sample"))
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", "EM0017_sample", "EM0019_sample"))
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
app$setInputs(whichSamples = "EM0017_sample")
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
app$setInputs(whichSamples = "EM0011_sample")
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample"))
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample"))
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", "EM0017_sample"))
app$setInputs(whichSamples = c("EM0011_sample", "EM0013_sample", "EM0015_sample", "EM0017_sample", "EM0019_sample"))
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
test_that("Single sample density plot produced", {
  
  samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
  
  expect_true(inherits(vals$export$plot, "plotly"))
  expect_equal(samp_names, vals$input$whichSamples)
  
})
app$takeScreenshot("screenshots/density_single.png")
app$snapshot(items = list(output = "parmsTable", export = "plot_attrs"))

#### UNCOMMENT WHEN SCATTER PLOTS CAN BE DOWNLOADED ####
# print("Single sample density plot tests passed. Moving to custom scatter plot....")
# 
# app$setInputs(chooseplots = "Custom Scatter Plot")
# Sys.sleep(0.5)
# app$setInputs(choose_single = "1")
# Sys.sleep(0.5)
# app$setInputs(whichSamples = "EM0019_sample")
# app$setInputs(plot_submit = "click")
# app$setInputs(vk_colors = "kmass")
# app$setInputs(title_input = "1")
# app$setInputs(scatter_x = "kmass")
# app$setInputs(scatter_y = "kdefect")
# app$setInputs(plot_submit = "click")
# #app$snapshot(list(output = "FxnPlot"))
# app$setInputs(add_plot = "click")
# 
# vals <- app$getAllValues()
# test_that("Scatter plot produced", {
#   
#   samp_names <- vals$export$plot_data$e_data %>% dplyr::select(-tidyselect::one_of(getEDataColName(vals$export$peakIcr2))) %>% names()
#   
#   expect_true(inherits(vals$export$plot, "plotly"))
#   expect_equal(samp_names, vals$input$whichSamples)
#   
# })
# app$takeScreenshot("screenshots/scatter_single.png")
# app$snapshot(items = list(output = "parmsTable", export = "plot_attrs"))


print("Single sample density plot tests passed. Moving to DOWNLOAD TAB....")

app$setInputs(top_page = "Download")
app$snapshotDownload("download_processed_data", filename = "images.zip")
app$snapshot(items = list(export = "images_out"))
