print("Performing more detailed test of group comparison plots, must pass filter tab tests first")
source("sourced_script/filter_tab_large.R", local = TRUE )

##### Group Comparison VK Plots
print("Beginning with VK plots.  Testing using G-test")
app$setInputs(top_page = "Visualize")
app$setInputs(chooseplots = "Van Krevelen Plot")
app$setInputs(choose_single = "3")
app$setInputs(whichGroups1 = c("EM0011_sample", "EM0013_sample", "EM0015_sample"),
              whichGroups2 = c("EM0017_sample", "EM0019_sample", "EM0061_sample"))
app$setInputs(summary_fxn = "uniqueness_gtest")
app$setInputs(pval = 0.1,
              pres_thresh = 1,
              plot_submit = "click")
app$setInputs(vkbounds = "bs2", wait_ = FALSE, values_ = FALSE)
app$setInputs(vkbounds = "0",
              x_axis_input = "2",
              y_axis_input = "3",
              plot_submit = "click")

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

app$snapshot(items = list(export = c("plot_attrs", "plot_layout", "plot_visdat")))

# New Parameters (proportion)
print("Testing uniqueness_prop....")
app$setInputs(pres_fn = "prop",
              summary_fxn = "uniqueness_prop",
              pres_thresh = 0.8,
              absn_thresh = 0.1,
              plot_submit = "click")

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

app$snapshot(items = list(export = c("plot_attrs", "plot_layout", "plot_visdat")))

# New Parameters (number of samples)
print("Testing uniqueness_nsamps....")
app$setInputs(pres_fn = "nsamps",
              summary_fxn = "uniqueness_nsamps",
              pres_thresh = 2,
              absn_thresh = 1,
              plot_submit = "click")

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

app$snapshot(items = list(export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/vk_groupcomparison.png")

print("VK groupcomparison tests passed, moving to Kendrick Plots")

# Group comparison kendrick plots
app$setInputs(chooseplots = "Kendrick Plot")
print("Testing using G-test")
app$setInputs(choose_single = "3",
              whichGroups1 = c("EM0011_sample", "EM0013_sample", "EM0015_sample"),
              whichGroups2 = c("EM0017_sample", "EM0019_sample", "EM0061_sample"))
app$setInputs(summary_fxn = "uniqueness_gtest",
              pval = 0.1,
              pres_thresh = 1,
              plot_submit = "click")

app$setInputs(x_axis_input = "2",
              y_axis_input = "3",
              update_axes = "click")

app$setInputs(add_plot = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c(allcolnames)))
})

test_that("Group kendrick plots produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(export = c("plot_attrs", "plot_layout", "plot_visdat")))

# New Parameters (proportion)
print("Testing uniqueness_prop....")
app$setInputs(pres_fn = "prop",
              summary_fxn = "uniqueness_prop",
              pres_thresh = 0.8,
              absn_thresh = 0.1,
              plot_submit = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
})
test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(export = c("plot_attrs", "plot_layout", "plot_visdat")))

# New Parameters (number of samples)
print("Testing uniqueness_nsamps....")
app$setInputs(pres_fn = "nsamps",
              summary_fxn = "uniqueness_nsamps",
              pres_thresh = 2,
              absn_thresh = 1,
              plot_submit = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("Make sure color by options are valid (columns exist)",{
  expect_true(all(vals$export$color_choices %in% allcolnames))
})
test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(export = c("plot_attrs", "plot_layout", "plot_visdat")))
app$takeScreenshot("screenshots/kendrick_groupcomparison.png")

print("Kendrick plot tests passed, moving to group density plots")

# Group Comparison Density Plots
app$setInputs(chooseplots = "Density Plot")
app$setInputs(choose_single = "3")
app$setInputs(whichGroups1 = c("EM0011_sample", "EM0013_sample", "EM0015_sample"),
              whichGroups2 = c("EM0017_sample", "EM0019_sample", "EM0061_sample"),
              plot_submit = "click")
app$setInputs(vk_colors = "kmass",
              title_input = "1",
              x_axis_input = "2",
              y_axis_input = "3",
              update_axes = "click")
app$setInputs(add_plot = "click")

vals <- app$getAllValues()
allcolnames <- c(vals$export$plot_data$e_data %>% colnames(), vals$export$plot_data$e_meta %>% colnames()) 

test_that("check plotting dataframe",{
  expect_true(all(vals$export$color_choices %in% c('bs1', 'bs2', allcolnames)))
  expect_true(inherits(vals$export$plot_data, "peakIcrData"))
  expect_true(!is.null(vals$export$plot_data %>% attr("group_DF")))
})

test_that("Group comparison plot produced", {
  expect_true(inherits(vals$export$plot, "plotly"))
})

app$snapshot(items = list(output = "parmsTable", export = c("plot_attrs", "plot_layout", "plot_visdat")))

app$takeScreenshot("screenshots/density_groupcomparisons.png")
print("Group comparison density plot tests passed, Moving to custom scatter plots....")