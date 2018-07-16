app <- ShinyDriver$new("../")
app$snapshotInit("test_visualize_large")
library(testthat)
library(fticRanalysis)

source("sourced_script/visualize_tab_large.R", local = TRUE)