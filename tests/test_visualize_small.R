app <- ShinyDriver$new("../")
app$snapshotInit("test_visualize_small")
library(testthat)
library(fticRanalysis)

source("sourced_script/visualize_tab_small.R", local = TRUE)