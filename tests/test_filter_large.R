app <- ShinyDriver$new("../")
app$snapshotInit("test_filter_large")
library(testthat)
library(fticRanalysis)

source("sourced_script/filter_tab_large.R", local = TRUE)