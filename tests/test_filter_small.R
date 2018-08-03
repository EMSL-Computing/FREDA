app <- ShinyDriver$new("../")
app$snapshotInit("test_filter_small")
library(testthat)
library(fticRanalysis)

source("sourced_script/filter_tab_small.R", local = TRUE)