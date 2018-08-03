app <- ShinyDriver$new("../")
app$snapshotInit("test_preprocess_large")
library(testthat)
library(fticRanalysis)

source("sourced_script/preprocess_tab_large.R", local = TRUE)