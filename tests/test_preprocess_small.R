app <- ShinyDriver$new("../")
app$snapshotInit("test_preprocess_small")
library(testthat)
library(fticRanalysis)

source("sourced_script/preprocess_tab_small.R", local = TRUE)