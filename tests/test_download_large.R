app <- ShinyDriver$new("../")
app$snapshotInit("test_download_large")
library(testthat)
library(fticRanalysis)

source("sourced_script/download_tab_large.R", local = TRUE)