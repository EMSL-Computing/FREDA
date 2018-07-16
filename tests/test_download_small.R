app <- ShinyDriver$new("../")
app$snapshotInit("test_download_small")
library(testthat)
library(fticRanalysis)

source("sourced_script/download_tab_small.R", local = TRUE)