app <- ShinyDriver$new("../")
app$snapshotInit("test_upload_large")
library(testthat)
library(fticRanalysis)

source("sourced_script/upload_tab_large.R", local = TRUE)