app <- ShinyDriver$new("../")
app$snapshotInit("test_upload_small")
library(testthat)
library(fticRanalysis)

source("sourced_script/upload_tab_small.R", local = TRUE)