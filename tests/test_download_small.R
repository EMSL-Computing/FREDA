app <- ShinyDriver$new("../")
app$snapshotInit("test_download_small")
library(testthat)
library(fticRanalysis)

old <- Sys.time()
source("sourced_script/download_tab_small.R", local = TRUE)
print(Sys.time() - old)