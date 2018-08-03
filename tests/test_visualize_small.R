library(testthat)
library(fticRanalysis)

app <- ShinyDriver$new("../")
app$snapshotInit("test_visualize_small")

source("sourced_script/visualize_tab_small.R", local = TRUE)