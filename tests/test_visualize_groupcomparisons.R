library(testthat)
library(fticRanalysis)

app <- ShinyDriver$new("../")
app$snapshotInit("test_visualize_groupcomparisons")

source("sourced_script/visualize_tab_groupcomparisons.R", local = TRUE)