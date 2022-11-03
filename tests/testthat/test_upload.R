library(shinytest2)

test_that("testing the upload tab", {
  context("basic-upload")
  app <- AppDriver$new(name = "FREDA", height = 1199, width = 800)
  app <- upload_basic(app)
  
  app$click("upload_dismiss")
  
  # Check that some inputs are corretly set
  c_col <- app$get_value(input = "c_column")
  n_col <- app$get_value(input = "n_column")
  s_col <- app$get_value(input = "s_column")
  h_col <- app$get_value(input = "h_column")
  o_col <- app$get_value(input = "o_column")
  p_col <- app$get_value(input = "p_column")
  
  expect_equal(c_col, "C")
  expect_equal(n_col, "N")
  expect_equal(s_col, "S")
  expect_equal(h_col, "H")
  expect_equal(o_col, "O")
  expect_equal(p_col, "P")
  
  # Check data successfully stored and properties are correct.
  uploaded_data <- app$get_value(export="uploaded_data")
  expect_s3_class(uploaded_data, c("peakData", "ftmsData"))
  print(all(dim(uploaded_data$e_data) == c(24442, 21)))
  expect_true(all(dim(uploaded_data$e_data) == c(24442, 21)))
  expect_true(all(dim(uploaded_data$f_data) == c(20, 2)))
  expect_true(all(dim(uploaded_data$e_meta) == c(24442, 11)))
  
  mysumm <- summary(uploaded_data)
  expect_equal(mysumm$Percent_Missing, 81.63714)
})
