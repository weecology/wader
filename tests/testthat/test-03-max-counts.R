context("Check max count data")

wader_data_path <- tempdir()
data <- max_counts(path = wader_data_path)

test_that("max count data.frame has correct column names", {
  expect_true(all(colnames(data)==c("year","colony","species","count" )))
})
