context("Check ground count data")

wader_data_path <- tempdir()

test_that("ground counts default has correct column names", {
  data <- ground_counts(path = wader_data_path)
  expect_true(all(colnames(data)==c("year","species","total")))
})

test_that("ground counts level = point has correct column names", {
  data <- ground_counts(level = "point",path = wader_data_path)
  expect_true(all(colnames(data) == c("year","date","latitude","longitude","species","count",
                                      "nests","chicks")))
})
