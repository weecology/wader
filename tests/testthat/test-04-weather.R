context("checks weather summary output")

wader_data_path <- tempdir()
daily_weather <- weather("daily", path = wader_data_path)
monthly_weather <- weather("monthly", path = wader_data_path)

test_that("'Daily' option returns 11 columns", {
  expect_that(dim(daily_weather)[2], equals(11))
  expect_equal(colnames(daily_weather),
               c("year", "month", "day", "date", "mintemp", "maxtemp", "meantemp",
                 "precipitation", "warm_days", "cool_precip", "warm_precip"))
})

test_that("Daily temperatures ok", {
  expect_equal(length(which((daily_weather$mintemp <= daily_weather$maxtemp) == FALSE)),0)
  expect_equal(length(which((daily_weather$meantemp <= daily_weather$maxtemp) == FALSE)),0)
  expect_equal(length(which((daily_weather$mintemp <= daily_weather$meantemp) == FALSE)),0)
})

test_that("Monthly option returns 13 columns", {
  expect_equal(dim(monthly_weather)[2], 13)
  expect_equal(sum(colnames(monthly_weather) ==
                    c("year", "month", "mintemp", "maxtemp", "meantemp",
                      "precipitation", "warm_days", "cool_precip", "warm_precip",
                      "anomaly_ppt", "anomaly_mint", "anomaly_maxt", "anomaly_meant")), 13)
})

test_that("Monthly temperatures ok", {
  expect_equal(length(which((monthly_weather$mintemp <= monthly_weather$maxtemp) == FALSE)),0)
  # expect_equal(length(which((monthly_weather$meantemp <= monthly_weather$maxtemp) == FALSE)),0)
  # expect_equal(length(which((monthly_weather$mintemp <= monthly_weather$meantemp) == FALSE)),0)
})
