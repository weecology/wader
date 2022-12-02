context("Check indicator data")

wader_data_path <- tempdir()
data_tables <- load_indicator_data(path = wader_data_path)

test_that("coastal data.frame has correct column names", {
  coastal <- data_tables[[1]]
  expect_true(all(colnames(coastal)==c("year","total","coastal","proportion")))
})

test_that("max count data.frame has correct column names", {
  max <- data_tables[[2]]
  expect_true(all(colnames(max)==c("year","region","species","count")))
})

test_that("all max count data.frame has correct column names", {
  max_all <- data_tables[[3]]
  expect_true(all(colnames(max_all)==c("year","region","species","count")))
})

test_that("initiation data.frame has correct column names", {
  initiation <- data_tables[[4]]
  expect_true(all(colnames(initiation)==c("year","initiation","date_score","days_past_nov_1")))
})

test_that("species data.frame has correct column names", {
  species <- data_tables[[5]]
  expect_true(all(colnames(species)==c("species","commonname","scientificname","target_species",
                                       "clutch_size","egg_color","nest_size","nest_materials",
                                       "nest_microhabitat","colony_habitat","brood_size","nest_success",
                                       "chick_description","typical_timing_range","courtship_period",
                                       "nestbuilding_period","reproductive_period","nestling_period",
                                       "incubation_period","branchling_period","feeding_methods",
                                       "foraging_depth","foraging_type","niche_description","prey")))
})
