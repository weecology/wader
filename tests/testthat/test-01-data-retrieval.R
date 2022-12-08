context("Check data retrieval")

wader_data_path <- tempdir()

test_that("download_observations and check_for_newer_data work", {
    skip_on_cran() # these download checks take a while to run
    expect_error(download_observations(wader_data_path, version = "0.20.0"), NA)
    expect_true(check_for_newer_data(wader_data_path))
    httptest::without_internet({
        expect_false(check_for_newer_data(wader_data_path))
    })
    unlink(file.path(wader_data_path, "EvergladesWadingBird"), recursive = TRUE)

    expect_error(download_observations(wader_data_path, version = "1.5.9"))
    expect_error(download_observations(wader_data_path, version = "1.000.0"))

    expect_error(download_observations(wader_data_path), NA)
    expect_false(check_for_newer_data(wader_data_path))
    unlink(file.path(wader_data_path, "EvergladesWadingBird"), recursive = TRUE)
})

test_that("load_indicator_data downloads data if missing", {
    skip_on_cran()
    expect_error(data_tables <- load_indicator_data(wader_data_path, download_if_missing = FALSE))
    expect_warning(data_tables <- load_indicator_data(wader_data_path))
})


test_that("load_indicator_data has the right format", {
    skip_on_cran()
    expect_error(data_tables <- load_indicator_data("repo"), NA)
    expect_equal(length(data_tables), 5)
    expect_equal(names(data_tables),
                 c("coastal_data","max_count","max_count_all","stork_initiation","species_table"))

    data_tables <- load_indicator_data(wader_data_path)
    expect_equal(length(data_tables), 5)
    expect_equal(names(data_tables),
                 c("coastal_data","max_count","max_count_all","stork_initiation","species_table"))
})

test_that("default data path functions work if unset", {
    Sys.unsetenv("WADER_DATA_PATH")
    expect_warning(result <- check_default_data_path(MESSAGE_FUN = warning),
                   "You don't appear to have a defined location for storing EvergladesWadingBird data.")
    expect_false(result)

    m <- capture_messages(check_default_data_path())
    expect_match(m, "You don't appear to have a defined location for storing EvergladesWadingBird data.", all = FALSE)
    expect_match(m, "Call .+ if you wish to set the default data path.", all = FALSE)
    expect_match(m, "EvergladesWadingBird data will be downloaded into .+ otherwise.", all = FALSE)

    expect_error(use_default_data_path())

    data_path <- tempdir()
    expect_error(m <- capture_messages(use_default_data_path(data_path)), NA)
    expect_match(m, "Call `usethis::edit_r_environ()` to open '.Renviron'", fixed = TRUE, all = FALSE)
    expect_match(m, "Store your data path with a line like:", all = FALSE)
    expect_match(m, "WADER_DATA_PATH=", all = FALSE)
    expect_match(m, "Make sure '.Renviron' ends with a newline!", all = FALSE)
})

test_that("default data path functions work if set", {
    Sys.setenv("WADER_DATA_PATH" = tempdir())
    expect_true(check_default_data_path())
    expect_equal(get_default_data_path(), tempdir())
})
