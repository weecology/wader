library(dplyr)
library(ncdf4)
library(raster)
library(stars)
library(sf)
library(stringr)
library(units)

load_boundaries <- function(path = file.path(wader::get_default_data_path(), "SiteandMethods/regions_layers/POLYGON.shp")) {
  boundaries <- st_read(path)
  return(boundaries)
}

calc_dry_days <- function(depth_data) {
   dry_days <- depth_data %>%
      mutate(dry_days = case_when(depth <= set_units(0, cm) ~ set_units(1, d),
                          depth > set_units(0, cm) ~ set_units(0, d),
                          is.na(depth) ~ set_units(NA, d)),
        .keep = "none") %>%
      st_apply(c(1, 2), sum)
  return(dry_days)
}

calc_recession <- function(depth_data) {
  times <- st_get_dimension_values(depth_data, 'time')
  recession <- filter(depth_data, time == min(times)) -
               filter(depth_data, time == max(times))
  days <- as.integer(max(times) - min(times))
  recession_rate <- recession / days
  return(recession_rate)
}

calc_reversals <- function(depth_data) {
  end_date_position <- st_dimensions(depth_data)$time$to
  depth_deltas <- depth_data[,,,2:end_date_position] -
                    depth_data[,,,1:(end_date_position - 1)]
  reversals <- depth_deltas %>%
    mutate(reversal = case_when(depth > set_units(0, cm) ~ set_units(1, d),
                                depth <= set_units(0, cm) ~ set_units(0, d),
                                is.na(depth) ~ set_units(NA, d)),
          .keep = "none") %>%
    st_apply(c(1, 2), sum)

}

extract_region_means <- function(raster, regions) {
  var_name <- names(raster)
  region_means <- aggregate(raster, regions, mean, na.rm = TRUE) %>%
    setNames(., "value")
  region_means_spdf <- regions %>%
    mutate(variable = var_name, value = region_means$value)
  return(region_means_spdf)
}

available_years <- function(eden_path) {
  eden_data_files <- list.files(file.path(eden_path), pattern = '_depth.nc')
  years <- eden_data_files %>%
    str_split('_', simplify = TRUE) %>%
    .[, 1] %>%
    unique()
  return(years)
}

#' @name get_eden_covariates
#'
#' @title Generate annual scale water covariates using EDEN data
#'
#' @param eden_path path where the EDEN data should be stored
#' @param year numeric vector of years to generate covariates for, defaults to all available years
#' @param boundaries_file name of a shape file holding the boundaries within which to calculate covariates
#'
#' @return data.frame covariate data including columns for region, year, covariate, value, and the geometry of the region
#'
#' @export
#'
get_eden_covariates <- function(eden_path,
                                years = available_years(eden_path),
                                boundaries_path = file.path(wader::get_default_data_path(), 'SiteandMethods/regions_layers/POLYGON.shp')) {
  eden_data_files <- list.files(file.path(eden_path), pattern = '_depth.nc')
  boundaries <- load_boundaries(boundaries_path)
  examp_eden_file <- read_stars(file.path(eden_path, eden_data_files[1]))
  boundaries_utm <- st_transform(boundaries, st_crs(examp_eden_file))

  covariates <- c()
  for (year in years) {
    print(paste("Processing ", year, "...", sep = ""))
    pattern <- file.path(paste(year, "_.*_depth.nc", sep = ''))
    nc_files <- list.files(eden_path, pattern, full.names = TRUE)
    year_data <- read_stars(nc_files, along = "time") %>%
      setNames(., "depth") %>%
      mutate(depth = case_when(depth < set_units(0, cm) ~ set_units(0, cm),
                                depth >= set_units(0, cm) ~ depth,
                                is.na(depth) ~ set_units(NA, cm)))

    breed_start <- as.POSIXct(paste0(year, '-01-01'))
    breed_end <- as.POSIXct(paste0(year, '-06-30'))
    breed_season_data <- year_data %>%
      filter(time >= breed_start, time <= breed_end)

    # Do a pre-breed/post-breed split to allow pre-breeding recession calculations
    # following Peterson 2017. Peterson does this on a per species basis. To start
    # just pick the mid-point for the different species to split on
    pre_breed_end <- as.POSIXct(paste0(year, '-03-01'))
    pre_breed_season_data <- year_data %>%
      filter(time >= breed_start, time <= pre_breed_end)
    post_breed_season_data <- year_data %>%
      filter(time >= pre_breed_end, time <= breed_end)

    # Calculate depth_breed from everwader
    breed_season_depth <- breed_season_data %>%
      st_apply(c(1, 2), mean) %>%
      setNames(., "breed_season_depth")
    init_depth <- breed_season_data[,,,1] %>%
      setNames(., "init_depth")

    # Calculate recession from everwader
    recession <- calc_recession(breed_season_data) %>%
      setNames(., "recession")
    pre_recession <- calc_recession(pre_breed_season_data) %>%
      setNames(., "pre_recession")
    post_recession <- calc_recession(post_breed_season_data) %>%
      setNames(., "post_recession")

    # Calculate dryindex from everwader
    # TODO: USGS code calculates this from t-3 3/31 to t 6/30 and converts
    # the first value to NA
    # To replicate we would need to load three years worth of data and be careful with
    # other applications using min/max or replace this with a different predictor
    # since it's unclear why the lag would be important here
    # we could also fit the lag rather than building it into the feature
    dry_days <- calc_dry_days(breed_season_data) %>%
      setNames(., "dry_days")

    # Calculate reversals following Peterson 2017
    reversals <- calc_reversals(breed_season_data) %>%
      setNames(., "reversals")

    predictors <- list(init_depth, breed_season_depth, recession, pre_recession,
                       post_recession, dry_days, reversals)
    for (predictor in predictors) {
      year_covariates <- extract_region_means(predictor, boundaries_utm) %>%
        mutate(year = year)
      covariates <- rbind(covariates, year_covariates)
    }
  }
  return(covariates)
}
