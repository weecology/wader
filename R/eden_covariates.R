# Calculate standard water depth covariates from EDEN data following WADEM model

#' @name load_boundaries
#'
#' @title Calculate dry days from everwader
#'
#' @param path path to regions shapefile
#' @param level region level to load (all, wcas, or subregions)
#'
#' @export
#'
load_boundaries <- function(path = file.path(wader::get_default_data_path(),
                                             "EvergladesWadingBird/SiteandMethods/regions"),
                                                                        level="subregions") {
  level <- tolower(level)
  boundaries <- sf::st_read(file.path(path,paste(level,".shp",sep = "")))
  return(boundaries)
}

#' @name calc_dry_days
#'
#' @title Calculate dry days from everwader
#'
#' @param depth_data depth .nc files
#'
#' @export
#'
calc_dry_days <- function(depth_data) {
   dry_days <- depth_data %>%
      dplyr::mutate(dry_days = dplyr::case_when(depth <= units::set_units(0, cm) ~
                                                  units::set_units(1, d),
                          depth > units::set_units(0, cm) ~ units::set_units(0, d),
                          is.na(depth) ~ units::set_units(NA, d)),
        .keep = "none") %>%
      stars::st_apply(c(1, 2), sum)
  return(dry_days)
}

#' @name calc_recession
#'
#' @title Calculate recession from everwader
#'
#' @param depth_data depth .nc files
#'
#' @export
#'
calc_recession <- function(depth_data) {
  times <- stars::st_get_dimension_values(depth_data, 'time')
  start_depth <- depth_data |>
    dplyr::filter(time == min(times)) |>
    stars::st_set_dimensions("time", values = NULL)
  end_depth <- depth_data |>
    dplyr::filter(time == max(times)) |>
    stars::st_set_dimensions("time", values = NULL)
  recession <- start_depth - end_depth
  days <- as.integer(max(times) - min(times))
  recession_rate <- recession / days
  return(recession_rate)
}

#' @name calc_reversals
#'
#' @title Calculate reversals following Peterson 2017
#'
#' @param depth_data depth .nc files
#'
#' @export
#'
calc_reversals <- function(depth_data) {
  end_date_position <- stars::st_dimensions(depth_data)$time$to
  depth_t <- depth_data[,,,2:end_date_position] |>
    stars::st_set_dimensions("time", values = seq(1, end_date_position - 1))
  depth_t_minus_1 <- depth_data[,,,1:(end_date_position - 1)] |>
      stars::st_set_dimensions("time", values = seq(1, end_date_position - 1))
  depth_deltas <- depth_t - depth_t_minus_1
  reversals <- depth_deltas %>%
    dplyr::mutate(reversal = dplyr::case_when(depth > units::set_units(0, cm) ~
                                                units::set_units(1, d),
                                depth <= units::set_units(0, cm) ~ units::set_units(0, d),
                                is.na(depth) ~ units::set_units(NA, d)),
          .keep = "none") %>%
    stars::st_apply(c(1, 2), sum)
}


#' @name extract_region_means
#'
#' @title Calculate region means from raster data
#'
#' @param raster variable raster
#' @param regions regions polygons
#'
#' @return region means spdf
#'
#' @export
#'
extract_region_means <- function(raster, regions) {
  var_name <- names(raster)
  region_means <- raster::aggregate(raster, regions, mean, na.rm=TRUE) %>%
    setNames(., "value")
  region_means_spdf <- regions %>%
    dplyr::mutate(variable = var_name, value = region_means$value)
  return(region_means_spdf)
}

#' @name available_years
#'
#' @title Get list of years available for covariate calculation
#'
#' @param eden_path path where the EDEN data should be stored
#'
#' @return vector of years
#'
#' @export
#'

available_years <- function(eden_path =
                      file.path(get_default_data_path(), 'EvergladesWadingBird/Water')) {
  eden_data_files <- list.files(file.path(eden_path), pattern = '_depth.nc')
  years <- eden_data_files %>%
    stringr::str_split('_', simplify = TRUE) %>%
    .[, 1] %>%
    unique()
  return(years)
}

#' @name get_eden_covariates
#'
#' @title Generate annual scale water covariates using EDEN data
#'
#' @param eden_path path where the EDEN data should be stored
#' @param year numeric vector of years to generate covariates for,
#' defaults to all available years
#' @param boundaries_file name of a shape file holding the boundaries
#' within which to calculate covariates
#' @param level region level to load (all, wcas, or subregions)
#'
#' @return data.frame covariate data including columns for region, year,
#' covariate, value, and the geometry of the region
#'
#' @export
#'
get_eden_covariates <- function(eden_path =
                        file.path(get_default_data_path(), 'EvergladesWadingBird/Water'),
                                years = available_years(eden_path),
                                boundaries_path =
        file.path(wader::get_default_data_path(), "EvergladesWadingBird/SiteandMethods/regions"),
                                level="subregions") {

  eden_data_files <- list.files(file.path(eden_path), pattern = '_depth.nc')
  boundaries <- load_boundaries(boundaries_path,level)
  examp_eden_file <- stars::read_stars(file.path(eden_path, eden_data_files[1]))
  boundaries_utm <- sf::st_transform(boundaries, sf::st_crs(examp_eden_file))

  covariates <- c()
  for (year in years) {
    print(paste("Processing ", year, "...", sep = ""))
    pattern <- file.path(paste(year, "_.*_depth.nc", sep = ''))
    nc_files <- list.files(eden_path, pattern, full.names = TRUE)
    year_data <- stars::read_stars(nc_files, along = "time") %>%
      setNames(., "depth") %>%
      dplyr::mutate(depth = dplyr::case_when(depth < units::set_units(0, cm) ~ units::set_units(0, cm),
                                depth >= units::set_units(0, cm) ~ depth,
                                is.na(depth) ~ units::set_units(NA, cm)))

    breed_start <- as.POSIXct(paste0(year, '-01-01'))
    breed_end <- as.POSIXct(paste0(year, '-06-30'))
    breed_season_data <- year_data %>%
      dplyr::filter(time >= breed_start, time <= breed_end)

    # Do a pre-breed/post-breed split to allow pre-breeding recession calculations
    # following Peterson 2017. Peterson does this on a per species basis. To start
    # just pick the mid-point for the different species to split on
    pre_breed_end <- as.POSIXct(paste0(year, '-03-01'))
    pre_breed_season_data <- year_data %>%
      dplyr::filter(time >= breed_start, time <= pre_breed_end)
    post_breed_season_data <- year_data %>%
      dplyr::filter(time >= pre_breed_end, time <= breed_end)

    # Calculate depth_breed from everwader
    breed_season_depth <- breed_season_data %>%
      stars::st_apply(c(1, 2), mean) %>%
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
        dplyr::mutate(year = year)
      covariates <- rbind(covariates, year_covariates)
    }
  }
  return(covariates)
}
