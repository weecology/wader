#' @title Weather by day or month
#'
#' @description Summarize hourly weather data to either daily or monthly
#'
#' @param level specify 'monthly', 'daily'
#' @param horizon Horizon (number of days) to use when calculating cumulative values
#' (eg warm weather precip)
#' @param temperature_limit Temperature limit (in C) to use when calculating cumulative values
#' (eg warm weather precip)
#'
#' @export
#'
weather <- function(level = "daily", horizon = 365, temperature_limit = 10,
                      path = get_default_data_path())
{
  options(dplyr.summarise.inform = FALSE)
  level <- tolower(level)
  weather <- load_datafile("Weather/weather.csv", na.strings = c(""), path = path) %>%

  ###########Summarize by Day ----------------------
    dplyr::select(c("date","tobs","tmin","tmax","tavg","prcp")) %>%
    dplyr::mutate(year = lubridate::year(.data$date),
                  month = lubridate::month(.data$date),
                  day = lubridate::day(.data$date)) %>%
    dplyr::group_by(.data$year, .data$month, .data$day, .data$date) %>%
    dplyr::summarize(mintemp = suppressWarnings(min(c(.data$tmin,.data$tmax,.data$tobs,.data$tavg), na.rm = TRUE)),
                     maxtemp = suppressWarnings(max(c(.data$tmin,.data$tmax,.data$tobs,.data$tavg), na.rm = TRUE)),
                     meantemp = suppressWarnings(mean(c(.data$tobs,.data$tavg), na.rm = TRUE)),
                     precipitation = suppressWarnings(mean(.data$prcp, na.rm = TRUE))) %>%
    dplyr::mutate(across(where(is.numeric), ~dplyr::na_if(., Inf)),
                  across(where(is.numeric), ~dplyr::na_if(., -Inf)),
                  across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(warm_days = zoo::rollapplyr(.data$mintemp, width = horizon,
                             FUN = function(x) length(which(x >= temperature_limit)), partial = TRUE)) %>%
    dplyr::mutate(cool_precip = zoo::rollapplyr(ifelse(.data$mintemp < temperature_limit,
                                                       .data$precipitation, 0),
                                            width = horizon, FUN = sum, partial = TRUE, na.rm = TRUE)) %>%
    dplyr::mutate(warm_precip = zoo::rollapplyr(ifelse(.data$mintemp >= temperature_limit,
                                                       .data$precipitation, 0),
                                                width = horizon, FUN = sum, partial = TRUE, na.rm = TRUE)) %>%
    dplyr::ungroup()

  if (level == "monthly") {
    ##########Summarize by Month -----------------
    normals <- load_datafile("Weather/prism_normals.csv", na.strings = c(""), path = path) %>%
      dplyr::filter(.data$month != "Annual") %>%
      dplyr::mutate(month = match(.data$month, month.name))
    weather <- weather %>%
      dplyr::group_by(.data$year, .data$month) %>%
      dplyr::summarize(mintemp = mean(.data$mintemp, na.rm = TRUE),
                       maxtemp = mean(.data$maxtemp, na.rm = TRUE),
                       meantemp = mean(.data$meantemp, na.rm = TRUE),
                       precipitation = sum(.data$precipitation, na.rm = TRUE),
                       warm_days = mean(.data$warm_days, na.rm = TRUE),
                       cool_precip = mean(.data$cool_precip, na.rm = TRUE),
                       warm_precip = mean(.data$warm_precip, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(c("year", "month", "mintemp", "maxtemp", "meantemp",
                      "precipitation", "warm_days", "cool_precip", "warm_precip")) %>%
      dplyr::mutate(mintemp = ifelse(is.finite(.data$mintemp), .data$mintemp, NA),
                    maxtemp = ifelse(is.finite(.data$maxtemp), .data$maxtemp, NA),
                    meantemp = ifelse(is.finite(.data$meantemp), .data$meantemp, NA)) %>%
      dplyr::full_join(normals, by="month") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(anomaly_ppt = .data$precipitation/.data$ppt,
                    anomaly_mint = .data$mintemp - .data$tmin,
                    anomaly_maxt = .data$maxtemp - .data$tmax,
                    anomaly_meant = .data$meantemp - .data$tmean) %>%
      dplyr::select(-c("ppt", "tmin", "tmax", "tmean")) %>%
      dplyr::arrange(.data$year, .data$month)
  }
  return(as.data.frame(weather))
}
