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
#' @inheritParams load_indicator_data
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
                     # meantemp = suppressWarnings(mean(c(.data$tobs,.data$tavg), na.rm = TRUE)),
                     meantemp = suppressWarnings(mean(c(.data$tmin,.data$tmax), na.rm = TRUE)),
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
      dplyr::mutate(anomaly_ppt = .data$precipitation - .data$ppt,
                    anomaly_mint = .data$mintemp - .data$tmin,
                    anomaly_maxt = .data$maxtemp - .data$tmax,
                    anomaly_meant = .data$meantemp - .data$tmean) %>%
      dplyr::select(-c("ppt", "tmin", "tmax", "tmean")) %>%
      dplyr::arrange(.data$year, .data$month)
  }
  return(as.data.frame(weather))
}

#' @title Weather figures for reporting
#'
#' @description Plot monthly deviations from mean
#'
#' @param data specify 'temperature', 'precipitation'
#' @param plot_year year to plot
#'
#' @inheritParams load_indicator_data
#'
#' @export
#'
weather_report <- function(data = "temperature", plot_year = as.numeric(format(Sys.Date(), "%Y")),
                    path = get_default_data_path())
{
data = tolower(data)

  weather <- weather(level = "monthly")

  sds <- weather %>%
    dplyr::group_by(.data$month) %>%
    dplyr::summarise(temperature = sd(.data$anomaly_meant, na.rm =TRUE),
                     precipitation = sd(.data$anomaly_ppt, na.rm =TRUE)) %>%
    dplyr::filter(.data$month %in% c(12,1:6)) %>%
    dplyr::select("month", "temperature", "precipitation")

  current <- weather %>%
    dplyr::mutate(year = ifelse(.data$month == 12, .data$year+1, .data$year)) %>%
    dplyr::filter(year == plot_year, .data$month %in% c(12,1:6)) %>%
    dplyr::select("month","anomaly_meant","anomaly_ppt") %>%
    dplyr::full_join(sds) %>%
    dplyr::arrange(.data$month) %>%
    dplyr::mutate(names=month.abb[.data$month]) %>%
    dplyr::mutate(names = factor(.data$names, levels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")))

  if(data=="temperature") {
    x <- ggplot2::ggplot(current)  +
         ggplot2::geom_bar(ggplot2::aes(x=names, y=anomaly_meant),
                           stat="identity",fill="red",color="red",width = 0.75) +
         ggplot2::geom_line(ggplot2::aes(x=names, y=temperature,group = 1),
                            stat="identity",linetype=2,linewidth=1.4) +
         ggplot2::geom_line(ggplot2::aes(x=names, y=-temperature,group = 1),
                            stat="identity",linetype=2,linewidth=1.4) +
         ggplot2::labs(y="Temperature deviation (C)") +
         ggplot2::theme_bw() +
         ggplot2::xlab(plot_year)
  }

  if(data=="precipitation") {
    x <- ggplot2::ggplot(current)  +
         ggplot2::geom_bar(ggplot2::aes(x=names, y=anomaly_ppt),
                           stat="identity",fill="red",color="red",width = 0.75) +
         ggplot2::geom_line(ggplot2::aes(x=names, y=precipitation,group = 1),
                            stat="identity",linetype=2,linewidth=1.4) +
         ggplot2::geom_line(ggplot2::aes(x=names, y=-precipitation,group = 1),
                            stat="identity",linetype=2,linewidth=1.4) +
         ggplot2::labs(y="Precipitation deviation (mm)") +
         ggplot2::theme_bw() +
         ggplot2::xlab(plot_year)
  }
  print(x)
}

#' @name water_report
#'
#' @title Plot water gauge data
#'
#' @description Create a plot of water gauge data
#'
#' @param path Depth data location
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param wca wca to plot
#' @inheritParams load_indicator_data
#'
#'
#' @return a data.frame
#'
#' @export
#'
water_report <- function(path = get_default_data_path(),
                       minyear = as.integer(format(Sys.Date(), "%Y"))-3,
                       maxyear = as.integer(format(Sys.Date(), "%Y")),
                       wca = "wca1",
                       download_if_missing = TRUE)
{
  depths <- load_datafile("Water/eden_depth.csv",
                          download_if_missing = download_if_missing) %>%
            dplyr::mutate(date=as.Date(date))
  monthly_means <- depths %>%
                   dplyr::mutate(month=lubridate::month(date)) %>%
                   dplyr::group_by(month, region) %>%
                   dplyr::summarise(min=mean(depth_min), max=mean(depth_max),
                                    min_sd=sd(depth_min), max_sd=sd(depth_max))

   depths %>%
    dplyr::filter(region==wca, dplyr::between(date,
                                as.Date(paste(minyear,'-01-01',sep="")),
                                as.Date(paste(maxyear,'-07-30',sep="")))) %>%
    merge(monthly_means) %>%
      dplyr::mutate(year=lubridate::year(date)) %>%
      dplyr::mutate(min=ifelse(date==as.Date(paste(year,'-',month,'-15',sep="")), min, NA),
                    max=ifelse(date==as.Date(paste(year,'-',month,'-15',sep="")), max, NA),
                    min_sd=ifelse(date==as.Date(paste(year,'-',month,'-15',sep="")), min-min_sd, NA),
                    max_sd=ifelse(date==as.Date(paste(year,'-',month,'-15',sep="")), max+max_sd, NA)) %>%
    ggplot2::ggplot()  +
    ggplot2::geom_line(ggplot2::aes(x=date, y=depth_mean), color="red", linewidth = 1.4) +
    ggplot2::geom_ribbon(ggplot2::aes(x=date, y=depth_mean, ymin=depth_min, ymax=depth_max, fill="red"), alpha=0.5) +
    ggplot2::geom_point(ggplot2::aes(x=date, y=min), shape = 4, size = 3) +
    ggplot2::geom_point(ggplot2::aes(x=date, y=max), shape = 3, size = 3) +
    ggplot2::geom_point(ggplot2::aes(x=date, y=min_sd), shape = 4, size = 3) +
    ggplot2::geom_point(ggplot2::aes(x=date, y=max_sd), shape = 3, size = 3) +
    ggplot2::labs(x="Date", y="Stage (cm)") +
    ggplot2::scale_x_date(date_labels="%b %y", date_breaks="4 months") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

}
