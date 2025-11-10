#' @name max_count_indicator
#'
#' @title Generate summaries of max count indicator data
#'
#' @description Create a table of rolling averages for the max count indicator data,
#' by year and region
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @inheritParams load_indicator_data
#'
#' @return a data.frame
#'
#' @export
#'
max_count_indicator <- function(path = get_default_data_path(),
                                  minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                                  window = 3,
                                  download_if_missing = TRUE)
{

  load_datafile(
    "Indicators/max_count_all.csv",
    path = path,
    download_if_missing = download_if_missing) %>%
               dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
               dplyr::select(-"region") %>%
               dplyr::arrange(.data$species, .data$year) %>%
               dplyr::group_by(.data$species) %>%
    dplyr::mutate(                             # rolling average
      count_mean = slider::slide_index_dbl(
        .x = .data$count,
        .i = .data$year,
        .f = mean,
        .before = window-1
      )
    )

}

#' @name initiation_indicator
#'
#' @title Generate summaries of wood stork nesting intiation indicator data
#'
#' @description Create a table of rolling averages for the date of wood stork nest initiation,
#' by year
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @inheritParams load_indicator_data
#'
#' @return a data.frame
#'
#' @export
#'
initiation_indicator <- function(path = get_default_data_path(),
                                minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                                window = 3,
                                download_if_missing = TRUE)
{

  load_datafile(
    "Indicators/stork_initiation.csv",
    path = path,
    download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(date_score_mean = zoo::rollapply(.data$date_score, FUN="mean",
                                                   width=window, align="right",
                                                   partial = TRUE),
                  date_score_sd = zoo::rollapply(.data$date_score, FUN="sd",
                                                   width=window, align="right",
                                                   partial = TRUE))
}

#' @name coastal_indicator
#'
#' @title Generate summaries of coastal nesting indicator data
#'
#' @description Create a table of rolling averages for the proportion of coastal nesting,
#' by year
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @inheritParams load_indicator_data
#'
#' @return a data.frame
#'
#' @export
#'
coastal_indicator <- function(path = get_default_data_path(),
                                minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                                window = 3,
                                download_if_missing = TRUE)
{

  load_datafile(
    "Indicators/coastal_nesting.csv",
    path = path,
    download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(proportion_mean = zoo::rollapply(.data$proportion, FUN="mean",
                                                   width=window, align="right",
                                                   partial = TRUE),
                  proportion_sd = zoo::rollapply(.data$proportion, FUN="sd",
                                                   width=window, align="right",
                                                   partial = TRUE))
}

#' @name foraging_indicator
#'
#' @title Generate summaries of tactile/visual foraging indicator data
#'
#' @description Create a table of rolling averages for the proportion of tactile/visual foragers,
#' by year
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @inheritParams load_indicator_data
#'
#' @return a data.frame
#'
#' @export
#'
foraging_indicator <- function(path = get_default_data_path(),
                              minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                              window = 3,
                              download_if_missing = TRUE)
{

  load_datafile(
    "Indicators/max_count_all.csv",
    path = path,
    download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear),
                  .data$species %in% c("wost","whib","greg")) %>%
    dplyr::select(-"region") %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise(proportion =
                    (.data$count[.data$species=="wost"]+.data$count[.data$species=="whib"])/.data$count[.data$species=="greg"]) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(proportion_mean = zoo::rollapply(.data$proportion, FUN="mean",
                                                   width=window, align="right",
                                                   partial = TRUE),
                  proportion_sd = zoo::rollapply(.data$proportion, FUN="sd",
                                                   width=window, align="right",
                                                   partial = TRUE))
}

#' @name supercolony_indicator
#'
#' @title Generate summaries of ibis supercolony indicator data
#'
#' @description Create a table of rolling averages for the interval between ibis supercolony
#' events, by year
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @inheritParams load_indicator_data
#'
#' @return a data.frame
#'
#' @export
#'
supercolony_indicator <- function(path = get_default_data_path(),
                               minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                               window = 3,
                               download_if_missing = TRUE)
{

  load_datafile(
    "Indicators/supercolony_interval.csv",
    path = path,
    download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::mutate(interval_mean = zoo::rollapply(.data$ibis_interval, FUN="mean",
                                                   width=window, align="right",
                                                   partial = TRUE),
                  interval_sd = zoo::rollapply(.data$ibis_interval, FUN="sd",
                                                 width=window, align="right",
                                                 partial = TRUE))
}

#' @name plot_foraging
#'
#' @title Plot tactile/visual foraging indicator data
#'
#' @description Create a table of rolling averages for the proportion of tactile/visual foragers,
#' by year, and plot with target value
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @inheritParams load_indicator_data
#'
#' @return a data.frame
#'
#' @export
#'
plot_foraging <- function(path = get_default_data_path(),
                               minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                               window = 3,
                               download_if_missing = TRUE)
{
  ylab <- ifelse(window > 1, "mean tactile/visual", "tactile/visual")
  foraging_indicator(path = path,
                     window = window,
                     download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%

    ggplot2::ggplot(ggplot2::aes(.data$year, .data$proportion_mean), color="black") +
    ggplot2::geom_hline(yintercept=32, linetype=2, color="black", linewidth=.5) +
                  ggplot2::geom_point(alpha=2, size=3) +
                  ggplot2::theme_bw() +
                  ggplot2::ylab(ylab)
  }

#' @name plot_coastal
#'
#' @title Plot coastal indicator data
#'
#' @description Create a table of rolling averages for the proportion of coastal nesters,
#' by year, and plot with target value
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @inheritParams load_indicator_data
#'
#' @return a data.frame
#'
#' @export
#'
plot_coastal <- function(path = get_default_data_path(),
                          minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                          window = 3,
                          download_if_missing = TRUE)
{
  ylab <- ifelse(window > 1, "Mean proportion nests in coastal colonies",
                 "Proportion nests in coastal colonies")
  coastal_indicator(path = path,
                     window = window,
                     download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%

    ggplot2::ggplot(ggplot2::aes(.data$year, .data$proportion_mean), color="black") +
    ggplot2::geom_hline(yintercept=.5, linetype=2, color="black", linewidth=.5) +
    ggplot2::geom_point(alpha=2, size=3) +
    ggplot2::theme_bw() +
    ggplot2::ylab(ylab)
}

#' @name plot_initiation
#'
#' @title Plot wood stork nest initiation data
#'
#' @description Create a table of rolling averages for the earliest stork nesting dates,
#' by year, and plot with target value. Initiation in November is a 5, initiation in March is a 1.
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @inheritParams load_indicator_data
#'
#' @return a data.frame
#'
#' @export
#'
plot_initiation <- function(path = get_default_data_path(),
                         minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                         window = 3,
                         download_if_missing = TRUE)
{
  ylab <- ifelse(window > 1, "Mean Initiation Date", "Initiation Date")
  load_datafile(
    "Indicators/stork_initiation.csv",
    path = path,
    download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%

    ggplot2::ggplot(ggplot2::aes(.data$year, .data$date_score), color="black") +
    ggplot2::geom_hline(yintercept=4.5, linetype=2, color="black", linewidth=.5) +
    ggplot2::geom_point(alpha=2, size=3, shape=15) +
    ggplot2::scale_y_reverse(limits=c(4,1), breaks=c(4,3,2,1),labels=c("December","January","February","March")) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank()) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(ylab)

}

#' @name plot_supercolony
#'
#' @title Plot supercolony intervals
#'
#' @description Create a table of rolling averages for supercolony events,
#' by year, and plot with target value.
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @inheritParams load_indicator_data
#'
#' @return a data.frame
#'
#' @export
#'
plot_supercolony <- function(path = get_default_data_path(),
                            minyear = 2001, maxyear = as.integer(format(Sys.Date(), "%Y")),
                            window = 3,
                            download_if_missing = TRUE)
{
  ylab <- ifelse(window > 1, "Ibis supercolony mean interval", "Ibis supercolony interval")
  supercolony_indicator(path = path,
                      window = window,
                      download_if_missing = download_if_missing) %>%

    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%

    ggplot2::ggplot(ggplot2::aes(.data$year, .data$interval_mean), color="black") +
    ggplot2::geom_hline(yintercept=1.6, linetype=2, color="black", linewidth=.5) +
    ggplot2::geom_point(alpha=2, size=3) +
    ggplot2::theme_bw() +
    ggplot2::ylab(ylab)
}

#' @name max_count_plot
#'
#' @title Generate summaries of max count data for target species and plot
#'
#' @description Create a table of rolling averages for the max count indicator data,
#' by year and region
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @inheritParams load_indicator_data
#'
#' @return a data.frame
#'
#' @export
#'
max_count_plot <- function(path = get_default_data_path(),
                                minyear = 1980, maxyear = as.integer(format(Sys.Date(), "%Y")),
                                window = 3,
                                download_if_missing = TRUE)
{
  ylab <- ifelse(window > 1, "Number of nesting pairs (running ave)", "Number of nesting pairs")

  load_datafile(
    "Indicators/max_count_all.csv",
    path = path,
    download_if_missing = download_if_missing) %>%
    dplyr::filter(.data$species %in% c("greg","sneg","whib","wost")) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::arrange(.data$species, .data$year) %>%
    dplyr::group_by(.data$species) %>%
    dplyr::mutate(                             # rolling average
      count_mean = slider::slide_index_dbl(
        .x = .data$count,
        .i = .data$year,
        .f = mean,
        .before = window-1
      )
    ) %>%

    ggplot2::ggplot(ggplot2::aes(x=.data$year, y=.data$count_mean, group=.data$species, color=.data$species)) +
    ggplot2::geom_line(linewidth=1.05) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::ylab(ylab)

    # for report table
    # dplyr::select(-count) %>%
    # tidyr::pivot_wider(names_from = species, values_from = count_mean) %>%
    # dplyr::mutate(dplyr::across(2:5, round, 0))
}
