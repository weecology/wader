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
#' @return a data.frame
#'
#' @export
#'
max_count_indicator <- function(path = get_default_data_path(),
                                  minyear = 1986, maxyear = 2021,
                                  window = 3,
                                  download_if_missing = TRUE)
{

  load_datafile("Indicators/max_count_all.csv",
                             download_if_missing = download_if_missing) %>%
               dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
               dplyr::select(-.data$region) %>%
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
#' @return a data.frame
#'
#' @export
#'
initiation_indicator <- function(path = get_default_data_path(),
                                minyear = 1986, maxyear = 2021,
                                window = 3,
                                download_if_missing = TRUE)
{

  load_datafile("Indicators/stork_initiation.csv",
                download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(date_score_mean = zoo::rollapply(.data$date_score, FUN="mean",
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
#' @return a data.frame
#'
#' @export
#'
coastal_indicator <- function(path = get_default_data_path(),
                                minyear = 1986, maxyear = 2021,
                                window = 3,
                                download_if_missing = TRUE)
{

  load_datafile("Indicators/coastal_nesting.csv",
                download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(proportion_mean = zoo::rollapply(.data$proportion, FUN="mean",
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
#' @return a data.frame
#'
#' @export
#'
foraging_indicator <- function(path = get_default_data_path(),
                              minyear = 1986, maxyear = 2021,
                              window = 3,
                              download_if_missing = TRUE)
{

  load_datafile("Indicators/max_count_all.csv",
                download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear),
                  .data$species %in% c("wost","whib","greg")) %>%
    dplyr::select(-.data$region) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise(proportion =
                    (.data$count[.data$species=="wost"]+.data$count[.data$species=="whib"])/.data$count[.data$species=="greg"]) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$year) %>%
    dplyr::mutate(proportion_mean = zoo::rollapply(.data$proportion, FUN="mean",
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
#' @return a data.frame
#'
#' @export
#'
supercolony_indicator <- function(path = get_default_data_path(),
                               minyear = 1931, maxyear = 2021,
                               window = 3,
                               download_if_missing = TRUE)
{

  load_datafile("Indicators/supercolony_interval.csv",
                download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::mutate(interval_mean = zoo::rollapply(.data$ibis_interval, FUN="mean",
                                                   width=window, align="right",
                                                   partial = TRUE))
}

#' @name plot_foraging
#'
#' @title Plot tactile/visual foraging indicator data
#'
#' @description Create a table of rolling averages for the proportion of tactile/visual foragers,
#' by year, and plot with thresholds
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @return a data.frame
#'
#' @export
#'
plot_foraging <- function(path = get_default_data_path(),
                               minyear = 1986, maxyear = 2021,
                               window = 3,
                               download_if_missing = TRUE)
{

  foraging_indicator(path = path,
                     minyear = minyear, maxyear = maxyear,
                     window = window,
                     download_if_missing = download_if_missing) %>%
    dplyr::mutate(color = dplyr::case_when(.data$proportion_mean<=32 ~ "black",
                                           .data$proportion_mean>32 ~ "darkgrey")) %>%

  ggplot2::ggplot(ggplot2::aes(year, proportion_mean, color=color)) +
                  ggplot2::geom_point(alpha=2, size=3) +
                  ggplot2::scale_colour_identity() +
                  ggplot2::theme_bw() +
                  ggplot2::ylab("tactile/visual") +
                  ggplot2::geom_hline(yintercept=30, linetype=2, color="black", size=.5)
  }

#' @name plot_coastal
#'
#' @title Plot coastal indicator data
#'
#' @description Create a table of rolling averages for the proportion of coastal nesters,
#' by year, and plot with thresholds
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @return a data.frame
#'
#' @export
#'
plot_coastal <- function(path = get_default_data_path(),
                          minyear = 1986, maxyear = 2021,
                          window = 3,
                          download_if_missing = TRUE)
{
  coastal_indicator(path = path,
                     minyear = minyear, maxyear = maxyear,
                     window = window,
                     download_if_missing = download_if_missing) %>%
    dplyr::mutate(color = dplyr::case_when(.data$proportion_mean<=.5 ~ "black",
                                           .data$proportion_mean>.5 ~ "darkgrey")) %>%

    ggplot2::ggplot(ggplot2::aes(year, proportion_mean, color=color)) +
    ggplot2::geom_point(alpha=2, size=3) +
    ggplot2::scale_colour_identity() +
    ggplot2::theme_bw() +
    ggplot2::ylab("Average proportion nests in coastal colonies") +
    ggplot2::geom_hline(yintercept=.5, linetype=2, color="black", size=.5)
}

#' @name plot_initiation
#'
#' @title Plot wood stork nest initiation data
#'
#' @description Create a table of rolling averages for the earliest stork nesting dates,
#' by year, and plot with thresholds
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @return a data.frame
#'
#' @export
#'
plot_initiation <- function(path = get_default_data_path(),
                         minyear = 1986, maxyear = 2021,
                         window = 3,
                         download_if_missing = TRUE)
{
  initiation_indicator(path = path,
                    minyear = minyear, maxyear = maxyear,
                    window = window,
                    download_if_missing = download_if_missing) %>%
    dplyr::mutate(color = dplyr::case_when(.data$date_score_mean<=4.5 ~ "black",
                                           .data$date_score_mean>4.5 ~ "darkgrey")) %>%

    ggplot2::ggplot(ggplot2::aes(year, date_score_mean, color=color)) +
    ggplot2::geom_point(alpha=2, size=3) +
    ggplot2::scale_colour_identity() +
    ggplot2::theme_bw() +
    ggplot2::ylab("Wood stork nesting date score") +
    ggplot2::geom_hline(yintercept=4.5, linetype=2, color="black", size=.5)
}

#' @name plot_supercolony
#'
#' @title Plot supercolony intervals
#'
#' @description Create a table of rolling averages for supercolony events,
#' by year, and plot with thresholds
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param window number of years over which to create a rolling average
#'
#' @return a data.frame
#'
#' @export
#'
plot_supercolony <- function(path = get_default_data_path(),
                            minyear = 1986, maxyear = 2021,
                            window = 3,
                            download_if_missing = TRUE)
{
  supercolony_indicator(path = path,
                      minyear = minyear, maxyear = maxyear,
                      window = window,
                      download_if_missing = download_if_missing) %>%
    dplyr::mutate(color = dplyr::case_when(.data$interval_mean>1.6 ~ "black",
                                           .data$interval_mean<=1.6 ~ "darkgrey")) %>%

    ggplot2::ggplot(ggplot2::aes(year, interval_mean, color=color)) +
    ggplot2::geom_point(alpha=2, size=3) +
    ggplot2::scale_colour_identity() +
    ggplot2::theme_bw() +
    ggplot2::ylab("Ibis supercolony mean interval") +
    ggplot2::geom_hline(yintercept=1.6, linetype=2, color="black", size=.5)
}

#' @name max_counts
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
#' @return a data.frame
#'
#' @export
#'
max_counts <- function(path = get_default_data_path(),
                                minyear = 1980, maxyear = 2021,
                                window = 3,
                                download_if_missing = TRUE)
{

  load_datafile("Indicators/max_count_all.csv",
                download_if_missing = download_if_missing) %>%
    dplyr::filter(.data$species %in% c("greg","sneg","whib","wost")) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::select(-.data$region) %>%
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

    ggplot2::ggplot(ggplot2::aes(x=year, y=count_mean, group=species, color=species)) +
    ggplot2::geom_line(size=1.05) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::ylab("Number of nesting pairs (3-yr running ave)")
}
