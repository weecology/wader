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

  load_datafile("Indicators/max_count_all.csv",
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

  load_datafile("Indicators/stork_initiation.csv",
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

  load_datafile("Indicators/coastal_nesting.csv",
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

  load_datafile("Indicators/max_count_all.csv",
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

  load_datafile("Indicators/supercolony_interval.csv",
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
#' by year, and plot with thresholds
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

  foraging_indicator(path = path,
                     window = window,
                     download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::mutate(color = dplyr::case_when(.data$proportion_mean<1 ~ "red4",
                          dplyr::between(.data$proportion_mean,1,32) ~ "orange",
                                         .data$proportion_mean>32 ~ "darkgreen")) %>%

    ggplot2::ggplot(ggplot2::aes(.data$year, .data$proportion_mean, color=.data$color)) +
    ggplot2::geom_hline(yintercept=32, linetype=2, color="darkgreen", linewidth=.5) +
    ggplot2::geom_hline(yintercept=2, linetype=2, color="orange", linewidth=.5) +
    ggplot2::geom_hline(yintercept=1, linetype=2, color="red4", linewidth=.5) +
                  ggplot2::geom_point(alpha=2, size=3) +
                  ggplot2::scale_colour_identity() +
                  ggplot2::theme_bw() +
                  ggplot2::ylab("tactile/visual")
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
  coastal_indicator(path = path,
                     window = window,
                     download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::mutate(color = dplyr::case_when(.data$proportion_mean<.1 ~ "red4",
                                           dplyr::between(.data$proportion_mean,.1,.5) ~ "orange",
                                           .data$proportion_mean>.5 ~ "darkgreen")) %>%

    ggplot2::ggplot(ggplot2::aes(.data$year, .data$proportion_mean, color=.data$color)) +
    ggplot2::geom_hline(yintercept=.5, linetype=2, color="darkgreen", linewidth=.5) +
    ggplot2::geom_hline(yintercept=.25, linetype=2, color="orange", linewidth=.5) +
    ggplot2::geom_hline(yintercept=.1, linetype=2, color="red4", linewidth=.5) +
    ggplot2::geom_point(alpha=2, size=3) +
    ggplot2::scale_colour_identity() +
    ggplot2::theme_bw() +
    ggplot2::ylab("Average proportion nests in coastal colonies")
}

#' @name plot_initiation
#'
#' @title Plot wood stork nest initiation data
#'
#' @description Create a table of rolling averages for the earliest stork nesting dates,
#' by year, and plot with thresholds. Initiation in November is a 5, initiation in March is a 1.
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
  load_datafile("Indicators/stork_initiation.csv",
                download_if_missing = download_if_missing) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::arrange(.data$year) %>%
    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::mutate(color = dplyr::case_when(.data$date_score<1.5 ~ "red4",
                                           dplyr::between(.data$date_score,1.5,2.5) ~ "orange",
                                           .data$date_score>2.5 ~ "darkgreen")) %>%

    ggplot2::ggplot(ggplot2::aes(.data$year, .data$date_score, color=.data$color)) +
    ggplot2::geom_hline(yintercept=2.5, linetype=2, color="orange", linewidth=.5) +
    ggplot2::geom_hline(yintercept=1.5, linetype=2, color="red4", linewidth=.5) +
    ggplot2::geom_point(alpha=2, size=3, shape=15) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_y_reverse(limits=c(4,1), breaks=c(4,3,2,1),labels=c("December","January","February","March")) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank()) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Initiation Date")

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
  supercolony_indicator(path = path,
                      window = window,
                      download_if_missing = download_if_missing) %>%

    dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
    dplyr::mutate(color = dplyr::case_when(.data$interval_mean>5 ~ "red4",
                                           dplyr::between(.data$interval_mean,1.6,5) ~ "orange",
                                           .data$interval_mean<1.6 ~ "darkgreen")) %>%

    ggplot2::ggplot(ggplot2::aes(.data$year, .data$interval_mean, color=.data$color)) +
    ggplot2::geom_hline(yintercept=1.6, linetype=2, color="darkgreen", linewidth=.5) +
    ggplot2::geom_hline(yintercept=2.5, linetype=2, color="orange", linewidth=.5) +
    ggplot2::geom_hline(yintercept=5, linetype=2, color="red4", linewidth=.5) +
    ggplot2::geom_point(alpha=2, size=3) +
    ggplot2::scale_colour_identity() +
    ggplot2::theme_bw() +
    ggplot2::ylab("Ibis supercolony mean interval")
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

  load_datafile("Indicators/max_count_all.csv",
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
    ggplot2::ylab("Number of nesting pairs (3-yr running ave)")

    # for report table
    # dplyr::select(-count) %>%
    # tidyr::pivot_wider(names_from = species, values_from = count_mean) %>%
    # dplyr::mutate(dplyr::across(2:5, round, 0))
}
