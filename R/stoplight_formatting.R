#' @name stoplight_foraging
#'
#' @title Plot tactile/visual foraging indicator data with stoplight colors and thresholds
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
stoplight_foraging <- function(path = get_default_data_path(),
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

#' @name stoplight_coastal
#'
#' @title Plot coastal indicator data with stoplight colors and thresholds
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
stoplight_coastal <- function(path = get_default_data_path(),
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

#' @name stoplight_initiation
#'
#' @title Plot wood stork nest initiation data with stoplight colors and thresholds
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
stoplight_initiation <- function(path = get_default_data_path(),
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
                                           dplyr::between(.data$date_score,1.5,4.5) ~ "orange",
                                           .data$date_score>4.5 ~ "darkgreen")) %>%

    ggplot2::ggplot(ggplot2::aes(.data$year, .data$date_score, color=.data$color)) +
    ggplot2::geom_hline(yintercept=4.5, linetype=2, color="darkgreen", linewidth=.5) +
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

#' @name stoplight_supercolony
#'
#' @title Plot supercolony intervals with stoplight colors and thresholds
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
stoplight_supercolony <- function(path = get_default_data_path(),
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
