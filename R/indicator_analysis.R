#' @name foraging_analysis
#'
#' @title Produce status figures of tactile/visual foraging indicator data
#'
#' @description Analyze a table of rolling averages for the proportion of tactile/visual foragers,
#' by year, and plot with thresholds by year
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
foraging_analysis <- function(path = get_default_data_path(),
                                minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                                window = 3,
                                download_if_missing = TRUE)
{
  library(ggplot2)
  foraging <- foraging_indicator(minyear = 2004) %>%
    dplyr::mutate(period="historic") %>%
    dplyr::mutate(period = replace(period, year > 2016, "modern"))

  A <- ggplot(foraging, aes(x=proportion_mean, fill=period)) +
    geom_density(adjust = 1, alpha=0.5) +
    xlim(0,10) +
    labs(title="Average Foraging Ratio", x="tactile/visual", y = "density") +
    theme_classic() +
    theme(legend.position = c(.85,.9))

  B <- ggplot(foraging, aes(x=year, y=proportion_mean,
                         ymin=proportion_mean-proportion_sd, ymax=proportion_mean+proportion_sd)) +
    geom_line(color="black", show.legend = FALSE) +
    geom_ribbon(alpha=0.2, fill="grey", show.legend = FALSE) +
    geom_ribbon(alpha=0.5, aes(color=NULL, fill=period), show.legend = FALSE) +
    labs(x="Year", y = "tactile/visual") +
    theme_classic()

  cowplot::plot_grid(A, B, labels = NULL)
}

#' @name initiation_analysis
#'
#' @title Produce status figures of wood stork nesting intiation indicator data
#'
#' @description Analyze a table of rolling averages for the date of wood stork nest initiation,
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
initiation_analysis <- function(path = get_default_data_path(),
                                 minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                                 window = 3,
                                 download_if_missing = TRUE)
{
library(ggplot2)
initiation <- initiation_indicator(minyear = 2004) %>%
              dplyr::mutate(period="historic") %>%
              dplyr::mutate(period = replace(period, year > 2016, "modern"))

  A <- ggplot(initiation, aes(x=date_score_mean, fill=period)) +
  geom_density(adjust = 1, alpha=0.5) +
  scale_x_reverse(limits=c(5,-.2), breaks=c(4,3,2,1,0),labels=c("December","January","February","March","April")) +
  labs(title="Average Stork Nest Initiation Date", x="Date Score", y = "density") +
  theme_classic() +
  theme(legend.position=c(.9,.8))

  B <- ggplot(initiation, aes(x=year, y=date_score_mean,
                       ymin=date_score_mean-date_score_sd, ymax=date_score_mean+date_score_sd)) +
  geom_line(color="black") +
  geom_ribbon(alpha=0.2, fill="grey") +
  geom_ribbon(alpha=0.5, aes(color=NULL, fill=period)) +
  scale_y_reverse(limits=c(4.55,0), breaks=c(4,3,2,1,0),labels=c("December","January","February","March","April")) +
  labs(x="Year", y = "Date Score") +
  theme_classic() +
  theme(legend.position="none")

  cowplot::plot_grid(A, B, labels = NULL)
}

#' @name coastal_analysis
#'
#' @title Produce status figures of wood stork nesting intiation indicator data
#'
#' @description Analyze a table of rolling averages for the proportion of coastal nesters,
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
coastal_analysis <- function(path = get_default_data_path(),
                                minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                                window = 3,
                                download_if_missing = TRUE)
{
library(ggplot2)
  coastal <- coastal_indicator(minyear = 2004) %>%
    dplyr::mutate(period="historic") %>%
    dplyr::mutate(period = replace(period, year > 2016, "modern"))

  A <- ggplot(coastal, aes(x=proportion_mean, fill=period)) +
    geom_density(adjust = 1.5, alpha=0.5) +
    xlim(0,.5) +
    labs(title="Average proportion nests in coastal colonies", x="proportion coastal", y = "density") +
    theme_classic() +
    theme(legend.position=c(.9,.8))

  B <- ggplot(coastal, aes(x=year, y=proportion_mean,
                         ymin=proportion_mean-proportion_sd, ymax=proportion_mean+proportion_sd)) +
    geom_line(color="black") +
    geom_ribbon(alpha=0.2, fill="grey") +
    geom_ribbon(alpha=0.5, aes(color=NULL, fill=period)) +
    labs(x="Year", y = "proportion coastal") +
    theme_classic() +
    theme(legend.position="none")

  cowplot::plot_grid(A, B, labels = NULL)
}

#' @name supercolony_analysis
#'
#' @title Produce status figures of ibis supercolony indicator data
#'
#' @description Analyze a table of rolling averages for the interval between ibis supercolony
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
supercolony_analysis <- function(path = get_default_data_path(),
                             minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                             window = 3,
                             download_if_missing = TRUE)
{
library(ggplot2)
  supercolony <- supercolony_indicator(minyear = 2004) %>%
    dplyr::mutate(period="historic") %>%
    dplyr::mutate(period = replace(period, year > 2016, "modern"))

  A <- ggplot(supercolony, aes(x=interval_mean, fill=period)) +
    geom_density(adjust = 1, alpha=0.5) +
    xlim(0,3) +
    labs(title="Ibis supercolony mean interval", x="mean interval", y = "density") +
    theme_classic() +
    theme(legend.position=c(.9,.8))

  B <- ggplot(supercolony, aes(x=year, y=interval_mean,
                      ymin=interval_mean-interval_sd, ymax=interval_mean+interval_sd)) +
    geom_line(color="black") +
    geom_ribbon(alpha=0.2, fill="grey") +
    geom_ribbon(alpha=0.5, aes(color=NULL, fill=period)) +
    labs(x="Year", y = "mean interval") +
    theme_classic() +
    theme(legend.position="none")

  cowplot::plot_grid(A, B, labels = NULL)
}
