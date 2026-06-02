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
    dplyr::mutate(period="baseline",
                  period = replace(period, year > 2016, "current"),
                  ymin=pmax(proportion_mean-proportion_sd,0),
                  ymax=proportion_mean+proportion_sd)

  ggplot(foraging, aes(x=year, y=proportion_mean, ymin=ymin, ymax=ymax)) +
    geom_point(size=2.2) +
    geom_line(color="black", linewidth=1.1) +
    geom_ribbon(alpha=0.4) +
    geom_hline(yintercept=32, linetype=2, color="black", linewidth=1) +
    geom_point(aes(x=year, y=proportion),shape = 8, size=2.2) +
    ylim(0,35) +
    labs(x="Year", y = "tactile/visual") +
    theme_classic() +
    theme(legend.position="none") +
    theme(
      text = element_text(family = "AppleGothic", size = 18),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    guides(color = "none", fill = "none")
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
              dplyr::mutate(period="baseline",
                            period = replace(period, year > 2016, "current"),
                            ymin=pmax(date_score_mean-date_score_sd,0),
                            ymax=date_score_mean+date_score_sd)

  ggplot(initiation, aes(x=year, y=date_score_mean, ymin=ymin, ymax=ymax)) +
  geom_point(size=2.2) +
  geom_line(color="black", linewidth=1.1) +
  geom_ribbon(alpha=0.4) +
  geom_hline(yintercept=4.5, linetype=2, color="black", linewidth=1) +
  geom_point(aes(x=year, y=date_score),shape = 8, size=2.2) +
  scale_y_reverse(limits=c(5,-.2), breaks=c(4,3,2,1,0),labels=c("December","January","February","March","April")) +
  labs(x="Year", y = "Date Score") +
  theme_classic() +
  theme(legend.position="none") +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme(
      text = element_text(family = "AppleGothic", size = 18),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    guides(color = "none", fill = "none")
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
    dplyr::mutate(period="historic",
                  period = replace(period, year > 2016, "modern"),
                  ymin=pmax(proportion_mean-proportion_sd,0),
                  ymax=proportion_mean+proportion_sd)

  ggplot(coastal, aes(x=year, y=proportion_mean, ymin=ymin, ymax=ymax)) +
    geom_point(size=2.2) +
    geom_line(color="black", linewidth=1.1) +
    geom_ribbon(alpha=0.4) +
    geom_hline(yintercept=.5, linetype=2, color="black", linewidth=1) +
    geom_point(aes(x=year, y=proportion),shape = 8, size=2.2) +
    ylim(0,.6) +
    labs(x="Year", y = "proportion coastal") +
    theme_classic() +
    theme(legend.position="none") +
    guides(fill = guide_legend(byrow = TRUE)) +
    theme(
      text = element_text(family = "AppleGothic", size = 18),
      # ggside.panel.border = element_blank(),
      # ggside.panel.grid = element_blank(),
      # ggside.panel.background = element_blank(),
      # ggside.axis.text = element_blank(),
      # ggside.axis.ticks = element_blank(),
      # ggside.panel.scale = .2,
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    guides(color = "none", fill = "none")
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
    dplyr::mutate(period="historic",
                  period = replace(period, year > 2016, "modern"),
                  ymin=pmax(interval_mean-interval_sd,0),
                  ymax=interval_mean+interval_sd)

  ggplot(supercolony, aes(x=year, y=interval_mean, ymin=ymin, ymax=ymax)) +
    geom_point(size=2.2) +
    geom_line(color="black", linewidth=1.1) +
    geom_ribbon(alpha=0.4) +
    geom_hline(yintercept=1.6, linetype=2, color="black", linewidth=1) +
    geom_point(aes(x=year, y=ibis_interval),shape = 8, size=2.2) +
    ylim(0,3) +
    labs(x="Year", y = "mean interval") +
    theme_classic() +
    theme(legend.position="none") +
    guides(fill = guide_legend(byrow = TRUE)) +
    theme(
      text = element_text(family = "AppleGothic", size = 18),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    guides(color = "none", fill = "none")
}

# a=foraging_analysis()
# b=initiation_analysis()
# c=coastal_analysis()
# d=supercolony_analysis()
# cowplot::plot_grid(a,b,c,d, labels = NULL)
