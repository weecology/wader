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
requireNamespace(ggplot2)
#  requireNamespace(ggside)
  foraging <- foraging_indicator(minyear = 2004) %>%
    dplyr::mutate(period="baseline") %>%
    dplyr::mutate(period = replace(period, year > 2016, "current"))

  ggplot(foraging, aes(x=year, y=proportion_mean,
                       ymin=proportion_mean-proportion_sd, ymax=proportion_mean+proportion_sd)) +
    geom_point(aes(color=period), size=2.2) +
    geom_line(color="black", linewidth=1.1) +
    geom_ribbon(alpha=0.2, fill="grey") +
    geom_ribbon(alpha=0.5, aes(color=NULL, fill=period)) +
    geom_hline(yintercept=32, linetype=2, color="black", linewidth=1) +
    ylim(0,35) +
    labs(x="Year", y = "tactile/visual") +
    theme_classic() +
    theme(legend.position="none") +
    guides(fill = guide_legend(byrow = TRUE)) +
    # geom_ysidedensity(aes(fill=period), alpha = .5) +
    # geom_ysidehline(yintercept = 32, linetype=2, color="darkgreen", linewidth=1) +
    scale_color_grey() +
    scale_fill_grey() +
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
# library(ggside)
initiation <- initiation_indicator(minyear = 2004) %>%
              dplyr::mutate(period="baseline") %>%
              dplyr::mutate(period = replace(period, year > 2016, "current"))

  ggplot(initiation, aes(x=year, y=date_score_mean,
                       ymin=date_score_mean-date_score_sd, ymax=date_score_mean+date_score_sd)) +
  geom_point(aes(color=period), size=2.2) +
  geom_line(color="black", linewidth=1.1) +
  geom_ribbon(alpha=0.2, fill="grey") +
  geom_ribbon(alpha=0.5, aes(color=NULL, fill=period)) +
  geom_hline(yintercept=4.5, linetype=2, color="black", linewidth=1) +
  scale_y_reverse(limits=c(5,-.2), breaks=c(4,3,2,1,0),labels=c("December","January","February","March","April")) +
  labs(x="Year", y = "Date Score") +
  theme_classic() +
  theme(legend.position="none") +
  guides(fill = guide_legend(byrow = TRUE)) +
  # geom_ysidedensity(aes(fill=period), alpha = .5) +
  # geom_ysidehline(yintercept = 4.5, linetype=2, color="darkgreen", linewidth=1) +
  scale_color_grey() +
  scale_fill_grey() +
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
# library(ggside)
  coastal <- coastal_indicator(minyear = 2004) %>%
    dplyr::mutate(period="historic") %>%
    dplyr::mutate(period = replace(period, year > 2016, "modern"))

  ggplot(coastal, aes(x=year, y=proportion_mean,
                      ymin=proportion_mean-proportion_sd, ymax=proportion_mean+proportion_sd)) +
    geom_point(aes(color=period), size=2.2) +
    geom_line(color="black", linewidth=1.1) +
    geom_ribbon(alpha=0.2, fill="grey") +
    geom_ribbon(alpha=0.5, aes(color=NULL, fill=period)) +
    geom_hline(yintercept=.5, linetype=2, color="black", linewidth=1) +
    ylim(0,.6) +
    labs(x="Year", y = "proportion coastal") +
    theme_classic() +
    theme(legend.position="none") +
    guides(fill = guide_legend(byrow = TRUE)) +
    # geom_ysidedensity(aes(fill=period), alpha = .5) +
    # geom_ysidehline(yintercept = .5, linetype=2, color="darkgreen", linewidth=1) +
    scale_color_grey() +
    scale_fill_grey() +
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
# library(ggside)
  supercolony <- supercolony_indicator(minyear = 2004) %>%
    dplyr::mutate(period="historic") %>%
    dplyr::mutate(period = replace(period, year > 2016, "modern"))

  ggplot(supercolony, aes(x=year, y=interval_mean,
                          ymin=interval_mean-interval_sd, ymax=interval_mean+interval_sd)) +
    geom_point(aes(color=period), size=2.2) +
    geom_line(color="black", linewidth=1.1) +
    geom_ribbon(alpha=0.2, fill="grey") +
    geom_ribbon(alpha=0.5, aes(color=NULL, fill=period)) +
    geom_hline(yintercept=1.6, linetype=2, color="black", linewidth=1) +
    ylim(0,3) +
    labs(x="Year", y = "mean interval") +
    theme_classic() +
    theme(legend.position="none") +
    guides(fill = guide_legend(byrow = TRUE)) +
    # geom_ysidedensity(aes(fill=period), alpha = .5) +
    # geom_ysidehline(yintercept = 1.6, linetype=2, color="darkgreen", linewidth=1) +
    scale_color_grey() +
    scale_fill_grey() +
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

# a=foraging_analysis()
# b=initiation_analysis()
# c=coastal_analysis()
# d=supercolony_analysis()
# cowplot::plot_grid(a,b,c,d, labels = NULL)
