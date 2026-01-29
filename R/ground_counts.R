#' @name ground_counts
#'
#' @title Generate ground count summaries
#'
#' @description Generate summaries of ground transect count at waypoint or region level
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param level level at which to summarize (point, region)
#' @inheritParams load_datafile
#'
#' @return a data.frame
#'
#' @export
#'
ground_counts <- function(path = get_default_data_path(),
                          minyear = 1996, maxyear = as.numeric(format(Sys.Date(), "%Y")),
                          level = "region")
{
  level <- tolower(level)
  options(dplyr.summarise.inform = FALSE)
  minyear <- as.integer(minyear); maxyear <- as.integer(maxyear)

    groundcounts <- load_datafile("Counts/ground_counts.csv", path=path) %>%
      dplyr::filter(.data$standard_survey==1,
                    .data$year>=minyear,
                    .data$year<=maxyear) %>%
      dplyr::select("year","date","latitude","longitude","species","count","nests","chicks")

    if (level=="region") {
      groundcounts <- groundcounts %>%
        dplyr::group_by(.data$year,.data$species) %>%
        dplyr::summarise(total=sum(.data$count, na.rm=TRUE)) %>%
        dplyr::ungroup()

      groundcounts_old <- load_datafile("Counts/groundcounts_table.csv",path=path) %>%
        dplyr::filter(!(.data$year %in% groundcounts$year)) %>%
        tidyr::pivot_longer(cols = -year , names_to = "species", values_to = "total")

      groundcounts <- rbind(groundcounts_old,groundcounts) %>%
        dplyr::filter(.data$year>=minyear,
                      .data$year<=maxyear) %>%
        dplyr::arrange(.data$year,.data$species)

    }
    return(groundcounts)
}
