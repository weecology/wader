#' @name max_counts
#'
#' @title Generate max count summaries
#'
#' @description Generate summaries of max count at colony, subregion or region level
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @param level level at which to summarize (colony, subregion, region, all)
#' @inheritParams load_datafile
#'
#' @return a data.frame
#'
#' @export
#'
max_counts <- function(path = get_default_data_path(),
                                minyear = 1986, maxyear = format(Sys.Date(), "%Y"),
                                level = "colony")
{
 level <- tolower(level)
 options(dplyr.summarise.inform = FALSE)

   colonies <- load_datafile("Counts/maxcounts.csv") %>%
     dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
     dplyr::group_by(.data$year, .data$colony, .data$region, .data$species) %>%
     dplyr::summarise(count = max(.data$count)) %>%
     dplyr::select("year", "region", "colony", "species", "count") %>%
     dplyr::arrange(.data$colony, .data$species, .data$year)

   regions <- load_datafile("Indicators/max_count.csv") %>%
     dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
     dplyr::mutate(region = replace(.data$region, .data$region=="loxahatchee", "wca1")) %>%
     dplyr::arrange(.data$species, .data$year)

  if(level == "colony") {out <- colonies}

  if(level == "subregion") {
    out <- colonies %>%
      dplyr::group_by(.data$year, .data$region, .data$species) %>%
      dplyr::summarise(count = sum(.data$count)) %>%
      dplyr::filter(region %in% c("2a","2b", "3an", "3as", "3ase", "3b")) %>%
      dplyr::bind_rows(regions) %>%
      dplyr::filter(region %in% c("wca1", "2a","2b", "3an", "3as", "3ase", "3b", "enp")) %>%
      dplyr::select("year", "region", "species", "count") %>%
      dplyr::arrange(.data$region, .data$species, .data$year)
  }

  if(level == "region") {
    out <- regions %>%
      dplyr::filter(region %in% c("wcas", "enp")) %>%
      dplyr::arrange(.data$species, .data$region, .data$year)
    }

  if(level == "all") {
    out <- load_datafile("Indicators/max_count_all.csv") %>%
      dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
      dplyr::select(-"region") %>%
      dplyr::arrange(.data$species, .data$year)
  }

return(out)
}
