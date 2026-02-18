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
                                minyear = 1986, maxyear = as.numeric(format(Sys.Date(), "%Y")),
                                level = "colony")
{
 level <- tolower(level)
 options(dplyr.summarise.inform = FALSE)
 minyear <- as.integer(minyear); maxyear <- as.integer(maxyear)

 colony_table <- load_datafile("SiteandMethods/colonies.csv", path = path) %>%
   dplyr::select(-c("latitude","longitude"))

   colonies <- load_datafile("Counts/maxcounts.csv", path = path) %>%
     dplyr::filter(dplyr::between(.data$year, minyear, maxyear),
                   !(.data$notes %in% c("presence", "present and nesting but numbers unknown"))) %>%
     dplyr::left_join(colony_table, by = dplyr::join_by(colony)) %>%
     dplyr::group_by(.data$year, .data$region, .data$subregion, .data$colony, .data$species) %>%
     dplyr::summarise(count = max(.data$count)) %>%
     dplyr::select("year", "region", "subregion","colony", "species", "count") %>%
     dplyr::arrange(.data$year, .data$colony, .data$species) %>%
     dplyr::ungroup()

   under40 <- load_datafile("Counts/maxcounts_under40.csv", path = path) %>%
     dplyr::filter(dplyr::between(.data$year, minyear, maxyear),
                   !(.data$notes %in% c("presence", "present and nesting but numbers unknown"))) %>%
     dplyr::group_by(.data$year, .data$region, .data$subregion, .data$species) %>%
     dplyr::summarise(count = max(.data$count)) %>%
     dplyr::select("year", "region", "subregion", "species", "count") %>%
     dplyr::arrange(.data$year, .data$region, .data$subregion, .data$species) %>%
     dplyr::ungroup()

  if(level == "colony") {out <- colonies}

  if(level == "subregion") {
    out <- colonies %>%
      dplyr::select(-"colony") %>%
      dplyr::bind_rows(under40) %>%
      dplyr::group_by(.data$year, .data$region, .data$subregion, .data$species) %>%
      dplyr::summarise(count = sum(.data$count)) %>%
      dplyr::select("year", "region", "subregion","species", "count") %>%
      dplyr::arrange(.data$year, .data$region, .data$subregion, .data$species) %>%
      dplyr::ungroup()
  }

  if(level == "region") {
    out <- colonies %>%
      dplyr::bind_rows(under40) %>%
      dplyr::group_by(.data$year, .data$region, .data$species) %>%
      dplyr::summarise(count = sum(.data$count)) %>%
      dplyr::select("year", "region", "species", "count")

    regions <- load_datafile("Indicators/max_count.csv", path = path) %>%
      dplyr::filter(dplyr::between(.data$year, minyear, maxyear)) %>%
      dplyr::mutate(region = replace(.data$region, .data$region=="loxahatchee", "1")) %>%
      dplyr::filter(.data$region %in% c("1", "enp")) %>%
      dplyr::anti_join(out, by = dplyr::join_by(year,region,species))

    out <- out %>%
      dplyr::bind_rows(regions) %>%
      dplyr::arrange(.data$year, .data$region, .data$species) %>%
      dplyr::ungroup()
    }

  if(level == "all") {
    out <- colonies %>%
      dplyr::bind_rows(under40) %>%
      dplyr::group_by(.data$year, .data$species) %>%
      dplyr::summarise(count = sum(.data$count)) %>%
      dplyr::select("year", "species", "count")

    all <- load_datafile("Indicators/max_count_all.csv", path = path) %>%
      dplyr::filter(dplyr::between(.data$year, minyear, maxyear),
                    !(species %in% c("cerp","total"))) %>%
      dplyr::select(-"region") %>%
      dplyr::anti_join(out, by = dplyr::join_by(year,species))

    out <- out %>%
      dplyr::bind_rows(all) %>%
      dplyr::arrange(.data$year, .data$species) %>%
      dplyr::ungroup()
  }

return(out)
}
