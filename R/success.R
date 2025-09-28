#' @name overall_success
#'
#' @title Generate summaries of nest success from data
#'
#' @description Create a table of overall nest success by species and year
#'
#' @param minyear Earliest year to include
#' @param maxyear Most recent year to include
#' @inheritParams load_datafile
#'
#' @return a data.frame
#'
#' @export

overall_success <- function(path = get_default_data_path(),
                            minyear = 1986, maxyear = as.integer(format(Sys.Date(), "%Y")),
                            download_if_missing = TRUE)
{
  load_datafile("Nesting/nest_success_summary.csv", path = path) %>%
    dplyr::group_by(year, species) %>%
    dplyr::mutate(species = toupper(species)) %>%
    dplyr::filter(year <= maxyear, year >= minyear) %>%
    dplyr::summarise(incubation_N=sum(incubation_k, na.rm=TRUE),
                     incubation_sumy=sum(incubation_sumy, na.rm=TRUE),
                     incubation_e=sum(incubation_e, na.rm=TRUE),
                     incubation_j=mean(incubation_j, na.rm=TRUE),
                     nestling_N=sum(nestling_k, na.rm=TRUE),
                     nestling_sumy=sum(nestling_sumy, na.rm=TRUE),
                     nestling_e=sum(nestling_e, na.rm=TRUE),
                     nestling_j=mean(nestling_j, na.rm=TRUE)) %>%
    dplyr::mutate(incubation_p = 1-((incubation_N-incubation_sumy)/incubation_e),
                  incubation_Success = incubation_p^incubation_j,
                  incubation_varp=(incubation_p*(1-incubation_p))/incubation_e,
                  incubation_var = incubation_varp*((incubation_j*(incubation_p^(incubation_j-1)))^2),
                  incubation_SD = sqrt(incubation_var),
                  nestling_p = 1-((nestling_N-nestling_sumy)/nestling_e),
                  nestling_Success = nestling_p^nestling_j,
                  nestling_varp=(nestling_p*(1-nestling_p))/nestling_e,
                  nestling_var = nestling_varp*((nestling_j*(nestling_p^(nestling_j-1)))^2),
                  nestling_SD = sqrt(nestling_var),
                  overall_Success = (incubation_p^incubation_j)*(nestling_p^nestling_j),
                  overall_var = ((incubation_Success^2)*nestling_var)+((nestling_Success^2)*incubation_var)+(incubation_var*nestling_var),
                  overall_SD = sqrt(overall_var)) %>%
    dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), ~ifelse(is.nan(.), NA, .))
}
