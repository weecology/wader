#' @name load_indicator_data
#' @aliases 
#'
#' @title Read in the Wading Bird data files
#'
#' @description Loads Wading Bird data files from either a user-defined
#'   path or the online Github repository. If the user-defined path is un-
#'   available, the default option is to download to that location.
#'
#' @param path either the file path that contains the Wading Bird folder or
#'  "repo", which then pulls data from the EvergladesWadingBird GitHub repository
#' @param download_if_missing if the specified file path doesn't have the
#'   EvergladesWadingBird folder, then download it


#' @rdname load_indicator_data
#' @description \code{\link{load_indicator_data}} loads the indicator data files
#'
#' @param quiet logical, whether to run without version messages
#'
#' @return \code{\link{load_indicator_data}} returns a list of 5 dataframes:
#'   \tabular{ll}{
#'     \code{coastal_data} \tab coastal nesting prevalence\cr
#'     \code{max_count} \tab regional max count by species\cr
#'     \code{max_count_all} \tab total max count by species\cr
#'     \code{stork_initiation} \tab date of earliest stork nesting\cr
#'     \code{species_table} \tab species names and attributes\cr
#'   }
#'
#' @export
#'
load_indicator_data <- function(path = get_default_data_path(),
                             download_if_missing = TRUE, quiet = FALSE)
{
  coastal_data <- load_datafile(file.path("Indicators", "coastal_nesting.csv"),
                               na.strings = "", path, download_if_missing,
                               quiet = quiet)
  max_count <- load_datafile(file.path("Indicators", "max_count.csv"),
                                 na.strings = "", path, download_if_missing)
  max_count_all <- load_datafile(file.path("Indicators", "max_count_all.csv"),
                                  na.strings = "NA", path, download_if_missing)
  stork_initiation <- load_datafile(file.path("Indicators", "stork_initiation.csv"),
                                  na.strings = "NA", path, download_if_missing)
  species_table <- load_datafile(file.path("SiteandMethods", "species_list.csv"),
                               na.strings = "NA", path, download_if_missing)

  return(mget(c("coastal_data", "max_count", "max_count_all",
                "stork_initiation", "species_table")))
}


#' @name load_datafile
#'
#' @title read in a raw datafile from the downloaded data or the GitHub repo
#'
#' @description does checking for whether a particular datafile exists and then
#'   reads it in, using na_strings to determine what gets converted to NA. It
#'   can also download the dataset if it's missing locally.
#'
#' @param datafile the path to the datafile within the folder for Portal data
#' @param quiet logical, whether to perform operations silently
#' @inheritParams load_indicator_data
#' @inheritParams utils::read.table
#'
#' @export
load_datafile <- function(datafile, na.strings = "", path = get_default_data_path(),
                          download_if_missing = TRUE, quiet = TRUE)
{

  ## define file paths
  if (tolower(path) == "repo")
  {
    base_path <- "https://raw.githubusercontent.com/weecology/EvergladesWadingBird/main"
  } else {
    tryCatch(base_path <- file.path(normalizePath(path, mustWork = TRUE), "EvergladesWadingBird"),
             error = function(e) stop("Specified path ", path, "does not exist. Please create it first."),
             warning = function(w) w)
  }
  datafile <- file.path(base_path, datafile)

  ## check if files exist and download if appropriate
  if (tolower(path) != "repo" && !file.exists(datafile))
  {
    if (download_if_missing) {
      warning("Proceeding to download data into specified path", path, "\n")
      tryCatch(download_observations(path),
               error = function(e) e,
               warning = function(w) w)
    } else {
      stop("Data files were not found in specified path", path, "\n")
    }
  }

  ## output message about data version
  if (!quiet)
  {
    version_file <- file.path(base_path, "version.txt")
    if (tolower(path) != "repo" && !file.exists(version_file))
    {
      message("Loading in data version < 1.1.0")
    } else {
      message("Loading in data version ", read.table(version_file)[1, 1])
    }
  }

  ## read in the data table and return
  tryCatch(read.csv(datafile, na.strings = na.strings, stringsAsFactors = FALSE),
  error = function(e) e,
  warning = function(w) w)
}


