#' @title Creates summaries of the EvergladesWadingBird data
#'
#' @description This package is designed to be an interface to the Wading Bird data,
#' which resides online at \url{https://github.com/weecology/EvergladesWadingBird}.
#' It contains a set of functions to download, clean, and summarize the data.
#'
#' @name wader
#' @docType package
#' @keywords package
#'
#' @importFrom lubridate "%m+%"
#' @importFrom rlang "!!" "!!!" ":=" .data
#' @importFrom utils head download.file read.csv unzip read.table tail
#' @importFrom stats median na.omit
#' @importFrom httr content GET stop_for_status

NULL

## quiets concerns of R CMD check re: variables used in NSE functions
if (getRversion() >= "2.15.1") utils::globalVariables(
  c(".", "n")
  )
