library(rvest)
library(lubridate)
library(dplyr)
library(ncdf4)
library(raster)

get_metadata <- function() {
  url <- "https://sflthredds.er.usgs.gov/thredds/catalog/eden/depths/catalog.html"
  metadata <- url %>%
    read_html() %>%
    html_table()
  metadata <- metadata[[1]] %>%
    filter(Dataset != "depths") %>% #Drop directory name from first row
    rename(dataset = Dataset, size = Size, last_modified = `Last Modified`) %>%
    mutate(last_modified = as.POSIXct(last_modified,
                             format = "%Y-%m-%dT%H:%M:%S"))
}

get_data_urls <- function(file_names) {
  base_url <- "https://sflthredds.er.usgs.gov/thredds/fileServer/eden/depths"
  urls <- file.path(base_url, file_names)
  return(list(file_names = file_names, urls = urls))
}

get_last_download <- function(eden_path, metadata, force_update = FALSE) {
  if ("last_download.csv" %in% list.files(eden_path) & !force_update) {
    last_download <- read.csv(file.path(eden_path, "last_download.csv"))
  } else {
    last_download <- data.frame(dataset = metadata$dataset, size = "0 Mbytes",
                       last_modified = as.POSIXct("1900-01-01 00:00:01",
                                         format = "%Y-%m-%d %H:%M:%S"))
  }
  return(last_download)
}

get_files_to_update <- function(eden_path, metadata, force_update = FALSE){
  last_download <- get_last_download(eden_path, metadata, force_update = force_update)
  to_update <- metadata %>%
    left_join(last_download, by = "dataset", suffix = c(".curr", ".last")) %>%
    filter(last_modified.curr > last_modified.last | size.curr != size.last | is.na(last_modified.last))
}

update_last_download <- function(eden_path, metadata){
  current_files <- list.files(eden_path, pattern = "*_depth.nc")
  if (identical(sort(current_files), sort(metadata$dataset))) {
    write.csv(metadata, file.path(eden_path, 'last_download.csv'))
  } else {
    warning("Some EDEN files that should have been downloaded are not\n",
            "present in the EDEN path so not updating last_download.csv")
  }
}

#' @name download_eden_depths
#'
#' @title Download the EDEN depths data
#'
#' @param eden_path path where the EDEN data should be stored
#' @param force_update if TRUE update all data files even if checks indicate
#'   that remote files are unchanged since the current local copies were
#'   created
#'
#' @return char vector of downloaded/updated files
#'
#' @export
#'
download_eden_depths <- function(eden_path, force_update = FALSE) {

  if (!dir.exists(eden_path)) {
    dir.create(eden_path, recursive = TRUE)
  }

  metadata <- get_metadata()
  to_update <- get_files_to_update(eden_path, metadata,
                 force_update = force_update)
  data_urls <- get_data_urls(to_update$dataset)
  options(timeout = 226)

  downloaded <- mapply(download.file,
    data_urls$urls,
    file.path(eden_path, data_urls$file_names))

  update_last_download(eden_path, metadata)

  return(file.path(eden_path, data_urls$file_names))
}

#' @name combine_eden_depths
#'
#' @title Combine the quarterly EDEN depths .nc files into a single .nc file
#'
#' @param eden_path path where the EDEN data should be stored
#'
#' @return char path of combined file
#'
#' @export
#'
combine_eden_depths <- function(eden_path) {
  file.remove(file.path(eden_path, "1991_q1_depth_out.nc"))
  system(paste("ncks", "--mk_rec_dmn", "time",
    file.path(eden_path, "1991_q1_depth.nc"),
    file.path(eden_path, "1991_q1_depth_out.nc")))
  file.remove(file.path(eden_path, "1991_q1_depth.nc"))
  file.rename(file.path(eden_path, "1991_q1_depth_out.nc"),
              file.path(eden_path, "1991_q1_depth.nc"))
  file.remove(file.path(eden_path, "eden_depth_combined.nc"))
  system(paste("ncrcat", file.path(eden_path, "*_depth.nc"),
    file.path(eden_path, "eden_depth_combined.nc")))
  return(file.path(eden_path, "eden_depth_combined.nc"))
}
