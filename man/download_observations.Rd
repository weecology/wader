% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_data.R
\name{download_observations}
\alias{download_observations}
\title{Download the EvergladesWadingBird repo}
\usage{
download_observations(
  path = get_default_data_path(),
  version = "latest",
  from_zenodo = FALSE,
  quiet = FALSE,
  verbose = FALSE,
  timeout = getOption("timeout")
)
}
\arguments{
\item{path}{Folder into which data will be downloaded}

\item{version}{Version of the data to download (default = "latest").
If \code{NULL}, returns.}

\item{from_zenodo}{logical; if \code{TRUE}, get info from Zenodo, otherwise GitHub}

\item{quiet}{logical, whether to download data silently.}

\item{verbose}{logical, whether to provide details of downloading.}

\item{timeout}{Positive \code{integer} or integer \code{numeric} seconds for timeout on downloads. Temporarily overrides the \code{"timeout"} option in \code{\link[base]{options}}.}
}
\value{
NULL invisibly.
}
\description{
Downloads specified version of the EvergladesWadingBird data.
}
