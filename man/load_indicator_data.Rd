% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_data.R
\name{load_indicator_data}
\alias{load_indicator_data}
\title{Read in the Wading Bird data files}
\usage{
load_indicator_data(
  path = get_default_data_path(),
  download_if_missing = TRUE,
  quiet = FALSE
)
}
\arguments{
\item{path}{either the file path that contains the Wading Bird folder or
"repo", which then pulls data from the EvergladesWadingBird GitHub repository}

\item{download_if_missing}{if the specified file path doesn't have the
EvergladesWadingBird folder, then download it}

\item{quiet}{logical, whether to run without version messages}
}
\value{
\code{\link{load_indicator_data}} returns a list of 5 dataframes:
\tabular{ll}{
\code{coastal_data} \tab coastal nesting prevalence\cr
\code{max_count} \tab regional max count by species\cr
\code{max_count_all} \tab total max count by species\cr
\code{stork_initiation} \tab date of earliest stork nesting\cr
\code{species_table} \tab species names and attributes\cr
}
}
\description{
Loads Wading Bird data files from either a user-defined
path or the online Github repository. If the user-defined path is un-
available, the default option is to download to that location.

\code{\link{load_indicator_data}} loads the indicator data files
}
