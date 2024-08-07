% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicator_analysis.R
\name{supercolony_analysis}
\alias{supercolony_analysis}
\title{Produce status figures of ibis supercolony indicator data}
\usage{
supercolony_analysis(
  path = get_default_data_path(),
  minyear = 1986,
  maxyear = as.integer(format(Sys.Date(), "\%Y")),
  window = 3,
  download_if_missing = TRUE
)
}
\arguments{
\item{path}{either the file path that contains the Wading Bird folder or
"repo", which then pulls data from the EvergladesWadingBird GitHub repository}

\item{minyear}{Earliest year to include}

\item{maxyear}{Most recent year to include}

\item{window}{number of years over which to create a rolling average}

\item{download_if_missing}{if the specified file path doesn't have the
EvergladesWadingBird folder, then download it}
}
\value{
a data.frame
}
\description{
Analyze a table of rolling averages for the interval between ibis supercolony
events, by year
}
