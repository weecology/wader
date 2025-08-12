# wader
[![R-CMD-check](https://github.com/weecology/wader/workflows/R-CMD-check/badge.svg)](https://github.com/weecology/wader/actions)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/weecology/wader/main/LICENSE)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1429290.svg)](https://doi.org/10.5281/zenodo.16814009)
[![NSF-1929730](https://img.shields.io/badge/NSF-2326954-blue.svg)](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2326954)
## Overview

The **wader** package provides functions to retrieve and
summarize the Everglades Wading Bird Project data. The data begin in
1986 (though earlier data are available from other sources) and are 
continuously updated today.

## Installation

You can install wader from github with:

    # install.packages("remotes")
    remotes::install_github("weecology/wader")

## Examples

1.  Download data repo:

<!-- -->
    download_observations(".")

2.  Load a data table

<!-- -->
    maxcounts <- load_datafile("Indicators/max_count_all.csv", download_if_missing = TRUE)
    
3.  Plot indicator data

<!-- -->
    plot_foraging()
    plot_coastal()
    plot_initiation()
    plot_supercolony()


## More Information

#### [EvergladesWadingBird Data Repo](https://github.com/weecology/EvergladesWadingBird)

The data repo contains metadata and data collection methods.
