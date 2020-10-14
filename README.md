
# cmapplot <img src="man/figures/logo.png" align="right" alt="" width="128" />

<!-- badges: start -->

![R build
status](https://github.com/CMAP-REPOS/cmapplot/workflows/R-CMD-check/badge.svg)
![pkgdown build
status](https://github.com/CMAP-REPOS/cmapplot/workflows/pkgdown/badge.svg)
<!-- badges: end -->

This R package provides themes and color scales for
[ggplot2](https://github.com/tidyverse/ggplot2), based on Chicago
Metropolitan Agency for Planning (CMAP) design guidelines.

## Installation

Run the following to install or update cmapplot:

``` r
## Install current version from GitHub
devtools::install_github("CMAP-REPOS/cmapplot", build_vignettes=TRUE)

## Then load the package as you would any other
library(cmapplot)
```

**Important note:** cmapplot works best when installed on a Windows
computer with the Whitney family of fonts installed (specifically the
Book, Medium, and Semibold variants). If installed on a Windows computer
without Whitney, the package will still work, but the fonts will default
to Calibri. If installed on macOS or Linux, the fonts will default to
Arial, even if Whitney is installed. Additionally, macOS users must
install [XQuartz](https://www.xquartz.org) before cmapplot can be
loaded. (This can be easily accomplished via the
[Homebrew](https://brew.sh) package manager with the command `brew cask
install xquartz`.)
