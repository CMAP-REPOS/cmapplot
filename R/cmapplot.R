#' cmapplot
#'
#' This package contains extra palettes, themes and geoms for \pkg{ggplot2}.
#'
#' Please see the help pages listed below:
#'
#' \itemize{
#'   \item \code{\link{geom_text_lastonly}}
#'   \item \code{\link{theme_cmap}}
#' }
#'
#' Please report issues and suggest improvements at Github:
#'
#' \url{https://github.com/CMAP-REPOS/cmapplot}
#'
#' @name cmapplot
#' @docType package
#' @import ggplot2 dplyr grid
#' @importFrom grDevices windowsFonts
#' @keywords internal
"_PACKAGE"

pkg.globals <- new.env(parent=emptyenv())  # An environment for storing any global variables

grDevices::windowsFonts(
  font_main = "TT Whitney Medium",
  font_note = "TT Whitney Book",
  font_title = "TT Whitney Semibold",
  font_label = "TT Whitney Semibold"
)
pkg.globals$font_main <- "font_main"
pkg.globals$font_note <- "font_note"
pkg.globals$font_title <- "font_title"
pkg.globals$font_label <- "font_label"
