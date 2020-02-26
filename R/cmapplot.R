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
#' @import ggplot2 dplyr grid scales grDevices graphics
#' @importFrom lubridate date_decimal
#' @keywords internal
"_PACKAGE"

cmapplot_globals <- new.env(parent=emptyenv())  # An environment for storing any global variables

grDevices::windowsFonts(
  font_reg = "Whitney Medium",
  font_lite = "Whitney Book",
  font_sbold = "Whitney Semibold"
)

cmapplot_globals$font_main <- "font_reg"  # "medium" weight for in-body text and x/y axis
cmapplot_globals$font_note <- "font_lite"  # "book" weight for notes and sources
cmapplot_globals$font_title <- "font_sbold"  # "semibold" weight for title
cmapplot_globals$font_label <- "font_sbold"  # "semibold" weight also for labels

check_cmap_fonts <- function() {
  graphics::plot(c(0,2), c(0,5), type="n", xlab="", ylab="")
  graphics::par(family=cmapplot_globals$font_note)
  graphics::text(1, 4, "font_note", cex=4)
  graphics::par(family=cmapplot_globals$font_main)
  graphics::text(1, 3, "font_main", cex=4)
  graphics::par(family=cmapplot_globals$font_title)
  graphics::text(1, 2, "font_title", cex=4)
  graphics::par(family=cmapplot_globals$font_label)
  graphics::text(1, 1, "font_label", cex=4)
}
#check_cmap_fonts()
