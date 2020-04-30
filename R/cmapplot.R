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
#' @import ggplot2 dplyr grid scales grDevices graphics rlang
#' @importFrom glue glue glue_collapse
#' @importFrom lubridate date_decimal
#' @keywords internal
"_PACKAGE"

cmapplot_globals <- new.env(parent=emptyenv())  # An environment for storing any global variables

if (.Platform$OS.type == "windows") {

  # Set CMAP fonts: use Whitney if installed, Calibri otherwise
  all_fonts <- sysfonts::font_files()
  whitney_fonts <- dplyr::filter(all_fonts,
    family %in% c("Whitney Medium", "Whitney Book", "Whitney Semibold") & face == "Regular"
  )
  cmapplot_globals$use_whitney = length(whitney_fonts$family) == 3
  rm(all_fonts)
  rm(whitney_fonts)

  if (cmapplot_globals$use_whitney) {
    grDevices::windowsFonts(
      sans = "Whitney Medium",  # Override the default font (Arial)
      font_reg = "Whitney Medium",
      font_lite = "Whitney Book",
      font_sbold = "Whitney Semibold"
    )
  } else {
    warning("Whitney is not installed on this PC, so CMAP theme will default to Calibri")
    grDevices::windowsFonts(
      sans = "Calibri",  # Override the default font (Arial)
      font_reg = "Calibri",
      font_lite = "Calibri Light",
      font_sbold = "Calibri"  # No separate semibold/bold font for Calibri
    )
  }

  cmapplot_globals$font_main <- "font_reg"  # "medium" weight for in-body text and x/y axis
  cmapplot_globals$font_note <- "font_lite"  # "book" weight for notes and sources
  cmapplot_globals$font_title <- "font_sbold"  # "semibold" weight for title
  cmapplot_globals$font_label <- "font_sbold"  # "semibold" weight also for labels
  if (cmapplot_globals$use_whitney) {
    cmapplot_globals$font_main_face <- "plain"
    cmapplot_globals$font_note_face <- "plain"
    cmapplot_globals$font_title_face <- "plain"
    cmapplot_globals$font_label_face <- "plain"
  } else {
    cmapplot_globals$font_main_face <- "plain"
    cmapplot_globals$font_note_face <- "plain"
    cmapplot_globals$font_title_face <- "bold"  # Calibri doesn't have a standalone bold/semibold typeface
    cmapplot_globals$font_label_face <- "bold"  # Calibri doesn't have a standalone bold/semibold typeface
  }

} else {

  # Assume no Whitney or Calibri on non-Windows (i.e. non-work) computers
  warning("CMAP theme will default to Arial on non-Windows platforms")
  cmapplot_globals$use_whitney = FALSE

  grDevices::X11Fonts(
    sans = grDevices::X11Fonts()$Arial  # Just give in and use Arial for everything :(
   )

  cmapplot_globals$font_main <- "sans"  # "medium" weight for in-body text and x/y axis
  cmapplot_globals$font_note <- "sans"  # "book" weight for notes and sources
  cmapplot_globals$font_title <- "sans"  # "semibold" weight for title
  cmapplot_globals$font_label <- "sans"  # "semibold" weight also for labels

  cmapplot_globals$font_main_face <- "plain"
  cmapplot_globals$font_note_face <- "plain"
  cmapplot_globals$font_title_face <- "bold"  # Arial doesn't have a standalone bold/semibold typeface
  cmapplot_globals$font_label_face <- "bold"  # Arial doesn't have a standalone bold/semibold typeface

}

check_cmap_fonts <- function() {
  graphics::plot(c(0,2), c(0,5), type="n", xlab="", ylab="")
  graphics::par(family=cmapplot_globals$font_note,
                font=ifelse(cmapplot_globals$font_note_face == "bold", 2, 1))
  graphics::text(1, 4, "font_note", cex=4)
  graphics::par(family=cmapplot_globals$font_main,
                font=ifelse(cmapplot_globals$font_main_face == "bold", 2, 1))
  graphics::text(1, 3, "font_main", cex=4)
  graphics::par(family=cmapplot_globals$font_title,
                font=ifelse(cmapplot_globals$font_title_face == "bold", 2, 1))
  graphics::text(1, 2, "font_title", cex=4)
  graphics::par(family=cmapplot_globals$font_label,
                font=ifelse(cmapplot_globals$font_label_face == "bold", 2, 1))
  graphics::text(1, 1, "font_label", cex=4)
}
#check_cmap_fonts()

ggplot2::update_geom_defaults("text", list(family=cmapplot_globals$font_main, face=cmapplot_globals$font_main_face))
