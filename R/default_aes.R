#' Calculate, fetch, and set aesthetic defaults
#'
#' Aesthetics for ggplot2's "geoms" (e.g. the default font for a label or width
#' for a line) cannot be set via a theme. However, ggplot2 allows these defaults
#' to be overridden at the package level. These functions work together to align
#' the most commonly used geoms to CMAP's design style. They are rarely needed
#' by users except in specific circumstances.
#'
#' Because ggplot2 does not allow for the "theming" of aesthetics, applying
#' \code{theme_cmap()} will change attributes of the axes, legends, etc, but not
#' the attributes of geoms. \code{finalize_plot()} implicitly makes these
#' changes for the user. However, you may want plots to use these defaults
#' outside of \code{finalize_plot()}. Run \code{apply_cmap_default_aes()} to use CMAP
#' defaults for all ggplot2 plots drawn in the current session. To reset to
#' \code{ggplot2} defaults (technically, to whatever the defaults were when
#' \code{cmapplot} was loaded), use \code{unapply_cmap_default_aes}.
#'
#' \code{calc_cmap_default_aes} interprets \code{cmapplot_globals} to figure out
#' what CMAP default aesthetics should be. It is run automatically when the cmapplot
#' package is loaded. Manual overrides to \code{cmapplot_globals}, which can be
#' triggered by \code{set_cmapplot_global()}, will not automatically change
#' these defaults. Run this after modifying global variables if you want your
#' changes to impact geom aesthetics.
#'
#' The geoms these functions currently override are listed in
#' \code{cmapplot_globals$geoms_to_change}.
#'
#' @param quietly Should the function suppress confirmation messages?
#'
#' @examples
#'
#' # IMPROVE AND BROADEN TO ACCOUNT FOR NEW FUNCTIONS
#'
#' \dontrun{
#' g <- ggplot(filter(grp_over_time, category == "Services"),
#'             aes(x = year, y = realgrp, color = cluster)) +
#'   geom_recessions(ymax = 0.4, text_nudge_x = 0.1) +
#'   theme_cmap(hline = 0,
#'              axislines = "x",
#'              legend.max.columns = 2) +
#'   ggtitle("Change in gross regional product over time") +
#'   geom_line() +
#'   scale_x_continuous("Year", breaks = seq(2007, 2017, 2)) +
#'   coord_cartesian(clip = "off") +
#' geom_text_lastonly(aes(label = realgrp), add_points = TRUE)
#'
#' # print normally
#' g
#'
#' # overwrite `default_aes`
#' apply_cmap_default_aes()
#' g
#'
#' # reset `default_aes`
#' unapply_cmap_default_aes()
#' g
#'
#' # finalize alters the defaults implicitly, but then resets them automatically.
#' finalize_plot(g)
#'
#' # you can also use `finalize` without these modifications
#' finalize_plot(g, use_cmap_aes = FALSE)
#' }
#'
#' @aliases cmap_default_aes
#'
#' @describeIn calc_cmap_default_aes Calculate CMAP default aesthetics
#'
#' @export
calc_cmap_default_aes <- function (quietly = FALSE) {

  # this list MUST have the same structure as `cmapplot_globals$geoms_that_change`
  defaults <- list(
    Label = list(
      family = cmapplot_globals$font$strong$family,
      fontface = ifelse(cmapplot_globals$font$strong$face == "bold", 2, 1),
      size = cmapplot_globals$fsize$M/ggplot2::.pt, # pt-to-mm conversion
      colour = cmapplot_globals$colors$blackish
    ),
    Line = list(
      size = gg_lwd_convert(cmapplot_globals$consts$lwd_plotline)
    ),
    Text = list(
      family = cmapplot_globals$font$strong$family,
      fontface = ifelse(cmapplot_globals$font$strong$face == "bold", 2, 1),
      size = cmapplot_globals$fsize$M/ggplot2::.pt, # pt-to-mm conversion
      colour = cmapplot_globals$colors$blackish
    ),
    TextLast = list(
      family = cmapplot_globals$font$strong$family,
      fontface = ifelse(cmapplot_globals$font$strong$face == "bold", 2, 1),
      size = cmapplot_globals$fsize$M/ggplot2::.pt, # pt-to-mm conversion
      colour = cmapplot_globals$colors$blackish
    ),
    PointLast = list(
      size = 2 * gg_lwd_convert(cmapplot_globals$consts$lwd_plotline)
    ),
    RecessionsText = list(
      family = cmapplot_globals$font$regular$family,
      fontface = ifelse(cmapplot_globals$font$regular$face == "bold", 2, 1),
      size = cmapplot_globals$fsize$S/ggplot2::.pt, # pt-to-mm conversion
      colour = cmapplot_globals$colors$blackish
    )
  )

  # Return this list only if it lines up with `geoms_that_change`.
  # Otherwise, throw an error.
  if( !setequal(names(defaults), cmapplot_globals$geoms_that_change) ){

    stop("DEV ISSUE: programmed list of `default_aes` does not line up with
         `cmapplot_globals$geoms_that_change`.", call. = FALSE)

  }

  # load in CMAP default aes to cmapplot_globals
  assign("default_aes_cmap", defaults, envir = cmapplot_globals)

  if(!quietly){
    message(
      paste0("Calculated CMAP default aesthetics for the following Geoms:",
             "\n  ", paste(cmapplot_globals$geoms_that_change, collapse = ", ")
      )
    )
  }

  invisible()

}


#'  Geoms that change
#'
#'  This is a list of all geoms whose aesthetics will be customized. It must
#'  parallel the list of geoms initialized in `calc_cmap_default_aes`.
#'
#'  @noRd
#'
cmapplot_globals$geoms_that_change <- c(
  "Label",
  "Line",
  "Text",
  "TextLast",
  "PointLast",
  "RecessionsText"
)


#' @describeIn calc_cmap_default_aes Apply CMAP aesthetic defaults to all ggplots in
#'   the current session
#'
#' @export
apply_cmap_default_aes <- function (quietly = FALSE) {

  set_default_aes(cmapplot_globals$default_aes_cmap)

  if (!quietly) {
    message(
      paste0("Aesthetic defaults overridden in the current session for the following Geoms:",
            "\n  ", paste(cmapplot_globals$geoms_that_change, collapse = ", "),
            "\nTo undo this change, run `unapply_cmap_default_aes()`."
      )
    )
  }
}


#' @describeIn calc_cmap_default_aes Reset modified geom aesthetics to their values
#'   when \code{cmapplot} was first loaded
#'
#' @export
unapply_cmap_default_aes <- function (quietly = FALSE) {

  set_default_aes(cmapplot_globals$default_aes_cached)

  if (!quietly) {
    message(
      paste0("Aesthetic defaults reset to values cached when pkg `cmapplot` was first loaded for Geoms:",
            "\n  ", paste(cmapplot_globals$geoms_that_change, collapse = ", ")
      )
    )
  }
}

#' Fetch `default_aes` from select geoms
#'
#' Internal function to fetch `default_aes` for geoms that this package plans to
#' overwrite. Used in `.onLoad` and in `finalize_plot`.
#'
#' @importFrom purrr map
#' @noRd
fetch_current_default_aes <- function () {

  purrr::map(
    cmapplot_globals$geoms_that_change,
    function (geom) {
      get(paste0("Geom", geom))$default_aes
    }
  ) %>%
    rlang::set_names(cmapplot_globals$geoms_that_change)
}

#' Set `default_aes` for select geoms
#'
#' Internal function to overwrite `default_aes` for the necessary geoms. Used in
#' `finalize_plot`, `apply_cmap_geom_defaults` and `unapply_cmap_geom_defaults`.
#'
#' @param values A named list of parameters, presumably from (or at least a
#'   parallel structure to) the output of `fetch_cmap_geom_defaults`.
#'
#' @importFrom purrr walk2
#' @noRd
set_default_aes <- function (values) {

  purrr::walk2(
    cmapplot_globals$geoms_that_change,
    values,
    ggplot2::update_geom_defaults
  )
}
