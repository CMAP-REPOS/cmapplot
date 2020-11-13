#' Initialize CMAP `default_aes` values
#'
#' Internal function to load in default aesthetics for modified geoms.
#' Loaded in to `cmapplot_globals` in `.onLoad`.
#'
#' This list's names MUST equal `cmapplot_globals$geoms_that_change`
#'
#' @noRd
init_cmap_default_aes <- function () {
  list(
    Line = list(
      size = ggplot_size_conversion(cmapplot_globals$consts$lwd_plotline)
    ),
    Text = list(
      family = cmapplot_globals$font$strong$family,
      fontface = ifelse(cmapplot_globals$font$strong$face == "bold", 2, 1),
      size = cmapplot_globals$fsize$M/ggplot2::.pt, # pt-to-mm conversion
      colour = cmapplot_globals$colors$blackish
    ),
    TextLast =list(
      family = cmapplot_globals$font$strong$family,
      fontface = ifelse(cmapplot_globals$font$strong$face == "bold", 2, 1),
      size = cmapplot_globals$fsize$M/ggplot2::.pt, # pt-to-mm conversion
      colour = cmapplot_globals$colors$blackish
    ),
    RecessionsText = list(
      family = cmapplot_globals$font$regular$family,
      fontface = ifelse(cmapplot_globals$font$regular$face == "bold", 2, 1),
      size = cmapplot_globals$fsize$S/ggplot2::.pt, # pt-to-mm conversion
      colour = cmapplot_globals$colors$blackish
    )
  )
}

#' Fetch and set aesthetic defaults
#'
#' These functions allow for setting and resetting default aesthetic values for
#' certain ggplot2 geoms. This is necessary for geoms to be "themed" to CMAP
#' style standards, because (at least at the moment) setting geom aesthetic
#' defaults on a plot-by-plot basis (such as with \code{ggplot2::theme}) is not
#' possible. The geoms impacted are stored in
#' \code{cmapplot_globals$geoms_to_change}.
#'
#' These functions are employed implictly within \code{\link{finalize_plot}} to
#' apply preferred aesthetic defaults to final outputs. They are only necessary
#' to use explicitly if you would like plots to use these defaults
#' pre-\code{finalize}.
#'
#' CAUTION: Running \code{apply_cmap_default_aes} will set defaults for all
#' ggplot2 plots drawn in the current session. To reset to \code{ggplot2}
#' defaults (technically, to whatever the defaults were when \code{cmapplot}
#' was loaded), use \code{unapply_cmap_default_aes}.
#'
#' Note: CMAP aesthetic defaults are loaded into
#' \code{cmapplot_globals$default_aes_cmap} by the internal pkg function
#' \code{init_cmap_default_aes} when \code{cmapplot} is first loaded into R.
#'
#' @param quietly Should the function suppress confirmation message?
#'
#' @examples
#' \dontrun{
#' g <- ggplot(mtcars, aes(x = mpg, y = disp, label = rownames(mtcars))) +
#'   geom_line() +
#'   geom_text() +
#'   geom_text_lastonly() +
#'   theme_cmap() +
#'   coord_cartesian(clip = "off")
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
#' @name cmap_default_aes
NULL

#' @describeIn cmap_default_aes Apply CMAP aesthetic defaults to all ggplots in
#'   the current session
#'
#' @export
apply_cmap_default_aes <- function (quietly = FALSE) {

  set_default_aes(cmapplot_globals$default_aes_cmap)

  if (!quietly) {
    message(
      paste("Geom defaults overridden in the current session for:",
            paste(cmapplot_globals$geoms_that_change, collapse = ", ")
      )
    )
  }
}


#' @describeIn cmap_default_aes Reset modified geom aesthetics to their values
#'   when \code{cmapplot} was first loaded
#'
#' @export
unapply_cmap_default_aes <- function (quietly = FALSE) {

  set_default_aes(cmapplot_globals$default_aes_cached)

  if (!quietly) {
    message(
      paste("Geom defaults reset for:",
            paste(cmapplot_globals$geoms_that_change, collapse = ", ")
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



