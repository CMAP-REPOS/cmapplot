#' Fetch and set aesthetic defaults
#'
#' These functions allow for fetching and setting default aesthetic values for
#' certain ggplot2 geoms. This is necessary for geoms to be "themed" to CMAP
#' style standards, because (at least at the moment) setting geom aesthetic
#' defaults on a plot-by-plot basis (such as with \code{ggplot2::theme}) is not
#' possible. The geoms impacted and their preferred settings are hardwired into
#' the functions, and at the moment include \code{Line}, \code{Text}, and the
#' text-based custom \code{cmapplot} geoms \code{TextLast} and
#' \code{RecessionsText}.
#'
#' These functions are employed implictly within \code{\link{finalize_plot}} to
#' apply preferred aesthetic defaults to final outputs. They are only necessary
#' to use explicitly if you would like plots to use these defaults
#' pre-\code{finalize}.
#'
#' CAUTION: Running \code{set_cmap_geom_defaults} will set defaults for any
#' ggplot drawn in the current session, including those without
#' \code{theme_cmap}. If this is not desired, you must re-run the function with
#' old values (see examples) or unload and reload the ggplot2 package when
#' you're done.
#'
#' @examples
#' g <- ggplot(mtcars, aes(x = mpg, y = disp, label = rownames(mtcars))) +
#'   geom_line() +
#'   geom_text() +
#'   geom_text_lastonly() +
#'   theme_minimal() +
#'   coord_cartesian(clip = "off")
#'
#' # print normally
#' g
#'
#' # before overwriting defaults, save the current ones
#' old <- fetch_cmap_geom_defaults()
#'
#' # overwite defaults
#' set_cmap_geom_defaults()
#' g
#'
#' # reset to original defaults
#' set_cmap_geom_defaults(old)
#' g
#'
#' \dontrun{
#' # finalize alters the defaults implicitly, but then resets them automatically.
#' finalize_plot(g)
#'
#' # you can also use `finalize` without these modifications
#' finalize_plot(g, use_cmap_aes = FALSE)
#' }
#'
#' @name cmap_geom_defaults
NULL


#' @describeIn cmap_geom_defaults Fetch geom defaults
#' @importFrom purrr map2
#' @export
fetch_cmap_geom_defaults <- function(){

  geom_changes <- tribble(
    ~geom, ~pkg,
    "Line", "ggplot2",
    "Text", "ggplot2",
    "TextLast", "cmapplot",
    "RecessionsText", "cmapplot"
  )

  purrr::map2(geom_changes$geom,
       geom_changes$pkg,
       function(geom, pkg){get(paste0("Geom", geom), as_environment(pkg))$default_aes}
  ) %>%
    set_names(geom_changes$geom)
}

#' @describeIn cmap_geom_defaults Set geom defaults, either to CMAP-preferred or
#'   user-inputed values
#'
#' @param values A list of lists representing the aesthetic defaults to set for
#'   the impacted geoms. Leave null, the default, to use the CMAP-preferred
#'   values. Use this field to input the output of \code{fetch_cmap_geom_defaults}
#'   in order to reset to ggplot defaults.
#' @param quietly Suppress confirmatory message
#'
#'
#' @importFrom purrr walk2
#' @export
set_cmap_geom_defaults <- function(values = NULL, quietly = FALSE){

  geom_changes <- tribble(
    ~geom, ~attr,
    "Line", list(size = ggplot_size_conversion(cmapplot_globals$consts$lwd_plotline)),
    "Text", list(family = cmapplot_globals$font$strong$family,
                 fontface = ifelse(cmapplot_globals$font$strong$face == "bold", 2, 1),
                 size = cmapplot_globals$fsize$M/ggplot2::.pt, # Accounts for the fact that text is sized in mm
                 colour = cmapplot_globals$colors$blackish),
    "TextLast", list(family = cmapplot_globals$font$strong$family,
                     fontface = ifelse(cmapplot_globals$font$strong$face == "bold", 2, 1),
                     size = cmapplot_globals$fsize$M/ggplot2::.pt, # Accounts for the fact that text is sized in mm
                     colour = cmapplot_globals$colors$blackish),
    "RecessionsText", list(family = cmapplot_globals$font$regular$family,
                           fontface = ifelse(cmapplot_globals$font$regular$face == "bold", 2, 1),
                           size = cmapplot_globals$fsize$S/ggplot2::.pt, # Accounts for the fact that text is sized in mm
                           colour = cmapplot_globals$colors$blackish)
  )

  if(is.null(values)){
    values <- geom_changes$attr
  }

  purrr::walk2(geom_changes$geom,
        values,
        ggplot2::update_geom_defaults
  )

  if(!quietly){
    message(
      paste("Geom defaults overridden in the current session for:",
            paste(geom_changes$geom, collapse = ", ")
      )
    )
  }

}
