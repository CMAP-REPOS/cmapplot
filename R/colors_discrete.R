#' Visualizing CMAP color palettes
#'
#' The cmapplot package contains a many color palettes extracted from the
#' larger, official CMAP color palette. Helper functions allow the user to
#' inspect the various palettes before applying them to plots.
#'
#' Palettes are stored in a tibble the \code{cmapplot_globals} environment. The
#' user can access this tibble with \code{\link{get_cmapplot_global}}, but it is
#' easier to access information about a single palette with \code{fetch_pal}.
#'
#' \code{viz_palette} and \code{viz_gradient} draw the palette to the plots
#' window. These functions are modified with respect from the
#' \href{https://github.com/ropenscilabs/ochRe}{ochRe package}.
#'
#' For more information about available cmapplot color palettes and how to apply
#' them, see \code{vignette("colors")}.
#'
#' @describeIn viz_palette Displays the colors of any cmapplot palette
#'
#' @param pal character, name of a a cmapplot palette, or a vector of colors
#'   representing a palette
#' @param ttl character, title to be displayed (the name of the palette)
#' @param num numeric, the number of colors to display
#'
#' @examples
#' # Visualize a single palette as individual colors
#' viz_palette("legislation")
#'
#' # Print names and types of all available palettes
#' as.data.frame(get_cmapplot_global("palettes")[1:2])
#'
#' @aliases cmap_palettes cmap_gradients cmap_colors
#'
#' @export
viz_palette <- function(pal, ttl = NULL, num = NULL) {

    # if `pal` is a named CMAP palette of any type...
    if (fetch_pal(pal[1], return = "exists")) {
        # use the palette as the title (unless a custom title has been provided)
        if (is.null(ttl) | missing(ttl)){ ttl <- pal }
        # and extract the palette colors
        pal <- fetch_pal(pal)
    } else {
        # otherwise, use the object name as the title (unless a custom title has been provided)
        if (is.null(ttl) | missing(ttl)){ ttl <- deparse(substitute(pal)) }
    }

    # use the palette's intrinsic length (unless a custom length has been provided)
    if (is.null(num) | missing(num)){ num <- length(pal) }

    pal_func <-
      function(n) {

        if (n > length(pal)) {
          grDevices::colorRampPalette(pal)(n)
        }

        else pal[1:n]
      }

    graphics::image(seq_len(num), 1, as.matrix(seq_len(num)), col = pal_func(num),
                    main = paste0(ttl, " (", length(pal), " colors in palette, ",
                                  num, " displayed)"),
                    xlab = "", ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}

#' Discrete palette prep function
#'
#' @param palette Choose from 'cmap_palettes' list, or use one of the gradients
#'   defined in the 'cmap_gradients' list (gradients will be automatically
#'   converted into discrete bins)
#' @param reverse Logical; reverse color order?
#' @param gradient Logical; is this palette a color gradient?
#' @param ... Additional parameters passed on to the scale type
#'
#' @noRd
cmap_pal_discrete <- function(palette = "prosperity", reverse = FALSE, gradient = FALSE) {
    pal <- fetch_pal(palette)

    if(palette == "race"){
        message("WARNING: The `race` palette should only be used with `cmap_fill_race()` or `cmap_color_race()`.")
    }

    if (reverse) {
        pal <- rev(pal)
    }
    return(
      function(n) {

        if (gradient == TRUE) {
          grDevices::colorRampPalette(pal)(n)
        }

        else {
          if (n > length(pal)) {

            message(paste("WARNING: The palette you selected has",
                          length(pal),
                          "colors, but your graphic has",
                          n,
                          "colors. cmapplot will interpolate additional colors, but please note that this is no longer aligned with CMAP design standards. Alternatively, if you need more than",
                          length(pal),
                          "colors for exploratory purposes, use ggplot2::scale_fill_discrete() or ggplot2::scale_color_discrete()."
            )
            )

            grDevices::colorRampPalette(pal)(n)
          }

          else pal[1:n]
        }
      }
    )
}

#' Apply discrete CMAP palettes to ggplot2 aesthetics
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or
#' color). See \code{link{cmap_palettes}} for a listing of available gradients.
#'
#' @param palette Choose from 'cmap_palettes' list, or use one of the gradients
#'   defined in the 'cmap_gradients' list (gradients will be automatically
#'   converted into discrete bins)
#' @param reverse Logical; reverse color order?
#' @param gradient Logical; is this palette a color gradient?
#' @param ... Additional parameters passed on to the scale type
#'
#' @examples
#' ggplot(pop_and_laborforce_by_age, aes(x = variable, y = value, fill = age)) +
#'    geom_col(position = position_stack(reverse = TRUE)) +
#'    facet_wrap(~year) +
#'    cmap_fill_discrete(palette = "community")
#'
#' ggplot(percentile_wages, aes(x = percentile, y = wage, color = cluster)) +
#'    geom_line() +
#'    cmap_color_discrete(palette = "prosperity")
#'
#' @describeIn cmap_fill_discrete For fill aesthetic
#' @export
cmap_fill_discrete <- function(palette = "prosperity", reverse = FALSE, gradient = FALSE, ...) {
    ggplot2::discrete_scale(
        "fill", "cmap_palettes",
        palette = cmap_pal_discrete(palette, reverse = reverse, gradient = gradient),
        ...
    )
}

#' @describeIn cmap_fill_discrete For color aesthetic
#' @export
cmap_color_discrete <- function(palette = "prosperity", reverse = FALSE, gradient = FALSE, ...) {
    ggplot2::discrete_scale(
        "colour", "cmap_palettes",
        palette = cmap_pal_discrete(palette, reverse = reverse, gradient = gradient),
        ...
    )
}

#' @describeIn cmap_fill_discrete For color aesthetic
#' @export
cmap_colour_discrete <- cmap_color_discrete
