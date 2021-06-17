#' CMAP discrete color palettes
#'
#' A selection of discrete color palettes from the CMAP color palette. These
#' include mixed color palettes and discrete versions of the gradients defined
#' in \code{link{cmap_fill_continuous}}.
#'
#' @examples
#' # Get names of available discrete palettes.
#' # (Call viz_palette(cmap_palettes$name_of_palette) to preview one.)
#' names(cmap_palettes)
#'
#' # Run the following function to visualize *all* discrete palettes
#' purrr::walk2(cmap_palettes, names(cmap_palettes), viz_palette)
#'
#' @export


#' Print palette for reference
#'
#' @param pal character, vector of (hexadecimal) colors representing a palette
#' @param ttl character, title to be displayed (the name of the palette)
#' @param num numeric, the number of colors to display
#'
#' @describeIn cmap_palettes Display CMAP palettes. Borrowed with respect from
#'   the \href{https://github.com/ropenscilabs/ochRe}{ochRe package}
#'
#' @export
viz_palette <- function(pal, ttl = deparse(substitute(pal)), num = length(pal)) {
    if (num <= 0) {
        stop("'num' should be > 0")
    }
    pal_func <- grDevices::colorRampPalette(pal)
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
#' @param ... Additional parameters passed on to the scale type
#'
#' @noRd
cmap_pal_discrete <- function(palette = "prosperity", reverse = FALSE) {
    pal <- cmap_palettes[[palette]]
    if (reverse) {
        pal <- rev(pal)
    }
    return(grDevices::colorRampPalette(pal))
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
cmap_fill_discrete <- function(palette = "prosperity", reverse = FALSE, ...) {
    ggplot2::discrete_scale(
        "fill", "cmap_palettes",
        palette = cmap_pal_discrete(palette, reverse = reverse),
        ...
    )
}

#' @describeIn cmap_fill_discrete For color aesthetic
#' @export
cmap_color_discrete <- function(palette = "prosperity", reverse = FALSE, ...) {
    ggplot2::discrete_scale(
        "colour", "cmap_palettes",
        palette = cmap_pal_discrete(palette, reverse = reverse),
        ...
    )
}

#' @describeIn cmap_fill_discrete For color aesthetic
#' @export
cmap_colour_discrete <- cmap_color_discrete
