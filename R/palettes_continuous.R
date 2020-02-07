###############################################################################
#
# Continuous palettes
#
#       Short palettes or long? (5 or 9 colors?)
#
#       diagonal along the color guide? Mix hues a little?
#
#       easier naming conventions?
#
#       * Add plot examples
#
###############################################################################


#' CMAP continuous color palettes
#'
#' A sample of comms-approved colors arranged into palettes
#'
#' @examples
#' viz_gradient(cmap_gradients$red_purple)
#'
#' @export
cmap_gradients <- list(

    orange_red = c("#f8ebdf", "#d88236", "#8c0000", "#3f0000"),

    red_gradient = c("#f8dfdf", "#cc0000", "#3f0000"),

    orange_gradient = c("#f8ebdf", "#cc5f00", "#3f1d00"),

    yellow_orange = c("#f8f4df", "#cca600", "#cc5f00", "#662f00"),

    yellow_gradient = c("#f8f4df", "#cca600", "#3f3400"),

    yellow_green = c("#f8f4df", "#e5e172", "#b4cc00", "#698c00", "#263f00"),

    green_gradient = c("#eef8df", "#7acc00", "#263f00"),

    green_teal = c("#eef8df", "#8de572", "#00cc1f", "#008c4b", "#003f3a"),

    teal_gradient = c("#dff8f6", "#00ccb8", "#003f3a"),

    teal_blue = c("#dff8f6", "#36d8ca", "#00becc", "#0084ac", "#00303f"),

    blue_gradient = c("#dff2f8", "#009ccc", "#00303f"),

    blue_purple = c("#dff2f8", "#72a6e5", "#5300ac", "#310066"),

    purple_gradient = c("#ebdff8", "#6300cc", "#1e003f"),

    red_purple = c("#f8dfdf", "#e57272", "#77008c", "#310066"),

    grey_gradient = c("#e3e8eb", "#7b929d", "#000000")
)


#' Visualize CMAP color gradients
#'
#' Displays 25 interpolated colors from the cmap continuous palettes.
#' Modeled after viz_palette from the \href{https://github.com/ropenscilabs/ochRe}{ochRe package}
#'
#' @param pal = select from cmap_gradients list
#' @param ttl = display title (optional)
#'
#' @examples
#' viz_gradient(cmap_gradients$orange_red)
#'
#' @export
viz_gradient <- function(pal, ttl = deparse(substitute(pal))) {

    pal_func <- colorRampPalette(pal)

    image(seq_len(30), 1, as.matrix(seq_len(30)), col = pal_func(30),
          main = ttl,
          xlab = "", ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}


#' Continuous palette prep function
#'
#' @param palette Choose from 'cmap_gradients' list
#' @param reverse Logical; reverse color order?
#'
#' @export
cmap_pal_gradient <- function(palette = "red_gradient",
                              reverse = FALSE) {

    pal <- cmap_gradients[[palette]]

    if (reverse) {
        pal <- rev(pal)
    }

    return(colorRampPalette(pal))
}


#' Apply continuous CMAP palettes to ggplot2 aesthetics
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or color)
#'
#' @param palette Choose from 'cmap_palettes' list
#' @param reverse Logical; reverse color order?
#'
#' @export
cmap_fill_continuous <- function(palette = "red_gradient",
                                 reverse = FALSE) {

    scale_fill_gradientn(colours = cmap_pal_gradient(palette,
                                                     reverse = reverse, ...)(256))
}


#' @rdname cmap_fill_continuous
#' @export
cmap_color_continuous <- function(palette = "red_gradient",
                                  reverse = FALSE) {

    scale_colour_gradientn(colours = cmap_pal_gradient(palette,
                                                       reverse = reverse, ...)(256))
}

#' @rdname cmap_fill_continuous
#' @export
cmap_colour_continuous <- cmap_color_continuous

