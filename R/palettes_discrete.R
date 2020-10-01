#' CMAP discrete color palettes
#'
#' A sample of comms-approved colors arranged into palettes
#'
#' @examples
#' viz_palette(cmap_palettes$mobility)
#'
#' ggplot(pop_and_laborforce_by_age, aes(x = variable, y = value, fill = age)) +
#'    geom_col(position = position_stack(reverse = TRUE)) +
#'    facet_wrap(~year) +
#'    cmap_fill_discrete(palette = "community")
#'
#' @export
cmap_palettes <- list(

    race = c(white    = "#72a6e5",
             black    = "#8de572",
             hispanic = "#cca600",
             asian    = "#cc3000",
             other    = "#8a9ea8"),

    prosperity = c("#662f00", "#e5d072", "#44008c", "#c8e572", "#c9a7ef"),

    community = c("#cc5f00", "#006b8c", "#e5a872", "#d2efa7", "#662f00"),

    environment = c("#00665c", "#b7e572", "#3f0030",  "#36d8ca", "#006b8c"),

    governance = c("#006b8c", "#efa7a7", "#8c4100", "#00303f", "#cca600", "#a7efe8"),

    mobility = c("#8c0000", "#e5bd72", "#a7efe8", "#6d8692", "#0084ac", "#efa7a7"),

    legislation = c("#00becc", "#cc5f00", "#3f0e00", "#cca600", "#003f8c", "#67ac00"),

    friday = c("#00093f", "#ac8c00", "#475c66", "#e5d072", "#b5c1c8", "#006b8c")

)


#' Print palette for reference
#'
#' Directly from the \href{https://github.com/ropenscilabs/ochRe}{ochRe package}
#'
#' @param pal character, vector of (hexadecimal) colours representing a palette
#' @param ttl character, title to be displayed (the name of the palette)
#' @param num numeric, the number of colours to display
#'
#' @examples
#' viz_palette(cmap_palettes$mobility)
#'
#' @export
viz_palette <- function(pal, ttl = deparse(substitute(pal)), num = length(pal)) {
    if (num <= 0) {
        stop("'num' should be > 0")
    }
    pal_func <- grDevices::colorRampPalette(pal)
    graphics::image(seq_len(num), 1, as.matrix(seq_len(num)), col = pal_func(num),
                    main = paste0(ttl, " (", length(pal), " colours in palette, ", num, " displayed)"),
                    xlab = "", ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}


#' Discrete palette prep function
#'
cmap_pal_discrete <- function(palette = "prosperity", reverse = FALSE) {
    pal <- cmap_palettes[[palette]]
    if (reverse) {
        pal <- rev(pal)
    }
    return(grDevices::colorRampPalette(pal))
}


#' Apply discrete CMAP palettes to ggplot2 aesthetics
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or color)
#'
#' @param palette Choose from 'cmap_palettes' list
#' @param reverse Logical; reverse color order?
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
#' @export
cmap_fill_discrete <- function(palette = "prosperity", reverse = FALSE) {
    ggplot2::discrete_scale(
        "fill", "cmap_palettes", palette = cmap_pal_discrete(palette, reverse = reverse)
    )
}

#' @rdname cmap_fill_discrete
#' @export
cmap_color_discrete <- function(palette = "prosperity", reverse = FALSE) {
    ggplot2::discrete_scale(
        "colour", "cmap_palettes", palette = cmap_pal_discrete(palette, reverse = reverse)
    )
}

#' @rdname cmap_fill_discrete
#' @export
cmap_colour_discrete <- cmap_color_discrete
