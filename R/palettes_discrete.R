###############################################################################

# Concerns:

# Consistency for group colors across multiple charts within report

# Colors for peer city comparisons: vibrant blue for Chicago, grey for all else

###############################################################################


# Modeled after ochRe palette package
    # https://github.com/ropenscilabs/ochRe/blob/master/R


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

    prosperity = c("gold"       = "#CCA600",
                   "brown"      = "#8C4100",
                   "cream"      = "#F8F4DF",
                   "light gold" = "#E5D072",
                   "purple"     = "#C9A7EF"),

    community = c("burnt orange" = "#CC5F00",
                  "cream"        = "#F8EBDF",
                  "blue"         = "#006B8C",
                  "lime green"   = "#D2EFA7",
                  "peach"        = "#E5A872"),

    environment = c("neon green"     = "#7ACC00",
                    "sky blue"       = "#A7DEEF",
                    "lightest green" = "#D2EFA7",
                    "purple"         = "#43008C",
                    "lime green"     = "#B7E572"),

    governance = c("blue"     = "#009CCC",
                   "olive"    = "#8C7200",
                   "ice"      = "#DFF2F8",
                   "peach"    = "#EFC8A7",
                   "sky blue" = "#72CAE5"),

    mobility = c("purple"       = "#6200CC",
                 "lilac"        = "#EBDFF8",
                 "green"        = "#548C00",
                 "sand"         = "#EFE1A7",
                 "light purple" = "#AA72E5")
)


#' Print palette for reference
#'
#' directly from ochRe package: github.com/ropenscilabs/ochRe
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

    if(num <= 0)
        stop("'num' should be > 0")

    pal_func <- colorRampPalette(pal)

    image(seq_len(num), 1, as.matrix(seq_len(num)), col = pal_func(num),
          main = paste0(ttl, " (", length(pal), " colours in palette, ", num, " displayed)"),
          xlab = "", ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}


#' Discrete palette prep function
#'
#' @param palette Choose from 'cmap_palettes' list
#' @param reverse Logical; reverse color order?
#'
#' @export
cmap_pal_discrete <- function(palette = "prosperity", reverse = FALSE) {

    pal <- cmap_palettes[[palette]]

    if (reverse) {
        pal <- rev(pal)
    }

    return(colorRampPalette(pal))
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
cmap_fill_discrete <- function(palette = "prosperity",
                               reverse = FALSE) {

    discrete_scale("fill", "cmap_palettes",
                   palette = cmap_pal_discrete(palette, reverse = reverse))
}

#' @rdname cmap_fill_discrete
#' @export
cmap_color_discrete <- function(palette = "prosperity",
                                reverse = FALSE) {

    discrete_scale("colour", "cmap_palettes",
                   palette = cmap_pal_discrete(palette, reverse = reverse))
}

#' @rdname cmap_fill_discrete
#' @export
cmap_colour_discrete <- cmap_color_discrete
