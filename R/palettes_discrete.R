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

    prosperity = c("brown"      = "#662f00",
                   "light gold" = "#E5D072",
                   "royal purple" = "#44008c",
                   "green"      = "#c8e572",
                   "lilac"      = "#C9A7EF"),

    community = c("burnt orange" = "#CC5F00",
                  "blue"         = "#006B8C",
                  "peach"        = "#E5A872",
                  "lime green"   = "#D2EFA7",
                  "brown"        = "#662f00"),

    environment = c("dark teal"  = "#00665c",
                    "lime green" = "#B7E572",
                    "plum"       = "#3f0030",
                    "light teal" = "#36d8ca",
                    "dark blue"  = "#006b8c"),

    governance = c("blue"          = "#006b8c",
                   "pink"          = "#efa7a7",
                   "brown"         = "#8c4100",
                   "marina trench" = "#00303f",
                   "gold"          = "#cca600",
                   "ice"           = "#a7efe8"),

    mobility = c("fire engine"             = "#8c0000",
                 "elementary school floor" = "#e5bd72",
                 "sky blue"                = "#a7efe8",
                 "cta sign grey"           = "#6d8692",
                 "warm blue"               = "#0084ac",
                 "salmon"                  = "#efa7a7"),

    legislation = c("blue"        = "#00becc",
                    "orange"      = "#cc5f00",
                    "maroon"      = "#3f0e00",
                    "gold"        = "#cca600",
                    "navy"        = "#003f8c",
                    "kelly green" = "#67ac00"),

    friday = c("navy"                        = "#00093f",
               "gold"                        = "#ac8c00",
               "dark grey"                   = "#475c66",
               "I don't like sand"           = "#e5d072",
               "sword that killed my father" = "#b5c1c8",
               "my favorite blue"            = "#006b8c")

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
