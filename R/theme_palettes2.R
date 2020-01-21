# G Ritzenthaler 01-14-2020


###############################################################################

# questions:

# How to refer to other packages?
  # for code credits?
  # for copied functions? (copy code or call package in front of function?)

# Do we have a need for continuous palettes? which colors are those?

# !!! Adjusting the alpha changes the colors, not just transparency

# Change default palette?

# Add more discrete palettes?

###############################################################################


# Modeled after ochRe palette package
    # https://github.com/ropenscilabs/ochRe/blob/master/R

# Palettes list
cmap_palettes <- list(

  prosperity = c("gold"       = "#CCA600",
                 "light gold" = "#E5D072",
                 "cream"      = "#F8F4DF",
                 "purple"     = "#C9A7EF",
                 "brown"      = "#8C4100"),

  community = c("burnt orange" = "#CC5F00",
                "peach"        = "#E5A872",
                "cream"        = "#F8EBDF",
                "lime green"   = "#D2EFA7",
                "blue"         = "#006B8C"),

  environment = c("neon green"     = "#7ACC00",
                  "lime green"     = "#B7E572",
                  "lightest green" = "#D2EFA7",
                  "sky blue"       = "#A7DEEF",
                  "purple"         = "#43008C"),

  governance = c("blue"     = "#009CCC",
                 "sky blue" = "#72CAE5",
                 "ice"      = "#DFF2F8",
                 "peach"    = "#EFC8A7",
                 "olive"    = "#8C7200"),

  mobility = c("purple"       = "#6200CC",
               "light purple" = "#AA72E5",
               "lilac"        = "#EBDFF8",
               "sand"         = "#EFE1A7",
               "green"        = "#548C00")
)


# Print palette for reference
    # directly from ochRe package:
        # https://github.com/ropenscilabs/ochRe/blob/master/R/utils.R
viz_palette <- function(pal, ttl = deparse(substitute(pal)), num = length(pal)) {
  if(num <= 0)
    stop("'num' should be > 0")
  pal_func <- colorRampPalette(pal)
  image(seq_len(num), 1, as.matrix(seq_len(num)), col = pal_func(num),
        main = paste0(ttl, " (", length(pal), " colours in palette, ", num, " displayed)"),
        xlab = "", ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}


# Palette prep function
cmap_pal <- function(palette = "prosperity", alpha = 1, reverse = FALSE) {
  pal <- cmap_palettes[[palette]]
  if (reverse) {
    pal <- rev(pal)
  }
  return(colorRampPalette(pal, alpha))
}


# scale fill function
scale_fill_cmap <- function(..., palette = "prosperity", discrete = TRUE,
                            alpha = 1, reverse = FALSE) {
  if (discrete) {
    discrete_scale("fill", "cmap_palettes",
                   palette = cmap_pal(palette, alpha = alpha,
                                      reverse = reverse))
  }
  else {
    scale_fill_gradientn(colours = cmap_pal(palette, alpha = alpha,
                                            reverse = reverse, ...)(256))
  }
}


# scale color function
scale_color_cmap <- function(..., palette = "prosperity", discrete = TRUE,
                             alpha = 1, reverse = FALSE) {
  if (discrete) {
    discrete_scale("colour", "cmap_palettes",
                   palette = cmap_pal(palette, alpha = alpha,
                                      reverse = reverse))
  }
  else {
    scale_fill_gradientn(colours = cmap_pal(palette, alpha = alpha,
                                            reverse = reverse, ...)(256))
  }
}

# catch spelling difference
scale_colour_cmap <- scale_color_cmap


# To use:

#   ggplot obj (aes(fill)) +
#     scale_fill_cmap(palette = "governance")
# OR
#   ggplot obj (aes(color)) +
#     scale_color_cmap(palette = "community")
