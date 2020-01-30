###############################################################################

# questions:

# How to refer to other packages?
  # for code credits?
  # for copied functions? (copy code or call package in front of function?)

# Add more discrete palettes?

# Consistency for group colors across multiple charts within report

# Colors for peer city comparisons: vibrant blue for Chicago, grey for all else

# Add extra arguments option? (...)

# Removed alpha option--was changing colors away from the approved color list

###############################################################################


# Modeled after ochRe palette package
    # https://github.com/ropenscilabs/ochRe/blob/master/R

# Palettes list
# re-ordered to separate similar colors
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
cmap_pal_discrete <- function(palette = "prosperity", reverse = FALSE) {
    
    pal <- cmap_palettes[[palette]]
    
    if (reverse) {
        pal <- rev(pal)
    }
    
    return(colorRampPalette(pal))
}


# scale fill function
cmap_fill_discrete <- function(palette = "prosperity", 
                               reverse = FALSE) {

    discrete_scale("fill", "cmap_palettes",
                   palette = cmap_pal_discrete(palette, reverse = reverse))
}


# scale color function
cmap_color_discrete <- function(palette = "prosperity",
                                reverse = FALSE) {
    
    discrete_scale("colour", "cmap_palettes",
                   palette = cmap_pal_discrete(palette, reverse = reverse))
}

# catch spelling difference
cmap_colour_discrete <- cmap_color_discrete


# To use:

#   ggplot obj (aes(fill)) +
#     cmap_fill_discrete(palette = "governance")
# OR
#   ggplot obj (aes(color)) +
#     cmap_colour_discrete(palette = "community")
