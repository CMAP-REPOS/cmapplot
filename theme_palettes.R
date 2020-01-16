# G Ritzenthaler 01-13-2020

### Wes Anderson package method ###############################################
# wesanderson source code:
    # https://github.com/karthik/wesanderson/blob/master/R/colors.R

cmap_palettes <- list(
    prosperity = c("#CCA600", "#E5D072", "#F8F4DF", "#C9A7EF", "#8C4100"),
    community = c("#CC5F00", "#E5A872", "#F8EBDF", "#D2EFA7", "#006B8C"),
    environment = c("#7ACC00", "#B7E572", "#D2EFA7", "#A7DEEF", "#43008C"),
    governance = c("#009CCC", "#72CAE5", "#DFF2F8", "#EFC8A7", "#8C7200"),
    mobility = c("#6200CC", "#AA72E5", "#EBDFF8", "#EFE1A7", "#548C00")
    )


cmap_palette <- function(name, n, type = c("discrete", "continuous")) {

    type <- match.arg(type)

    pal <- cmap_palettes[[name]]

    if (is.null(pal))
        stop("Palette not found.")

    if (missing(n)) {
        n <- length(pal)
    }

    if (type=="discrete" && n > length(pal)) {
        stop("Number of requested colors greater than what palette can offer")
    }

    out <- switch(type,
                  continuous = colorRampPalette(pal)(n),
                  discrete = pal[1:n]
                  )

    structure(out, class = "palette", name = name)
}

###############################################################################

# So far this only works with discrete data, up to 5 categories

# Use:
#   ggplot +
#       scale_fill_manual(values = cmap_palette("mobility"))


# Print palette for reference
    # from ochRe package:
        # https://github.com/ropenscilabs/ochRe/blob/master/R/utils.R
viz_palette <- function(pal, ttl = deparse(substitute(pal)), num = length(pal)) {
    if(num <= 0)
        stop("'num' should be > 0")
    pal_func <- colorRampPalette(pal)
    image(seq_len(num), 1, as.matrix(seq_len(num)), col = pal_func(num),
          main = paste0(ttl, " (", length(pal), " colours in palette, ", num, " displayed)"),
          xlab = "", ylab = "", xaxt = "n", yaxt = "n",  bty = "n")
}

