# Initialize environment for cmapplot global variables
cmapplot_globals <- new.env(parent = emptyenv())

# Establish names of preferred fonts
cmapplot_globals$preferred_font <- list(
  strong = "Whitney Semibold",
  regular = "Whitney Medium",
  light = "Whitney Book"
)

# Set up default font handling
# (Note: overridden by .onLoad() if Whitney is available)
cmapplot_globals$use_whitney <- FALSE
cmapplot_globals$font <- list(
  strong = list(family = "sans", face = "bold"),
  regular = list(family = "sans", face = "plain"),
  light = list(family = "sans", face = "plain"))

# Set common font sizes (pts)
cmapplot_globals$fsize <- list(
  S = 11,
  M = 14,
  L = 17
)

# Define CMAP colors
cmapplot_globals$colors <- list(
  blackish = "#222222"
)

#' @importFrom tibble tribble
#'
# Define CMAP palettes
cmapplot_globals$palettes <- tibble::tribble(
  ~name, ~type, ~colors,
  # discrete
  "prosperity",  "discrete", c("#662f00", "#e5d072", "#44008c", "#c8e572", "#c9a7ef"),
  "community",   "discrete", c("#cc5f00", "#006b8c", "#e5a872", "#d2efa7", "#662f00"),
  "environment", "discrete", c("#00665c", "#b7e572", "#3f0030",  "#36d8ca", "#006b8c"),
  "governance",  "discrete", c("#006b8c", "#efa7a7", "#8c4100", "#00303f", "#cca600", "#a7efe8"),
  "mobility",    "discrete", c("#8c0000", "#e5bd72", "#a7efe8", "#6d8692", "#0084ac", "#efa7a7"),
  "legislation", "discrete", c("#00becc", "#cc5f00", "#3f0e00", "#cca600", "#003f8c", "#67ac00"),
  "friday",      "discrete", c("#00093f", "#ac8c00", "#475c66", "#e5d072", "#b5c1c8", "#006b8c"),
  "race",        "discrete", c(white    = "#75a5d8",
                               black    = "#84c87e",
                               hispanic = "#d8ba39",
                               asian    = "#e77272",
                               other    = "#607b88"),

  # Single-hue sequential
  "reds",      "sequential", c("#efa7a7", "#e57272", "#d83636", "#cc0000", "#ac0000", "#8c0000", "#660000"),
  "oranges",   "sequential", c("#efc9a7", "#e5a872", "#d88236", "#cc5f00", "#ac5000", "#8c4100", "#662f00"),
  "yellows",   "sequential", c("#efe1a7", "#e5d072", "#d8ba36", "#cca600", "#ac8c00", "#8c7200", "#665300"),
  "greens",    "sequential", c("#d2efa7", "#b7e572", "#97d836", "#7acc00", "#67ac00", "#548c00", "#3d6600"),
  "teals",     "sequential", c("#a7efe8", "#72e5db", "#36d8ca", "#00ccb8", "#00ac9c", "#008c7e", "#00665c"),
  "blues",     "sequential", c("#a7deef", "#72cae5", "#36b2d8", "#009ccc", "#0084ac", "#006b8c", "#004e66"),
  "purples",   "sequential", c("#c9a7ef", "#aa72e5", "#8436d8", "#6300cc", "#5300ac", "#44008c", "#310066"),
  "grays",     "sequential", c("#e3e8eb", "#dbe1e4", "#d2d9de", "#c3cdd3", "#b5c1c8", "#a7b5be", "#9daab3",
                               "#8a9ea8", "#7b929d", "#6d8692", "#5e7a87", "#475c66", "#2f3d44"),

  # Multi-hue sequential
  "yellow_orange_red", "sequential", c("#efe1a7", "#e5bd72", "#d88236", "#cc3000", "#8c0000"),
  "green_teal_blue",   "sequential", c("#d2efa7", "#72e584", "#00ccb8", "#00838c", "#004e66"),
  "orange_red",        "sequential", c("#efc9a7", "#e59a72", "#cc3000", "#8c1000", "#660000"),
  "yellow_orange",     "sequential", c("#efe1a7", "#e5c672", "#cc8200", "#8c4100", "#662f00"),
  "yellow_green",      "sequential", c("#f8f4df", "#e5e172", "#b4cc00", "#698c00", "#3d6600"),
  "green_teal",        "sequential", c("#d2efa7", "#8de572", "#00cc1f", "#008c4b", "#00665c"),
  "teal_blue",         "sequential", c("#a7efe8", "#72e5e3", "#00becc", "#00778c", "#004e66"),
  "red_purple",        "sequential", c("#efa7a7", "#e5729e", "#cc0099", "#77008c", "#310066"),

  # Multi-hue diverging
  "yellow_purple", "divergent", c("#8c7200", "#ac8c00", "#cca600", "#d8ba36", "#e5d072", "#e3e8eb",
                                  "#aa72e5", "#8436d8", "#6300cc", "#5300ac", "#44008c"),
  "orange_blue",   "divergent", c("#8c4100", "#ac5000", "#cc5f00", "#d88236", "#e5a872", "#e3e8eb",
                                  "#72cae5", "#36b2d8", "#009ccc", "#0084ac", "#006b8c"),
  "red_teal",      "divergent", c("#660000", "#8c0000", "#cc0000", "#d83636", "#e57272", "#e3e8eb",
                                  "#72e5db", "#36d8ca", "#00ccb8", "#00ac9c", "#008c7e"),
  "purple_green",  "divergent", c("#44008c", "#5300ac", "#6300cc", "#8436d8", "#aa72e5", "#e3e8eb",
                                  "#b7e572", "#97d836", "#7acc00", "#67ac00", "#548c00"),
  "blue_yellow",   "divergent", c("#006b8c", "#0084ac", "#009ccc", "#36b2d8", "#72cae5", "#e3e8eb",
                                  "#e5d072", "#d8ba36", "#cca600", "#ac8c00", "#8c7200"),
  "teal_orange",   "divergent", c("#008c7e", "#00ac9c", "#00ccb8", "#36d8ca", "#72e5db", "#e3e8eb",
                                  "#e5a872", "#d88236", "#cc5f00", "#ac5000", "#8c4100"),
  "green_red",     "divergent", c("#548c00", "#67ac00", "#7acc00", "#97d836", "#b7e572", "#e3e8eb",
                                  "#e57272", "#d83636", "#cc0000", "#8c0000", "#660000")
)

# Establish plotting constants in bigpts (1/72 of inch)
cmapplot_globals$consts <- list(
  lwd_gridline = 0.3,
  lwd_strongline = 1,
  lwd_plotline = 3,
  lwd_topline = 2,
  length_ticks = 7,
  margin_topline_t = 5,
  margin_title_t = 5,
  margin_title_b = 5,
  margin_caption_b = 5,
  margin_legend_t = 5,
  margin_legend_i = 8,
  margin_legend_b = 10,
  margin_plot_b = 5,
  margin_sidebar_l = 2,
  margin_plot_l = 10,
  margin_plot_r = 10,
  margin_panel_r = 10,
  leading_title = 1,
  leading_caption = 1
)

# List of all geoms whose aesthetics will be modified by cmapplot
cmapplot_globals$geoms_that_change <- c(
  "Label",
  "Line",
  "Text",
  "TextLast",
  "PointLast",
  "RecessionsText"
)


#'The cmapplot_globals environment
#'
#'The \code{cmapplot_globals} environment contains a list of predefined
#'variables for use by the cmapplot package and its users. It includes commonly
#'used colors, font and font size specifications, and a list of constants which
#'aid in drawing cmap-themed plots. It cannot be accessed directly, but the
#'helper functions described here provide the user access if needed.
#'
#'@section Plot Constants: The primary portion of these global variables of
#'  interest to the user is \code{cmapplot_globals$consts}, a list of default
#'  constants that set certain plot aesthetics. Units of all plot constants are
#'  "bigpts": 1/72 of an inch. Most plot constants are invoked (and can be
#'  overridden) in \code{\link{finalize_plot}}: these are marked below with an
#'  \strong{F}. Some are used/can be overridden in \code{\link{theme_cmap}}:
#'  these are marked with \strong{T}.
#'
#'  \itemize{ \item \code{lwd_strongline}: This stronger-width line is drawn
#'  vertically or horizontally with the \code{hline, vline} args of
#'  \code{theme_cmap()}. \strong{(T)} \item \code{lwd_gridline}: This
#'  thinner-width line is drawn vertically or horizontally with the
#'  \code{gridlines, axislines} args of \code{theme_cmap()}. \strong{(T)} \item
#'  \code{lwd_plotline}: The width of any lines drawn by geoms in the plot (e.g.
#'  \code{geom_line}) but not explicitly sized by the geom's aesthetic.
#'  Implemented by \code{finalize_plot} or by \code{apply_cmap_default_aes} but
#'  not overridable in either context. (Modify by setting the size explicitly in
#'  the geom, but see \code{gg_lwd_convert} first.) \item \code{lwd_topline}:
#'  The width of the line above the plot. \strong{(F)} \item
#'  \code{length_ticks}: The length of the axis ticks (if shown). \strong{(T)}
#'  \item \code{margin_topline_t}: The margin between the top edge of the image
#'  and the top line. \strong{(F)} \item \code{margin_title_t}: The margin
#'  between the top line and the title. \strong{(F)} \item
#'  \code{margin_title_b}: The margin between the title and the caption when
#'  both are drawn in the sidebar. \strong{(F)} \item \code{margin_caption_b}:
#'  The margin between the bottom of the caption and the bottom edge of the
#'  image. \strong{(F)} \item \code{margin_legend_t}: The margin between the top
#'  line and the plot box (i.e., the top of the legend). \strong{(F)} \item
#'  \code{margin_legend_i}: The margin between legends (this only applies in
#'  plots with two or more legends and does not affect legend spacing on plots
#'  with single legends that have multiple rows). \strong{(T, F)} \item
#'  \code{margin_legend_b}: The margin between the bottom of the legend and the
#'  rest of the plot. \strong{(T, F)} \item \code{margin_plot_b}: The margin
#'  between the bottom of the plot and the bottom edge of the image (or top of
#'  caption). \strong{(F)} \item \code{margin_sidebar_l}: The margin between the
#'  left edge of the image and the title and caption, when the sidebar exists.
#'  Deducted from \code{title_width}. \strong{(F)} \item \code{margin_plot_l}:
#'  The margin between the left edge of the plot and the sodebar. \strong{(F)}
#'  \item \code{margin_plot_r}: The margin between the right edge of the plot
#'  and the edge of the image. \strong{(F)} \item \code{margin_panel_r}: Padding
#'  between the plot and its right-hand drawing extent. Override this based on
#'  space needed for x axis labels. \strong{(T)} \item \code{leading_title}:
#'  Text leading for Title text. \strong{(F)} \item \code{leading_caption}: Text
#'  leading for Caption text. \strong{(F)} }
#'
#' @aliases cmapplot_globals
#'
#' @describeIn get_cmapplot_globals Get the entire environment as a list.
#'
#' @export
get_cmapplot_globals <- function(){
  as.list(cmapplot_globals)
}

#' Get a value from the cmapplot_globals environment
#'
#' @examples
#'
#' # These are the same:
#' get_cmapplot_global("consts$lwd_gridline")
#' get_cmapplot_global("consts", "lwd_gridline")
#'
#' @describeIn get_cmapplot_globals Get a specific global value
#'
#' @export
get_cmapplot_global <- function(...){

  # establish vector of sublocations
  names <- unlist(stringr::str_split(c(...), "\\$"))

  # fetch the top-level element from the list
  var <- get(names[1], envir = cmapplot_globals)

  # recurse over additional names to extract the right value
  for(i in seq_along(names[-1])+1){
    var <- var[[names[i]]]
  }

  if(is.null(var)){
    stop(paste0("object '", paste(names, collapse = "$"), "' not found"))
  }

  return(var)

}


#' Set a value in the cmapplot_globals environment
#'
#' @param value the value to be set
#'
#' @param ... The path to the variable within \code{cmapplot_globals} to be
#'   get/set. The function willparse \code{$}, or recursive list elements can be
#'   split over multiple arguments (e.g. \code{"font$strong$family"} is
#'   equivalent to \code{"font", "strong", "family"}).
#'
#' @param quietly suppress confirmatory messages
#'
#' @examples
#'
#' # Globals can be modified if needed
#' set_cmapplot_global(5, "consts$lwd_gridline")
#' get_cmapplot_global("consts$lwd_gridline")
#'
#' @describeIn get_cmapplot_globals Set a specific global value
#'
#' @export
set_cmapplot_global <- function(value, ..., quietly = FALSE){

  # do a get of the specific attribute to make sure it exists.
  # this will error if the path is null
  p <- get_cmapplot_global(...)

  # establish vector of sublocations
  names <- unlist(stringr::str_split(c(...), "\\$"))

  # get the top-level item
  item <- get_cmapplot_global(names[1])

  # build a string to evaluate that modifies some element of the item.
  str <- paste0(
    "item",
    ifelse(length(names) > 1, paste0("$", paste(names[-1], collapse = "$")), ""),
    " <- ",
    ifelse(is.character(value), paste0("'", value, "'"), value)
  )

  # replace the specific item by evaluating the string
  eval(parse(text = str))

  # and replace the top level item in the globals env
  assign(names[1], item, envir = cmapplot_globals)

  # report
  if(!quietly){
    cat(paste0(
      "Item:      ", paste(names, collapse = "$"), "\n",
      "Old value: ", ifelse(is.character(p), paste0("'", p, "'"), p), "\n",
      "New value: ", ifelse(is.character(value), paste0("'", value, "'"), value)
    ))
  }
  invisible()
}
