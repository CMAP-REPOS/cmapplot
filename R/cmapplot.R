#' cmapplot
#'
#' This package contains extra palettes, themes and geoms for \pkg{ggplot2},
#' based on Chicago Metropolitan Agency for Planning (CMAP) design guidelines.
#'
#' Detailed documentation can be viewed at
#' \url{https://cmap-repos.github.io/cmapplot}.
#'
#' Please report issues and suggest improvements at
#' \url{https://github.com/CMAP-REPOS/cmapplot/issues}.
#'
#' @name cmapplot
#' @docType package
#' @import dplyr ggplot2 graphics grDevices grid gridtext rlang scales systemfonts
#' @importFrom glue glue glue_collapse
#' @keywords internal
"_PACKAGE"


# Update fonts based on system -- *must* be done with .onLoad()
.onLoad <- function(...) {

  family <- name <- path <- NULL

  # check for Whitney
  #all_fonts <- systemfonts::system_fonts()
  all_fonts <- systemfonts::system_fonts()
  whitney_core <- all_fonts$name[all_fonts$name %in% c("Whitney-Medium", "Whitney-Book", "Whitney-Semibold")]
  assign("use_whitney", length(whitney_core) >= 3, envir = cmapplot_globals)

  if(!get("use_whitney", envir = cmapplot_globals)){

  }

  if(get("use_whitney", envir = cmapplot_globals)){
    # # Register all Whitney fonts (note: this registers italic fonts both as
    # # variants of core fonts and as standalone fonts, so there is some
    # # duplication.)
    # whitney_fonts <- select(filter(all_fonts, family == "Whitney"), name, path)
    # purrr::walk2(whitney_fonts$name, whitney_fonts$path, systemfonts::register_font)

    # Update font variables
    assign("font",
           list(strong = list(family = "Whitney-Semibold", face = "plain"),
                regular = list(family = "Whitney-Medium", face = "plain"),
                light = list(family = "Whitney-Book", face = "plain")),
           envir = cmapplot_globals)
  }

  # Load CMAP preferred default.aes (can't be done until fonts are specified)
  assign("default_aes_cmap",
         init_cmap_default_aes(),
         envir = cmapplot_globals)

  # Cache existing default.aes
  assign("default_aes_cached",
         fetch_current_default_aes(),
         envir = cmapplot_globals)
}


.onAttach <- function(...){
  if(!get("use_whitney", envir = cmapplot_globals)){
    packageStartupMessage(
      "WARNING: Whitney was not found on this machine, so CMAP theme will use your default sans-serif font"
    )
  }
}
