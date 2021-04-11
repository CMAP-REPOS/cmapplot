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
  all_fonts <- systemfonts::system_fonts()
  whitney_core <- c("Whitney-Medium", "Whitney-Book", "Whitney-Semibold")
  assign("use_whitney",
         length(all_fonts$name[all_fonts$name %in% whitney_core]) >= 3,
         envir = cmapplot_globals)

  # If on a Mac, and !use_whitney, attempt to register from user's fonts folder
  ## SHOULD THIS BE CHANGED TO ONLY IMPACT THE VM? SUCH AS USER == "runner"???
  if(.Platform$OS.type != "windows" & !get("use_whitney", envir = cmapplot_globals)){
    # attempt to register fonts in user's fonts folder
    ## SHOULD THIS BE CHANGED TO ONLY WHITNEY FONTS?
    user_dir <- paste0(Sys.getenv("HOME"), "/Library/Fonts")
    user_font_names <- sub("-Adv.otf$", "", list.files(user_dir))
    user_font_paths <- list.files(user_dir, full.names = TRUE)
    purr::walk2(user_font_names, user_font_paths, systemfonts::register_font)

    registry_fonts <- systemfonts::registry_fonts()
    assign("use_whitney",
           length(registry_fonts$name[registry_fonts$name %in% whitney_core]) >= 3,
           envir = cmapplot_globals)
  }

  # Update font names
  if(get("use_whitney", envir = cmapplot_globals)){
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
