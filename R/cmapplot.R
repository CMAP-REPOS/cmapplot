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

  # If font registry already contains whitney core, set use_whitney == TRUE
  fonts_present <- systemfonts::registry_fonts() %>%
    dplyr::filter(family %in% cmapplot_globals$preferred_font) %>%
    nrow() >= 12

  assign("use_whitney",
         fonts_present,
         envir = cmapplot_globals)


  # Else, Find and register necessary whitney varients using systemfonts (or,
  # alternatively, find them manually in ~/Library/Fonts). Then, if font
  # registry contains whitney core, set use_whitney == TRUE.
  #
  # (This is necessary because R looks up font by "family" with only basic
  # variation (bold, italic, etc), so fonts like "Whitney-Book" are inaccessible
  # by default.)
  if(!get("use_whitney", envir = cmapplot_globals)){
    whitney_paths <- systemfonts::system_fonts() %>%
      dplyr::filter(family == "Whitney") %>%
      .[["path"]]

    # On some OSX systems (e.g. pkgdown GHA VM) system_fonts() cannot find fonts
    # installed in the user fonts directory. In any case where system_fonts()
    # sees no Whitney fonts, if `user_dir` exists, it too is checked for fonts.
    user_dir <- paste0(Sys.getenv("HOME"), "/Library/Fonts")
    if(length(whitney_paths) == 0 & dir.exists(user_dir)){
      warning(paste("alternative search in", user_dir))
      whitney_paths <- list.files(user_dir, full.names = TRUE) %>%
        .[grepl("Whitney-", .)]
    }

    warning(paste(whitney_paths, collapse = "\n"))

    # register preferred strong font (Whitney Semibold), with variants
    systemfonts::register_font(
      name = cmapplot_globals$preferred_font$strong,
      plain = find_path("Whitney-Semibold-Adv", whitney_paths),
      bold = find_path("Whitney-Black-Adv", whitney_paths),
      italic = find_path("Whitney-SemiboldItal-Adv", whitney_paths),
      bolditalic = find_path("Whitney-BlackItal-Adv", whitney_paths)
    )

    # register preferred regular font (Whitney Medium), with variants
    systemfonts::register_font(
      name = cmapplot_globals$preferred_font$regular,
      plain = find_path("Whitney-Medium-Adv", whitney_paths),
      bold = find_path("Whitney-Bold-Adv", whitney_paths),
      italic = find_path("Whitney-MediumItal-Adv", whitney_paths),
      bolditalic = find_path("Whitney-BoldItal-Adv", whitney_paths)
    )

    # register preferred light font (Whitney Book), with variants
    systemfonts::register_font(
      name = cmapplot_globals$preferred_font$light,
      plain = find_path("Whitney-Book-Adv", whitney_paths),
      bold = find_path("Whitney-Semibold-Adv", whitney_paths),
      italic = find_path("Whitney-BookItal-Adv", whitney_paths),
      bolditalic = find_path("Whitney-SemiboldItal-Adv", whitney_paths)
    )

    check_for_fonts(set_global = TRUE)
  }

  # Update font names
  if(get("use_whitney", envir = cmapplot_globals)){
    assign("font",
           list(strong = list(family = cmapplot_globals$preferred_font$strong, face = "plain"),
                regular = list(family = cmapplot_globals$preferred_font$regular, face = "plain"),
                light = list(family = cmapplot_globals$preferred_font$light, face = "plain")),
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
