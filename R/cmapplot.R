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
  check_for_fonts(set_global = TRUE)

  # Else, Find and register necessary whitney varients using systemfonts (or,
  # alternatively, find them manually in ~/Library/Fonts). Then, if font
  # registry contains whitney core, set use_whitney == TRUE.
  #
  # (This is necessary because R looks up font by "family" with only basic
  # variation (bold, italic, etc), so fonts like "Whitney-Book" are inaccessible
  # by default.)
  if(!get("use_whitney", envir = cmapplot_globals)){
    packageStartupMessage("Attempting to register Whitney fonts...")
    message("Attempting to register Whitney fonts...")

    whitney_fonts <- systemfonts::system_fonts() %>%
      dplyr::filter(family == "Whitney") %>%
      dplyr::select(name, path)

    # On some OSX systems (e.g. pkgdown GHA VM) system_fonts() cannot find fonts
    # installed in the user fonts directory. In any case where system_fonts()
    # sees no Whitney fonts, if `user_dir` exists it too is checked for fonts.
    user_dir <- paste0(Sys.getenv("HOME"), "/Library/Fonts")
    if(nrow(whitney_fonts == 0) & dir.exists(user_dir)){
      whitney_fonts <- list.files(user_dir, full.names = TRUE) %>%
        as.data.frame() %>%
        rlang::set_names("path") %>%
        dplyr::filter(stringr::str_detect(path, "Whitney-")) %>%  # Hope "Whitney-" is not in username!
        dplyr::mutate(name = stringr::str_extract(path, "Whitney-[:alpha:]*(?=-Adv.otf$)"))

      message(paste(whitney_fonts, collapse = "\n"))
    }

    # register preferred strong font (Whitney Semibold), with variants
    systemfonts::register_font(
      name = cmapplot_globals$preferred_font$strong,
      plain = find_path("Whitney-Semibold", whitney_fonts),
      bold = find_path("Whitney-Black", whitney_fonts),
      italic = find_path("Whitney-SemiboldItalic", whitney_fonts),
      bolditalic = find_path("Whitney-BlackItalic", whitney_fonts)
    )

    # register preferred regular font (Whitney Medium), with variants
    systemfonts::register_font(
      name = cmapplot_globals$preferred_font$regular,
      plain = find_path("Whitney-Medium", whitney_fonts),
      bold = find_path("Whitney-Bold", whitney_fonts),
      italic = find_path("Whitney-MediumItalic", whitney_fonts),
      bolditalic = find_path("Whitney-BoldItalic", whitney_fonts)
    )

    # register preferred light font (Whitney Book), with variants
    systemfonts::register_font(
      name = cmapplot_globals$preferred_font$light,
      plain = find_path("Whitney-Book", whitney_fonts),
      bold = find_path("Whitney-Semibold", whitney_fonts),
      italic = find_path("Whitney-BookItalic", whitney_fonts),
      bolditalic = find_path("Whitney-SemiboldItalic", whitney_fonts)
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
