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

  # If font registry already contains Whitney core, set use_whitney == TRUE
  fonts_present <- systemfonts::registry_fonts() %>%
    dplyr::filter(family %in% cmapplot_globals$preferred_font) %>%
    nrow() >= 12

  assign("use_whitney",
         fonts_present,
         envir = cmapplot_globals)


  # Else, Find and register necessary Whitney variants using systemfonts (or,
  # alternatively, find them manually in ~/Library/Fonts). Then, if font
  # registry contains Whitney core, set use_whitney == TRUE.
  if(!get("use_whitney", envir = cmapplot_globals)){
    whitney_paths <- dplyr::filter(systemfonts::system_fonts(), family == "Whitney")
    whitney_paths <- whitney_paths[["path"]]

    # On some OSX systems (e.g. pkgdown GHA VM) system_fonts() cannot find fonts
    # installed in the user fonts directory. In any case where system_fonts()
    # sees no Whitney fonts, if `user_dir` exists, it too is checked for fonts.
    user_dir <- paste0(Sys.getenv("HOME"), "/Library/Fonts")
    if(length(whitney_paths) == 0 & dir.exists(user_dir)){
      whitney_paths <- list.files(user_dir, full.names = TRUE)
      whitney_paths <- grep("Whitney-", whitney_paths)
    }

    # If all Whitney fonts have been found, attempt to register preferred
    # fonts. If any registration fails, the try object should fail before
    # `use_whitney` is set to TRUE.
    if(length(whitney_paths >= 12)){
      try({

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

        packageStartupMessage(paste0(
          "cmapplot has registered the following fonts for use in this R session:\n   ",
          paste(cmapplot_globals$preferred_font, collapse = ", ")
        ))

        assign("use_whitney",
               TRUE,
               envir = cmapplot_globals)
      })
    }
  }

  # Update font names
  if(get("use_whitney", envir = cmapplot_globals)){
    assign("font",
           list(strong = list(family = cmapplot_globals$preferred_font$strong, face = "plain"),
                regular = list(family = cmapplot_globals$preferred_font$regular, face = "plain"),
                light = list(family = cmapplot_globals$preferred_font$light, face = "plain")),
           envir = cmapplot_globals)
  } else {
    packageStartupMessage(
      "cmapplot cannot locate Whitney fonts, so CMAP themes will use your default sans-serif font"
    )
  }

  # if (VERSION >= 1.4){
  #   check ragg, set ragg, notify
  # } else {
  #   message about need to update rstudio
  # }

  # Load CMAP preferred default.aes (can't be done until fonts are specified)
  assign("default_aes_cmap",
         init_cmap_default_aes(),
         envir = cmapplot_globals)

  # Cache existing default.aes
  assign("default_aes_cached",
         fetch_current_default_aes(),
         envir = cmapplot_globals)
}
