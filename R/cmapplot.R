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

  # if font registry already contains whitney core, set use_whitney == TRUE
  check_for_whitney_core(set_global = TRUE)

  # else, register all whitney fonts in systemfonts. Then, if font registry
  # contains whitney core, set use_whitney == TRUE
  #
  # (This is necessary because R looks up font by "family" with only basic
  # variation (bold, italic, etc), so fonts like "Whitney-Book" are inaccessible
  # by default. Font registration allows us to use the font's "name" as it's
  # "family" so R can identify it.)
  if(!get("use_whitney", envir = cmapplot_globals)){

    whitney_fonts <- systemfonts::system_fonts() %>%
      dplyr::filter(family == "Whitney") %>%
      dplyr::select(name, path)

    purrr::walk2(whitney_fonts$name, whitney_fonts$path, systemfonts::register_font)

    check_for_whitney_core(set_global = TRUE)
  }

  # else, if not on Windows, check for /Library/Fonts directory.
  # register all Whitney fonts in this folder.
  # If font registry contains whitney core, set use_whitney == TRUE
  if(!get("use_whitney", envir = cmapplot_globals) & .Platform$OS.type != "windows"){

    user_dir <- paste0(Sys.getenv("HOME"), "/Library/Fonts")

    if(dir.exists(user_dir)){
      whitney_fonts <- list.files(user_dir, full.names = TRUE) %>%
        as.data.frame() %>%
        rlang::set_names("path") %>%
        dplyr::filter(stringr::str_detect(path, "Whitney")) %>% # Will error for username of "Whitney"
        dplyr::mutate(name = stringr::str_extract(path, "Whitney-[:alpha:]*(?=-Adv.otf$)"))

      purrr::walk2(whitney_fonts$name, whitney_fonts$path, systemfonts::register_font)

      check_for_whitney_core(set_global = TRUE)
    }
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
