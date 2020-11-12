
#' @export
fetch_cmap_geom_defaults <- function(){

  geom_changes <- tribble(
    ~geom, ~pkg,
    "Line", "ggplot2",
    "Text", "ggplot2",
    "TextLast", "cmapplot",
    "RecessionsText", "cmapplot"
  )

  map2(geom_changes$geom,
       geom_changes$pkg,
       function(geom, pkg){get(paste0("Geom", geom), as_environment(pkg))$default_aes}
  ) %>%
    set_names(geom_changes$geom)
}

#' @export
set_cmap_geom_defaults <- function(values = NULL, quietly = FALSE){

  geom_changes <- tribble(
    ~geom, ~attr,
    "Line", list(size = ggplot_size_conversion(cmapplot_globals$consts$lwd_plotline)),
    "Text", list(family = cmapplot_globals$font$strong$family,
                 fontface = ifelse(cmapplot_globals$font$strong$face == "bold", 2, 1),
                 size = cmapplot_globals$fsize$M/ggplot2::.pt, # Accounts for the fact that text is sized in mm
                 colour = cmapplot_globals$colors$blackish),
    "TextLast", list(family = cmapplot_globals$font$strong$family,
                     fontface = ifelse(cmapplot_globals$font$strong$face == "bold", 2, 1),
                     size = cmapplot_globals$fsize$M/ggplot2::.pt, # Accounts for the fact that text is sized in mm
                     colour = cmapplot_globals$colors$blackish),
    "RecessionsText", list(family = cmapplot_globals$font$regular$family,
                           fontface = ifelse(cmapplot_globals$font$regular$face == "bold", 2, 1),
                           size = cmapplot_globals$fsize$S/ggplot2::.pt, # Accounts for the fact that text is sized in mm
                           colour = cmapplot_globals$colors$blackish)
  )

  if(is.null(values)){
    values <- geom_changes$attr
  }

  walk2(geom_changes$geom,
        values,
        ggplot2::update_geom_defaults
  )

  if(!quietly){
    message(
      paste("Geom defaults overridden in the current session for:",
            paste(geom_changes$geom, collapse = ", ")
      )
    )
  }

}
