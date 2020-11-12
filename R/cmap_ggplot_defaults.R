

#' @export
fetch_cmap_gg_defaults <- function(){
  map2(cmapplot_globals$geom_changes$geom,
       cmapplot_globals$geom_changes$pkg,
       function(geom, pkg){get(paste0("Geom", geom), as_environment(pkg))$default_aes}
  ) %>%
    set_names(cmapplot_globals$geom_changes$geom)
}

#' @export
set_cmap_gg_defaults <- function(values = NULL, quietly = FALSE){
  if(is.null(values)){
    values <- cmapplot_globals$geom_changes$attr
  }

  walk2(cmapplot_globals$geom_changes$geom,
        values,
        ggplot2::update_geom_defaults
  )

  if(!quietly){
    message(
      paste("Geom defaults overridden in the current session for:",
            paste(cmapplot_globals$geom_changes$geom, collapse = ", ")
      )
    )
  }

}
