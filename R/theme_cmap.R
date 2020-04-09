#' Add CMAP theme to ggplot chart
#'
#'
#'
#' @export
theme_cmap <- function(xlab = NULL, ylab = NULL, xorigin = NULL, yorigin = NULL) {

  # Generate an explicit message to user if Whitney font family is not available
  if (!(cmapplot_globals$use_whitney)){
    message("'Whitney' font family not found. Using Calibri...")
  }

  # Generate list of elements to return.
  # The first element is the default theme.
  # Additional elements add to or overwrite the default theme based on function arguments
  elements <- list(

    # Primary theme
    ggplot2::theme(
      # Default text
      text = ggplot2::element_text(family = cmapplot_globals$font_main,
                                   face = cmapplot_globals$font_main_face,
                                   size = 12,
                                   color="#222222"),

      # Title text
      plot.title = ggplot2::element_text(family=cmapplot_globals$font_title,
                                         face=cmapplot_globals$font_title_face,
                                         size=14),

      # Text elements not displayed
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),

      #Legend format
      legend.position = "top",
      legend.text.align = 0,
      legend.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),

      #Axis format
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),

      #Grid lines
      panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
      panel.grid.major.x = ggplot2::element_blank(),

      #Blank background
      panel.background = ggplot2::element_blank(),

      #Strip background
      strip.background = ggplot2::element_rect(fill="white"),

      #Facet wrap text
      strip.text = ggplot2::element_text(size  = 12,  hjust = 0)
    ),

    # re-introduce x label if specified
    if(!is.null(xlab)){
      ggplot2::theme(axis.title.x = element_text())
    },
    if(!is.null(xlab)){
      ggplot2::xlab(xlab)
    },

    # re-introduce y label if specified
    if(!is.null(ylab)){
      ggplot2::theme(axis.title.y = element_text())
    },
    if(!is.null(ylab)){
      ggplot2::ylab(ylab)
    },

    # add x origin line if specified
    if(!is.null(xorigin)){
      ggplot2::geom_hline(yintercept = xorigin, size = 1, color = "#222222")
    },

    # add y origin line if specified
    if(!is.null(yorigin)){
      ggplot2::geom_vline(xintercept = yorigin, size = 1, color = "#222222")
    }
  )


  # filter out NA elements before returning
  return(magrittr::extract(elements, !is.na(elements)))
}





# ### TEST PLOTS ###
# ggplot(ggplot2::diamonds) +
#   geom_point(aes(x=carat, y=price, color=cut), alpha=0.25) +
#   labs(title="Diamond price by weight", x="Weight (carat)", y="Price (dollars)") +
#   theme_cmap(xorigin = 0, yorigin = 1)
#
#
#
#
# filter(traded_emp_by_race, variable %in% c("SpecializedTraded", "UnspecializedTraded")) %>%
#   ggplot(aes(x = reorder(Race, -value), y = value, fill = variable)) +
#   geom_col(position = position_stack(reverse = TRUE)) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_cmap(ylab = "Percent", xorigin = TRUE)
