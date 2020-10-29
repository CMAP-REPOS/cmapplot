
theme_cmap_base <- function(debug = FALSE,overrides = list()){

  # create list of plot constants, from globals unless overridden by user
  consts <- utils::modifyList(cmapplot_globals$consts, overrides)

  t <- theme(
      # element defaults
      line = element_line(linetype = "solid", colour = cmapplot_globals$colors$blackish),
      rect = if(debug){ element_rect(colour = "red", fill = NA) } else{ element_blank() },
      text = element_text(family = cmapplot_globals$font$regular$family,
                          face = cmapplot_globals$font$regular$face,
                          size = cmapplot_globals$fsize$reg,
                          color = cmapplot_globals$colors$blackish),

      # axis
      axis.title = element_blank(),
      axis.text = element_text(family = cmapplot_globals$font$light$family,
                               face = cmapplot_globals$font$light$face,
                               size = cmapplot_globals$fsize$reg),
      #axis.text.x = element_text(margin = margin(t = 5)),
      axis.ticks = element_blank(),
      axis.line = element_blank(),

      # legend
      legend.backgound = NULL,
      legend.margin = margin(
        consts$padding_legend[1]+5,
        consts$padding_legend[2],
        consts$padding_legend[3],
        consts$padding_legend[4],
        "bigpts"),
      legend.spacing.y = grid::unit(consts$margin_legend_i, "bigpts"),
      legend.key.size = grid::unit(consts$legend_key_size, "bigpts"),
      legend.text.align = 0,
      legend.title = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.box = "vertical",
      legend.box.just = "left",
      legend.box.spacing = grid::unit(consts$margin_legend_b, "bigpts"),

      # panel and plot
      panel.grid = element_blank(),
      plot.title = element_text(family = cmapplot_globals$font$strong$family,
                                face = cmapplot_globals$font$strong$face,
                                size = cmapplot_globals$fsize$big,
                                hjust = 0),
      plot.subtitle = element_blank(),
      plot.caption = element_text(family = cmapplot_globals$font$light$family,
                                  face = cmapplot_globals$font$light$face,
                                  size = cmapplot_globals$fsize$sml,
                                  hjust = 1),
      plot.margin = margin(consts$padding_plot[1] + 5,
                           consts$padding_plot[2] + 5,
                           consts$padding_plot[3] + 5,
                           consts$padding_plot[4] + 5,
                           "bigpts"),

      # strip
      strip.text = ggplot2::element_text(hjust = 0),

      complete = TRUE

    )

  ggthemes::theme_foundation() %+replace% t
}

t <- theme_cmap_base(debug = TRUE)
t$legend.background

econ_plot + labs(title = "I am a graph", caption = "source info here") + t





g + t


t2<- ggplot2:::theme_all_null() %+replace% t

t2$rect
t$plot.background

g + t2

View(t2)

g + theme_cmap_base(debug = TRUE)

g <- g + labs(title = "I am a graph", caption = "source info here")
