
# this is a complete theme built from scratch.
# it is modeled off of `ggplot2::theme_grey()`
theme_cmap_base <- function(debug = FALSE) {

  # The half-line (base-fontsize / 2) sets up the basic vertical
  # rhythm of the theme. Most margins will be set to this value.
  # However, when we work with relative sizes, we may want to multiply
  # `half_line` with the appropriate relative size. This applies in
  # particular for axis tick sizes. And also, for axis ticks and
  # axis titles, `half_size` is too large a distance, and we use `half_size/2`
  # instead.
  half_line <- cmapplot_globals$fsize$reg / 2

  consts <- cmapplot_globals$consts

  t <- theme(

    # building blocks
    line = element_line(
      colour = cmapplot_globals$colors$blackish,
      size = consts$lwd_gridline,
      linetype = 1, lineend = "butt"),

    rect = element_rect(
      fill = NA, colour = if(debug){"red"}else{NA},
      size = 0.5, linetype = 1),

    text = element_text(
      family = cmapplot_globals$font$regular$family,
      face = cmapplot_globals$font$regular$face,
      size = cmapplot_globals$fsize$reg,
      color = cmapplot_globals$colors$blackish,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = margin(), debug = FALSE),

    # axis
    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(family = cmapplot_globals$font$light$family,
                                      face = cmapplot_globals$font$light$face,
                                      size = cmapplot_globals$fsize$reg),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks =         element_blank(),
    axis.ticks.length =  unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title =         element_blank(),

    legend.background =  NULL,
    legend.spacing.x =   unit(2 * half_line, "pt"),
    legend.spacing.y =   grid::unit(consts$margin_legend_i, "bigpts"),
    legend.margin = margin(0, 0, 0, 0 - cmapplot_globals$fsize$reg, "pt"),
    legend.key =         element_blank(),
    legend.key.size =    grid::unit(consts$legend_key_size, "bigpts"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        NULL,
    legend.text.align =  0,
    legend.title =       element_blank(),
    legend.position =    "top",
    legend.direction =   "horizontal",
    legend.justification = "left",
    legend.box =         "vertical",
    legend.box.margin =  margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_rect(colour = if(debug){"red"}else{NA}), # this should inherit from rect when NULL but it doesnt
    legend.box.just =    "left",
    legend.box.spacing = grid::unit(consts$margin_legend_b, "bigpts"),

    panel.background =   NULL,
    panel.border =       element_blank(),
    panel.grid =         element_blank(),
    panel.spacing =      unit(half_line, "pt"),
    panel.spacing.x =    NULL,
    panel.spacing.y =    NULL,
    panel.ontop    =     FALSE,

    strip.background =   NULL,
    strip.text =         element_text(hjust = 0),
    strip.text.x =       NULL,
    strip.text.y =       element_text(angle = -90),
    strip.text.y.left =  element_text(angle = 90),
    strip.placement =    "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),


    plot.background =    element_blank(),
    plot.title =         element_text(family = cmapplot_globals$font$strong$family,
                                      face = cmapplot_globals$font$strong$face,
                                      size = cmapplot_globals$fsize$big,
                                      hjust = 0, vjust = 1,
                                      margin = margin(b = half_line)),
    plot.title.position = "panel",
    plot.subtitle =      element_blank(),
    plot.caption =       element_text(family = cmapplot_globals$font$light$family,
                                      face = cmapplot_globals$font$light$face,
                                      size = cmapplot_globals$fsize$sml,
                                      hjust = 1, vjust = 1,
                                      margin = margin(t = half_line)),
    plot.caption.position = "panel",
    plot.tag = element_blank(),
    plot.margin = margin(consts$padding_plot[1] + 5,
                         consts$padding_plot[2] + 20,
                         consts$padding_plot[3] + 5,
                         consts$padding_plot[4] + 5,
                         "bigpts"),

    complete = TRUE
  )

  # make sure all elements are set to NULL if not explicitly defined.
  #ggplot2:::theme_all_null() %+replace% t
  theme_gray() %+replace% t
}


# axis.title.x =       element_text(margin = margin(t = half_line / 2), vjust = 1, inherit.blank = FALSE),
# axis.title.x.top =   element_text(margin = margin(b = half_line / 2), vjust = 0),
# axis.title.y =       element_text(angle = 90, margin = margin(r = half_line / 2), vjust = 1),
# axis.title.y.right = element_text(angle = -90,margin = margin(l = half_line / 2), vjust = 0),

#View(theme_cmap_base())

# econ_plot <- ggplot(data = cluster_jobchange,
#                     mapping = aes(
#                       x = reorder(name, jobchange),
#                       y = jobchange,
#                       fill = category,
#                       alpha = assessment)) +
#   geom_col() +
#   coord_flip() +
#   scale_y_continuous(labels = scales::comma)
#
#econ_plot + facet_wrap("category") + labs(title = "I am a graph", caption = "source info here") + theme_cmap_base(debug = TRUE) + theme(axis.title.x = element_text())

# exprt using GUI, Cairo drivers

# legend text copies in as Calibri 14
# title text copies in as Calibri-Bold, Bold, 17
# caption copies in as CairoFont 11
# axis copies in as CairoFont 14
