# this is a complete theme built from scratch.
# it is modeled off of `ggplot2::theme_grey()`
theme_cmap_base <- function(consts = cmapplot_globals$consts,
                            debug = FALSE,
                            right_margin = 20
                            ) {

  # The half-line sets up the basic vertical rhythm of the theme.
  half_line <- cmapplot_globals$fsize$reg / 2

  t <- theme(

    # building blocks
    line = element_line(
      colour = cmapplot_globals$colors$blackish,
      size = consts$lwd_gridline,
      linetype = 1, lineend = "butt",
      inherit.blank = TRUE),

    rect = element_rect(
      fill = NA, colour = ifelse(debug, "red", NA),
      size = 0.5, linetype = 1,
      inherit.blank = TRUE),

    text = element_text(
      family = cmapplot_globals$font$regular$family,
      face = cmapplot_globals$fgiont$regular$face,
      size = cmapplot_globals$fsize$reg,
      color = cmapplot_globals$colors$blackish,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = margin(), debug = debug,
      inherit.blank = TRUE),

    # axis
    axis.line =          element_blank(),
    axis.line.x =        NULL,
    axis.line.y =        NULL,
    axis.text =          element_text(family = cmapplot_globals$font$light$family,
                                      face = cmapplot_globals$font$light$face,
                                      size = cmapplot_globals$fsize$reg),
    axis.text.x =        element_text(margin = margin(t = half_line / 2), vjust = 1),
    axis.text.x.top =    element_text(margin = margin(b = half_line / 2), vjust = 0),
    axis.text.y =        element_text(margin = margin(r = half_line / 2), hjust = 1),
    axis.text.y.right =  element_text(margin = margin(l = half_line / 2), hjust = 0),
    axis.ticks =         element_blank(),
    axis.ticks.length =  unit(0, "pt"), # determines space btwn axis text & panel even when ticks are off
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title =         element_blank(),

    legend.background =  NULL,
    legend.spacing.x =   grid::unit(half_line, "pt"),
    legend.spacing.y =   grid::unit(consts$margin_legend_i, "bigpts"),
    legend.margin =      margin(l = 0 - half_line),
    legend.key =         element_blank(),
    legend.key.size =    grid::unit(cmapplot_globals$fsize$reg, "pt"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        NULL,
    legend.text.align =  0,
    legend.title =       element_blank(),
    legend.position =    "top",
    legend.direction =   "horizontal",
    legend.justification = "left",
    legend.box =         "vertical",
    legend.box.margin =  margin(0, 0, 0, 0),
    legend.box.background = element_rect(colour = ifelse(debug, "red", NA)), # this should inherit from rect when NULL but it doesnt
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
    plot.margin = margin(3, 3 + right_margin, 3, 3),

    complete = TRUE
  )

  # make sure all elements are set to NULL if not explicitly defined.
  theme_gray() %+replace% t
}


# econ_plot + theme(axis.title.x = element_text(color = "red", inherit.blank = TRUE)) + theme(axis.title = element_blank())

# this line of code is important. inherit_blanks allow parents to null out children. Lets put
# all possible additions in this base, and then zero them out selectively in the modified theme.



# axis.title.x =       element_text(margin = margin(t = half_line / 2), vjust = 1, inherit.blank = FALSE),
# axis.title.x.top =   element_text(margin = margin(b = half_line / 2), vjust = 0),
# axis.title.y =       element_text(angle = 90, margin = margin(r = half_line / 2), vjust = 1),
# axis.title.y.right = element_text(angle = -90,margin = margin(l = half_line / 2), vjust = 0),

#View(theme_cmap_base())

econ_plot <- ggplot(data = cmapplot::cluster_jobchange,
                    mapping = aes(
                      x = reorder(name, jobchange),
                      y = jobchange,
                      fill = category,
                      alpha = assessment)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)
