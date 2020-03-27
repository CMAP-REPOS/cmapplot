#' Add CMAP theme to ggplot chart
#'
#' @export
theme_cmap <- function() {
  theme(

    # Text format:
    # This sets the default font family
    text = element_text(family=cmapplot_globals$font_main),

    # This sets the font, size, type and colour of text for the chart's title
    plot.title = element_text(family=cmapplot_globals$font_title,
                              face=cmapplot_globals$font_title_face,
                              size=28,
                              color="#222222"),

    # This sets the font, size, type and colour of text for the chart's subtitle, as well
    #   as setting a margin between the title and the subtitle
    plot.subtitle = element_text(family=cmapplot_globals$font_title,
                                 face=cmapplot_globals$font_title_face,
                                 size=22,
                                 margin=margin(9,0,9,0)),

    plot.caption = element_blank(),
    # This leaves the caption text element empty, because it is set elsewhere in the finalise
    #   plot function

    # Legend format
    # This sets the position and alignment of the legend, removes a title and backround for it
    #   and sets the requirements for any text within the legend. The legend may often need some
    #   more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",

    legend.text.align = 0,

    legend.background = element_blank(),

    legend.title = element_blank(),

    legend.key = element_blank(),

    legend.text = element_text(family=cmapplot_globals$font_main,
                               face=cmapplot_globals$font_main_face,
                               size=18,
                               color="#222222"),

    # Axis format
    # This sets the text font, size and colour for the axis test, as well as setting the margins
    #   and removes lines and ticks. In some cases, axis lines and axis ticks are things we would
    #   want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = element_blank(),

    axis.text = element_text(family=cmapplot_globals$font_main,
                             face=cmapplot_globals$font_main_face,
                             size=18,
                             color="#222222"),

    axis.text.x = element_text(margin=margin(5, b = 10)),

    axis.text.y = element_text(vjust = -0.25,
                               hjust = 0,
                               margin = margin(r = -27.5)),

    # scale_y_continuous(labels = scales::percent),

    axis.ticks.x = element_blank(),

    axis.ticks.y = element_line(color = "black",
                                size = .3),

    axis.ticks.length = unit(27.5, "points"),

    axis.line = element_blank(),

    # Grid lines
    # This removes all minor gridlines and adds major y gridlines. In many cases you will want
    #   to change this to remove y gridlines and add x gridlines. The cookbook shows you examples
    #   for doing so

    panel.grid.major.y = element_line(color="black",
                                      size = .3),

    panel.grid.minor.y = element_blank(),

    panel.grid.major.x = element_blank(),

    panel.grid.minor.x = element_blank(),

    # Blank background
    # This sets the panel background as blank, removing the standard grey ggplot background colour
    #   from the plot
    panel.background = element_blank(),

    # Strip background (#This sets the panel background for facet-wrapped plots to white, removing
    #   the standard grey ggplot background colour and sets the title size of the facet-wrap title
    #   to font size 22)
    strip.background = element_rect(fill="white"),

    strip.text = element_text(size  = 22,  hjust = 0)

    )

}
