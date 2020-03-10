
# cmapplot

This R package provides themes and color scales for
[`ggplot2`](https://github.com/tidyverse/ggplot2), based on Chicago
Metropolitan Agency for Planning (CMAP) design guidelines.

## Installation

Run the following:

``` r
# install.packages("devtools")
devtools::install_github("CMAP-REPOS/cmapplot")
# then load the package as you would any other
library(cmapplot)
```

## Examples

For testing, the `cmapplot` package contains a variety of sample
datasets. Each dataset has a man file that can be queried for additional
details. Datasets currently included are:

    #>  cluster_jobchange
    #>  economy_basic
    #>  grp_over_time
    #>  percentile_wages
    #>  pop_and_laborforce_by_age
    #>  traded_emp_by_race

The following provided code uses the sample datasets to produce
publishable or near-publishable graphics:

``` r
# A bar chart
ggplot(cluster_jobchange, aes(x = reorder(name, jobchange), y = jobchange, fill = category)) +
  geom_col() +
  coord_flip() +
  theme_cmap()
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

``` r
# a stacked bar chart
filter(traded_emp_by_race, variable %in% c("SpecializedTraded", "UnspecializedTraded")) %>%
  ggplot(aes(x = reorder(Race, -value), y = value, fill = variable)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_y_continuous(labels = scales::percent) +
    theme_cmap()
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

``` r
# a grouped and stacked bar chart (via `interaction()`)
ggplot(pop_and_laborforce_by_age, aes(x = interaction(year, variable), y = value, fill = age)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme_cmap()
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

``` r
# a grouped and stacked bar chart (via `interaction()`)
ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_cmap()
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

``` r
# a non-time-series line chart
ggplot(percentile_wages, aes(x = percentile, y = wage, color = cluster)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  theme_cmap()
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

``` r
# a time-series line chart
ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
  geom_line() +
  geom_text_lastonly(add_points = TRUE) +
  theme_cmap()
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

## Using CMAP palettes in R ggplot

Palettes based on the CMAP color palette can be applied directly to
ggplot graphics. The package contains both discrete and continuous color
palettes. Each type of palette can be applied to either the color or
fill attributes of a ggplot.

### Discrete palettes

Add discrete palettes by adding either a `cmap_fill_discrete` or
`cmap_color_discrete` object to a ggplot. Note that discrete palettes
will automatically interpolate additional colors if the dataset has more
colors than the palette. This can be helpful but is not ideal for
finished graphics.

``` r
ggplot(percentile_wages, aes(x = percentile, y = wage, color = cluster)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  theme_cmap() +
  cmap_color_discrete(palette = "prosperity")
```

![](man/figures/README-unnamed-chunk-10-1.png)<!-- -->

The following gradients are available:

<img src="man/figures/README-unnamed-chunk-11-1.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-11-2.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-11-3.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-11-4.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-11-5.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-11-6.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-11-7.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-11-8.png" width="50%" height="40%" />

### Continuous palettes

Add continuous palettes by adding either a `cmap_fill_continuous` or
`cmap_color_continuous` object to a ggplot. For example:

``` r
percentile_wages %>% 
  filter(cluster %in% c("Biopharmaceuticals", "Hospitality and Tourism", "Paper and Packaging")) %>% 
  ggplot(aes(x = cluster, y = wage, color = percentile)) +
    geom_point(size = 5) +
    scale_y_continuous(labels = scales::dollar) +
    coord_flip() +
    theme_cmap() +
    cmap_color_continuous(palette = "multi_red_purple")
```

![](man/figures/README-unnamed-chunk-12-1.png)<!-- -->

#### Single-hue sequential gradients

<img src="man/figures/README-unnamed-chunk-13-1.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-13-2.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-13-3.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-13-4.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-13-5.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-13-6.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-13-7.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-13-8.png" width="50%" height="40%" />

#### Multi-hue sequential gradients

<img src="man/figures/README-unnamed-chunk-14-1.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-14-2.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-14-3.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-14-4.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-14-5.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-14-6.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-14-7.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-14-8.png" width="50%" height="40%" />

#### Multi-hue diverging gradients

<img src="man/figures/README-unnamed-chunk-15-1.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-15-2.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-15-3.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-15-4.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-15-5.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-15-6.png" width="50%" height="40%" /><img src="man/figures/README-unnamed-chunk-15-7.png" width="50%" height="40%" />
