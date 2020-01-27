# this file includes sample ggplot code to visualize the various sample datasets included in the package.

library(tidyverse)
library(cmapplot)


# A bar chart
ggplot(cluster_jobchange, aes(x = reorder(`cluster name`,`job change 2001-17`), y = `job change 2001-17`, fill = category)) +
  geom_col() +
  coord_flip() +
  theme_cmap()


# a stacked bar chart
ggplot(filter(traded_emp_by_race, variable %in% c("SpecializedTraded", "UnspecializedTraded")), aes(x = reorder(Race, -value), y = value, fill = variable)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent) + theme_cmap()


# a grouped and stacked bar chart (via `interaction()`)
ggplot(pop_and_laborforce_by_age, aes(x = interaction(year, variable), y = value, fill = age)) +
  geom_col(position = position_stack(reverse = TRUE))


# a grouped and stacked bar chart (via `interaction()`)
ggplot(economy_basic, aes(x = interaction(year, variable), y = value, fill = sector)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) + theme_cmap()


# a non-time-series line chart
ggplot(percentile_wages, aes(x = percentile, y = wage, color = cluster)) +
  geom_line() + theme_cmap()


# a time-series line chart
ggplot(grp_over_time, aes(x = year, y = realgrp, color = cluster)) +
  geom_line() + theme_cmap()
