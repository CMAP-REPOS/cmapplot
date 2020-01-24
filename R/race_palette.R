###############################################################################
#
# Race theme:
#   ggplot +
#       cmap_fill_race(white = "factorname", african_american = "factorname",
#                      etc....)
#
#   need an offical "other" color -- ask Nancy
#     non-white color needed?
#
###############################################################################

library(ggplot2)


# Fill aesthetic
cmap_fill_race <- function(white, african_american, hispanic, asian, other) {

  race_palette <- c(
    wht = "#72A6E5",
    blk = "#8CE572",
    hisp = "#CCA600",
    asn = "#CC2F00",
    oth = "#6200CC"
  )

  if(!missing(white)){
    names(race_palette)[1] <- white
  }

  if(!missing(african_american)){
    names(race_palette)[2] <- african_american
  }

  if(!missing(hispanic)){
    names(race_palette)[3] <- hispanic
  }

  if(!missing(asian)){
    names(race_palette)[4] <- asian
  }

  if(!missing(other)){
    names(race_palette)[5] <- other
  }

  scale_fill_manual(values = race_palette)

}


# Color aesthetic
cmap_color_race <- function(white, african_american, hispanic, asian, other) {

  race_palette <- c(
    wht = "#72A6E5",
    blk = "#8CE572",
    hisp = "#CCA600",
    asn = "#CC2F00",
    oth = "#6200CC"
  )

  if(!missing(white)){
    names(race_palette)[1] <- white
  }

  if(!missing(african_american)){
    names(race_palette)[2] <- african_american
  }

  if(!missing(hispanic)){
    names(race_palette)[3] <- hispanic
  }

  if(!missing(asian)){
    names(race_palette)[4] <- asian
  }

  if(!missing(other)){
    names(race_palette)[5] <- other
  }

  scale_color_manual(values = race_palette)
}

# account for posh spelling differences
cmap_colour_race <- cmap_color_race



#### Testing / how to use ####

#traded_emp_by_race %>%
#  filter(Race!="Regional average" & Race!="Other") %>%
#  ggplot(.) +
#    geom_col(aes(x = variable, y = value, fill = Race)) +
#    cmap_fill_race(white = "White", african_american = "Black",
#                   hispanic = "Hispanic", asian = "Asian")
