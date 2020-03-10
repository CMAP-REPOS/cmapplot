#' Apply official CMAP race/ethnicity chart colors to ggplot2
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or color).
#' Specify your dataset's unique race factor names (as case-sensitive strings) in the arguments.
#' All categories are optional in case your dataset does not have all of them.
#'
#' @param white White/Caucasian
#' @param black Black/African American
#' @param hispanic Hispanic/Latino
#' @param asian Asian
#' @param other Other
#'
#' @examples
#' library(dplyr)
#' traded_emp_by_race %>%
#'     filter(Race!="Regional average" & Race!="Other") %>%
#'     ggplot(.) +
#'     geom_col(aes(x = variable, y = value, fill = Race)) +
#'     cmap_fill_race(white = "White", black = "Black",
#'                    hispanic = "Hispanic", asian = "Asian")
#'
#' @export
cmap_fill_race <- function(white, black, hispanic, asian, other) {

    race_palette <- cmap_palettes$race

    if(!missing(white)){
        names(race_palette)[1] <- white
    }

    if(!missing(black)){
        names(race_palette)[2] <- black
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

    ggplot2::scale_fill_manual(values = race_palette)

}


#' @rdname cmap_fill_race
#' @export
cmap_color_race <- function(white, black, hispanic, asian, other) {

    race_palette <- cmap_palettes$race

    if(!missing(white)){
        names(race_palette)[1] <- white
    }

    if(!missing(black)){
        names(race_palette)[2] <- black
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

    ggplot2::scale_color_manual(values = race_palette)
}

#' @rdname cmap_fill_race
#' @export
cmap_colour_race <- cmap_color_race
