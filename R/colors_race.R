#' Race palette prep
#'
#' @param white White/Caucasian
#' @param black Black/African American
#' @param hispanic Hispanic/Latino
#' @param asian Asian
#' @param other Other
#'
#' @noRd
make_race_palette <- function(white, black, hispanic, asian, other) {
    race_palette <- cmap_palettes$race
    if (!missing(white)) { names(race_palette)[1] <- white }
    if (!missing(black)) { names(race_palette)[2] <- black }
    if (!missing(hispanic)) { names(race_palette)[3] <- hispanic }
    if (!missing(asian)) { names(race_palette)[4] <- asian }
    if (!missing(other)) { names(race_palette)[5] <- other }
    return(race_palette)
}


#' Apply official CMAP race/ethnicity chart colors to ggplot2 aesthetics
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or color).
#' Specify your dataset's unique race factor names (as case-sensitive strings) in the arguments.
#' All categories are optional in case your dataset does not have some of them.
#'
#' @param white White/Caucasian
#' @param black Black/African American
#' @param hispanic Hispanic/Latino
#' @param asian Asian
#' @param other Other
#'
#' @examples
#' ggplot(dplyr::filter(traded_emp_by_race, Race!="Regional average" &
#'                      variable=="SpecializedTraded")) +
#'    geom_col(aes(x = Race, y = value, fill = Race)) +
#'    cmap_fill_race(white = "White", black = "Black",
#'                   hispanic = "Hispanic", asian = "Asian",
#'                   other = "Other")
#'
#' @describeIn cmap_fill_race For fill aesthetic
#' @export
cmap_fill_race <- function(white, black, hispanic, asian, other) {
    race_palette <- make_race_palette(white, black, hispanic, asian, other)
    ggplot2::scale_fill_manual(values = race_palette)
}


#' @describeIn cmap_fill_race For color aesthetic
#' @export
cmap_color_race <- function(white, black, hispanic, asian, other) {
    race_palette <- make_race_palette(white, black, hispanic, asian, other)
    ggplot2::scale_color_manual(values = race_palette)
}

#' @describeIn cmap_fill_race For color aesthetic
#' @export
cmap_colour_race <- cmap_color_race
