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

    race_palette <- fetch_pal("race")
    pal <- c()

    if (missing(white) & missing(black) & missing(hispanic) & missing(asian) & missing(other)) {
      pal <- race_palette # if no parameters specified, return default race palette
    } else {

      passed <- unlist(as.list(match.call())[-1]) # vector of args actually passed

      for (i in names(race_palette)) {
        if (i %in% names(passed)) {
          pal[passed[i]] <- race_palette[i]
        }
      }
    }

    return (pal)
}

#' Apply official CMAP race/ethnicity chart colors to ggplot2 aesthetics
#'
#' Pick the function depending on the aesthetic of your ggplot object (fill or color).
#' Specify your dataset's unique race factor names (as case-sensitive strings) in the arguments.
#' All categories are optional in case your dataset does not have some of them or contains the default
#' values of the race palette.
#'
#' @param white Data value to map CMAP's White/Caucasian color onto (case-sensitive).
#' @param black Data value to map CMAP's Black/African American color onto (case-sensitive).
#' @param hispanic Data value to map CMAP's Hispanic/Latino color onto (case-sensitive).
#' @param asian Data value to map CMAP's Asian color onto (case-sensitive).
#' @param other Data value to map CMAP's Other/Multiple Races color onto (case-sensitive).
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
    .args <- as.list(match.call()[-1])
    race_palette <- do.call(make_race_palette, .args)
    ggplot2::scale_fill_manual(values = race_palette)
}


#' @describeIn cmap_fill_race For color aesthetic
#' @export
cmap_color_race <- function(white, black, hispanic, asian, other) {
    .args <- as.list(match.call()[-1])
    race_palette <- do.call(make_race_palette, .args)
    ggplot2::scale_color_manual(values = race_palette)
}

#' @describeIn cmap_fill_race For color aesthetic
#' @export
cmap_colour_race <- cmap_color_race
