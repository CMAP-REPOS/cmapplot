# cmapplot 1.1.0
February 22, 2020

## Changelog
This update primarily makes many changes to `finalize_plot()` to enable printing plots without the left-hand "title column" -- the area that contains the title and the caption. Most but not all changes are under the hood and should not impact the user. Those that will impact the user include:

* setting `title_width = 0` now has the effect of eliminating the title and shifting the caption from the title column to directly below the plot
* There is a new argument, `caption_align`, which takes numeric range 0 to 1. `0`, the default, aligns the caption bottom or left (in title-column and below-plot captions, respectively). `1` aligns the caption top or right. `0.5` centers. The argument `caption_valign` has been deprecated.
* Separately, this version also creates this `NEWS.md` file for the pkgdown website.  

Under-the-hood changes to `finalize_plot()` are documented in PR #111, specifically [here](https://github.com/CMAP-REPOS/cmapplot/pull/111#issuecomment-782779446). 

## Backward compatibility notes
Users who have written code with previous versions of cmapplot should note these known compatibility issues:

* In `finalize_plot()`, the argument `caption_valign` has been deprecated and will now issue a message alert (but will still work, for now). Please update your code to ues the new argument `caption_align`. 


# cmapplot 1.0.3
February 2, 2020

## Changelog
* Improvement of tickmark handling via addition of `axisticks`  argument in `theme_cmap()` (#100)
* Substantial backend simplification of how `theme_cmap()` generates theme objects (not of substantial significance to users) (#100)
* Fixed bug where custom color functions (e.g. `cmap_fill_continuous()` etc) did not allow for passing other arguments on to ggplot2's `scale` functions (#102 #103)
* modified `finalize_plot()` to accept `title_width = 0` without causing a fuss. This is a short-term fix, with more improvements coming. (#107)
* In `cmapplot_globals$consts`, eliminated `margin_title_r`, which created space between the title/caption and plotbox inside the title column. Replaced it  with `margin_plot_l`, which creates the same buffer but does so in the plot column, not the title column. This was necessary to keep an active lefthand margin in situations where `title_width = 0`. (#107)

(This list does not include documentation changes.)

## Backward compatibility notes
Users who have written code with previous versions of cmapplot should note these known compatibility issues:
* `margin_title_r` no longer exists. Code that overrides this in `finalize_plot()` should not error, but will also have no effect on your plot. Change to `margin_plot_l`.
* Titles and captions will be a bit wider (the width of the title and caption grobs are no longer modified by `margin_title_r`)
* Plots will be a bit narrower (the width of the plotbox is now modified by `margin_plot_l`)


# cmapplot 1.0.0
December 1, 2020

* initial package release 
