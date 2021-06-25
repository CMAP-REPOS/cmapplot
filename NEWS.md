# cmapplot 1.2.0
PR # 123

This is a version-level update that includes many fixes and new features and introduces new dependencies, most significantly RStudio 1.4 or greater.

### New features
* cmapplot now utilizes the new systemfonts package for custom font rendering, rather than sysfonts on Windows and X11fonts on Mac and other Unix-based machines. In addition, raster exports from `finalize_plot` now rely on the new raster drawing package ragg. These changes improve font accuracy and consistency across platforms. Note that this improvement does not yet extend to svg outputs, but may in the future (#134). 
* When an axis breaks are 4-digit years, new function `abbr_years` allows the conversion of specific years to 2-digit abbreviations, as is common on some designed CMAP graphics.
* `cmapplot_globals`, which contains key package constants, is now an internal environment rather than an exported list. It can be accessed via new functions `get_cmapplot_globals` and `get_cmapplot_global`. Constants can now be overridden by the user for the current session by using `set_cmapplot_global`. Note that this does not yet apply to geom aesthetics set by the package, but may in the future (issue #117).
* All palettes programmed into the package have been moved into a tibble at `cmapplot_globals$palettes`. A new function `fetch_pal` can be used to get details about any specific palette. 

### Bug fixes
* Continuous color gradients can now be used on discrete color scales. E.g. `cmap_color_discrete` and `cmap_fill_discrete` can now call gradients as well as discrete palettes (see #70 and #119)
* Nomenclature in help files (e.g. "palette" vs "scale") has been adjusted for clarity.
* internal table `recessions` has been updated to include the business cycle contraction that began in Feb 202 (no end date for this cycle yet). 
* `geom_recessions` and related `update_recessions` have been updated to allow for ongoing recessions, improve NBER source for recessions table, and decrease likelihood of data fetch errors. 

### Backend changes
* pkgdown site now correctly displays Whitney fonts and margin description images 
* backend script reorganization
* improvements to pkgdown build github action, including ability to publish a test site

### Backward compatibility notes
* **This package will now only render Whitney Fonts in RStudio when RStudio version >= 1.4**
* `finalize_plot`'s `window` mode has been disabled for now, due to inability to use ragg drivers in independent window devices. Use `mode = "plot"` and click the "Zoom" button in the plot window instead.
* `cmapplot_globals`, the exported list of package constants, has been removed (See new features `set_cmapplot_global` etc)
* Color lists `cmap_palettes` and `cmap_gradients` have been removed (This information has been moved to `cmapplot_globals$palettes`. To access palette colors directly, use, say `fetch_pal("reds")` rather than `cmap_gradients$reds`. 
* `viz_palette` and `viz_gradient` now take as a first argument either the name of a palette (e.g. `"reds"`) or the color palette itself (e.g. `fetch_pal("reds")`). `viz_palette(cmap_gradient$reds)` no longer works.
* `integer_breaks` removed from package 


# cmapplot 1.1.0
PR #111 | February 24, 2021

PR #115 | March 2, 2021

This pair of updates primarily makes many changes to `finalize_plot()` to enable printing plots without the left-hand "sidebar" -- the area that contains the title and the caption. Most but not all changes are under the hood and should not impact the user. Those that will impact the user include:

* There is a new argument, `inherit`, that allows the user to specify whether cmapplot should attempt to inherit titles and/or captions from the underlying ggplot object.
* The argument `title_width` has been renamed `sidebar_width` to better reflect its function.
* Setting `sidebar_width = 0` now has the effect of shifting the title above the topline and shifting the caption from the title column to directly below the plot. If the user does not want a title on the vertical layout graphic, this can be achieved by leaving `title = ""`, the default, and (if relevant) specifying that `finalize_plot()` should not attempt to inherit a title from the underlying ggplot object.
* There is a new argument, `caption_align`, which takes numeric range 0 to 1. `0`, the default, aligns the caption bottom or left (in title-column and below-plot captions, respectively). `1` aligns the caption top or right. `0.5` centers. The argument `caption_valign` has been deprecated.
* The value `margin_title_l` has been replaced with `margin_sidebar_l`, which only affects horizontal layout graphics. The margin for titles and captions in the vertical layout is based on `margin_plot_l`.
* Separately, this version also creates this `NEWS.md` file for the pkgdown website.

Under-the-hood changes to `finalize_plot()` are documented in PR #111, specifically [here](https://github.com/CMAP-REPOS/cmapplot/pull/111#issuecomment-782779446). 

### Backward compatibility notes
Users who have written code with previous versions of cmapplot should note these known compatibility issues:
* In `finalize_plot()`, the argument `caption_valign` has been deprecated and will now issue a message alert (but will still work, for now). Please update your code to use the new argument `caption_align`.
* In `finalize_plot()`, the argument `title_width` has been deprecated and will now issue a message alert (but will still work, for now). Please update your code to use the new argument `sidebar_width`.
* Any overrides using the deprecated value `margin_title_l` will no longer have any affect. Use `margin_sidebar_l` instead.


# cmapplot 1.0.4
PR #110 | February 3, 2021

* The ggplot2 geom `geom_label` is now added to the list of geoms for which text aesthetics are automatically updated to CMAP style (ie to Whitney fonts). Previously, only `geom_text` and the custom `geom_text_last` were available in CMAP style. 


# cmapplot 1.0.3
PR #107 | February 2, 2021

* Modified `finalize_plot()` to accept `title_width = 0` without causing a fuss. This is a short-term fix, with more improvements coming.
* In `cmapplot_globals$consts`, eliminated `margin_title_r`, which created space between the title/caption and plotbox inside the title column. Replaced it  with `margin_plot_l`, which creates the same buffer but does so in the plot column, not the title column. This was necessary to keep an active left-hand margin in situations where `title_width = 0`.

### Backward compatibility notes
Users who have written code with previous versions of cmapplot should note these known compatibility issues:
* `margin_title_r` no longer exists. Code that overrides this in `finalize_plot()` should not error, but will also have no effect on your plot. Change to `margin_plot_l`.
* Titles and captions will be a bit wider (the width of the title and caption grobs are no longer modified by `margin_title_r`).
* Plots will be a bit narrower (the width of the plotbox is now modified by `margin_plot_l`).


# cmapplot 1.0.2
PR #103 | January 11, 2021

* Fixed bug where custom color functions (e.g. `cmap_fill_continuous()` etc) did not allow for passing other arguments on to ggplot2's `scale` functions (see issue #102).


# cmapplot 1.0.1
PR #100 | December 9, 2020

* Improvement of tickmark handling via addition of `axisticks`  argument in `theme_cmap()` and new `length_ticks` default value in `cmapplot_globals$consts`.
* Substantial backend simplification of how `theme_cmap()` generates theme objects (not of substantial significance to users).


# cmapplot 1.0.0
December 1, 2020

* Initial package release.
