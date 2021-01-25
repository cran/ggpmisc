---
editor_options:
  markdown:
    wrap: 72
---

# ggpmisc 0.3.8

-   **CODE BREAKING:** functions `stat_fit_glance()` ,
    `stat_fit_augment()` , `stat_fit_tidy()` and `stat_fit_tb()` now
    import the *tidiers* from package 'generics' instead of from
    'broom'. As a result, users must now explicitly load the package
    where the methods to be used are defined, such as 'broom' or
    'broom.mixed' or define them before calling these statistics.
-   Add formal parameter `glance.args` to `stat_fit_glance()` ,
    parameter `tidy.ars` to `stat_fit_tidy()` and `stat_fit_tb()` and
    parameter `augment.args` to `stat_fit_augment()` as some
    specializations of `broom::glance()`, `broom::tidy()` and
    `stat_fit_augment()` accept arguments specific to a given fitting
    method.
-   Fix bug: `stat_fit_tidy()` would fail with `quantreg::rq()` and any
    other fit methods that do not return by default standard error
    estimates for parameter estimates (Thanks to Mark Neal for reporting
    the problem).
-   Revise `stat_fit_glance()`, `stat_fit_augment()` and
    `stat_fit_tidy()` to ensure compatibility with `cor.test()` and
    other functions that require an object rather than a quoted
    expression as argument for `data` .
-   Add formal parameter `p.digits` to `stat_fit_tb()`.
-   New vignette explaining how the grammar of graphics has been
    expanded to better support annotations.
-   Fix bug: `try_tibble.ts()` and `try_data_frame()` did not handle
    correctly the conversion of dates for some time series, which also
    could affect `ggplot.ts()`.
-   Fix bug: `stat_peaks()` and `stat_valleys()` generated wrong labels
    if a `Date` object was mapped to *x (the bug did not affect POSIX or
    datetime, and was obvious as it resulted in a shift in dates by
    several decades)*.
-   **Move git repository from Bitbucket to Github.**
-   Set up Github action for CRAN-checks on Windows, OS X and Ubuntu.

# ggpmisc 0.3.7

-   Update `stat_fit_tb()` to support renaming of terms/parameter names
    in the table (Suggested by Big Old Dave and Z. Lin). In addition
    implement selection, reordering and renaming of columns and
    terms/parameters using positional indexes and pattern matching of
    truncated names in addition to whole names. Improve formatting of
    small *P*-values.
-   Update `stat_fmt_tb()` to support the same expanded syntax as
    `stat_fit_tb()`.
-   Add `stat_dens1d_filter()`, `stat_dens1d_filter_g()` and
    `stat_dens1d_labels()`, to complement existing
    `stat_dens2d_filter()`, `stat_dens2d_filter_g()` and
    `stat_dens2d_labels()`.
-   Update `stat_dens2d_filter()`, `stat_dens2d_filter_g()` and
    `stat_dens2d_labels()` adding formal parameters `keep.sparse` and
    `invert.selection`, as available in the new 1D versions.
-   Update `stat_dens2d_labels()` to accept not only character strings
    but also functions as argument to `label.fill` as the new
    `stat_dens1d_labels()` does.
-   Revise documentation including the *User Guide*.

# ggpmisc 0.3.6

-   Override `ggplot2::annotate()` adding support for aesthetics `npcx`
    and `npcy`.
-   Add `stat_summary_xy()` and `stat_centroid()`.
-   Revise `stat_poly_eq()` to support labeling of equations according
    to group.
-   Implement `output.type` `"markdown"` in `stat_poly_eq()` usable with
    `geom_richtext()` from package 'ggtext'.

# ggpmisc 0.3.5

-   Add support for "table themes" to geom_table() and
    `geom_table_npc()`.

# ggpmisc 0.3.4

-   Add support for p.value.label and f.value.label to `stat_poly_eq()`.
-   Update to track deprecations in 'ggplot2' (\>= 3.3.0).

# ggpmisc 0.3.3

-   Fix bug in `stat_poly_eq()`.
-   Minor revision of the *User Guide* and documentation.

# ggpmisc 0.3.2

This version implements some new features and fixes bugs in the features
introduced in version 0.3.1, please do rise an issue if you notice any
remaining bugs! Some reported weaknesses in the documentation have been
addressed. This updated version depends on 'ggplot2' (\>= 3.2.1).

-   Add support for *volcano* and *quadrant* *plots* of outcomes.

-   Add geometries `geom_vhlines()` and `geom_quadrant_lines()`.

-   Add convenience scales `scale_x_logFC()` and `scale_y_logFC()` for
    data expressed as fold change.

-   Add convenience scales `scale_x_Pvalue()`, scale_y\_Pvalue(),
    `scale_x_FDR()`, `scale_y_FDR()`.

-   Add convenience scales `scale_colour_outcome()`,
    `scale_fill_outcome()` and `scale_shape_outcome()` for data
    expressed as ternary or binary outcomes.

-   Add conversion functions `outcome2factor()` and `threshold2factor()`
    to convert vectors of numeric outcomes into factors with 2 or 3
    levels.

-   Add conversion function `xy_outcomes2factor()` and
    `xy_thresholds2factor()` to combine two vectors of numeric outcomes
    into a 4-level factor.

-   Improve support for model-fit annotations.

-   Update `stat_poly_eq()` so that optionally instead of text labels it
    can return numeric values extracted from the fit object.

-   Document with examples how to pass weights and covariates to
    statistics based on methods from package 'broom'. Highlight the
    differences among `stat_poly_eq()` and the `stat_fit_xxx()`
    statistics implemented using package 'broom'.

-   Revise `stat_apply_fun()` to allow simultaneous application of
    functions to *x* and *y* aesthetics, and handling of `diff()` and
    other functions returning slightly shorter vectors than their input.

-   Support in `stat_fit_tb()`, `stat_fit_augment()`, `stat_fit_tidy()`
    and `stat_fit_glance()` the use of character strings as position
    arguments for parameters `label.x` and `label.y` when using geoms
    based on *x* and *y* aesthetics in addition to when using those
    taking the `npcx` and `npcy` aesthetics.

# ggpmisc 0.3.1

This is a major update, with a few cases in which old code may need to
be revised to work, and many cases in which there will be subtle
differences in the positions of labels used as annotations. The many new
features may still have some bugs, please do rise an issue if you notice
one!

Version requiring 'ggplot2' (\>= 3.1.0).

Add new geometries, several of them accepting *x* and *y* in *npc* units
through the new aesthetics `npcx` and `npcy`, allowing positioning
relative to plotting area irrespective of native data units and scale
limits. These geometries are useful on their own for annotations in
particular they allow consistent positioning of textual summaries. By
default they do not inherit the plot's aesthetic mappings making their
behaviour remain by default in-between that of true geometries and that
of annotate().

-   Add `geom_text_npc()` and `geom_label_npc()` using aesthetics npcx
    and npcy.
-   Add `geom_table_npc()` using aesthetics npcx and npcy.
-   Add `geom_plot()` and `geom_plot_npc()` which can be used to add
    inset plots to a ggplot.
-   Add `geom_grob()` and `geom_grob_npc()` which can be used to add
    inset *grobs* to a ggplot.
-   Add `geom_x_margin_point()`, `geom_y_margin_point()`,
    `geom_x_margin_arrow()` and `geom_y_margin_arrow()` which behave
    similarly to `geom_hline()` and `geom_vline()` but plot points or
    arrows instead of lines. Add `geom_x_margin_grob()` and
    `geom_y_margin_grob()` with similar behaviour but for adding
    *grobs*.
-   Revise textual-summary statistics to use the new *npc* version of
    geometries.
-   This may break old code that used `geom_table()` and depended on the
    old default of `inherit.aes=TRUE`.
-   Add "summarize" statistics for groups and panels.
-   Add `stat_apply_panel()` and `stat_apply_group()`.
-   Add workaround to `stat_fit_glance()` and improve diagnosis of
    unsupported input. Replace bad example in the corresponding
    documentation (workaround for bug reported by Robert White).
-   Update documentation.
-   Add and revise examples.
-   Revise vignette.

# ggpmisc 0.3.0

Version requiring 'ggplot2' (\>= 3.0.0), now in CRAN. **Low level
manipulation and debug methods and functions moved to new package
'gginnards' available through CRAN.**

-   Remove debug stats and geoms -\> 'gginnards'.
-   Remove layer manipulation functions -\> 'gginnards'.
-   Add support for "weight" aesthetic in `stat_poly_eq()` (fixing bug
    reported by S.Al-Khalidi).
-   Add support for column selection and renaming to `stat_fit_tb()`.
-   Add new statistic `stat_fmt_tb()` for formatting of tibbles for
    addition to plots as tables.
-   Rename `stat_quadrat_count()` into `stat_quadrant_count()`
    (miss-spelling).
-   Revise vignette.

# ggpmisc 0.2.17.9902

Non-CRAN version with additional functionality, but requiring the
development version of 'ggplot2'.

-   Track code breaking change in 'ggplot2' commit \#2620 (2018-05-17).

# ggpmisc 0.2.17.9900

Non-CRAN version with additional functionality, but requiring the
development version of 'ggplot2' \>= 2.2.1.9000 (\>= commit of
2017-02-09) from Github. Visit

-   `geom_table()`, a geom for adding a layer containing one or more
    tables to a plot panel.
-   `stat_fit_tb()` a stat that computes a tidy tabular version of the
    summary or ANOVA table from a model fit.

# ggpmisc 0.2.17

CRAN version

-   Add `stat_quadrat_count()` a stat that computes the number of
    observations in each quadrant of a plot panel ignoring grouping.

-   Fix bugs, one of which is code breaking: the names of returned
    parameter estimates have changed in `stat_fit_tidy()` now pasting
    `"_estimate"` to avoid name clashes with mapped variables.

# ggpmisc 0.2.16

-   Revise `stat_fit_tidy()` so that it returns *p*-values for
    parameters, in addition to estimates and their standard errors.
-   BUG FIX: Revise `geom_debug()` adding missing default arguments.
-   Add functions for manipulation of layers in ggplot objects:
    `delete_layers()`, `append_layers()`, `move_layers()`,
    `shift_layers()`, `which_layers()`, `extract_layers()`,
    `num_layers()`, `top_layer()` and `bottom_layer()`.

# ggpmisc 0.2.15

Add `stat_fit_tidy()` implemented using `broom::tidy()`. Makes it
possible to add the fitted equation for any fitted model supported by
package 'broom', as long as the user supplies within aes() the code to
build a label string. Update user guide.

# ggpmisc 0.2.14

Fix bug in `stat_poly_equation()` `eq.x.rhs` argument ignored when using
expressions.

# ggpmisc 0.2.13

-   Fix bugs in `try_tibble()` and `try_data_frame()` which made them
    fail silently with some objects of class `"ts"` in the case of
    numeric (decimal date) index for time. In addition lack of special
    handling for classes `"yearmon"` and `"yearqrt"` from package 'zoo',
    lead to erroneous date shifts by a few days.
-   Add methods `ggplot.ts()` and `ggplot.xts()`.

# ggpmisc 0.2.12

-   Change default value for parameter `label.fill` in
    `stat_dens2d_labels()` from `NA` to `""`.
-   Improve documentation using current 'ggrepel' version, which
    implements changes that make `stat_dens2d_labels()` useful.

# ggpmisc 0.2.11

-   Add `stat_dens2d_labels()`, a statistic that resets label values to
    `NA` by default, or any character string supplied as argument, in
    regions of a panel with high density of observations.

-   Add `stat_den2d_filter()`, a statistic that filters-out/filters-in
    observations in regions of a panel with high density of
    observations. These two statistics are useful for labeling or
    highlighting observations in regions of a panel with low density.
    Both stats use a compute_panel function.

-   Add `stat_den2d_filter_g()`, a statistic that filters-out/filters-in
    observations in regions of a group with high density of
    observations. This statistics is useful for highlighting
    observations. It uses a compute_group function. They use internally
    `MASS:kde2d` to estimate densities and default values for parameters
    are adjusted dynamically based on the number of observations.

# ggpmisc 0.2.10

-   Add user-requested feature: allow user to specify number 'digits'
    used in formatting numbers in labels in `stat_poly_eq()`.
-   Update `try_data_frame()` to return an object of class `"tibble"`
    and add `try_tibble()` as synonym.
-   Update documentation and start using package 'staticdocs' to build a
    documentation web site.

# ggpmisc 0.2.9

-   Add support for *tikz* in `stat_poly_eq()`.
-   Fix bug in `stat_poly_eq()`.
-   Fix bug in `geom_debug()`.
-   Fix bug in `stat_fit_augment()`.

# ggpmisc 0.2.8

-   Enhance `stat_poly_eq()` so that 1) position of labels according to
    *npc* (relative positions using normalized coordinates), as well as
    by named positions `"top"`, `"bottom"`, `"right"`, `"left"` and
    `"center"` is now implemented; 2) when grouping is present, suitable
    `vjust` values are computed to automatically position the labels for
    the different groups without overlap. Default label positions are
    now relative to the range of each panel's $x$ and $y$ scales,
    eliminating in most cases the need to manually tweak label
    positions.

-   Add `stat_fit_glance()` uses package 'broom' for maximum flexibility
    in model function choice when wanting to add labels based on
    information from a model fit, at the expense of very frequently
    having to explicitly set aesthetics, and always having to add code
    to do the formatting of the values to be used in labels. Label
    position is as described above for `stat_poly_eq()`.

-   Add `stat_fit_deviations()` for highlighting residuals in plots of
    fitted models. This statistic currently supports only `lm()` fits.
    By default geom "segment" is used to highlight the deviations of the
    observations from a fitted model.

-   Add `stat_fit_residuals()` for plotting residuals from a fitted
    model on their own in plots matching plots of lm fits plotted with
    stat_smooth() even with grouping or facets. This statistic currently
    supports only `lm()` fits. By default geom "point" is used to plot
    the residual from a fitted model.

-   Add preliminary version of `stat_fit_augment()`, which uses package
    'broom' for maximum flexibility in model function choice, to augment
    the data with additional columns of values derived from a model fit.

# ggpmisc 0.2.7

-   Add support for AIC and BIC labels to `stat_poly_eq()`.
-   Add pretty-printing of parameter values expressed in engineering
    notation in `stat_poly_eq()`.
-   Add support for user-supplied label coordinates in `stat_poly_eq()`.
-   Improve `stat_debug_panel()` and stat_debug_group() so that they can
    optionally print to the console a summary of the data received as
    input.
-   Add `geom_debug()`, a geom that summarizes its data input to the
    console, and produces no visible graphical output.

# ggpmisc 0.2.6

-   Add support for user-supplied *lhs* and for user-supplied
    *rhs*-variable name in the equation label in `stat_poly_eq()`.

# ggpmisc 0.2.5

-   Remove one example to remove a package dependency.

# ggpmisc 0.2.4

-   Improve handling of time zones in `try_data_frame()`.
-   Revise documentation and vignette.

# ggpmisc 0.2.3

-   `stat_poly_eq()` changed to include the *lhs* (left hand side) of
    the equation by default.

# ggpmisc 0.2.2

-   Add function try_data_frame() to convert R objects including time
    series objects of all classes accepted by `try.xts()` into data
    frames suitable for plotting with `ggplot()`.

-   Update `stat_peaks()` and `stat_valleys()` to work correctly when
    the x aesthetic uses a `Date` or `Datetime` continuous scale such as
    `ggplot()` sets automatically for `POSIXct` variables mapped to the
    *x* aesthetic.

# ggpmisc 0.2.1

-   Rename `stat_debug()` as `stat_debug_group()` and add
    `stat_debug_panel()`.
-   Add `stat_peaks()` and `stat_valleys()` (these are simpler versions
    of `ggspectra::stat_peaks()` and `ggspectra::stat_valleys()` for use
    with any numerical data (rather than light spectra).

# ggpmisc 0.1.0

*First version.*

-   Add `stat_poly_eq()`
-   Add `stat_debug()`
