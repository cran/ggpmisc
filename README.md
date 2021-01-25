
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpmisc <img src="man/figures/logo-ggpmisc.png" align="right" width="150" />

[![cran
version](https://www.r-pkg.org/badges/version/ggpmisc)](https://cran.r-project.org/package=ggpmisc)
[![R build
status](https://github.com/aphalo/ggpmisc/workflows/R-CMD-check/badge.svg)](https://github.com/aphalo/ggpmisc/actions)

## Purpose

Package ‘**ggpmisc**’ (Miscellaneous Extensions to ‘ggplot2’) is a set
of extensions to R package ‘ggplot2’ (&gt;= 3.0.0) with emphasis on
annotations and highlighting related to fitted models and data
summaries. Data summaries shown as text, tables or equations are
implemented. New geoms support insets in ggplots. The grammar of
graphics is extended to support native plot coordinates (npc) so that
annotations can be easily positioned using special geometries and
scales. New position functions facilitate the labeling of observations
by nudging data labels away or towards curves or a focal virtual center.

## Extended Grammar of graphics

The position of annotations within the plotting area depends in most
cases on graphic design considerations rather than on properties such as
the range of values in the data being plotted. In particular, the
location within the plotting area of large annotation objects like
model-fit summaries, location maps, plots, and tables needs usually to
be set independently of the `x` and `y` scales, re-scaling or any
transformations. To acknowledge this, the Grammar of Graphics is here
expanded by supporting *x* and *y* positions expressed in ‘grid’ “npc”
units in the range 0..1. This is implemented with new
(pseudo-)aesthetics *npcx* and *npcy* and their corresponding scales,
plus geometries and a revised `annotate()` function. The new aesthetics
function in “parallel” with the *x* and *y* aesthetics used for plotting
data. The advantage of this approach is that the syntax used for
annotations becomes identical to that used for plotting data and that
annotations with approach *cleanly* support facets in a way consistent
with the rest of the grammar.

## Aesthetics and scales

Scales `scale_npcx_continuous()` and `scale_npcy_continuous()` and the
corresponding new aesthetics `npcx` and `npcy` make it possible to add
graphic elements and text to plots using coordinates expressed in `npc`
units for the location within the plotting area.

Scales `scale_x_logFC()` and `scale_y_logFC()` are suitable for plotting
of log fold change data. Scales `scale_x_Pvalue()`, `scale_y_Pvalue()`,
`scale_x_FDR()` and `scale_y_FDR()` are suitable for plotting *p*-values
and adjusted *p*-values or false discovery rate (FDR). Default arguments
are suitable for volcano and quadrant plots as used for transcriptomics,
metabolomics and similar data.

Scales `scale_colour_outcome()`, `scale_fill_outcome()` and
`scale_shape_outcome()` and functions `outome2factor()`,
`threshold2factor()`, `xy_outcomes2factor()` and
`xy_thresholds2factor()` used together make it easy to map ternary
numeric outputs and logical binary outcomes to color, fill and shape
aesthetics. Default arguments are suitable for volcano, quadrant and
other plots as used for genomics, metabolomics and similar data.

## Geometries

Geometries `geom_table()`, `geom_plot()` and `geom_grob()` make it
possible to add inset tables, inset plots, and arbitrary ‘grid’
graphical objects including bitmaps and vector graphics as layers to a
ggplot using native coordinates for `x` and `y`.

Geometries `geom_text_npc()`, `geom_label_npc()`, `geom_table_npc()`,
`geom_plot_npc()` and `geom_grob_npc()`, `geom_text_npc()` and
`geom_label_npc()` are versions of geometries that accept positions on
*x* and *y* axes using aesthetics `npcx` and `npcy` values expressed in
“npc” units.

Geometries `geom_x_margin_arrow()`, `geom_y_margin_arrow()`,
`geom_x_margin_grob()`, `geom_y_margin_grob()`, `geom_x_margin_point()`
and `geom_y_margin_point()` make it possible to add marks along the *x*
and *y* axes. `geom_vhlines()` and `geom_quadrant_lines()` draw vertical
and horizontal reference lines within a single layer.

## Statistics

Statistic `stat_fmt_tb()` helps with the formatting of tables to be
plotted with `geom_table()`.

Statistics `stat_peaks()` and `stat_valleys()` can be used to highlight
and/or label maxima and minima in a plot.

Statistics that help with reporting the results of model fits are
`stat_poly_eq()`, `stat_fit_residuals()`, `stat_fit_deviations()`,
`stat_fit_glance()`, `stat_fit_augment()`, `stat_fit_tidy()` and
`stat_fit_tb()`.

Four statistics, `stat_dens2d_filter()`, `stat_dens2d_label()`,
`stat_dens1d_filter()` and `stat_dens1d_label()`, implement tagging or
selective labeling of observations based on the local 2D density of
observations in a panel. Another two statistics,
`stat_dens1d_filter_g()` and `stat_dens1d_filter_g()` compute the
density by group instead of by plot panel. These six stats are designed
to work well together with `geom_text_repel()` and `geom_label_repel()`
from package ‘ggrepel’.

A summary statistic using special grouping for quadrants
`stat_quadrant_counts()` can be used to automate labeling with the
number of observations.

The statistics `stat_apply_panel()` and `stat_apply_group()` can be
useful for applying arbitrary functions returning numeric vectors. They
are specially useful with functions lime `cumsum()`, `cummax()` and
`diff()`.

## Positions

Two enhanced versions of `position_nudge()` are provided,
`position_nudge_center()` and `position_nudge_line()`. These functions
make it possible to apply nudging that varies automatically according to
the relative position of points with respect to arbitrary points or
lines, or with respect to a polynomial or smoothing spline fitted
on-the-fly to the the observations.

## ggplot methods

Being `ggplot()` defined as a generic method in ‘ggplot2’ makes it
possible to define specializations, and we provide two for time series
stored in objects of classes `ts` and `xts` which automatically convert
these objects into tibbles and set by default the aesthetic mappings for
`x` and `y` automatically. A companion function `try_tibble()` is also
exported.

## MIGRATED

Functions for the manipulation of layers in ggplot objects, together
with statistics and geometries useful for debugging extensions to
package ‘ggplot2’, earlier included in this package are now in package
‘gginnards’. [![cran
version](https://www.r-pkg.org/badges/version/gginnards)](https://cran.r-project.org/package=gginnards)

## Examples

``` r
library(ggpmisc)
library(ggrepel)
```

In the first example we plot a time series using the specialized version
of `ggplot()` that converts the time series into a tibble and maps the
`x` and `y` aesthetics automatically. We also highlight and label the
peaks using `stat_peaks`.

``` r
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", angle = 66,
             hjust = -0.1, x.label.fmt = "%Y") +
  stat_peaks(geom = "rug", colour = "red", sides = "b") +
  expand_limits(y = 8000)
```

![](man/figures/README-readme-03-1.png)<!-- -->

In the second example we add the equation for a fitted polynomial plus
the adjusted coefficient of determination to a plot showing the
observations plus the fitted curve, deviations and confidence band. We
use `stat_poly_eq()`.

``` r
formula <- y ~ x + I(x^2)
ggplot(cars, aes(speed, dist)) +
  geom_point() +
  stat_fit_deviations(method = "lm", formula = formula, colour = "red") +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")),
               formula = formula, parse = TRUE)
```

![](man/figures/README-readme-04-1.png)<!-- -->

The same figure as in the second example but this time annotated with
the ANOVA table for the model fit. We use `stat_fit_tb()` which can be
used to add ANOVA or summary tables.

``` r
formula <- y ~ x + I(x^2)
ggplot(cars, aes(speed, dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_tb(method = "lm",
              method.args = list(formula = formula),
              tb.type = "fit.anova",
              tb.vars = c(Effect = "term", 
                          "df",
                          "M.S." = "meansq", 
                          "italic(F)" = "statistic", 
                          "italic(P)" = "p.value"),
              tb.params = c(x = 1, "x^2" = 2),
              label.y.npc = "top", label.x.npc = "left",
              size = 2.5,
              parse = TRUE)
#> Dropping params/terms (rows) from table!
```

![](man/figures/README-readme-05-1.png)<!-- -->

A plot with an inset plot.

``` r
p <- ggplot(mtcars, aes(factor(cyl), mpg, colour = factor(cyl))) +
  stat_boxplot() +
  labs(y = NULL) +
  theme_bw(9) + theme(legend.position = "none")

ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() +
  annotate("plot_npc", npcx = "left", npcy = "bottom", label = p) +
  expand_limits(y = 0, x = 0)
```

![](man/figures/README-readme-06-1.png)<!-- -->

A quadrant plot with counts and labels, using `geom_text_repel()` from
package ‘ggrepel’.

``` r
ggplot(quadrant_example.df, aes(logFC.x, logFC.y)) +
  geom_point(alpha = 0.3) +
  geom_quadrant_lines() +
  stat_quadrant_counts() +
  stat_dens2d_filter(color = "red", keep.fraction = 0.02) +
  stat_dens2d_labels(aes(label = gene), keep.fraction = 0.02, 
                     geom = "text_repel", size = 2, colour = "red") +
  scale_x_logFC(name = "Transcript abundance after A%unit") +
  scale_y_logFC(name = "Transcript abundance after B%unit")
```

![](man/figures/README-unnamed-chunk-1-1.png)<!-- -->

## Installation

Installation of the most recent stable version from CRAN:

``` r
install.packages("ggpmisc")
```

Installation of the current unstable version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("aphalo/ggpmisc")
```

## Documentation

HTML documentation is available at
(<https://docs.r4photobiology.info/ggpmisc/>), including a *User Guide*.

News about updates are regularly posted at
(<https://www.r4photobiology.info/>).

## Contributing

Please report bugs and request new features at
(<https://github.com/aphalo/ggpmisc/issues>). Pull requests are welcome
at (<https://github.com/aphalo/ggpmisc>).

## Citation

If you use this package to produce scientific or commercial
publications, please cite according to:

``` r
citation("ggpmisc")
#> 
#> To cite package 'ggpmisc' in publications use:
#> 
#>   Pedro J. Aphalo (2021). ggpmisc: Miscellaneous Extensions to
#>   'ggplot2'. https://docs.r4photobiology.info/ggpmisc/,
#>   https://github.com/aphalo/ggpmisc.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ggpmisc: Miscellaneous Extensions to 'ggplot2'},
#>     author = {Pedro J. Aphalo},
#>     year = {2021},
#>     note = {https://docs.r4photobiology.info/ggpmisc/,
#> https://github.com/aphalo/ggpmisc},
#>   }
```

## License

© 2016-2021 Pedro J. Aphalo (<pedro.aphalo@helsinki.fi>). Released under
the GPL, version 2 or greater. This software carries no warranty of any
kind.
