% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-peaks.R
\name{stat_peaks}
\alias{stat_peaks}
\alias{stat_valleys}
\title{Local maxima (peaks) or minima (valleys)}
\usage{
stat_peaks(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  ...,
  span = 5,
  global.threshold = 0,
  local.threshold = 0,
  local.reference = "median",
  strict = FALSE,
  label.fmt = NULL,
  x.label.fmt = NULL,
  y.label.fmt = NULL,
  extract.peaks = NULL,
  orientation = "x",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = TRUE
)

stat_valleys(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  ...,
  span = 5,
  global.threshold = 0.01,
  local.threshold = NULL,
  local.reference = "median",
  strict = FALSE,
  label.fmt = NULL,
  x.label.fmt = NULL,
  y.label.fmt = NULL,
  extract.valleys = NULL,
  orientation = "x",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{The aesthetic mapping, usually constructed with
\code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be set
at the layer level if you are overriding the plot defaults.}

\item{data}{A layer specific dataset - only needed if you want to override
the plot defaults.}

\item{geom}{The geometric object to use display the data}

\item{position}{The position adjustment to use for overlapping points
on this layer}

\item{...}{other arguments passed on to \code{\link[ggplot2]{layer}}. This can
include aesthetics whose values you want to set, not map. See
\code{\link[ggplot2]{layer}} for more details.}

\item{span}{odd positive integer A peak is defined as an element in a
sequence which is greater than all other elements within a moving window of
width \code{span} centred at that element. The default value is 5, meaning
that a peak is taller than its four nearest neighbours. \code{span = NULL}
extends the span to the whole length of \code{x}.}

\item{global.threshold}{numeric A value belonging to class
\code{"AsIs"} is interpreted as an absolute minimum height or depth
expressed in data units. A bare \code{numeric} value (normally between 0.0
and 1.0), is interpreted as relative to the range of the data. In both
cases it sets a \emph{global} height (depth) threshold below which peaks
(valleys) are ignored. A bare negative \code{numeric} value indicates the
\emph{global} height (depth) threshold below which peaks (valleys) are be
ignored. If \code{global.threshold = NULL}, no threshold is applied and all
peaks are returned.}

\item{local.threshold}{numeric A value belonging to class \code{"AsIs"} is
interpreted as an absolute minimum height (depth) expressed in data units
relative to the within-window computed minimum (maximum) value. A bare
\code{numeric} value (normally between 0.0 and 1.0), is interpreted as
expressed in units relative to the range of the data. In both cases
\code{local.threshold} sets a \emph{local} height (depth) threshold below
which peaks (valleys) are ignored. If \code{local.threshold = NULL} or if
\code{span} spans the whole of \code{x}, no threshold is applied.}

\item{local.reference}{character One of \code{"minimum"}/\code{maximum} or
\code{"median"}. The reference used to assess the height of the peak,
either the minimum value within the window or the median of all values in
the window.}

\item{strict}{logical flag: if \code{TRUE}, an element must be strictly
greater than all other values in its window to be considered a peak.}

\item{label.fmt, x.label.fmt, y.label.fmt}{character  strings giving a format
definition for construction of character strings labels with function
\code{\link{sprintf}} from \code{x} and/or \code{y} values.}

\item{extract.peaks, extract.valleys}{If \code{TRUE} only the rows containing
peaks or valleys are returned. If \code{FALSE} the whole of \code{data} is
returned but with labels set to \code{NA} in rows not containing peaks or
valleys. If \code{NULL}, the default, \code{TRUE}, is used unless the geom
name passed as argument is \code{"text_repel"} or \code{"label_repel"}.}

\item{orientation}{character The orientation of the layer can be set to
either \code{"x"}, the default, or \code{"y"}.}

\item{na.rm}{a logical value indicating whether NA values should be
stripped before the computation proceeds.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2]{borders}}.}
}
\value{
A data frame with one row for each peak (or valley) found in the
  data extracted from the input \code{data} or all rows in data. Added
  columns contain the labels.
}
\description{
\code{stat_peaks} finds at which x positions the global y maximun or local
y maxima are located. \code{stat_valleys} finds at which x positions the
global y minimum or local y minima located. They both support filtering
of relevant peaks. \strong{Axis flipping is supported.}
}
\details{
These stats use \code{geom_point} by default as it is the geom most
  likely to work well in almost any situation without need of tweaking. The
  default aesthetics set by these stats allow their direct use with
  \code{geom_text}, \code{geom_label}, \code{geom_line}, \code{geom_rug},
  \code{geom_hline} and \code{geom_vline}. The formatting of the labels
  returned can be controlled by the user.

  Two tests make it possible to ignore irrelevant peaks or valleys. One test
  controlled by (\code{global.threshold}) is based on the absolute
  height/depth  of peaks/valleys and can be used in all cases to ignore
  globally low peaks and shallow valleys. A second test controlled by
  (\code{local.threshold}) is available when the window defined by `span`
  does not include all observations and can be used to ignore peaks/valleys
  that are not locally prominent. In this second approach the height/depth of
  each peak/valley is compared to a summary computed from other values within
  the window where it was found. In this second case, the reference value
  used is the summary indicated by \code{local.reference}. The values
  \code{global.threshold} and \code{local.threshold} if bare numeric are
  relative to the range of \emph{y}. Thresholds for ignoring too small peaks
  are applied after peaks are searched for, and threshold values can in some
  cases result in no peaks being displayed.

  Date time scales are recognized and labels formatted accordingly.
}
\note{
These stats work nicely together with geoms \code{geom_text_repel} and
  \code{geom_label_repel} from package \code{\link[ggrepel]{ggrepel}} to
  solve the problem of overlapping labels
  by displacing them. To discard overlapping labels use \code{check_overlap =
  TRUE} as argument to \code{geom_text}.

  By default the labels are character values ready to be added as is, but
  with a suitable \code{label.fmt} labels suitable for parsing by the geoms
  (e.g. into expressions containing Greek letters or super or subscripts) can
  be also easily obtained.
}
\section{Computed and copied variables in the returned data frame}{

\describe{
  \item{x}{x-value at the peak (or valley) as numeric}
  \item{y}{y-value at the peak (or valley) as numeric}
  \item{x.label}{x-value at the peak (or valley) formatted as character}
  \item{y.label}{y-value at the peak (or valley) formatted as character}
}
}

\section{Default aesthetics}{

Set by the statistic and available to geoms.
\describe{
  \item{label}{stat(x.label)}
  \item{xintercept}{stat(x)}
  \item{yintercept}{stat(y)}
}
}

\section{Required aesthetics}{

Required by the statistic and need to be set with \code{aes()}.
\describe{
  \item{x}{numeric, wavelength in nanometres}
  \item{y}{numeric, a spectral quantity}
}
}

\examples{
# lynx and Nile are time.series objects recognized by
# ggpp::ggplot.ts() and converted on-the-fly with a default mapping

# numeric, date times and dates are supported with data frames

# using defaults
ggplot(Nile) +
  geom_line() +
  stat_peaks(colour = "red") +
  stat_valleys(colour = "blue")

# using wider window
ggplot(Nile) +
  geom_line() +
  stat_peaks(colour = "red", span = 11) +
  stat_valleys(colour = "blue", span = 11)

# global threshold for peak height
ggplot(Nile) +
  geom_line() +
  stat_peaks(colour = "red",
             global.threshold = 0.5) # half of data range

ggplot(Nile) +
  geom_line() +
  stat_peaks(colour = "red",
             global.threshold = I(1100)) + # data unit
             expand_limits(y = c(0, 1500))

# local (within window) threshold for peak height
# narrow peaks at the tip and locally tall

ggplot(Nile) +
  geom_line() +
  stat_peaks(colour = "red",
             span = 9,
             local.threshold = 0.3,
             local.reference = "farthest")

# with narrower window
ggplot(Nile) +
  geom_line() +
  stat_peaks(colour = "red",
             span = 5,
             local.threshold = 0.25,
             local.reference = "farthest")

ggplot(lynx) +
  geom_line() +
  stat_peaks(colour = "red",
             local.threshold = 1/5,
             local.reference = "median")

ggplot(Nile) +
  geom_line() +
  stat_valleys(colour = "blue",
               global.threshold = I(700))

# orientation is supported
ggplot(lynx, aes(lynx, time)) +
  geom_line(orientation = "y") +
  stat_peaks(colour = "red", orientation = "y") +
  stat_valleys(colour = "blue", orientation = "y")

# default aesthetic mapping supports additional geoms
ggplot(lynx) +
  geom_line() +
  stat_peaks(colour = "red") +
  stat_peaks(colour = "red", geom = "rug")

ggplot(lynx) +
  geom_line() +
  stat_peaks(colour = "red") +
  stat_peaks(colour = "red", geom = "text", hjust = -0.1, angle = 33)

ggplot(lynx, aes(lynx, time)) +
  geom_line(orientation = "y") +
  stat_peaks(colour = "red", orientation = "y") +
  stat_peaks(colour = "red", orientation = "y",
             geom = "text", hjust = -0.1)

# Force conversion of time series time into POSIXct date time
ggplot(lynx, as.numeric = FALSE) +
  geom_line() +
  stat_peaks(colour = "red") +
  stat_peaks(colour = "red",
             geom = "text",
             hjust = -0.1,
             x.label.fmt = "\%Y",
             angle = 33)

ggplot(Nile, as.numeric = FALSE) +
  geom_line() +
  stat_peaks(colour = "red") +
  stat_peaks(colour = "red",
             geom = "text_s",
             position = position_nudge_keep(x = 0, y = 60),
             hjust = -0.1,
             x.label.fmt = "\%Y",
             angle = 90) +
  expand_limits(y = 2000)

ggplot(lynx, as.numeric = FALSE) +
  geom_line() +
  stat_peaks(colour = "red",
             geom = "text_s",
             position = position_nudge_to(y = 7600),
             arrow = arrow(length = grid::unit(1.5, "mm")),
             point.padding = 0.7,
             x.label.fmt = "\%Y",
             angle = 90) +
  expand_limits(y = 9000)

}
\seealso{
\code{\link[photobiology]{find_peaks}}, which is used internally.

\code{\link{find_peaks}}, for the functions used to located the
  peaks and valleys.
}
