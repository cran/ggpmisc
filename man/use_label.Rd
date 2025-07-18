% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use-label.R
\name{use_label}
\alias{use_label}
\title{Assemble label and map it}
\usage{
use_label(..., labels = NULL, other.mapping = NULL, sep = "*\\", \\"*")
}
\arguments{
\item{...}{character Strings giving the names of the label components in the
order they will be included in the combined label.}

\item{labels}{character A vector with the name of the label components. If
provided, values passed through \code{...} are ignored.}

\item{other.mapping}{An unevaluated expression constructed with function
\code{\link[ggplot2]{aes}} to be included in the returned value.}

\item{sep}{character A string used as separator when pasting the label
components together.}
}
\value{
A mapping to the \code{label} aesthetic and optionally additional
  mappings as an unevaluated R expression, built using function
  \code{\link[ggplot2]{aes}}, ready to be passed as argument to the
  \code{mapping} parameter of the supported statistics.
}
\description{
Assemble model-fit-derived text or expressions and map them to
the \code{label} aesthetic.
}
\details{
Statistics \code{\link{stat_poly_eq}}, \code{\link{stat_ma_eq}},
  \code{\link{stat_quant_eq}} and \code{\link{stat_correlation}} return
  multiple text strings to be used individually or assembled into longer
  character strings depending on the labels actually desired. Assembling and
  mapping them requires verbose R code and familiarity with R expression
  syntax. Function \code{use_label()} automates these two tasks and accepts
  abbreviated familiar names for the parameters in addition to the name of
  the columns in the data object returned by the statistics. The default
  separator is that for expressions.

  The statistics return variables with names ending in \code{.label}. This
  ending can be omitted, as well as \code{.value} for \code{f.value.label},
  \code{t.value.label}, \code{z.value.label}, \code{S.value.label} and
  \code{p.value.label}. \code{R2} can be used in place of \code{rr}.
  Furthermore, case is ignored.

  Function \code{use_label()} calls \code{aes()} to create a mapping for
  the \code{label} aesthetic, but it can in addition combine this mapping
  with other mappings created with \code{aes()}.
}
\note{
Function \code{use_label()} can be only used to generate an argument
  passed to formal parameter \code{mapping} of the statistics
  \code{\link{stat_poly_eq}}, \code{\link{stat_ma_eq}},
  \code{\link{stat_quant_eq}} and \code{\link{stat_correlation}}.
}
\examples{
# generate artificial data
set.seed(4321)
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x = x,
                      y = y * 1e-5,
                      group = c("A", "B"),
                      y2 = y * 1e-5 + c(2, 0))

# give a name to a formula
formula <- y ~ poly(x, 3, raw = TRUE)

# default label constructed by use_label()
ggplot(data = my.data,
       mapping = aes(x = x, y = y2, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label(),
               formula = formula)

# user specified label components
ggplot(data = my.data,
       mapping = aes(x = x, y = y2, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq", "F"),
              formula = formula)

# user specified label components and separator
ggplot(data = my.data,
       mapping = aes(x = x, y = y2, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("R2", "F", sep = "*\" with \"*"),
               formula = formula)

# combine the mapping to the label aesthetic with other mappings
ggplot(data = my.data,
       mapping = aes(x = x, y = y2)) +
  geom_point(mapping = aes(colour = group)) +
  stat_poly_line(mapping = aes(colour = group), formula = formula) +
  stat_poly_eq(mapping = use_label("grp", "eq", "F",
                                   aes(grp.label = group)),
              formula = formula)

# combine other mappings with default labels
ggplot(data = my.data,
       mapping = aes(x = x, y = y2)) +
  geom_point(mapping = aes(colour = group)) +
  stat_poly_line(mapping = aes(colour = group), formula = formula) +
  stat_poly_eq(mapping = use_label(aes(colour = group)),
              formula = formula)

# example with other available components
ggplot(data = my.data,
       mapping = aes(x = x, y = y2, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq", "adj.R2", "n"),
               formula = formula)

# multiple labels
ggplot(data = my.data,
       mapping = aes(x, y2, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("R2", "F", "P", "AIC", "BIC"),
               formula = formula) +
  stat_poly_eq(mapping = use_label(c("eq", "n")),
               formula = formula,
               label.y = "bottom",
               label.x = "right")

# quantile regression
ggplot(data = my.data,
       mapping = aes(x, y)) +
  stat_quant_band(formula = formula) +
  stat_quant_eq(mapping = use_label("eq", "n"),
                formula = formula) +
  geom_point()

# major axis regression
ggplot(data = my.data, aes(x = x, y = y)) +
  stat_ma_line() +
  stat_ma_eq(mapping = use_label("eq", "n")) +
  geom_point()

# correlation
ggplot(data = my.data,
       mapping = aes(x = x, y = y)) +
  stat_correlation(mapping = use_label("r", "t", "p")) +
  geom_point()

}
\seealso{
\code{\link{stat_poly_eq}}, \code{\link{stat_ma_eq}},
  \code{\link{stat_quant_eq}} and \code{\link{stat_correlation}}.
}
