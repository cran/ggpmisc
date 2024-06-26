% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-eq-label.R
\name{poly2character}
\alias{poly2character}
\title{Convert a polynomial into character string}
\usage{
poly2character(
  x,
  decreasing = getOption("ggpmisc.decreasing.poly.eq", FALSE),
  digits = 3,
  keep.zeros = TRUE
)
}
\arguments{
\item{x}{a \code{polynomial} object.}

\item{decreasing}{logical It specifies the order of the terms; in increasing
(default) or decreasing powers.}

\item{digits}{integer Giving the number of significant digits to use for
printing.}

\item{keep.zeros}{logical It indicates if zeros are to be retained in the
formatted coefficients.}
}
\value{
A \code{character} string.
}
\description{
Differs from \code{polynom::as.character.polynomial()} in that trailing zeros
are preserved.
}
\note{
This is an edit of the code in package 'polynom' so that trailing zeros are
 retained during the conversion. It is not defined using a different name
 so as not to interfere with the original.
}
\examples{
poly2character(1:3)
poly2character(1:3, decreasing = TRUE)

}
