#' Assemble label and map it
#'
#' Assemble model-fit-derived text or expressions and map them to
#' the \code{label} aesthetic.
#'
#' @param ... character Strings giving the names of the label components in the
#'   order they will be included in the combined label.
#' @param labels character A vector with the name of the label components. If
#'   provided, values passed through \code{...} are ignored.
#' @param sep character A string used as separator when pasting the label
#'   components together.
#' @param other.mapping An unevaluated expression constructed with function
#'   \code{\link[ggplot2]{aes}} to be included in the returned value.
#'
#' @details Statistics \code{\link{stat_poly_eq}}, \code{\link{stat_ma_eq}},
#'   \code{\link{stat_quant_eq}} and \code{\link{stat_correlation}} return
#'   multiple text strings to be used individually or assembled into longer
#'   character strings depending on the labels actually desired. Assembling and
#'   mapping them requires verbose R code and familiarity with R expression
#'   syntax. Function \code{use_label()} automates these two tasks and accepts
#'   abbreviated familiar names for the parameters in addition to the name of
#'   the columns in the data object returned by the statistics. The default
#'   separator is that for expressions.
#'
#'   The statistics return variables with names ending in \code{.label}. This
#'   ending can be omitted, as well as \code{.value} for \code{f.value.label},
#'   \code{t.value.label}, \code{z.value.label}, \code{S.value.label} and
#'   \code{p.value.label}. \code{R2} can be used in place of \code{rr}.
#'   Furthermore, case is ignored.
#'
#'   Function \code{use_label()} calls \code{aes()} to create a mapping for
#'   the \code{label} aesthetic, but it can in addition combine this mapping
#'   with other mappings created with \code{aes()}.
#'
#' @return A mapping to the \code{label} aesthetic and optionally additional
#'   mappings as an unevaluated R expression, built using function
#'   \code{\link[ggplot2]{aes}}, ready to be passed as argument to the
#'   \code{mapping} parameter of the supported statistics.
#'
#' @note Function \code{use_label()} can be only used to generate an argument
#'   passed to formal parameter \code{mapping} of the statistics
#'   \code{\link{stat_poly_eq}}, \code{\link{stat_ma_eq}},
#'   \code{\link{stat_quant_eq}} and \code{\link{stat_correlation}}.
#'
#' @seealso \code{\link{stat_poly_eq}}, \code{\link{stat_ma_eq}},
#'   \code{\link{stat_quant_eq}} and \code{\link{stat_correlation}}.
#'
#' @export
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x = x,
#'                       y = y * 1e-5,
#'                       group = c("A", "B"),
#'                       y2 = y * 1e-5 + c(2, 0))
#'
#' # give a name to a formula
#' formula <- y ~ poly(x, 3, raw = TRUE)
#'
#' # default label constructed by use_label()
#' ggplot(data = my.data,
#'        mapping = aes(x = x, y = y2, colour = group)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(mapping = use_label(),
#'                formula = formula)
#'
#' # user specified label components
#' ggplot(data = my.data,
#'        mapping = aes(x = x, y = y2, colour = group)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(mapping = use_label("eq", "F"),
#'               formula = formula)
#'
#' # user specified label components and separator
#' ggplot(data = my.data,
#'        mapping = aes(x = x, y = y2, colour = group)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(mapping = use_label("R2", "F", sep = "*\" with \"*"),
#'                formula = formula)
#'
#' # combine the mapping to the label aesthetic with other mappings
#' ggplot(data = my.data,
#'        mapping = aes(x = x, y = y2)) +
#'   geom_point(mapping = aes(colour = group)) +
#'   stat_poly_line(mapping = aes(colour = group), formula = formula) +
#'   stat_poly_eq(mapping = use_label("grp", "eq", "F",
#'                                    aes(grp.label = group)),
#'               formula = formula)
#'
#' # combine other mappings with default labels
#' ggplot(data = my.data,
#'        mapping = aes(x = x, y = y2)) +
#'   geom_point(mapping = aes(colour = group)) +
#'   stat_poly_line(mapping = aes(colour = group), formula = formula) +
#'   stat_poly_eq(mapping = use_label(aes(colour = group)),
#'               formula = formula)
#'
#' # example with other available components
#' ggplot(data = my.data,
#'        mapping = aes(x = x, y = y2, colour = group)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(mapping = use_label("eq", "adj.R2", "n"),
#'                formula = formula)
#'
#' # multiple labels
#' ggplot(data = my.data,
#'        mapping = aes(x, y2, colour = group)) +
#'   geom_point() +
#'   stat_poly_line(formula = formula) +
#'   stat_poly_eq(mapping = use_label("R2", "F", "P", "AIC", "BIC"),
#'                formula = formula) +
#'   stat_poly_eq(mapping = use_label(c("eq", "n")),
#'                formula = formula,
#'                label.y = "bottom",
#'                label.x = "right")
#'
#' # quantile regression
#' ggplot(data = my.data,
#'        mapping = aes(x, y)) +
#'   stat_quant_band(formula = formula) +
#'   stat_quant_eq(mapping = use_label("eq", "n"),
#'                 formula = formula) +
#'   geom_point()
#'
#' # major axis regression
#' ggplot(data = my.data, aes(x = x, y = y)) +
#'   stat_ma_line() +
#'   stat_ma_eq(mapping = use_label("eq", "n")) +
#'   geom_point()
#'
#' # correlation
#' ggplot(data = my.data,
#'        mapping = aes(x = x, y = y)) +
#'   stat_correlation(mapping = use_label("r", "t", "p")) +
#'   geom_point()
#'
use_label <- function(...,
                      labels = NULL,
                      other.mapping = NULL,
                      sep =  "*\", \"*") {
  if (!length(labels)) {
    labels <- list(...)
    if (length(labels)) {
      for (i in seq_along(labels)) {
        if (inherits(labels[[i]], "uneval")) {
          other.mapping <- labels[[i]]
          labels[[i]] <- NULL
        }
      }
    }
    if (length(labels)) { # <- NULL above makes this necessary
      if (length(labels[[1]]) > 1L) {
        # backwards compatibility: accept vector as if passed by position to labels
        labels <- labels[[1]]
      }
    }
  }
  if (!length(labels)) {
    labels <- c("eq", "p.value")
  }

  if (length(labels) > 5) {
    warning("Pasting first 5 labels and discarding others.")
    labels <- labels[1:5]
  }

  # accept upper case equivalents
  labels <- tolower(labels)
  # accept short names lacking ".label" as ending
  truncated.labels <- !grepl("\\.label$|\\.f$", labels)
  labels[truncated.labels] <- paste(labels[truncated.labels], ".label", sep = "")
  # accept R2 and CI
  labels <- gsub("r2\\.", "rr.", labels)
  labels <- gsub("ci\\.label$", "confint.label", labels)
  # accept F and P
  labels <- gsub("^f\\.label$", "f.value.label", labels)
  labels <- gsub("^p\\.label$", "p.value.label", labels)
  labels <- gsub("^t\\.label$", "t.value.label", labels)
  labels <- gsub("^z\\.label$", "z.value.label", labels)
  labels <- gsub("^s\\.label$", "s.value.label", labels)
  # force AIC, BIC and S in capitals
  labels <- gsub("^aic.label$", "AIC.label", labels)
  labels <- gsub("^bic.label$", "BIC.label", labels)
  labels <- gsub("^s.value.label$", "S.value.label", labels)
  # make mapping to label aesthetic
  label.mapping <-
    switch(length(labels),
           ggplot2::aes(label =
                          ggplot2::after_stat(.data[[labels[1]]])),
           ggplot2::aes(label =
                          paste(ggplot2::after_stat(.data[[labels[1]]]),
                                ggplot2::after_stat(.data[[labels[2]]]),
                                sep = sep)),
           ggplot2::aes(label =
                          paste(ggplot2::after_stat(.data[[labels[1]]]),
                                ggplot2::after_stat(.data[[labels[2]]]),
                                ggplot2::after_stat(.data[[labels[3]]]),
                                sep = sep)),
           ggplot2::aes(label =
                          paste(ggplot2::after_stat(.data[[labels[1]]]),
                                ggplot2::after_stat(.data[[labels[2]]]),
                                ggplot2::after_stat(.data[[labels[3]]]),
                                ggplot2::after_stat(.data[[labels[4]]]),
                                sep = sep)),
           ggplot2::aes(label =
                          paste(ggplot2::after_stat(.data[[labels[1]]]),
                                ggplot2::after_stat(.data[[labels[2]]]),
                                ggplot2::after_stat(.data[[labels[3]]]),
                                ggplot2::after_stat(.data[[labels[4]]]),
                                ggplot2::after_stat(.data[[labels[5]]]),
                                sep = sep))
    )
  if (!is.null(other.mapping)) {
    utils::modifyList(other.mapping, label.mapping)
  } else {
    label.mapping
  }
}
