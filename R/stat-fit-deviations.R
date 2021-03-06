#' Residuals from model fit as segments
#'
#' \code{stat_fit_deviations} fits a linear model and returns fitted values and
#' residuals ready to be plotted as segments.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and should not inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param method function or character If character, "lm", "rlm", "lqs" and "rq"
#'   are implemented. If a function, it must support parameters \code{formula}
#'   and \code{data}.
#' @param method.args named list with additional arguments.
#' @param formula a "formula" object. Using aesthetic names instead of
#'   original variable names.
#'
#' @details This stat can be used to automatically highlight residuals as
#'   segments in a plot of a fitted model equation. This stat only
#'   generates the residuals, the predicted values need to be separately added
#'   to the plot, so to make sure that the same model formula is used in all
#'   steps it is best to save the formula as an object and supply this object as
#'   argument to the different statistics.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. In other words, it respects the grammar of graphics and
#'   consequently within the model \code{formula} names of aesthetics like $x$
#'   and $y$ should be used instead of the original variable names. This helps
#'   ensure that the model is fitted to the same data as plotted in other
#'   layers.
#'
#' @note In the case of \code{method = "rq"} quantiles is fixed at \code{tau = 0.5}
#'   unless \code{method.args} has length > 0.
#'
#' @section Computed variables: Data frame with same \code{nrow} as \code{data}
#'   as subset for each group containing five numeric variables. \describe{
#'   \item{x}{x coordinates of observations} \item{y.fitted}{x coordinates of
#'   fitted values} \item{y}{y coordinates of observations} \item{y.fitted}{y
#'   coordinates of fitted values}}
#'
#'   To explore the values returned by this statistic we suggest the use of
#'   \code{\link[gginnards]{geom_debug}}. An example is shown below, where one
#'   can also see in addition to the computed values the default mapping of the
#'   fitted values to aesthetics \code{xend} and \code{yend}.
#'
#' @family statistics for linear model fits
#'
#' @examples
#' library(gginnards) # needed for geom_debug()
#' library(MASS)
#'
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
#' my.data <- data.frame(x, y, group = c("A", "B"), y2 = y * c(0.5,2))
#'
#' # give a name to a formula
#' my.formula <- y ~ poly(x, 3, raw = TRUE)
#'
#' # plot linear regression
#' ggplot(my.data, aes(x, y)) +
#'   geom_smooth(method = "lm", formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, colour = "red") +
#'   geom_point()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_smooth(method = "lm", formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, method = stats::lm, colour = "red") +
#'   geom_point()
#'
#' # plot robust regression
#' ggplot(my.data, aes(x, y)) +
#'   stat_smooth(method = "rlm", formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, method = "rlm", colour = "red") +
#'   geom_point()
#'
#' # plot resistant regression
#' ggplot(my.data, aes(x, y)) +
#'   stat_fit_deviations(formula = my.formula, method = "lqs", colour = "red") +
#'   geom_point()
#'
#' # plot quantile regression (= median regression)
#' ggplot(my.data, aes(x, y)) +
#'   stat_quantile(formula = my.formula, quantiles = 0.5) +
#'   stat_fit_deviations(formula = my.formula, method = "rq", colour = "red") +
#'   geom_point()
#'
#' # plot quantile regression (= "quartile" regression)
#' ggplot(my.data, aes(x, y)) +
#'   stat_quantile(formula = my.formula, quantiles = 0.75) +
#'   stat_fit_deviations(formula = my.formula, colour = "red",
#'                       method = "rq", method.args = list(tau = 0.75)) +
#'   geom_point()
#'
#' # plot, using geom_debug() to explore the after_stat data
#' ggplot(my.data, aes(x, y)) +
#'   geom_smooth(method = "lm", formula = my.formula) +
#'   stat_fit_deviations(formula = my.formula, colour = "red",
#'   geom = "debug") +
#'   geom_point()
#'
#' @export
#'
stat_fit_deviations <- function(mapping = NULL, data = NULL, geom = "segment",
                               method = "lm",
                               method.args = list(),
                               formula = NULL,
                               position = "identity",
                               na.rm = FALSE, show.legend = FALSE,
                               inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatFitDeviations, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  formula = formula,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitDeviations <-
  ggplot2::ggproto("StatFitDeviations", ggplot2::Stat,
                   compute_group = function(data,
                                            scales,
                                            method,
                                            method.args,
                                            formula) {
                     stopifnot(!any(c("formula", "data") %in% names(method.args)))
                     if (is.function(method)) {
                       fun <- method
                     } else if (is.character(method)) {
                       if (method == "rq" && length(method.args) == 0) {
                         method.args <- list(tau = 0.5)
                       }
                       fun <- switch(method,
                                     lm = stats::lm,
                                     rlm = MASS::rlm,
                                     lqs = MASS::lqs,
                                     rq = quantreg::rq,
                                     stop("Method '", method, "' not yet implemented.")
                       )
                     } else {
                       stop("Method '", method, "' not yet implemented.")
                     }
                     mf <- do.call(fun,
                                   args = c(list(formula = formula, data = data),
                                            method.args))
                     fitted.vals <- fitted(mf)
                     data.frame(x = data$x,
                                y = data$y,
                                x.fitted = data$x,
                                y.fitted = fitted.vals,
                                hjust = 0)
                   },
                   default_aes =
                     ggplot2::aes(xend = stat(x.fitted),
                                  yend = stat(y.fitted)),
                   required_aes = c("x", "y")
  )
