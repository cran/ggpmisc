# broom::tidy as tibble -----------------------------------------------------

#' @title Model-fit summary or ANOVA
#'
#' @description \code{stat_fit_tb} fits a model and returns a "tidy" version of
#'   the model's summary or ANOVA table, using package 'broom'. The annotation
#'   is added to the plots in tabular form.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
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
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param method character.
#' @param method.args list of arguments to pass to \code{method}.
#' @param tb.type character One of "fit.summary", "fit.anova" or "fit.coefs".
#' @param digits integer indicating the number of significant digits to be used.
#' @param tb.vars character vector, optionally named, used to select and or
#'   rename the columns of the table returned.
#' @param label.x.npc,label.y.npc \code{numeric} with range 0..1 or character.
#'   Coordinates to be used for positioning the output, expressed in "normalized
#'   parent coordinates" or character string. If too short they will be
#'   recycled.
#' @param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'   for absolute positioning of the output. If too short they will be recycled.
#'
#' @section Computed variables: The output of \code{\link[broom]{tidy}} is
#'   returned as a single "cell" in a tibble (i.e. a tibble nested within a
#'   tibble). The returned \code{data} object contains a single, containing the
#'   result from a single model fit to all data in a panel. If grouping is
#'   present, it is ignored.
#'
#' @seealso \code{\link[broom]{tidy}} for details on how the tidying of the
#'   resulst of model fits is done. See \code{\link{geom_table}} for details
#'   on how the formating and location of the table can be adjusted.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' # t-test example
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' group <- factor(c(rep("A", 4), rep("B", 5)))
#' my.df <- data.frame(x, group)
#'
#' ggplot(my.df, aes(group, x)) +
#'   geom_point() +
#'   stat_fit_tb(method = "t.test",
#'               tb.vars = c("italic(t)" = "estimate", "italic(P)" = "p.value"),
#'               parse = TRUE)
#'
stat_fit_tb <- function(mapping = NULL, data = NULL, geom = "table_npc",
                        method = "lm",
                        method.args = list(formula = y ~ x),
                        tb.type = "fit.summary",
                        tb.vars = NULL,
                        digits = 3,
                        label.x = "center", label.y = "top",
                        label.x.npc = NULL, label.y.npc = NULL,
                        position = "identity",
                        na.rm = FALSE, show.legend = FALSE,
                        inherit.aes = TRUE,
                        ...) {
  # backwards compatibility
  if (!is.null(label.x.npc)) {
    stopifnot(grepl("_npc", geom))
    label.x <- label.x.npc
  }
  if (!is.null(label.y.npc)) {
    stopifnot(grepl("_npc", geom))
    label.y <- label.y.npc
  }
  ggplot2::layer(
    stat = StatFitTb, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  tb.type = tb.type,
                  tb.vars = tb.vars,
                  digits = digits,
                  label.x = label.x,
                  label.y = label.y,
                  npc.used = grepl("_npc", geom),
                  na.rm = na.rm,
                  ...)
  )
}

# Defined here to avoid a note in check --as-cran as the imports from 'broom'
# are not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
fit_tb_compute_panel_fun <- function(data,
                                     scales,
                                     method,
                                     method.args,
                                     tb.type,
                                     tb.vars,
                                     digits,
                                     npc.used = TRUE,
                                     label.x,
                                     label.y) {
  force(data)

  if (length(unique(data$x)) < 2) {
    # Not enough data to perform fit
    return(data.frame())
  }

  # support setting of table position per panel
  panel.idx <- as.integer(as.character(data$PANEL[1]))
  if (length(label.x) >= panel.idx) {
    label.x <- label.x[panel.idx]
  } else if (length(label.x) > 0) {
    label.x <- label.x[1]
  }
  if (length(label.y) >= panel.idx) {
    label.y <- label.y[panel.idx]
  } else if (length(label.y) > 0) {
    label.y <- label.y[1]
  }

  method.args <- c(method.args, list(data = quote(data)))
  if (is.character(method)) method <- match.fun(method)
  mf <- do.call(method, method.args)

  if (tolower(tb.type) %in% c("fit.anova", "anova")) {
    mf_tb <- broom::tidy(stats::anova(mf))
  } else if (tolower(tb.type) %in% c("fit.summary", "summary")) {
    mf_tb <- broom::tidy(mf)
  } else if (tolower(tb.type) %in% c("fit.coefs", "coefs")) {
    mf_tb <- broom::tidy(mf)[c("term", "estimate")]
  }

  num.cols <- sapply(mf_tb, is.numeric)
  mf_tb[num.cols] <- signif(mf_tb[num.cols], digits = digits)

  if(!is.null(tb.vars)) {
    mf_tb <- dplyr::select(mf_tb, !!tb.vars)
  }

  # we need to enclose the tibble in a list to mannualy nest the table in
  # data.
  z <- tibble::tibble(mf_tb = list(mf_tb))

  if (npc.used) {
    margin.npc = 0.05
    npc.positions <- c(right = 1 - margin.npc,
                       left = 0 + margin.npc,
                       centre = 0.5,
                       center = 0.5,
                       middle = 0.5,
                       top = 1 - margin.npc,
                       bottom = 0 + margin.npc)
    if (is.character(label.x)) {
      label.x <- npc.positions[label.x]
    }
    if (is.character(label.y)) {
      label.y <- npc.positions[label.y]
    }
    z$npcx <- label.x
    z$x <- NA_real_
    z$npcy <- label.y
    z$y <- NA_real_
  } else {
    z$x <- label.x
    z$npcx <- NA_real_
    z$y <- label.y
    z$npcy <- NA_real_
  }
  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitTb <-
  ggplot2::ggproto("StatFitTb", ggplot2::Stat,
                   compute_panel = fit_tb_compute_panel_fun,
                   default_aes =
                     ggplot2::aes(hjust = "inward",
                                  vjust = "inward",
                                  label = stat(mf_tb)),
                   required_aes = c("x", "y")
  )
