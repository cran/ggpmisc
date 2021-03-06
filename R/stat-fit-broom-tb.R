# generics::tidy as tibble -----------------------------------------------------

#' @title Model-fit summary or ANOVA
#'
#' @description \code{stat_fit_tb} fits a model and returns a "tidy" version of
#'   the model's summary or ANOVA table, using '\code{tidy()} methods from
#'   packages 'broom', 'broom.mixed', or other sources. The annotation is added
#'   to the plots in tabular form.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset, only needed if you want to override
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
#' @param method.args,tidy.args lists of arguments to pass to \code{method}
#'   and to \code{tidy()}.
#' @param tb.type character One of "fit.summary", "fit.anova" or "fit.coefs".
#' @param digits integer indicating the number of significant digits
#'   to be used for all numeric values in the table.
#' @param p.digits integer indicating the number of decimal places to round
#'   p-values to, with those rounded to zero displayed as the next larger
#'   possible value preceded by "<". If \code{p.digits} is outside the
#'   range 1..22 no rounding takes place.
#' @param tb.vars,tb.params character or numeric vectors, optionally named, used
#'   to select and/or rename the columns or the parameters in the table
#'   returned.
#' @param label.x.npc,label.y.npc \code{numeric} with range 0..1 or character.
#'   Coordinates to be used for positioning the output, expressed in "normalized
#'   parent coordinates" or character string. If too short they will be
#'   recycled.
#' @param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'   for absolute positioning of the output. If too short they will be recycled.
#' @param table.theme NULL, list or function A gridExtra ttheme defintion, or
#'   a constructor for a ttheme or NULL for default.
#' @param table.rownames,table.colnames logical flag to enable or disabling
#'   printing of row names and column names.
#' @param table.hjust numeric Horizontal justification for the core and column
#'   headings of the table.
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#'
#' @details \code{stat_fit_tb} Applies a model fitting function per panel,
#'   using the grouping factors from easthetic mappings in the fitted model.
#'   This is suitable, for example for analysis of variance used to test for
#'   differences among groups.
#'
#'   The argument to \code{method} can be any fit method for which a
#'   suitable \code{tidy()} method is available, including non-linear
#'   regression. Fit methods retain their default arguments unless orverridden.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. In other words, it respects the grammar of graphics and
#'   consequently within arguments passed through \code{method.args} names of
#'   aesthetics like $x$ and $y$ should be used instead of the original variable
#'   names, while data is automatically passed the data frame. This helps ensure
#'   that the model is fitted to the same data as plotted in other layers.
#'
#' @section Computed variables: The output of \code{tidy()} is
#'   returned as a single "cell" in a tibble (i.e. a tibble nested within a
#'   tibble). The returned \code{data} object contains a single, containing the
#'   result from a single model fit to all data in a panel. If grouping is
#'   present, it is ignored.
#'
#'   To explore the values returned by this statistic, which vary depending
#'   on the model fitting function and model formula we suggest the use of
#'   \code{\link[gginnards]{geom_debug}}. An example is shown below.
#'
#' @seealso \code{\link[broom]{broom}} and
#'   \code{broom.mixed} for details on how the tidying of the
#'   result of model fits is done. See \code{\link{geom_table}} for details on
#'   how inset tables respond to mapped aesthetics and table themes. For details
#'   on predefined table themes see \code{\link{ttheme_gtdefault}}.
#'
#' @family Statistics calling generic tidier methods.
#'
#' @export
#'
#' @examples
#' library(broom)
#'
#' # data for examples
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' covariate <- sqrt(x) + rnorm(9)
#' group <- factor(c(rep("A", 4), rep("B", 5)))
#' my.df <- data.frame(x, group, covariate)
#'
#' # Linear regression fit summary, by default
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb() +
#'   expand_limits(y = 70)
#'
#' # Linear regression fit summary, by default
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(digits = 2, p.digits = 4) +
#'   expand_limits(y = 70)
#'
#' # Linear regression fit summary
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.type = "fit.summary") +
#'   expand_limits(y = 70)
#'
#' # Linear regression ANOVA table
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.type = "fit.anova") +
#'   expand_limits(y = 70)
#'
#' # Linear regression fit coeficients
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.type = "fit.coefs") +
#'   expand_limits(y = 70)
#'
#' # Polynomial regression
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(method.args = list(formula = y ~ poly(x, 2))) +
#'   expand_limits(y = 70)
#'
#' # Polynomial regression with renamed parameters
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(method.args = list(formula = y ~ poly(x, 2)),
#'               tb.params = c("x^0" = 1, "x^1" = 2, "x^2" = 3),
#'               parse = TRUE) +
#'   expand_limits(y = 70)
#'
#' # Polynomial regression with renamed parameters and columns
#' # using numeric indexes
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(method.args = list(formula = y ~ poly(x, 2)),
#'               tb.params = c("x^0" = 1, "x^1" = 2, "x^2" = 3),
#'               tb.vars = c("Term" = 1, "Estimate" = 2, "S.E." = 3,
#'                           "italic(F)-value" = 4, "italic(P)-value" = 5),
#'               parse = TRUE) +
#'   expand_limits(y = 70)
#'
#' # ANOVA summary
#' ggplot(my.df, aes(group, x)) +
#'   geom_point() +
#'   stat_fit_tb() +
#'   expand_limits(y = 70)
#'
#' # ANOVA table
#' ggplot(my.df, aes(group, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.type = "fit.anova") +
#'   expand_limits(y = 70)
#'
#' # ANOVA table with renamed and selected columns
#' # using column names
#' ggplot(my.df, aes(group, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.type = "fit.anova",
#'               tb.vars = c(Effect = "term", "df", "italic(F)" = "statistic",
#'                           "italic(P)" = "p.value"),
#'               parse = TRUE)
#'
#' # ANOVA table with renamed and selected columns
#' # using column names with partial matching
#' ggplot(my.df, aes(group, x)) +
#'   geom_point() +
#'   stat_fit_tb(tb.type = "fit.anova",
#'               tb.vars = c(Effect = "term", "df", "italic(F)" = "stat",
#'                           "italic(P)" = "p"),
#'               parse = TRUE)
#'
#' # ANOVA summary
#' ggplot(my.df, aes(group, x)) +
#'   geom_point() +
#'   stat_fit_tb() +
#'   expand_limits(y = 70)
#'
#' # ANCOVA (covariate not plotted)
#' ggplot(my.df, aes(group, x, z = covariate)) +
#'   geom_point() +
#'   stat_fit_tb(method.args = list(formula = y ~ x + z),
#'               tb.vars = c(Effect = "term", "italic(F)" = "statistic", "italic(P)" = "p.value"),
#'               parse = TRUE)
#'
#' # t-test
#' ggplot(my.df, aes(group, x)) +
#'   geom_point() +
#'   stat_fit_tb(method = "t.test",
#'               tb.vars = c("italic(t)" = "statistic", "italic(P)" = "p.value"),
#'               parse = TRUE)
#'
#' # t-test (equal variances assumed)
#' ggplot(my.df, aes(group, x)) +
#'   geom_point() +
#'   stat_fit_tb(method = "t.test",
#'               method.args = list(formula = y ~ x, var.equal = TRUE),
#'               tb.vars = c("italic(t)" = "statistic", "italic(P)" = "p.value"),
#'               parse = TRUE)
#'
#' # Linear regression using a table theme
#' ggplot(my.df, aes(covariate, x)) +
#'   geom_point() +
#'   stat_fit_tb(table.theme = ttheme_gtlight) +
#'   expand_limits(y = 70)
#'
stat_fit_tb <- function(mapping = NULL, data = NULL, geom = "table_npc",
                        method = "lm",
                        method.args = list(formula = y ~ x),
                        tidy.args = list(),
                        tb.type = "fit.summary",
                        tb.vars = NULL,
                        tb.params = NULL,
                        digits = 3,
                        p.digits = digits,
                        label.x = "center", label.y = "top",
                        label.x.npc = NULL, label.y.npc = NULL,
                        position = "identity",
                        table.theme = NULL,
                        table.rownames = FALSE,
                        table.colnames = TRUE,
                        table.hjust = 1,
                        parse = FALSE,
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
                  tidy.args = tidy.args,
                  tb.type = tb.type,
                  tb.vars = tb.vars,
                  tb.params = tb.params,
                  digits = digits,
                  p.digits = p.digits,
                  label.x = label.x,
                  label.y = label.y,
                  npc.used = grepl("_npc", geom),
                  table.theme = table.theme,
                  table.rownames = table.rownames,
                  table.colnames = table.colnames,
                  table.hjust = table.hjust,
                  parse = parse,
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
                                     tidy.args,
                                     tb.type,
                                     tb.vars,
                                     tb.params,
                                     digits,
                                     p.digits,
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

  if (is.character(method)) method <- match.fun(method)
  if ("data" %in% names(method.args)) {
    message("External 'data' passed can be inconsistent with plot!\n",
            "These data must be available at the time of printing!!!")
  } else if (any(grepl("formula|fixed|random|model", names(method.args)))) {
    #    method.args <- c(method.args, list(data = quote(data)))  works in most cases and avoids copying data
    method.args <- c(method.args, list(data = data)) # cor.test() needs the actual data
  } else {
    message("Only the 'formula' interface of methods is well supported.")
    if ("x" %in% names(method.args)) {
      message("Passing data$x as 'x'.")
      method.args[["x"]] <- data[["x"]]
    }
    if ("y" %in% names(method.args)) {
      message("Passing data$y as 'y'.")
      method.args[["y"]] <- data[["y"]]
    }
  }
  mf <- do.call(method, method.args)

  if (tolower(tb.type) %in% c("fit.anova", "anova")) {
    tidy.args <- c(x = quote(stats::anova(mf)), tidy.args)
    mf_tb <- do.call(generics::tidy, tidy.args)
  } else if (tolower(tb.type) %in% c("fit.summary", "summary")) {
    tidy.args <- c(x = quote(mf), tidy.args)
    mf_tb <- do.call(generics::tidy, tidy.args)
  } else if (tolower(tb.type) %in% c("fit.coefs", "coefs")) {
    tidy.args <- c(x = quote(mf), tidy.args)
    mf_tb <- do.call(generics::tidy, tidy.args)[c("term", "estimate")]
  }

  # reduce number of significant digits of all numeric columns
  num.cols <- sapply(mf_tb, is.numeric)
  mf_tb[num.cols] <- signif(mf_tb[num.cols], digits = digits)
  # treat p.value as a special case
  if ("p.value" %in% colnames(mf_tb) && p.digits > 0 && p.digits <= 22) {
    mf_tb[["p.value"]] <- round(mf_tb[["p.value"]], digits = p.digits)
    limit.text <- paste("<", format(1 * 10^-p.digits, nsmall = p.digits))
    mf_tb[["p.value"]] <- ifelse(mf_tb[["p.value"]] > 0,
                                 format(mf_tb[["p.value"]], nsmall = p.digits),
                                 limit.text)
  }

  if (!is.null(tb.params) && !is.null(mf_tb)) {
    if (is.character(tb.params)) {
      idxs <- pmatch(tb.params, mf_tb[[1]])
      if (length(idxs) < length(tb.params) || anyNA(idxs)) {
        warning("Attempt to select nonexistent params")
        idxs <- stats::na.omit(idxs)
        # no renaming possible, as we do not know which name was not matched
        tb.params <- unname(tb.params)
      }
    } else {
      idxs <- unname(tb.params)
      if (any(idxs > nrow(mf_tb))) {
        warning("Attempt to select nonexistent params")
        idxs <- idxs[idxs <= nrow(mf_tb)]
        tb.params <- tb.params[idxs]
      }
    }
    if (length(idxs) < nrow(mf_tb)) {
      message("Dropping params/terms (rows) from table!")
    }
    if (is.character(tb.params)) {
      idxs <- pmatch(tb.params, mf_tb[[1]])
    } else {
      idxs <- unname(tb.params)
    }
    if (length(idxs) < 1L) {
      warning("No matching parameters(s).")
      mf_tb <- NULL
    } else {
      mf_tb <- mf_tb[idxs, ]
      if (!is.null(names(tb.params))) {
        # support renaming of only some selected columns
        selector <- names(tb.params) != ""
        mf_tb[[1]][selector] <- names(tb.params)[selector]
      }
    }
  }

  if (!is.null(tb.vars)) {
    if (is.character(tb.vars)) {
      idxs <- pmatch(tb.vars, colnames(mf_tb))
       if (length(idxs) < length(tb.vars) || anyNA(idxs)) {
        warning("Attempt to select nonexistent columns by name")
        idxs <- stats::na.omit(idxs)
        # no renaming possible, as we do not know which name was not matched
        tb.vars <- unname(tb.vars)
       }
     } else {
      idxs <- unname(tb.vars)
      if (any(idxs > ncol(mf_tb))) {
        warning("Attempt to select nonexistent columns")
        idxs <- idxs[idxs <= ncol(mf_tb)]
        tb.vars <- tb.vars[idxs]
      }
    }
    if (!(1L %in% idxs)) {
      message("Dropping param names from table!")
    }
    if (length(idxs) < 1L) {
      warning("No matching column(s).")
      mf_tb <- NULL
    } else {
      mf_tb <- mf_tb[ , idxs]
      if (!is.null(names(tb.vars))) {
        # support renaming of only some selected columns
        selector <- names(tb.vars) != ""
        colnames(mf_tb)[selector] <- names(tb.vars)[selector]
      }
    }
  }

  # we need to enclose the tibble in a list to manually nest the table in
  # data.
  z <- tibble::tibble(mf_tb = list(mf_tb))

  if (npc.used) {
    margin.npc <- 0.05
  } else {
    margin.npc <- 0
  }

  if (is.character(label.x)) {
    label.x <- switch(label.x,
                      right = (1 - margin.npc),
                      center = 0.5,
                      centre = 0.5,
                      middle = 0.5,
                      left = (0 + margin.npc)
    )
    if (!npc.used) {
      x.delta <- abs(diff(range(data$x)))
      x.min <- min(data$x)
      label.x <- label.x * x.delta + x.min
    }
  }
  if (is.character(label.y)) {
    label.y <- switch(label.y,
                      top = (1 - margin.npc),
                      center = 0.5,
                      centre = 0.5,
                      middle = 0.5,
                      bottom = (0 + margin.npc)
    )
    if (!npc.used) {
        y.delta <- abs(diff(range(data$y)))
        y.min <- min(data$y)
        label.y <- label.y * y.delta + y.min
      }
  }
  if (npc.used) {
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

