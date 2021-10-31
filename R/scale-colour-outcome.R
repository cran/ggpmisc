#' Colour and fill scales for ternary outcomes
#'
#' Manual scales for colour and fill aesthetics with defaults suitable for the
#' three way outcome from some statistical tests.
#'
#' @param ... other named arguments passed to \code{scale_manual}.
#' @param name The name of the scale, used for the axis-label.
#' @param ns.colour,down.colour,up.colour,de.colour The colour definitions to use for each
#'   of the three possible outcomes.
#' @param na.colour colour definition used for NA.
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful,
#'   for example, to apply colour settings to the colour and fill aesthetics at
#'   the same time, via aesthetics = c("colour", "fill").
#'
#' @details These scales only alter the \code{breaks}, \code{values}, and
#'   \code{na.value} default arguments of
#'   \code{scale_colour_manual()} and \code{scale_fill_manual()}. Please, see
#'   documentation for \code{\link[ggplot2]{scale_manual}} for details.
#'
#' @export
#'
#' @family Functions for quadrant and volcano plots
#'
#' @examples
#'
#' set.seed(12346)
#' outcome <- sample(c(-1, 0, +1), 50, replace = TRUE)
#' my.df <- data.frame(x = rnorm(50),
#'                     y = rnorm(50),
#'                     outcome2 = outcome2factor(outcome, n.levels = 2),
#'                     outcome3 = outcome2factor(outcome))
#'
#' ggplot(my.df, aes(x, y, colour = outcome3)) +
#'   geom_point() +
#'   scale_colour_outcome() +
#'   theme_bw()
#'
#' ggplot(my.df, aes(x, y, colour = outcome2)) +
#'   geom_point() +
#'   scale_colour_outcome() +
#'   theme_bw()
#'
#' ggplot(my.df, aes(x, y, fill = outcome3)) +
#'   geom_point(shape = 21) +
#'   scale_fill_outcome() +
#'   theme_bw()
#'
scale_colour_outcome <- function(...,
                                 name = "Outcome",
                                 ns.colour = "grey80",
                                 up.colour = "red",
                                 down.colour = "dodgerblue2",
                                 de.colour = "goldenrod",
                                 na.colour = "black",
                                 aesthetics = "colour") {
  ggplot2::scale_colour_manual(...,
                               name = name,
                               values = c("down" = down.colour,
                                          "uncertain" = ns.colour,
                                          "up" = up.colour,
                                          "de" = de.colour),
                               na.value = na.colour,
                               aesthetics = aesthetics)
}


#' @rdname scale_colour_outcome
#'
#' @export
#'
scale_color_outcome <- scale_colour_outcome

#' @rdname scale_colour_outcome
#'
#' @export
#'
scale_fill_outcome <- function(...,
                               name = "Outcome",
                               ns.colour = "grey80",
                               up.colour = "red",
                               down.colour = "dodgerblue2",
                               de.colour = "goldenrod",
                               na.colour = "black",
                               aesthetics = "fill") {
  ggplot2::scale_fill_manual(...,
                             name = name,
                             values = c("down" = down.colour,
                                        "uncertain" = ns.colour,
                                        "up" = up.colour,
                                        "de" = de.colour),
                             na.value = na.colour,
                             aesthetics = aesthetics)
}

