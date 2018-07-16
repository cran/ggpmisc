## ---- include=FALSE, echo=FALSE------------------------------------------
library(knitr)
opts_chunk$set(fig.path = 'figure/guide-pos-', fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 4)
options(warnPartialMatchArgs = FALSE,
        tibble.print.max = 4,
        tibble.print.min = 4)

## ------------------------------------------------------------------------
library(ggpmisc)
library(ggplot2)
library(ggrepel)
library(xts)
library(lubridate)
library(tibble)
library(nlme)

## ------------------------------------------------------------------------
old_theme <- theme_set(theme_bw())

## ------------------------------------------------------------------------
ggplot(lynx) + geom_line()

## ------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line()

## ------------------------------------------------------------------------
ggplot(AirPassengers) + geom_line()

## ------------------------------------------------------------------------
ggplot(AirPassengers, as.numeric = FALSE) + geom_line()

## ------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5) +
  ylim(-100, 7300)

## ------------------------------------------------------------------------
ggplot(lynx) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5) +
  ylim(-100, 7300)

## ------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", vjust = 1.5, x.label.fmt = "%Y") +
  ylim(-100, 7300)

## ------------------------------------------------------------------------
ggplot(lynx) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", vjust = -0.5, x.label.fmt = "%4.0f") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", vjust = 1.5, x.label.fmt = "%4.0f") +
  ylim(-100, 7300)

## ------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", angle = 66,
             hjust = -0.1, x.label.fmt = "%Y") +
  ylim(NA, 8000)

## ------------------------------------------------------------------------
ggplot(lynx, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "rug", colour = "red") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "rug", colour = "blue")

## ------------------------------------------------------------------------
set.seed(4321)
# generate artificial data
x <- -99:100
y <- x + rnorm(length(x), mean = 0, sd = abs(x))
my.data <- data.frame(x, 
                      y, 
                      group = c("A", "B"))

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_hline(yintercept = 0, colour = "red") +
  geom_vline(xintercept = 0, colour = "red") +
  geom_point() +
  stat_quadrant_counts(colour = "red")

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_hline(yintercept = 0, colour = "red") +
  geom_point() +
  stat_quadrant_counts(colour = "red", pool.along = "x")

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_quadrant_counts(quadrants = 0L, labels.range.x = c(-90, -90), hjust = 0)

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_hline(yintercept = 0, colour = "red") +
  geom_vline(xintercept = 0, colour = "red") +
  geom_point() +
  stat_quadrant_counts(colour = "red", quadrants = c(2, 4))

## ------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_hline(yintercept = 0, colour = "red") +
  geom_vline(xintercept = 0, colour = "red") +
  geom_point() +
  stat_quadrant_counts(colour = "red") +
  facet_wrap(~group)

## ------------------------------------------------------------------------
set.seed(4321)
# generate artificial data
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x, 
                      y, 
                      group = c("A", "B"), 
                      y2 = y * c(0.5,2),
                      block = c("a", "a", "b", "b"),
                      wt = sqrt(x))

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..adj.rr.label..), formula = formula, 
               parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..AIC.label..), 
               formula = formula, 
               parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, 
               parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., 
                                  ..AIC.label.., ..BIC.label..,
                                  sep = "*\",\"~~")),
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..adj.rr.label.., 
                                  sep = "~~italic(\"with\")~~")),
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = paste("atop(", ..AIC.label.., ",", ..BIC.label.., ")", sep = "")), 
               formula = formula, 
               parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               eq.with.lhs = FALSE,
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               eq.with.lhs = "italic(hat(y))~`=`~",
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  labs(x = expression(italic(z)), y = expression(italic(h)) ) + 
  stat_poly_eq(aes(label = ..eq.label..),
               eq.with.lhs = "italic(h)~`=`~",
               eq.x.rhs = "~italic(z)",
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 2, raw = TRUE)
ggplot(my.data, aes(x, log10(y + 1e6))) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               eq.with.lhs = "plain(log)[10](italic(y)+10^6)~`=`~",
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 5, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3) - 1
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula, 
               parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), size = 3,
               formula = formula, parse = TRUE) +
  facet_wrap(~group)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..), size = 3,
               formula = formula, parse = TRUE) +
  facet_wrap(~group, scales = "free_y")

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               formula = formula, parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               formula = formula, parse = TRUE, label.y.npc = "center")

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..eq.label..),
               formula = formula, parse = TRUE, label.y.npc = 0.75)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, fill = block)) +
  geom_point(shape = 21, size = 3) +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.33,
               formula = formula, parse = TRUE) +
  facet_wrap(~group, scales = "free_y")

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y2, colour = group, fill = block)) +
  geom_point(shape = 21, size = 3) +
  geom_smooth(method = "lm", formula = formula) +
  stat_poly_eq(aes(label = ..rr.label..), size = 3,
               geom = "label", alpha = 0.2,
               formula = formula, parse = TRUE,
               label.y.npc = 0.66) +
  facet_wrap(~group, scales = "free_y")

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula, resid.type = "working")

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y, color = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula)

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_deviations(formula = formula, color = "red") +
  geom_point()

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y, color = group)) +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_deviations(formula = formula) +
  geom_point()

## ------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y)) +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_deviations(formula = formula, color = "red",
                      arrow = arrow(length = unit(0.015, "npc"), 
                                   ends = "both")) +
  geom_point()

## ------------------------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = formula),
                  geom = "text",
                  aes(label = signif(..p.value.., digits = 4)))

## ------------------------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly() correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, color = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = formula),
                  geom = "text", 
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))

## ------------------------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, color = group)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = formula),
                  label.x.npc = "right",
                  label.y.npc = "bottom",
                  geom = "text", 
                  aes(label = paste("P-value = ", signif(..p.value.., digits = 4), sep = "")))

## ------------------------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_glance(method = "nls", 
                  method.args = list(formula = micmen.formula),
                  geom = "text",
                  aes(label = paste("AIC = ", signif(..AIC.., digits = 3), 
                                    ", BIC = ", signif(..BIC.., digits = 3),
                                    sep = "")))

## ------------------------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_tidy(method = "nls", 
                method.args = list(formula = micmen.formula),
                geom = "text",
                label.x.npc = "right",
                label.y.npc = "bottom",
                aes(label = paste("V[m]~`=`~", signif(..Vm_estimate.., digits = 3),
                                  "%+-%", signif(..Vm_se.., digits = 2),
                                  "~~~~K~`=`~", signif(..K_estimate.., digits = 3),
                                  "%+-%", signif(..K_se.., digits = 2),
                                  sep = "")),
                parse = TRUE)

## ------------------------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_tidy(method = "nls", 
                method.args = list(formula = micmen.formula),
                geom = "text",
                label.x.npc = 0.9,
                label.y.npc = 0.3,
                aes(label = paste("V~`=`~frac(", signif(..Vm_estimate.., digits = 2), "~C,",
                                  signif(..K_estimate.., digits = 2), "+C)",
                                  sep = "")),
                parse = TRUE) +
  labs(x = "C", y = "V")

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_tb(method = "lm",
              method.args = list(formula = formula),
              tb.vars = c(Parameter = "term", 
                          Estimate = "estimate", 
                          "s.e." = "std.error", 
                          "italic(t)" = "statistic", 
                          "italic(P)" = "p.value"),
              label.y.npc = "top", label.x.npc = "left",
              parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
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
               label.y.npc = "top", label.x.npc = "left",
              parse = TRUE)

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula) +
  stat_fit_tb(method = "lm",
              method.args = list(formula = formula),
              tb.type = "fit.coefs",
              label.y.npc = "center", label.x.npc = "left")

## ------------------------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K)
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  facet_wrap(~state) +
  geom_point() +
  geom_smooth(method = "nls",
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_tb(method = "nls",
              method.args = list(formula = micmen.formula),
              tb.type = "fit.coefs",
              label.x.npc = 0.67,
              label.y.npc = c(0.6, 0.3)) +
  theme(legend.position = "none") +
  labs(x = "C", y = "V")

## ------------------------------------------------------------------------
ggplot(chickwts, aes(factor(feed), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova",
              label.x.npc = 1.07, hjust = 1,
              label.y.npc = "bottom") +
  expand_limits(y = 0)

## ------------------------------------------------------------------------
ggplot(chickwts, aes(factor(feed), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova",
              label.x.npc = 1.1, hjust = 0,
              label.y.npc = 0, vjust = 1) +
  expand_limits(y = 0) +
  coord_flip()

## ------------------------------------------------------------------------
ggplot(chickwts, aes(factor(feed), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova",
              angle = 90,
              tb.vars = c(Effect = "term", 
                          "df",
                          "M.S." = "meansq", 
                          "italic(F)" = "statistic", 
                          "italic(P)" = "p.value"),
              label.x.npc = 0.5, hjust = 0.5,
              label.y.npc = 0, vjust = 1,
              parse = TRUE) +
  expand_limits(y = 0) +
  coord_flip()

## ------------------------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula))

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, color = group)) +
  geom_point() +
  stat_fit_augment(method = "lm", 
                   method.args = list(formula = formula))

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y)) +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula),
                   geom = "point",
                   y.out = ".resid")

## ------------------------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y, color = group)) +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula),
                   geom = "point",
                   y.out = ".std.resid")

## ------------------------------------------------------------------------
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  stat_fit_augment(method = "nls",
                   method.args = args)

## ------------------------------------------------------------------------
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))
ggplot(mtcars, aes(wt, mpg)) +
  stat_fit_augment(method = "nls",
                   method.args = args,
                   geom = "point",
                   y.out = ".resid")

## ------------------------------------------------------------------------
args <- list(model = y ~ SSlogis(x, Asym, xmid, scal),
             fixed = Asym + xmid + scal ~1,
             random = Asym ~1 | group,
             start = c(Asym = 200, xmid = 725, scal = 350))
ggplot(Orange, aes(age, circumference, color = Tree)) +
  geom_point() +
  stat_fit_augment(method = "nlme",
                   method.args = args,
                   augment.args = list(data = quote(data)))

## ------------------------------------------------------------------------
random_string <- function(len = 6) {
paste(sample(letters, len, replace = TRUE), collapse = "")
}

# Make random data.
set.seed(1001)
d <- tibble::tibble(
  x = rnorm(100),
  y = rnorm(100),
  group = rep(c("A", "B"), c(50, 50)),
  lab = replicate(100, { random_string() })
)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red")

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, color = group)) +
   stat_dens2d_filter(keep.fraction = 0.25,
                      size = 3,
                      color = "black") +
   geom_point()

## ------------------------------------------------------------------------
ggplot(data = d, aes(x + rep(c(-2,2), rep(50,2)), 
                     y, color = group)) +
   geom_point() +
   stat_dens2d_filter(shape = 1, size = 3,
                      keep.fraction = 0.25)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x + rep(c(-2,2), rep(50,2)), 
                     y, color = group)) +
   geom_point() +
   stat_dens2d_filter_g(shape = 1, size = 3,
                      keep.fraction = 0.25)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() + 
  stat_dens2d_filter(geom = "text_repel")

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.5)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels()

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels(keep.fraction = 0.45)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels(keep.fraction = 0.25,
                     vjust = -0.3)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels(geom = "text_repel", 
                     keep.fraction = 0.45)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels(geom = "label_repel", 
                     keep.fraction = 0.25)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
geom_point() +
stat_dens2d_labels(geom = "text_repel",
keep.fraction = 0.25, angle = 90)

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y, label = lab, color = group)) +
  geom_point() +
  stat_dens2d_labels(geom = "label_repel", 
                     keep.fraction = 0.35, 
                     alpha = 0.5,
                     label.fill = NA)

## ------------------------------------------------------------------------
tb <- tibble(date = ymd(c("2017-05-21", "2007-05-21"), tz = "UCT"),
             value = 1:2 * 10)
data.tb <- tibble(x = 0, y = 8e5, tb = list(tb))
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_table(data = data.tb, aes(label = tb), hjust = 0, vjust = 0) +
  theme_bw()

## ------------------------------------------------------------------------
tb <- tibble(date = ymd(c("2017-05-21", "2007-05-21"), tz = "UCT"),
             value = 1:2 * 10)
data.tb <- tibble(x = 0, y = 8e5, tb = list(tb))
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_table(data = data.tb, aes(label = tb), hjust = 0, vjust = 0,
             stat = "fmt_tb", tb.vars = c(Date = "date", Amount = "value")) +
  theme_bw()

## ------------------------------------------------------------------------
tb <- tibble(date = ymd(c("2017-05-21", "2007-05-21"), tz = "UCT"),
             value = 1:2 * 10)
data.tb <- tibble(x = 25, y = 8e5, tb = list(tb))
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_table(data = data.tb, aes(label = tb), colour = "blue", size = 4) +
  theme_bw()

## ------------------------------------------------------------------------
tb.pm <- tibble(parameter = c("frac(beta[1], a^2)", "frac(beta[2], a^3)"),
             value = c("10^2.4", "10^3.532"))
data.tb <- tibble(x = 12.5, y = 8.5e5, tb = list(tb.pm))
ggplot(my.data, aes(x, y)) +
  geom_point() +
  geom_table(data = data.tb, aes(label = tb), parse = TRUE) +
  theme_bw()

## ------------------------------------------------------------------------
class(austres)
austres.df <- try_tibble(austres)
class(austres.df)
lapply(austres.df, "class")
head(austres.df, 4)

## ------------------------------------------------------------------------
austres.df <- try_tibble(austres, as.numeric = TRUE)
lapply(austres.df, "class")
head(austres.df, 4)

## ------------------------------------------------------------------------
class(lynx)
lynx.df <- try_tibble(lynx)
class(lynx.df)
lapply(lynx.df, "class")
head(lynx.df, 3)

## ------------------------------------------------------------------------
lynx.df <- try_tibble(lynx, "year")
head(lynx.df, 3)

## ------------------------------------------------------------------------
lynx_n.df <- try_tibble(lynx, "year", as.numeric = TRUE)
lapply(lynx_n.df, "class")
head(lynx_n.df, 3)

## ------------------------------------------------------------------------
try_tibble(1:5)

## ------------------------------------------------------------------------
try_tibble(letters[1:5])

## ------------------------------------------------------------------------
try_tibble(factor(letters[1:5]))

## ------------------------------------------------------------------------
try_tibble(list(x = rep(1,5), y = 1:5))

## ------------------------------------------------------------------------
try_tibble(data.frame(x = rep(1,5), y = 1:5))

## ------------------------------------------------------------------------
try_tibble(matrix(1:10, ncol = 2))

## ------------------------------------------------------------------------
make_data_tbl <- function(nrow = 100, rfun = rnorm, ...) {
  if (nrow %% 2) {
    nrow <- nrow + 1
  }
  
  set.seed(1001)
  
  tibble::tibble(
    x = rfun(nrow, ...),
    y = rfun(nrow, ...),
    group = rep(c("A", "B"), c(nrow / 2, nrow / 2))
  )
}

## ------------------------------------------------------------------------
old_theme <- theme_set(theme_bw())

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(300), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", 
                     keep.sparse = FALSE, 
                     keep.fraction = 1/3)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(300), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", 
                     keep.sparse = FALSE, 
                     keep.fraction = 1/3)+
  stat_dens2d_filter(color = "blue", 
                     keep.fraction = 1/3)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(300, rfun = runif), aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", keep.fraction = 1/2)

## ------------------------------------------------------------------------
ggplot(data = make_data_tbl(300, rfun = rgamma, shape = 2), 
       aes(x, y)) +
  geom_point() +
  stat_dens2d_filter(color = "red", keep.fraction = 1/3)

