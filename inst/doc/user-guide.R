## ----include=FALSE, echo=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 6.5, fig.height = 4,
               dev = "ragg_png")
options(warnPartialMatchArgs = FALSE,
        tibble.print.max = 4,
        tibble.print.min = 4,
        dplyr.summarise.inform = FALSE)
eval_flag <- TRUE # evaluate all code chunks

## ----message=FALSE------------------------------------------------------------
library(ggpmisc)
library(tibble)
library(dplyr)
library(quantreg)

eval_nlme <-  requireNamespace("nlme", quietly = TRUE)
if (eval_nlme) library(nlme)
eval_broom <-  requireNamespace("broom", quietly = TRUE)
if (eval_broom) library(broom)
eval_broom_mixed <-  requireNamespace("broom.mixed", quietly = TRUE)
if (eval_broom_mixed) library(broom.mixed)
eval_gginnards <-  requireNamespace("gginnards", quietly = TRUE)
if (eval_gginnards) library(gginnards)

## -----------------------------------------------------------------------------
old_theme <- theme_set(theme_bw())

## -----------------------------------------------------------------------------
set.seed(4321)
x <- (1:100) / 10
# linear
y.sd1 <- x + rnorm(length(x), mean = 0, sd = 1)
y.sd3 <- x + rnorm(length(x), sd = 3)
y.sdinc <- x + rnorm(length(x), mean = 0, 
                     sd = seq(from = 1, to = 3, length.out = length(x)))
outliers <- sample(seq_along(x), size = 5)
# 3rd degree polynomial
y.poly <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
y.poly <- y.poly / max(y.poly)

my.data <- data.frame(x = x,
                      y = y.sd1,
                      y.sd3 = y.sd3,
                      y.sdinc = y.sdinc,
                      y.desc = - y.sd1,
                      y.grp = y.sd1 + c(0, 1),
                      y.otlr = ifelse(seq_along(x) %in% outliers,
                                      y.sd3,
                                      y.sd1),
                      wght.otlr = 
                        ifelse(seq_along(x) %in% outliers, 1/3, 1),
                      y.poly = y.poly,
                      y.poly.grp = y.poly * c(1, 1.5) + c(0, 0.2),
                      wght.sqrt = sqrt(x),
                      group = c("A", "B"),
                      group.abcd = c("a", "b", "c", "d"), 
                      block = c("a", "a", "b", "b"))

head(my.data)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  geom_point() +
  stat_correlation()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.grp, colour = group)) +
  geom_point() +
  stat_correlation()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.grp, color = group)) +
  geom_point() +
  stat_correlation(method = "spearman")

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.grp, color = group)) +
  geom_point() +
  stat_correlation(mapping = use_label("r", "t", "P", "n"))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.grp)) +
  geom_point() +
  stat_correlation(mapping = 
                     aes(label = after_stat(cor.label),
                         color = 
                           after_stat(ifelse(cor > 0.955, 
                                             "red", "black")))) +
  scale_color_identity() +
  facet_wrap(~group)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n"), formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq", "R2"), formula = formula) +
  stat_poly_eq(mapping = 
                 f_use_label("method", "n",
                             format = 
                               "fitted by %s to %s observations"),
               output.type = "text",
               formula = formula,
               label.y = 0.88)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("adj.R2"), formula = formula) +
  stat_poly_eq(mapping = use_label("AIC"), label.x = "right", label.y = "bottom", size = 3,
               formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq", "adj.R2", sep = "*\" with \"*"),
               formula = formula) +
  labs(x = expression(italic(x)), y = expression(italic(y)))

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq"),
               eq.with.lhs = FALSE,
               formula = formula)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq"),
               eq.with.lhs = "italic(hat(y))~`=`~",
               formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq", "R2"),
               eq.with.lhs = "italic(h)~`=`~",
               eq.x.rhs = "~italic(z)",
               formula = formula) +
  labs(x = expression(italic(z)), y = expression(italic(h)))

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, log10(y.poly + 1e6))) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(mapping = use_label("eq"),
               eq.with.lhs = "plain(log)[10](italic(delta)+10^6)~`=`~",
               eq.x.rhs = "~Omega",
               formula = formula) +
  labs(y = expression(plain(log)[10](italic(delta)+10^6)),
       x = expression(Omega)) +
  scale_y_continuous(expand = expansion(c(0.1, 0.2)),
                     labels = function(x) {sprintf("6 + %.0e", x - 6)})

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3) - 1
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)), formula = formula)

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = 
                     after_stat(
                       ifelse(adj.r.squared > 0.3,
                                   paste(eq.label, adj.rr.label, 
                                         sep = "*\", \"*"),
                                   adj.rr.label))),
               formula = formula) +
  labs(x = expression(italic(x)), y = expression(italic(y)))

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly.grp)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)), size = 2.5,
               formula = formula) +
  facet_wrap(~group)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly.grp)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)), 
               size = 2.5,
               formula = formula) +
  facet_wrap(~group, scales = "free_y")

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, 
       aes(x, y.poly.grp, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)), 
               formula = formula)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, 
       aes(x, y.poly.grp, 
           linetype = group,
           grp.label = group)) +
  geom_point() +
  stat_poly_line(formula = formula, colour = "black") +
  stat_poly_eq(aes(label = 
                     after_stat(paste("bold(", grp.label, "*\":\")~~", 
                                      eq.label, sep = ""))),
               formula = formula)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data,
       aes(x, y.poly.grp, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)),
               formula = formula,
               label.x = "centre",
               vstep = 0.1)

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data,
       aes(x, y.poly.grp, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(eq.label)),
               formula = formula,
               label.x = c("right", "centre"),
               label.y = c("bottom", "top")) +
  scale_y_continuous(expand = expansion(c(0.12, 0.12)))

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, 
       aes(x, y.poly.grp, colour = group.abcd)) +
  geom_point(shape = 21, size = 3) +
  stat_poly_line(formula = formula) +
  stat_poly_eq(aes(label = after_stat(rr.label)), 
               size = 3, 
               formula = formula) +
  facet_wrap(~group, scales = "free_y")

## ----eval=eval_flag-----------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, 
       aes(x, y.poly.grp, colour = group.abcd)) +
  geom_point(shape = 21, size = 3) +
  stat_poly_line(formula = formula) +
  stat_poly_eq(use_label("R2"),
               size = 3, 
               label.y = c(0.95, 0.95, 0.9, 0.9),
               formula = formula) +
  facet_wrap(~group, scales = "free_y")

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, 
       aes(x, y.poly.grp, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(geom = "text", 
               aes(label = after_stat(eq.label)),
               label.x = c(10, 9), 
               label.y = c(-0.15, 1.8),
               hjust = "inward",
               formula = formula)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.sd3)) +
  geom_point() +
  stat_poly_line(color = "blue") +
  stat_poly_eq(mapping = use_label("R2", "eq"), color = "blue") +
  stat_poly_line(color = "red", orientation = "y") +
  stat_poly_eq(mapping = use_label("R2", "eq"), color = "red", 
               orientation = "y", label.y = 0.89)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.sd3)) +
  geom_point() +
  stat_ma_line() +
  stat_ma_eq(mapping = use_label("eq"))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.sd3)) +
  geom_point() +
  stat_ma_line(color = "blue") +
  stat_ma_eq(mapping = use_label("R2", "eq"), 
             color = "blue") +
  stat_ma_line(color = "red",
               orientation = "y",
               linetype = "dashed") +
  stat_ma_eq(mapping = use_label("R2", "eq"),
             color = "red", 
             orientation = "y",
             label.y = 0.9)

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_quant_band(formula = y ~ poly(x, 2))

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y.poly)) +
  stat_quant_line(formula = y ~ poly(x, 2), 
                  quantiles = c(0.05, 0.95)) +
  geom_point()

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_quant_line(formula = y ~ poly(x, 2), 
                  quantiles = 0.5)

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_quant_band(formula = formula, 
                  color = "black", 
                  fill = "grey60") +
  stat_quant_eq(f_use_label("qtl", "eq", format = "%s*\": \"*%s"),
                formula = formula) +
  theme_classic()

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, 
       aes(x, y.poly.grp, group = group, grp.label = group)) +
  geom_point() +
  stat_quant_line(formula = formula) +
  stat_quant_eq(f_use_label("grp", "qtl", "eq",
                            format = "%s*\" \"*%s*\": \"*%s"),
                size = 2.7,
                formula = formula)

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, 
       aes(x, y.poly.grp, group = group, linetype = group, 
           shape = group, grp.label = group)) +
  geom_point() +
  stat_quant_line(formula = formula, 
                  quantiles = c(0.05, 0.95), 
                  color = "black") +
  stat_quant_eq(aes(label = 
                      after_stat(
                        paste(grp.label, "*\", \"*",
                              qtl.label, "*\": \"*",
                              eq.label,
                              sep = ""))),
                size = 2.7,
                formula = formula, 
                quantiles = c(0.05, 0.95)) +
  theme_classic()

## ----warning=FALSE------------------------------------------------------------
ggplot(my.data, aes(x, y.sd3)) +
  geom_point() +
  stat_quant_line(formula = y ~ x, 
                  color = "blue",
                  quantiles = 0.05, 
                  se = FALSE) +
  stat_quant_eq(mapping = use_label("eq"), 
                formula = y ~ x,
                color = "blue",
                quantiles = 0.05) +
  stat_quant_line(formula = x ~ y, 
                  color = "red", 
                  quantiles = 0.95,
                  se = FALSE) +
  stat_quant_eq(mapping = use_label("eq"), 
                formula = x ~ y, 
                color = "red", 
                quantiles = 0.95, 
                label.y = 0.9)

## -----------------------------------------------------------------------------
ggplot(faithful, aes(x = waiting)) +
   stat_distrmix_line() +
   stat_distrmix_eq() +
  scale_x_continuous(limits = c(0, 110))

## -----------------------------------------------------------------------------
ggplot(faithful, aes(y = waiting)) +
   stat_distrmix_line() +
   stat_distrmix_eq(label.x = "right", label.y = "bottom")

## -----------------------------------------------------------------------------
ggplot(faithful, aes(x = waiting)) +
   stat_distrmix_line(components = "sum") +
   stat_distrmix_eq(label.x = "middle", label.y = "bottom")

## -----------------------------------------------------------------------------
 ggplot(faithful, aes(x = waiting)) +
   stat_distrmix_area() +
   stat_distrmix_eq(colour = "white", 
                    label.x = "middle", label.y = 0.08)

## -----------------------------------------------------------------------------
 ggplot(faithful, aes(x = waiting)) +
   stat_distrmix_area(aes(fill = after_stat(quant.splits != 2)), 
                      colour = "black", outline.type = "upper",
                      quantiles = c(0.025, 0.975),
                      show.legend = FALSE) +
  scale_fill_manual(values = c("grey80", "grey20"))

## -----------------------------------------------------------------------------
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33)  +
  stat_multcomp() +
  expand_limits(y = 0)

## -----------------------------------------------------------------------------
# position of contrasts' bars (manual)
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33)  +
  stat_multcomp(p.adjust.method = "holm", 
                adj.method.tag = 3,
                size = 2.75) +
  expand_limits(y = 0)

## -----------------------------------------------------------------------------
# position of contrasts' bars (manual)
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33)  +
  stat_multcomp(p.adjust.method = "holm", 
                adj.method.tag = -3,
                size = 2.75) +
  expand_limits(y = 0)

## -----------------------------------------------------------------------------
# position of contrasts' bars (manual)
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33)  +
  stat_multcomp(adj.method.tag = "ajustada",
                size = 2.75) +
  expand_limits(y = 0)

## -----------------------------------------------------------------------------
# position of contrasts' bars (manual)
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33)  +
  stat_multcomp(label.y = c(7, 4, 1),
                contrasts = "Dunnet",
                size = 2.75) +
  expand_limits(y = 0)

## -----------------------------------------------------------------------------
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
   stat_multcomp(label.y = 
                   seq(from = 15, 
                       by = -3, 
                       length.out = 6),
                 size = 2.5) +
   expand_limits(y = 0)

## -----------------------------------------------------------------------------
means <-
  aggregate(mpg$hwy,
            by = list(mpg$cyl), 
            FUN = mean, 
            na.rm = TRUE)[["x"]]

ggplot(mpg, aes(factor(cyl), hwy)) +
  stat_summary(fun.data = mean_se) +
  stat_multcomp(label.type = "letters",
                label.y = c(18, means), # 18 is for critical P label
                position = position_nudge(x = 0.1))

## -----------------------------------------------------------------------------
# Using other geometries
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(label.type = "letters",
                adj.method.tag = FALSE,
                geom = "label")

## -----------------------------------------------------------------------------
# Using other geometries
ggplot(mpg, aes(hwy, factor(cyl))) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(label.type = "letters",
                adj.method.tag = FALSE,
                geom = "label")

## -----------------------------------------------------------------------------
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(aes(x = stage(start = factor(cyl),
                              after_stat = xmax)),
                geom = "text",
                label.y = "bottom",
                vstep = 0,
                contrasts = "Dunnet")

## -----------------------------------------------------------------------------
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(aes(x = stage(start = factor(cyl),
                              after_stat = xmax),
                    label = after_stat(stars.label)),
                geom = "text",
                label.y = "bottom",
                vstep = 0,
                contrasts = "Dunnet")

## -----------------------------------------------------------------------------
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(aes(colour = after_stat(p.signif)),
                size = 2.75) +
  scale_colour_manual(values = c("grey60", "black")) +
  theme_bw()

## -----------------------------------------------------------------------------
ggplot(mpg, aes(factor(cyl), hwy)) +
  geom_boxplot(width = 0.33) +
  stat_multcomp(aes(fill = after_stat(p.value) < 0.01),
                size = 2.5,
                arrow = grid::arrow(angle = 45,
                                    length = unit(1, "mm"),
                                    ends = "both")) +
  scale_fill_manual(values = c("white", "lightblue"))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.otlr, colour = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals()

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.otlr, colour = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(method = "rlm",
                     mapping = aes(size = after_stat(posterior.weights)),
                     alpha = 1/2) +
  scale_size_area(name = "Posterior\nweights", max_size = 3)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.otlr, colour = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(method = "rlm",
                     mapping = 
                       aes(y = stage(start = y.otlr,
                                     after_stat = posterior.weights)),
                     alpha = 1/2) +
  scale_y_continuous(name = "Posterior weights")

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.otlr, 
                    weight = wght.otlr, 
                    colour = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula, 
                     mapping = aes(size = after_stat(posterior.weights)),
                     weighted = FALSE) +
  scale_size_area(name = "Posterior\nweights", max_size = 3)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.otlr, 
                    weight = wght.otlr, 
                    colour = group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_fit_residuals(formula = formula, 
                     mapping = aes(size = after_stat(posterior.weights)),
                     weighted = TRUE) +
  scale_size_area(name = "Posterior\nweights", max_size = 3)

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  stat_poly_line() +
  stat_fit_deviations(colour = "red") +
  geom_point()

## ----eval=eval_flag-----------------------------------------------------------
ggplot(my.data, aes(x, y)) +
  stat_poly_line() +
  geom_point() +
  stat_fit_deviations(arrow = arrow(length = unit(0.015, "npc"), 
                                   ends = "both"))

## -----------------------------------------------------------------------------
ggplot(my.data, aes(x, y.otlr)) +
  stat_poly_line(method = "rlm") +
  stat_fit_deviations(formula = formula, method = "rlm",
                      mapping = aes(colour = after_stat(posterior.weights)),
                      linewidth = 1,
                      show.legend = TRUE) +
  scale_color_gradient(name = "Posterior\nweight",
                       low = "red", high = "grey60", limits = c(0, 1)) +
  geom_point()

## ----eval=eval_nlme-----------------------------------------------------------
ggplot(my.data, aes(x, y.sdinc)) +
  stat_fit_deviations(method = "gls", 
                      method.args = list(weights = varPower(form = ~ x)),
                      mapping = 
                        aes(colour = after_stat(posterior.weights)),
                      linewidth = 1,
                      show.legend = TRUE) +
  stat_poly_line(method = "gls", 
                 method.args = list(weights = varPower(form = ~ x))) +
  geom_point() +
  scale_colour_gradient2(name = "Power of variance\nweight",
                         midpoint = 1, low = "blue", high = "red", mid = "grey60")

## ----eval=eval_nlme-----------------------------------------------------------
ggplot(my.data, aes(x, y.sdinc)) +
  stat_fit_residuals(method = "gls", 
                     method.args = list(weights = varPower(form = ~ x)),
                     geom = "point",
                     mapping = 
                       aes(y = stage(start = y.sdinc,
                                     after_stat = posterior.weights),
                           colour = after_stat(posterior.weights))) +
  scale_colour_gradient2(name = "Power of\nvariance\nweight",
                         midpoint = 1, low = "blue", high = "red", mid = "grey80")

## ----eval=eval_broom----------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly() correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly.grp, colour = group)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_fit_glance(method = "lm", 
                  method.args = list(formula = formula),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = sprintf("italic(P)*\"-value = \"*%.3g", 
                                      after_stat(p.value))),
                  parse = TRUE)

## ----eval=eval_broom----------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  stat_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_glance(method = "nls", 
                  method.args = list(formula = micmen.formula),
                  aes(label = 
                        after_stat(
                          paste("AIC = ", signif(AIC, digits = 3), 
                                ", BIC = ", signif(BIC, digits = 3),
                                sep = ""))),
                  label.x = "centre", 
                  label.y = "bottom")

## ----eval=eval_broom----------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_fit_tb(method.args = list(formula = formula),
              tb.vars = c(Parameter = "term", 
                          Estimate = "estimate", 
                          "s.e." = "std.error", 
                          "italic(t)" = "statistic", 
                          "italic(P)" = "p.value"),
              label.y = "top", label.x = "left",
              parse = TRUE)

## ----eval=eval_broom----------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_fit_tb(method.args = list(formula = formula),
              tb.type = "fit.anova",
              tb.vars = c(Effect = "term", 
                          df = "df",
                          "italic(F)" = "statistic", 
                          "italic(P)" = "p.value"),
              tb.params = c(x = 1, "x^2" = 2, "x^3" = 3, Resid = 4),
              label.y = "top", label.x = "left",
              parse = TRUE)

## ----eval=eval_broom----------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(method = "lm", formula = formula) +
  stat_fit_tb(method = "lm",
              method.args = list(formula = formula),
              tb.type = "fit.coefs", parse = TRUE,
              label.y = "center", label.x = "left")

## ----eval=eval_broom----------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K)
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  facet_wrap(~state) +
  geom_point() +
  stat_smooth(method = "nls",
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_tb(method = "nls",
              method.args = list(formula = micmen.formula),
              tb.type = "fit.coefs",
              label.x = 0.9,
              label.y = c(0.75, 0.2)) +
  theme(legend.position = "none") +
  labs(x = "C", y = "V")

## ----eval=eval_broom----------------------------------------------------------
ggplot(chickwts, aes(reorder(factor(feed), weight), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova",
              label.x = "center",
              label.y = "bottom") +
  labs(x = "Feed") +
  expand_limits(y = 0)

## ----eval=eval_broom----------------------------------------------------------
ggplot(chickwts, aes(reorder(factor(feed), weight), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova", label.x = "left", size = 3) +
  scale_x_discrete(expand = expansion(mult = c(0.2, 0.5))) +
  labs(y = "Feed") +
  coord_flip()

## ----eval=(eval_flag && eval_broom)-------------------------------------------
ggplot(chickwts, aes(reorder(factor(feed), weight), weight)) +
  stat_summary(fun.data = "mean_se") +
  stat_fit_tb(tb.type = "fit.anova",
              angle = 90, size = 3,
              label.x = "right", label.y = "center",
              hjust = 0.5, vjust = 0,
              tb.vars = c(Effect = "term", 
                          "df",
                          "M.S." = "meansq", 
                          "italic(F)" = "statistic", 
                          "italic(P)" = "p.value"),
              parse = TRUE) +
  scale_x_discrete(name = "Feed",
                   expand = expansion(mult = c(0.1, 0.35))) +
  expand_limits(y = 0)

## ----eval=eval_broom----------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  stat_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_tidy(method = "nls", 
                method.args = list(formula = micmen.formula),
                label.x = "right",
                label.y = "bottom",
                aes(label = 
                      after_stat(
                        paste("V[m]~`=`~", signif(Vm_estimate, digits = 3),
                              "%+-%", signif(Vm_se, digits = 2),
                              "~~~~K~`=`~", signif(K_estimate, digits = 3),
                              "%+-%", signif(K_se, digits = 2),
                              sep = ""))),
                parse = TRUE)

## ----eval=eval_broom----------------------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  stat_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_fit_tidy(method = "nls", 
                method.args = list(formula = micmen.formula),
                size = 3,
                label.x = "center",
                label.y = "bottom",
                vstep = 0.12,
                aes(label =
                      after_stat(
                        paste("V~`=`~frac(",
                              signif(Vm_estimate, digits = 2), "~C,",
                              signif(K_estimate, digits = 2), "+C)",
                              sep = ""))),
                parse = TRUE) +
  labs(x = "C", y = "V")

## ----eval=eval_broom----------------------------------------------------------
stat_micmen_eq <- function(vstep = 0.12,
                           size = 3,
                           ...) {
  stat_fit_tidy(method = "nls", 
                method.args = list(formula = micmen.formula),
                aes(label =
                      after_stat(
                        paste("V~`=`~frac(",
                              signif(Vm_estimate, digits = 2), "~C,",
                              signif(K_estimate, digits = 2), "+C)",
                              sep = ""))),
                parse = TRUE,
                vstep = vstep,
                size = size,
                ...)
}

## ----eval=(eval_flag && eval_broom)-------------------------------------------
micmen.formula <- y ~ SSmicmen(x, Vm, K) 
ggplot(Puromycin, aes(conc, rate, colour = state)) +
  geom_point() +
  stat_smooth(method = "nls", 
              formula = micmen.formula,
              se = FALSE) +
  stat_micmen_eq(label.x = "center",
                label.y = "bottom") +
  labs(x = "C", y = "V")

## ----eval=eval_broom----------------------------------------------------------
my_formula <- y ~ x
my.format <- 'y~"="~%.3g+%.3g~x*", with "*italic(P)~"="~%.3f'
ggplot(mpg, aes(displ, 1 / hwy)) +
  geom_point() +
  stat_quantile(quantiles = 0.5, formula = my_formula) +
  stat_fit_tidy(method = "rq",
                method.args = list(formula = y ~ x, tau = 0.5), 
                tidy.args = list(se.type = "nid"),
                mapping = aes(label = 
                                after_stat(
                                  sprintf(fmt = my.format,
                                          Intercept_estimate, 
                                          x_estimate,
                                          x_p.value))),
                parse = TRUE)

## ----eval=eval_broom----------------------------------------------------------
stat_rq_eqn <- 
  function(formula = y ~ x, 
           tau = 0.5,
           method = "br",
           mapping = 
             aes(label = 
                   after_stat(
                     sprintf(
                       'y~"="~%.3g+%.3g~x*", with "*italic(P)~"="~%.3f',
                       Intercept_estimate, x_estimate, x_p.value))),
           parse = TRUE,
           ...) {
    method.args <- list(formula = formula, 
                        tau = tau, 
                        method = method)
    stat_fit_tidy(method = "rq",
                  method.args = method.args, 
                  tidy.args = list(se.type = "nid"),
                  mapping = mapping,
                  parse = parse,
                  ...)
  }

## ----eval=(eval_flag && eval_broom)-------------------------------------------
ggplot(mpg, aes(displ, 1 / hwy)) +
  geom_point() +
  stat_quantile(quantiles = 0.5, formula = my_formula) +
  stat_rq_eqn(tau = 0.5, formula = my_formula)

## ----eval=eval_broom----------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula))

## ----eval=eval_broom----------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly.grp, colour = group)) +
  geom_point() +
  stat_fit_augment(method = "lm", 
                   method.args = list(formula = formula))

## ----eval=(eval_flag && eval_broom)-------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly)) +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula),
                   geom = "point",
                   y.out = ".resid") +
  labs(y = "Residuals")

## ----eval=eval_broom----------------------------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly.grp, colour = group)) +
  stat_fit_augment(method = "lm",
                   method.args = list(formula = formula),
                   geom = "point",
                   y.out = ".std.resid") +
  labs(y = "Residuals")

## ----eval=(eval_flag && eval_broom)-------------------------------------------
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  stat_fit_augment(method = "nls",
                   method.args = args)

## ----eval=(eval_flag && eval_broom)-------------------------------------------
args <- list(formula = y ~ k * e ^ x,
             start = list(k = 1, e = 2))
ggplot(mtcars, aes(wt, mpg)) +
  stat_fit_augment(method = "nls",
                   method.args = args,
                   geom = "point",
                   y.out = ".resid") +
  labs(y = "Residuals")

## ----eval=(eval_nlme && eval_broom_mixed)-------------------------------------
args <- list(model = y ~ SSlogis(x, Asym, xmid, scal),
             fixed = Asym + xmid + scal ~1,
             random = Asym ~1 | group,
             start = c(Asym = 200, xmid = 725, scal = 350))
ggplot(Orange, aes(age, circumference, colour = Tree)) +
  geom_point() +
  stat_fit_augment(method = "nlme",
                   method.args = args,
                   augment.args = list(data = quote(data)),
                   geom = "line")

## -----------------------------------------------------------------------------
# force same behavious as in an interactive R session
old.options <- options(ggpmisc.stat.vars.message = "nicknames")

## -----------------------------------------------------------------------------
formula <- y ~ poly(x, 3, raw = TRUE)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_poly_eq(formula = formula)

## ----eval=(eval_broom)--------------------------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly() correctly
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_fit_glance(aes(label = 
                        after_stat(
                          sprintf("italic(P)*\"-value = \"*%.3g", 
                                  p.value))),
                  parse = TRUE,
                  method.args = list(formula = formula),
                  label.x = "right",
                  label.y = "bottom")

## -----------------------------------------------------------------------------
options(old.options)

## ----eval=(eval_broom && eval_gginnards)--------------------------------------
# formula <- y ~ poly(x, 3, raw = TRUE)
# broom::augment does not handle poly() correctly!
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_fit_glance(aes(label = 
                        after_stat(
                          sprintf("italic(P)*\"-value = \"*%.3g", 
                                  p.value))),
#                  parse = TRUE,
                  geom = "debug_group",
                  method.args = list(formula = formula),
                  label.x = "right",
                  label.y = "bottom")

## ----eval=(eval_broom && eval_gginnards)--------------------------------------
formula <- y ~ x + I(x^2) + I(x^3)
ggplot(my.data, aes(x, y.poly)) +
  geom_point() +
  stat_poly_line(formula = formula) +
  stat_fit_tb(geom = "debug_panel",
              summary.fun = str,
              method.args = list(formula = formula),
              tb.vars = c(Parameter = "term", 
                          Estimate = "estimate", 
                          "s.e." = "std.error", 
                          "italic(t)" = "statistic", 
                          "italic(P)" = "p.value"),
#              parse = TRUE,
              label.y = "top",
              label.x = "left")

## -----------------------------------------------------------------------------
head(volcano_example.df) 

## -----------------------------------------------------------------------------
ggplot(volcano_example.df, 
       aes(logFC, PValue, colour = outcome2factor(outcome))) +
  geom_point() +
  scale_x_logFC(name = "Transcript abundance%unit") +
  scale_y_Pvalue() +
  scale_colour_outcome() +
  stat_quadrant_counts(data = function(x) {subset(x, outcome != 0)})

## -----------------------------------------------------------------------------
ggplot(volcano_example.df, 
       aes(logFC, PValue, colour = outcome2factor(outcome, n.levels = 2))) +
  geom_point() +
  scale_x_logFC(name = "Transcript abundance%unit", log.base.labels = 2) +
  scale_y_Pvalue() +
  scale_colour_outcome(values = "outcome:de") +
  stat_quadrant_counts(data = function(x) {subset(x, outcome != 0)})

## -----------------------------------------------------------------------------
head(quadrant_example.df)

## -----------------------------------------------------------------------------
  ggplot(subset(quadrant_example.df, 
                xy_outcomes2factor(outcome.x, outcome.y) != "none"),
         aes(logFC.x, logFC.y, 
             colour = outcome2factor(outcome.x), 
             fill = outcome2factor(outcome.y))) +
  geom_quadrant_lines(linetype = "dotted") +
  stat_quadrant_counts(size = 3, colour = "white") +
  geom_point(shape = "circle filled") +
  scale_x_logFC(name = "Transcript abundance for x%unit") +
  scale_y_logFC(name = "Transcript abundance for y%unit") +
  scale_colour_outcome() +
  scale_fill_outcome() +
  theme_dark()

## -----------------------------------------------------------------------------
all_quadrant_counts <- function(...) {
  list(  
    stat_quadrant_counts(data = . %>% filter(outcome.xy.fct == "xy"), ...),
    stat_quadrant_counts(data = . %>% filter(outcome.xy.fct == "x"), pool.along = "y", ...),
    stat_quadrant_counts(data = . %>% filter(outcome.xy.fct == "y"), pool.along = "x", ...),
    stat_quadrant_counts(data = . %>% filter(outcome.xy.fct == "none"), quadrants = 0L, ...)
  )
}

## -----------------------------------------------------------------------------
all_quadrant_lines <- function(...) { 
  list(
    geom_hline(data =  data.frame(outcome.xy.fct = factor(c("xy", "x", "y", "none"),
                                                          levels = c("xy", "x", "y", "none")),
                                  yintercept = c(0, NA, 0, NA)),
               aes(yintercept = yintercept),
               na.rm = TRUE,
               ...),
    geom_vline(data =  data.frame(outcome.xy.fct = factor(c("xy", "x", "y", "none"),
                                                          levels = c("xy", "x", "y", "none")),
                                  xintercept = c(0, 0, NA, NA)),
               aes(xintercept = xintercept),
               na.rm = TRUE,
               ...)
  )
}

## -----------------------------------------------------------------------------
quadrant_example.df %>%
  mutate(.,
         outcome.x.fct = outcome2factor(outcome.x),
         outcome.y.fct = outcome2factor(outcome.y),
         outcome.xy.fct = xy_outcomes2factor(outcome.x, outcome.y)) %>%
  ggplot(., aes(logFC.x, logFC.y, colour = outcome.x.fct, fill = outcome.y.fct)) +
  geom_point(shape = 21) +
  all_quadrant_lines(linetype = "dotted") +
  all_quadrant_counts(size = 3, colour = "white") +
  scale_x_logFC(name = "Transcript abundance for x%unit") +
  scale_y_logFC(name = "Transcript abundance for y%unit") +
  scale_colour_outcome() +
  scale_fill_outcome() +
  facet_wrap(~outcome.xy.fct) +
  theme_dark()

