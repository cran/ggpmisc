context("stat_peaks")

library(lubridate)

make_data_tbl <- function(nrow = 100, rfun = rnorm, ...) {
  if (nrow %% 2) {
    nrow <- nrow + 1
  }

  set.seed(101)

  data.frame(
    x = 0:(nrow - 1),
    x.date = ydm("2000-01-01") + days(0:(nrow - 1)),
    x.datetime = ydm_hms("2000-01-01 00:00:00", tz = "EET") + days(0:(nrow - 1)),
    y = rfun(nrow, ...),
    group = rep(c("A", "B"), nrow / 2)
  )
}

shuffle_data <- function(data) {
  data[sample(1:nrow(data), nrow(data)), ]
}

if (isNamespaceLoaded(name = "package:ggpmisc")) detach(package:ggpmisc, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggpp")) detach(package:ggpp, unload = TRUE)
if (isNamespaceLoaded(name = "package:ggplot2")) detach(package:ggplot2, unload = TRUE)

test_that("peaks_noload", {
  vdiffr::expect_doppelganger("stat_peaks_numeric_noload",
                              ggplot2::ggplot(data = make_data_tbl(30), aes(x, y)) +
                                ggplot2::geom_point() +
                                ggplot2::geom_line() +
                                ggpmisc::stat_peaks(colour = "red") +
                                ggplot2::expand_limits(y = c(-2.5, 2.5))
  )
})

library(ggpmisc)

test_that("numbers_tb", {
  vdiffr::expect_doppelganger("stat_peaks_numeric_01",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric_s01",
                              ggplot(data = shuffle_data(make_data_tbl(30)), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__02",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__03",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5,
                                           x.label.fmt = "x = %.0f") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__04",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(mapping = aes(label = after_stat(y.label)),
                                           geom = "text", hjust = -0.1,
                                           y.label.fmt = "lambda~`=`~%.2f",
                                           angle = 90,
                                           parse = TRUE) +
                              expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__05",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = NULL) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_numeric__06",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = 11) +
                                expand_limits(y = c(-2.5, 2.5))
  )
})

test_that("dates_tb", {
  vdiffr::expect_doppelganger("stat_peaks_date_01",
                              ggplot(data = make_data_tbl(30), aes(x.date, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_s01",
                              ggplot(data = shuffle_data(make_data_tbl(30)), aes(x.date, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_02",
                              ggplot(data = make_data_tbl(30), aes(x.date, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_03",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5,
                                           x.label.fmt = "%b-%d") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_04",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5,
                                           x.label.fmt = "lambda~`=`~\"%b-%d\"",
                                           parse = TRUE) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_05",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(mapping = aes(label = after_stat(y.label)),
                                           geom = "text", hjust = -0.1,
                                           y.label.fmt = "lambda~`=`~%.2f",
                                           angle = 90,
                                           parse = TRUE) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_06",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = NULL) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_date_07",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = 11) +
                                expand_limits(y = c(-2.5, 2.5))
  )
})

test_that("datetimes_tb", {
  vdiffr::expect_doppelganger("stat_peaks_datetime_01",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_s01",
                              ggplot(data = shuffle_data(make_data_tbl(30)), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_02",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_03",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5,
                                           x.label.fmt = "%b-%d") +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_04",
                              ggplot(data = make_data_tbl(30), aes(x.datetime, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(geom = "text", vjust = -0.5,
                                           x.label.fmt = "lambda~`=`~\"%b-%d\"",
                                           parse = TRUE) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_05",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(mapping = aes(label = after_stat(y.label)),
                                           geom = "text", hjust = -0.1,
                                           y.label.fmt = "lambda~`=`~%.2f",
                                           angle = 90,
                                           parse = TRUE) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_06",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = NULL) +
                                expand_limits(y = c(-2.5, 2.5))
  )
  vdiffr::expect_doppelganger("stat_peaks_datetime_07",
                              ggplot(data = make_data_tbl(30), aes(x, y)) +
                                geom_point() +
                                geom_line() +
                                stat_peaks(colour = "red", span = 11) +
                                expand_limits(y = c(-2.5, 2.5))
  )
})

test_that("many_layers", {
  if (packageVersion("ggpp") >= "0.4.4") {
    vdiffr::expect_doppelganger("stat_peaks_ml_01",
                                ggplot(mtcars, aes(mpg, hp)) +
                                  geom_line() +
                                  geom_point() +
                                  stat_valleys(span = NULL,
                                               strict = TRUE,
                                               geom = "text_s",
                                               mapping = aes(label = paste(after_stat(y.label), after_stat(x.label))),
                                               x.label.fmt = "at %.1f mpg ",
                                               y.label.fmt = "hp = %.1f",
                                               colour = "blue",
                                               colour.target = "segment",
                                               arrow = grid::arrow(length = unit(0.1, "inches")),
                                               position = position_nudge_keep(x = -1, y = -20),
                                               hjust = 1) +
                                  stat_peaks(span = 5,
                                             strict = TRUE,
                                             geom = "text_s",
                                             mapping = aes(label = paste(after_stat(y.label), after_stat(x.label))),
                                             x.label.fmt = "at %.1f mpg",
                                             y.label.fmt = "hp = %.1f\n",
                                             colour = "red",
                                             colour.target = "segment",
                                             arrow = grid::arrow(length = unit(0.1, "inches")),
                                             position = position_nudge_keep(x = 1, y = 0),
                                             hjust = 0) +
                                  expand_limits(y = 0)
    )
  }
})

