test_that("breaks_right() works with numeric vectors", {
  expect_identical(
    breaks_right(c(10, 30)),
    seq(10, 30, 5)
  )

  expect_length(breaks_right(c(10, 30),
               n = 10),
               11)

  expect_length(breaks_right(c(10, 30),
                             n = 1),
                2)
})

test_that("breaks_right() works with date vectors", {
  econ_dates <- c(min(ggplot2::economics$date),
                 max(ggplot2::economics$date))

  expect_identical(breaks_right(econ_dates),
                   seq.Date(as.Date("1975-04-01"),
                            as.Date("2015-04-01"),
                                    by = "10 years"))
})

test_that("breaks_right() works when supplied to scale_x_date()", {
  p <- ggplot2::ggplot(ggplot2::economics,
                       ggplot2::aes(x = date, y = unemploy)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_date(breaks = breaks_right)

  expect_s3_class(p, "gg")

})
