test_that("djpr axis functions return object of correct class", {
  expect_s3_class(
    djpr_y_continuous(),
    c("ggproto", "ScaleContinuousPosition")
  )

  expect_s3_class(
    djpr_x_continuous(),
    c("ggproto", "ScaleContinuousPosition")
  )
})

library(ggplot2)
test_axes <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  djpr_y_continuous() +
  djpr_x_continuous() +
  labs(title = "Test that axis functions behave as expected")

test_that("djpr axis functions modify plot as expected", {
  vdiffr::expect_doppelganger("test_axes", test_axes, path = "")
})
