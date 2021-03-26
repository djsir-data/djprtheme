test_that("theme_mc() creates a ggplot2 theme", {
  expect_s3_class(theme_mc(), "gg")
})
