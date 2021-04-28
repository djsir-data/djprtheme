test_that("djpr_pal() works", {
  expect_length(djpr_pal(), 5)
  expect_type(djpr_pal(), "character")

  for (i in seq_len(10)) {
    res <- suppressWarnings(djpr_pal(i))
    expect_length(res, i)
    expect_type(res, "character")
  }

  expect_error(djpr_pal(11))

})

test_that("djpr_*_manual() wrappers work", {
  expect_s3_class(djpr_colour_manual(5), "ggproto")
  expect_s3_class(djpr_fill_manual(5), "ggproto")
})

library(ggplot2)
test_plot <- ggplot(mtcars,
                    aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  djpr_colour_manual(3) +
  theme_minimal() +
  labs(title = "Testing three-colour plot")

test_that("plots with DJPR colours look as expected", {
  vdiffr::expect_doppelganger("test_plot", test_plot, path = "")
})
