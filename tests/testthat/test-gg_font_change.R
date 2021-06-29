library(ggplot2)
#
# test_that("gg_font_change fails with unexpected input", {
#
# })

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 3, colour = "black") +
  geom_text(aes(label = mpg),
            size = 3) +
  theme_minimal()

test_that("gg_font_change works", {
  # Before we run gg_font_change() there should be 1 aes_param (size)
  expect_length(p$layers[[2]]$aes_params, 1)

  p_f <- gg_font_change(p)

  # Check that gg_font_change is adding `family` to `aes_params` of
  # the text layer
  expect_length(p_f$layers[[2]]$aes_params, 2)
  expect_identical(p_f$layers[[2]]$aes_params$family, "Arial")

  # Check that the expected font parameter is there after building the ggplot
  p_b <- ggplot_build(p_f)
  # Note that gg_font_change() adds family to every layer, but
  # geom_point() layer should not have font set after ggplot_build()
  expect_null(p_b$data[[1]]$family)
  expect_identical(unique(p_b$data[[2]]$family),
                   "Arial")

  # Check that our object with modified font can be saved
  td <- tempdir()
  on.exit(unlink(td))
  tpng <- file.path(td, "test.png")
  tpptx <- file.path(td, "test.pptx")

  ggsave(filename = tpng,
         plot = p_f)

  expect_true(file.exists(tpng))

  djprtheme::djpr_save_pptx(destination = tpptx,
                            plot = p_f)

  expect_true(file.exists(tpptx))
})
# Check serif font works
test_that("serif font works", {
  p_serif <- gg_font_change(p, "serif")

  vdiffr::expect_doppelganger("p_serif",
                              p_serif)

})
