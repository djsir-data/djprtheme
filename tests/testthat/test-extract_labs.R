library(ggplot2)
library(patchwork)

plot1 <- ggplot(economics, aes(x = date, y = unemploy)) +
  geom_point() +
  theme_djpr() +
  labs(subtitle = "Plot 1 subtitle",
       caption = "Plot 1 caption",
       title = "Plot 1 title")

# Create a second plot
plot2 <- ggplot(economics, aes(x = date, y = uempmed)) +
  geom_point() +
  theme_djpr() +
  labs(subtitle = "Plot 2 subtitle")

# Now combine these plots using patchwork
comb_plots <- plot1 + plot2 +
  plot_annotation(title = "Combined plot title",
                  caption = "Data source")

test_that("extract_labs() behaves as expected", {

  # Single plot
  expect_identical(extract_labs(plot1, "subtitle"), "Plot 1 subtitle")
  expect_identical(extract_labs(plot1, "caption"), "Plot 1 caption")
  expect_identical(extract_labs(plot1, "title"), "Plot 1 title")
  expect_error(extract_labs(plot1, "foobar"))


  expect_identical(extract_labs(comb_plots, "title"), "Combined plot title")
  expect_identical(extract_labs(comb_plots, "caption"), "Data source")

  expect_error(extract_labs(LETTERS, "title"))
})

test_that("remove_labs() behaves as expected", {
  expect_null(remove_labs(plot1)$labels$title)
  expect_null(remove_labs(plot1)$labels$subtitle)
  expect_null(remove_labs(plot1)$labels$caption)

  expect_null(remove_labs(comb_plots)$patches$annotation$title)
  expect_null(remove_labs(comb_plots)$patches$annotation$subtitle)
  expect_null(remove_labs(comb_plots)$patches$annotation$caption)

  vdiffr::expect_doppelganger("regular plot, no labels",
                              remove_labs(plot1))

  vdiffr::expect_doppelganger("pathwork plot, no labels",
                              remove_labs(comb_plots))
})
