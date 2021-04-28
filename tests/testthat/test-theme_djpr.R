test_that("theme_djpr() returns expected type of object", {
  expect_s3_class(theme_djpr(), "gg")
})

library(ggplot2)
base_scatter <- ggplot(mtcars,
            aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = "A title goes here",
       subtitle = "A subtitle goes here",
       caption = "A caption goes here")

regular_scatter <- base_scatter +
  theme_djpr()

test_that("plot styled with theme_djpr() looks correct", {
  vdiffr::expect_doppelganger("regular_scatter",
                              regular_scatter,
                              path = "")
})

scatter <- base_scatter +
  theme_djpr(chart_type = "scatter")

test_that("scatterplsot styled with theme_djpr() looks correct", {
  vdiffr::expect_doppelganger("scatter",
                              scatter,
                              path = "")
})

bar <- iris %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(sep_len = mean(Sepal.Length)) %>%
  ggplot(aes(x = Species, y = sep_len)) +
  geom_col()

basic_bar <- bar +
  theme_djpr()

test_that("regular bar chart styled with theme_djpr() looks correct", {
  vdiffr::expect_doppelganger("basic_bar",
                              basic_bar,
                              path = "")
})

flipped_bar <- bar +
  coord_flip() +
  theme_djpr(flipped = TRUE)

test_that("flipped bar chart styled with theme_djpr() looks correct", {
  vdiffr::expect_doppelganger("flipped_bar",
                              flipped_bar,
                              path = "")
})

