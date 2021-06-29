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


# Colour pyramid ----
# The pyramid shows the default colours and their ordering
library(dplyr, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
make_col_tibble <- function(n) {
  dplyr::tibble(
    col = suppressWarnings(djpr_pal(n)),
    order = c(1:n),
    n = n
  )
}

djpr_colours <- lapply(c(1:10), make_col_tibble) %>%
  dplyr::bind_rows()

djpr_colours$col <- factor(djpr_colours$col)

djpr_colours <- djpr_colours %>%
  dplyr::mutate(n_desc = dplyr::if_else(n == 1, "n = 1", paste0("  ", n)))

pyramid <- djpr_colours %>%
  ggplot(aes(
    x = reorder(n_desc, -n),
    y = order,
    fill = col
  )) +
  geom_tile(col = "white", size = 1) +
  scale_fill_manual(values = levels(djpr_colours$col)) +
  coord_flip(expand = FALSE, ylim = c(0, 13)) +
  theme_djpr() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "These are the colours of the DJPR palette")

test_that("Colour pyramid is unchanged", {
  vdiffr::expect_doppelganger("pyramid",
    pyramid
  )
})
