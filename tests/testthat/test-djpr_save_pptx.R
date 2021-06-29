# To keep output slides around, use:
# Sys.setenv(TEST_KEEP_OUTPUT="1")

retain_slides <- Sys.getenv("TEST_KEEP_OUTPUT") == "1"
output_dir <- tempdir()

check_slide <- function(basename, generate_slide) {
  # Generate slide
  path <- tempfile(basename, output_dir, fileext=".pptx")
  result <- generate_slide(path)
  expect_true(file.exists(path))

  # Read in slide and check visually
  pres <- officer::read_pptx(path)
  slide_idxs <- seq_len(length(pres))
  pres_plots <- lapply(slide_idxs, function(idx) {
    plot_slide(pres, idx)
  })
  lapply(slide_idxs, function(idx) {
    vdiffr::expect_doppelganger(
      paste(basename, idx, sep="-"),
      pres_plots[[idx]]
    )
  })

  # Cleanup
  if(!retain_slides) {
    unlink(path)
    expect_false(file.exists(path))
  }
}

plot_slide <- function(pres, slide=1) {
  elements <- officer::slide_summary(pres, slide)
  elements <- dplyr::filter(elements, !is.na(cx))
  ggplot2::ggplot(elements) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin=offx,
        ymin=offy,
        xmax=offx + cx,
        ymax=offy + cy,
        fill=factor(ph_label, levels=unique(ph_label))
      )
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x=offx + cx/2, y=offy + cy/2, label=text)
      , colour="yellow"
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_void() +
    ggplot2::labs(fill=NULL)
}

test_that("Layout exports", {
  # Some plots to test with
  test_plot_1 <-
    ggplot2::ggplot(mtcars) +
    ggplot2::geom_point(ggplot2::aes(hp, mpg)) +
    ggplot2::labs(
      title = "mtcars",
      subtitle = "A chart about cars",
      caption = "R builtin datasets"
    )

  test_plot_2 <-
    ggplot2::ggplot(faithful) +
    ggplot2::geom_point(aes(eruptions, waiting)) +
    ggplot2::labs(
      title="Old Faithful",
      sidebar=c("This", "is", "old", "faithful")
    )

  # Basic layouts

  check_slide("slide-full", function(fn) {
    djpr_save_pptx(fn, test_plot_1)
  })

  check_slide("slide-full-2", function(fn) {
    djpr_save_pptx(fn, test_plot_2)
  })

  check_slide("slide-half", function(fn) {
    djpr_save_pptx(fn, test_plot_1, layout="half")
  })

  check_slide("slide-twothirds", function(fn) {
    djpr_save_pptx(fn, test_plot_1, layout="twothirds")
  })

  # Signpost variants

  check_slide("slide-full-sgn", function(fn) {
    djpr_save_pptx(fn, test_plot_1, signpost="SECTION")
  })

  check_slide("slide-half-sgn", function(fn) {
    djpr_save_pptx(fn, test_plot_1, layout="half", signpost="SECTION")
  })

  check_slide("slide-twothirds-sgn", function(fn) {
    djpr_save_pptx(fn, test_plot_1, layout="twothirds", signpost="SECTION")
  })

  # Sidebar
  check_slide("slide-sidebar", function(fn) {
    djpr_save_pptx(fn, test_plot_2, layout="twothirds", signpost="SECTION")
  })

  # Slide pack
  check_slide("slide-pack", function(fn) {
    djpr_save_pptx(NULL, test_plot_1) %>%
      djpr_save_pptx(
        test_plot_1 + labs(sidebar=c("some", "commentary", "points")),
        layout="twothirds",
        signpost="SECTION"
      ) %>%
      djpr_save_pptx(
        test_plot_2,
        layout="half",
        signpost="SECTION"
      ) %>%
      print(target=fn)
  })
})

if (retain_slides) {
  message(sprintf("Test slides saved to: %s", output_dir))
}

