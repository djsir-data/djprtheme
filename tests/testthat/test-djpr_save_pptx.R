# TODO: remove need to manually inspect results

# To keep output slides around, use:
# Sys.setenv(TEST_KEEP_OUTPUT="1")
test_that("variations export without errors", {
  # Basic test plot
  the_plot <-
    ggplot2::ggplot(mtcars) +
    ggplot2::geom_point(ggplot2::aes(hp, mpg)) +
    ggplot2::labs(
      title = "mtcars", subtitle = "A chart about cars", caption = "R builtin datasets"
    )

  # Variant without all the labels
  the_plot2 <-
    ggplot2::ggplot(mtcars) +
    ggplot2::geom_point(ggplot2::aes(hp, mpg)) +
    ggplot2::labs(
      title = "mtcars"
    )

  retain_slides <- Sys.getenv("TEST_KEEP_OUTPUT") == "1"

  output_dir <- tempdir()
  # Generate file names so we can delete them later
  fns <- sapply(1:7, function(i) {
    tempfile(paste0("slide", i, "_"), output_dir, ".pptx")
  })
  djpr_save_pptx(
    the_plot + labs(title = "full"),
    fns[1]
  )
  djpr_save_pptx(
    the_plot2 + labs(title = "full, some titles missing"),
    fns[2]
  )
  djpr_save_pptx(
    the_plot + labs(title = "half"),
    fns[3],
    layout = "half"
  )
  djpr_save_pptx(
    the_plot + labs(title = "twothirds"),
    fns[4],
    layout = "twothirds"
  )
  djpr_save_pptx(
    the_plot + labs(title = "full+signpost"),
    fns[5],
    signpost = "SGN"
  )
  djpr_save_pptx(
    the_plot + labs(title = "half+signpost"),
    fns[6],
    layout = "half",
    signpost = "SGN"
  )
  djpr_save_pptx(
    the_plot + labs(title = "twothirds+signpost"),
    fns[7],
    layout = "twothirds",
    signpost = "SGN"
  )

  if (retain_slides) {
    message(sprintf("Saving output slides to: %s", output_dir))
  } else {
    lapply(fns, unlink)
  }

  # This test just checks that the above code finishes without error
  success <- T
  expect_true(success)
})
