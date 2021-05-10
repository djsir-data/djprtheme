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
  fns <- sapply(1:9, function(i) {
    tempfile(paste0("slide", i, "_"), output_dir, ".pptx")
  })
  djpr_save_pptx(
    fns[1],
    the_plot + labs(title = "full")
  )
  djpr_save_pptx(
    fns[2],
    the_plot2 + labs(title = "full, some titles missing")
  )
  djpr_save_pptx(
    fns[3],
    the_plot + labs(title = "half"),
    layout = "half"
  )
  djpr_save_pptx(
    fns[4],
    the_plot + labs(title = "twothirds"),
    layout = "twothirds"
  )
  djpr_save_pptx(
    fns[5],
    the_plot + labs(title = "full+signpost"),
    signpost = "SGN"
  )
  djpr_save_pptx(
    fns[6],
    the_plot + labs(title = "half+signpost"),
    layout = "half",
    signpost = "SGN"
  )
  djpr_save_pptx(
    fns[7],
    the_plot + labs(title = "twothirds+signpost"),
    layout = "twothirds",
    signpost = "SGN"
  )

  # Test the sidebar
  djpr_save_pptx(
    fns[8],
    the_plot + labs(sidebar = c("some", "dot", "points")),
    layout = "twothirds"
  )

  # Test slidepack generation
  djpr_save_pptx(NULL, the_plot) %>%
    djpr_save_pptx(
      the_plot2 + labs(sidebar="commentary points"),
      layout="half",
      signpost="Section"
    ) %>%
    print(target=fns[9])


  # This test just checks that the above code finishes without error
  success <- T
  expect_true(success)

  # Test that all files exist
  expect_true(all(file.exists(fns)))

  # Test that all files are PPTX documents
  sapply(fns,
         function(x) {
           officer::read_pptx(x) %>%
             expect_s3_class("rpptx")
           }
         )

  if (retain_slides) {
    message(sprintf("Saving output slides to: %s", output_dir))
  } else {
    unlink(fns)
  }
})
