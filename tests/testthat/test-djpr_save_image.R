library(ggplot2)

test_that("djpr_save_image() saves an image", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  for (size in c("full", "twothirds", "half", "quarter")) {
    djpr_save_image("test.png", p,
      size = size
    )
    on.exit(unlink("test.png"))

    expect_true(file.exists("test.png"))
    expect_true(file.size("test.png") > 1)
  }
})
