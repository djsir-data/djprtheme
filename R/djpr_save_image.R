#' A function that is a wrapper around ggplot2::ggsave().
#' This enables users to save images in certain pre-defined sizes: full slide, half slide,
#' 2/3rds slide, and 1/4 slide. These sizes will be appropriate for inclusion in a DJPR PPT deck.
#' The function should remove title and subtitle by default, as these
#' should be added on the slide itself rather than included in the image.

# "retina" = 320 dpi
djpr_save_image <- function(filename, plot = last_plot(), size = "full", dpi = "retina") {

# remove title

# remove subtitle

# define sizes
if (size == "full") {
  width <- 20
  height <- 20
  } else if (size == "threequarter") {
    width <- 15
    height <- 15
  } else if (size == "half") {
    width <- 10
    height <- 10
  } else if (size == "quarter") {
    width <- 5
    height <- 5
  } else {
    stop("Error: Parameter size not properly defined. Options are 'full', 'threequarter', 'half' and 'quarter'")
  }

# ggsave function
ggsave(filename=filename, plot = plot, width = width, height = height, units = "cm", dpi=dpi)

}

