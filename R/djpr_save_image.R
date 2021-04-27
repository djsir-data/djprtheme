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
  width <- 27
  height <- 14.36
  } else if (size == "threequarter") {
    width <- 20.25
    height <- 10.77
  } else if (size == "half") {
    width <- 13.5
    height <- 7.18
  } else if (size == "quarter") {
    width <- 6.75
    height <- 3.59
  } else {
    stop("Error: Parameter size not properly defined. Options are 'full', 'threequarter', 'half' and 'quarter'")
  }

# ggsave function
ggsave(filename=filename, plot = plot, width = width, height = height, units = "cm", dpi=dpi)

}

