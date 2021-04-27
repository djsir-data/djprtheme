#' A function that is a wrapper around ggplot2::ggsave().
#' This enables users to save images in certain pre-defined sizes: full slide, half slide,
#' 2/3rds slide, and 1/4 slide to fit DJPR powerpoint deck template.
#' The function also removes title and subtitle by default, as these
#' should be added on the slide itself rather than included in the image.
#' @name djpr_save_image
#' @param filename name of the file to be saved into
#' @param plot object of plot to be saved, default is the last plot from last_plot()
#' @param size size of picture. Options are: full, threequarter, half and quarter
#' @param dpi resolution of picture. Default is 'retina' which is 320 dpi.
#' @import ggrepel
#' @import ggplot2
#' @export

djpr_save_image <- function(filename, plot = last_plot(), size = "full", dpi = "retina") {

# remove title and subtitle
plot <- last_plot(x = x, y = y, main = NULL, sub = NULL)   # works for plot()
plot <- theme(main.title = element_blank())

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

