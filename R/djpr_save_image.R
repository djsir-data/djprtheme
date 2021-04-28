#' Save ggplot2 plots as static images at the right size for DJPR PowerPoint decks.
#'
#' This function is a wrapper around ggplot2::ggsave(). It is designed for
#' saving plots as static images that will be then included in a DJPR PPTX deck.
#'
#' Plots can be saved in pre-defined sizes: full slide, half slide,
#' 2/3rds slide, and 1/4 slide, to fit the DJPR-SPP PowerPoint deck template.
#'
#' The function removes the plot's title and subtitle by default, as these
#' should be added on the slide itself rather than included in the image.
#'
#' @name djpr_save_image
#' @param filename name, including path and extension, of the file to be saved
#' @param object name of the ggplot2 plot to be saved;
#' default is the last plot, using `ggplot2::last_plot()`
#' @param size size of picture. Options are: "full", "twothirds",
#' "half" and "quarter"
#' @param remove_title `TRUE` by default. When `TRUE`, title and subtitle will
#' be removed from the plot before saving. If `FALSE`, title and subtitle are
#' retained.
#' @param dpi resolution of picture. Default is 'retina' which is 320 dpi.
#' @import ggrepel
#' @import ggplot2
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#'
#' djpr_save_image("test.png", p)
#'
#' # Or to save an image in the right size to fill half a Powerpoint slide
#'
#' djpr_save_image("test.png", p, size = "half")
#' }
#' @export

djpr_save_image <- function(filename,
                            object = last_plot(),
                            size = c("full", "twothirds", "half", "quarter"),
                            remove_title = TRUE,
                            dpi = "retina") {

  # Check inputs
  size <- match.arg(size)
  stopifnot(inherits(object, "gg"))
  stopifnot(is.logical(remove_title))

  # Remove title and subtitle if requested
  if (remove_title) {
    object$labels$title <- NULL
    object$labels$subtitle <- NULL
  }

  # define sizes
  if (size == "full") {
    width <- 27
    height <- 14.36
  } else if (size == "twothirds") {
    width <- 18
    height <- 9.57
  } else if (size == "half") {
    width <- 13.5
    height <- 7.18
  } else if (size == "quarter") {
    width <- 6.75
    height <- 3.59
  } else {
    stop("Error: Parameter size not properly defined. Options are 'full', 'twothirds', 'half' and 'quarter'")
  }

  # ggsave function
  ggplot2::ggsave(filename = filename,
                  plot = object,
                  width = width,
                  height = height,
                  units = "cm",
                  dpi = dpi)
}
