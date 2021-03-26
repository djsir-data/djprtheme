#' Style a plot in a dumb style
#' @param base_size Base font size
#' @return a ggplot2 theme object
#' @examples
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_mc()
#' @export

theme_mc <- function(base_size = 16) {
  ggplot2::theme_minimal() +
    theme(panel.grid.major = element_line(size = 2),
          text = element_text(size = base_size))
}
