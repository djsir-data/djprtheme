#' Don't expand below the y axis / to the left of the x-axis.
#'
#' These functions are particularly useful for bar charts and similar charts
#' with a zero baseline.
#'
#' @param expand_bottom default is 0. This will ensure that your x-axis is at
#'   the bottom value of your plotted data. Increase to add some buffer between
#'   the lowest point in your data and the x-axis. Note that the value is
#'   interpreted as a fraction of the total plotting space - a value of 1 will
#'   add white space equal to the whole area of your data.
#' @param expand_top default is 0.015. This will ensure that a small amount of
#'   white space is added to the top of your chart. Increase to add more white
#'   space.
#' @param expand_left default is 0. This will ensure your y-axis is at the
#'   lowest value of your plotted value.
#' @param expand_right default is 0.015. This will ensure that a small amount of
#'   white space is added to the right of your chart.
#' @param ... arguments passed to scale_y_continuous or scale_x_continuous
#' @author Matt Cowgill
#' @examples
#'
#' # Here's a basic chart in the DJPR style:
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_djpr()
#'
#' # By default our chart (p) will have a little whitespace below the
#' # 10 on the y axis. Remove that with:
#'
#' p +
#'   djpr_y_continuous()
#'
#' # These functions are particularly useful with bar charts
#' library(dplyr)
#' p2 <- iris %>%
#'   group_by(Species) %>%
#'   summarise(length = mean(Sepal.Length)) %>%
#'   ggplot(aes(x = Species, y = length)) +
#'   geom_col() +
#'   theme_djpr()
#'
#' # p2 has space below the 0 line, which we don't want
#' p2 +
#'   djpr_y_continuous()
#' @name djpr_axes
#' @aliases NULL
NULL

#' @rdname djpr_axes
#' @export

djpr_y_continuous <- function(expand_bottom = 0, expand_top = 0.015, ...) {
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(
      expand_bottom,
      expand_top
    )),
    ...
  )
}


#' @rdname djpr_axes
#' @export

djpr_x_continuous <- function(expand_left = 0,
                              expand_right = 0.015,
                              ...) {
  ggplot2::scale_x_continuous(
    expand = ggplot2::expansion(mult = c(
      expand_left,
      expand_right
    )),
    ...
  )
}
