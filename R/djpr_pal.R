#' Create a Djpr-appropriate palette for your chart.
#'
#' @param n Numeric. The number of levels in your colour scale. Minimum value is
#'   1, maximum is 10. Using more than 6 is not recommended. If you don't
#'   specify `n`, a five-colour palette will be used, which may not look right.
#'   Specify `n`.
#'
#'   By default, n = 2 will give you light orange and dark orange. Use n = "2a"
#'   if you want light orange and cool_grey_11.
#' @param reverse Logical. FALSE by default. Setting to TRUE reverses the
#'   standard colour order. Standard colour order runs from light to dark. If
#'   you set reverse to TRUE, colours will run from dark to light.
#' @param faded Logical. FALSE by default. Setting to TRUE returns the faded
#'   variations of the standard colours.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     theme_djpr() +
#'     scale_colour_manual(values = djpr_pal(n = 3))
#'
#' p
#'
#' # Alternatively, use djpr_colour_manual(), which is a wrapper
#' # around scale_colour_manual():
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     theme_djpr() +
#'     djpr_colour_manual(3)
#'
#' p
#'
#'
#' # When applying colours as a fill, use djpr_fill_manual, which is
#' # a wrapper around scale_colour_manual:
#'
#' p <- ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) +
#'   geom_histogram() +
#'     theme_djpr() +
#'     djpr_fill_manual(3)
#' p
#'
#'
#' @export




djpr_pal <- function(n = 0, reverse = FALSE, faded = FALSE) {


  if (n == 0) {
    n <- 5
    "Your chart will probably look better if you specify n in djpr_pal()."
  }


  if (n > 6 & n <= 10) {
    warning("Using more than six colours is not recommended.")
  }


  if (n > 10 & n != "2a") {
    stop(paste0("You've requested ", n,
                " colours; djpr_pal() only supports up to 10."))
  }


  if (isFALSE(faded)) {
    palette <- regular_palette(n)
  }


  palette
}


regular_palette <- function(n) {


  if (n == 1) {
    palette <- djprtheme::djpr_blue
  } else if (n == "2a") {
    palette <- c(djprtheme::djpr_pacific_blue,
                 djprtheme::djpr_blue)
  } else if (n == 2) {
    palette <- c(djprtheme::djpr_blue,
                 djprtheme::djpr_green)
  } else if (n == 3) {
    palette <- c(djprtheme::djpr_pacific_blue,
                 djprtheme::djpr_cool_grey_1,
                 djprtheme::djpr_cobalt)
  } else if (n == 4) {
    palette <- c(djprtheme::djpr_pacific_blue,
                 djprtheme::djpr_blue,
                 djprtheme::djpr_green,
                 djprtheme::djpr_spray)
  } else if (n == 5) {
    palette <- c(djprtheme::djpr_pacific_blue,
                 djprtheme::djpr_blue,
                 djprtheme::djpr_green,
                 djprtheme::djpr_spray,
                 djprtheme::djpr_cool_grey_1)
  } else if (n == 6) {
    palette <- c(djprtheme::djpr_pacific_blue,
                 djprtheme::djpr_cobalt,
                 djprtheme::djpr_blue,
                 djprtheme::djpr_green,
                 djprtheme::djpr_spray,
                 djprtheme::djpr_cool_grey_1)
  } else if (n == 7) {
    palette <- c(djprtheme::djpr_pacific_blue,
                 djprtheme::djpr_cobalt,
                 djprtheme::djpr_blue,
                 djprtheme::djpr_green,
                 djprtheme::djpr_spray,
                 djprtheme::djpr_cool_grey_1,
                 djprtheme::djpr_cool_grey_11)
  } else if (n == 8) {
    palette <- c(djprtheme::djpr_pacific_blue,
                 djprtheme::djpr_cobalt,
                 djprtheme::djpr_iris_blue,
                 djprtheme::djpr_blue,
                 djprtheme::djpr_green,
                 djprtheme::djpr_spray,
                 djprtheme::djpr_cool_grey_1,
                 djprtheme::djpr_cool_grey_11)
  } else if (n == 9) {
    palette <- c(djprtheme::djpr_pacific_blue,
                 djprtheme::djpr_cobalt,
                 djprtheme::djpr_iris_blue,
                 djprtheme::djpr_blue,
                 djprtheme::djpr_green,
                 djprtheme::djpr_spray,
                 djprtheme::djpr_cool_grey_1,
                 djprtheme::djpr_golden_yellow,
                 djprtheme::djpr_cool_grey_11)
  } else if (n == 10) {
    palette <- c(djprtheme::djpr_pacific_blue,
                 djprtheme::djpr_cobalt,
                 djprtheme::djpr_iris_blue,
                 djprtheme::djpr_blue,
                 djprtheme::djpr_green,
                 djprtheme::djpr_persian_green,
                 djprtheme::djpr_spray,
                 djprtheme::djpr_cool_grey_1,
                 djprtheme::djpr_golden_yellow,
                 djprtheme::djpr_cool_grey_11)
  }
  palette
}

djpr_colour_manual <- function(x){
  scale_colour_manual(values=djpr_pal(n=x))
}

djpr_fill_manual <- function(x){
  scale_fill_manual(values=djpr_pal(n=x))
}

#' @rdname palette
#' @export
