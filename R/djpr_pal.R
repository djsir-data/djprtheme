#' Create a DJPR-appropriate palette for your ggplot2 chart
#' @rdname palette
#' @param n Numeric. The number of levels in your colour scale. Minimum value is
#'   1, maximum is 10. Using more than 6 is not recommended. If you don't
#'   specify `n`, a five-colour palette will be used, which may not look right.
#'   Specify `n`.
#' @param reverse Logical. FALSE by default. Setting to TRUE reverses the
#'   standard colour order. Standard colour order runs from light to dark. If
#'   you set reverse to TRUE, colours will run from dark to light.
#' @param ... Arguments passed to `ggplot2::scale_*_manual()`
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'   geom_point() +
#'   theme_djpr() +
#'   scale_colour_manual(values = djpr_pal(n = 3))
#'
#' p
#'
#' # Alternatively, use djpr_colour_manual(), which is a wrapper
#' # around scale_colour_manual():
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'   geom_point() +
#'   theme_djpr() +
#'   djpr_colour_manual(3)
#'
#' p
#'
#'
#' # When applying colours as a fill, use djpr_fill_manual, which is
#' # a wrapper around scale_colour_manual:
#'
#' p <- ggplot(mtcars, aes(x = mpg, fill = factor(cyl))) +
#'   geom_histogram() +
#'   theme_djpr() +
#'   djpr_fill_manual(3)
#' p
#' @export

djpr_pal <- function(n = 0, reverse = FALSE) {
  if (n == 0) {
    n <- 5
    "Your chart will probably look better if you specify n in djpr_pal()."
  }

  if (n > 6 & n <= 10) {
    warning("Using more than six colours is not recommended.")
  }


  if (n > 10) {
    stop(paste0(
      "You've requested ", n,
      " colours; djpr_pal() only supports up to 10."
    ))
  }

  palette <- regular_palette(n)

  palette
}


regular_palette <- function(n) {
  if (n == 1) {
    palette <- djprtheme::djpr_cobalt                          #004C97
  } else if (n == 2) {
    palette <- c(
      djprtheme::djpr_cobalt,                                  #004C97
      djprtheme::djpr_lima                                     #78BE20
    )
  } else if (n == 3) {
    palette <- c(
      djprtheme::djpr_cobalt,                                  #004C97
      djprtheme::djpr_lima,                                    #78BE20
      djprtheme::djpr_electric_lime                            #CEDC00
    )
  } else if (n == 4) {
    palette <- c(
      djprtheme::djpr_cobalt,                                  #004C97
      djprtheme::djpr_lima,                                    #78BE20
      djprtheme::djpr_electric_lime,                           #CEDC00
      djprtheme::djpr_bondi_blue                               #009CA6
    )
  } else if (n == 5) {
    palette <- c(
      djprtheme::djpr_cobalt,                                  #004C97
      djprtheme::djpr_lima,                                    #78BE20
      djprtheme::djpr_electric_lime,                           #CEDC00
      djprtheme::djpr_bondi_blue,                              #009CA6
      djprtheme::djpr_cool_grey_1                              #d9d9d6
    )
  } else if (n == 6) {
    palette <- c(
      djprtheme::djpr_cobalt,                                  #004C97
      djprtheme::djpr_lima,                                    #78BE20
      djprtheme::djpr_electric_lime,                           #CEDC00
      djprtheme::djpr_bondi_blue,                              #009CA6
      djprtheme::djpr_cool_grey_1,                             #d9d9d6
      djprtheme::djpr_black_rock                               #343641
    )
  } else if (n == 7) {
    palette <- c(
      djprtheme::djpr_cobalt,                                  #004C97
      djprtheme::djpr_lima,                                    #78BE20
      djprtheme::djpr_electric_lime,                           #CEDC00
      djprtheme::djpr_bondi_blue,                              #009CA6
      djprtheme::djpr_cool_grey_1,                             #d9d9d6
      djprtheme::djpr_black_rock,                              #343641
      djprtheme::djpr_dark_tangerine                           #FF9E1B
    )
  } else if (n == 8) {
    palette <- c(
      djprtheme::djpr_cobalt,                                  #004C97
      djprtheme::djpr_lima,                                    #78BE20
      djprtheme::djpr_electric_lime,                           #CEDC00
      djprtheme::djpr_bondi_blue,                              #009CA6
      djprtheme::djpr_cool_grey_1,                             #d9d9d6
      djprtheme::djpr_black_rock,                              #343641
      djprtheme::djpr_dark_tangerine,                          #FF9E1B
      djprtheme::djpr_green                                    #69972C
    )
  } else if (n == 9) {
    palette <- c(
      djprtheme::djpr_cobalt,                                  #004C97
      djprtheme::djpr_lima,                                    #78BE20
      djprtheme::djpr_electric_lime,                           #CEDC00
      djprtheme::djpr_bondi_blue,                              #009CA6
      djprtheme::djpr_cool_grey_1,                             #d9d9d6
      djprtheme::djpr_black_rock,                              #343641
      djprtheme::djpr_dark_tangerine,                          #FF9E1B
      djprtheme::djpr_green,                                   #69972C
      djprtheme::djpr_cool_grey_11                             #53565a
    )
  } else if (n == 10) {
    palette <- c(
      djprtheme::djpr_cobalt,                                  #004C97
      djprtheme::djpr_lima,                                    #78BE20
      djprtheme::djpr_electric_lime,                           #CEDC00
      djprtheme::djpr_bondi_blue,                              #009CA6
      djprtheme::djpr_cool_grey_1,                             #d9d9d6
      djprtheme::djpr_black_rock,                              #343641
      djprtheme::djpr_dark_tangerine,                          #FF9E1B
      djprtheme::djpr_green,                                   #69972C
      djprtheme::djpr_cool_grey_11,                            #53565a
      djprtheme::djpr_persimmon                                #E35205
    )
  }
  palette
}

#' @rdname palette
#' @export
djpr_colour_manual <- function(n, reverse = FALSE, ...) {
  scale_colour_manual(values = djpr_pal(n = n, reverse = reverse),
                      ...)
}

#' @rdname palette
#' @export
djpr_fill_manual <- function(n, reverse = FALSE, ...) {
  scale_fill_manual(values = djpr_pal(n = n, reverse = reverse),
                    ...)
}
