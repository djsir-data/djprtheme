#' Change the font in all text layers of a ggplot
#'
#' @param p A ggplot2 object
#' @param font Name of a font, such as "Arial", "sans", or "Roboto"
#' @return A ggplot object with the `family` parameter of text layers
#' set to the provided font
#' @examples
#'
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   geom_text(aes(label = mpg))
#'
#' gg_font_change(p, "serif")
#' @export

gg_font_change <- function(p,
                           font = "Arial") {

  stopifnot(inherits(p, "gg"))
  stopifnot(is.character(font))
  stopifnot(length(font) == 1)

  add_f <- function(layer, font) {
    layer$aes_params$family <- font
    layer
  }

  p$layers <- lapply(p$layers,
                     add_f,
                     font = font)

  p
}
