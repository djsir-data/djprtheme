gg_font_change <- function(p,
                           font = "Arial") {

  add_f <- function(layer, font) {
    layer$aes_params$family <- font
    layer
  }

  p$layers <- purrr::map(p$layers,
                         add_f,
                         font = font)

  p
}
