#' Enable and use the custom VIC font in plots
#'
#' After calling this, ggplot outputs will use the VIC font.
#'
#' This requires the suggested packages `showtext` and `sysfonts` to work.
#' @export
djpr_use_fonts <- function() {
  stopifnot(requireNamespace("showtext", quietly = TRUE))
  stopifnot(requireNamespace("sysfonts", quietly = TRUE))

  pkgname <- utils::packageName()
  sysfonts::font_add(
    "VIC-font",
    regular = system.file("fonts/VIC-Regular.otf", package = pkgname),
    bold = system.file("fonts/VIC-Bold.otf", package = pkgname),
    italic = system.file("fonts/VIC-Italic.otf", package = pkgname),
    bolditalic = system.file("fonts/VIC-BoldItalic.otf", package = pkgname)
  )
  showtext::showtext_auto()
  utils::assignInMyNamespace("needs_showtext", T)
  options(djprtheme.base_font_family = "VIC-font")
}

needs_showtext <- F

#' Run an expression with showtext turned off
#'
#' This is needed because showtext interferes with slide output.
#' @param expr Expression to run.
#' @export
without_showtext <- function(expr) {
  tryCatch({
    showtext::showtext_auto(F)
    expr
  }, finally = {
    if(needs_showtext) {
      showtext::showtext_auto()
    }
  })
}
