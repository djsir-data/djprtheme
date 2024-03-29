#' Style your ggplot2 plots in the DJPR style.
#'
#'
#' @name theme_djpr
#' @param base_size Size for text elements. Default is 14.
#' @param base_family Font family for text elements. Default is "sans".
#' @param chart_type "normal" by default. Set to "scatter" for scatter plots.
#' @param flipped FALSE by default. Set to TRUE if using coord_flip(). If set to
#'   TRUE, the theme will show a vertical axis line, ticks & panel grid, while
#'   hiding the horizontals. Ignored for type = "scatter".
#' @param background "white" by default.
#' @param legend "none" by default. Set to "right", "left", "top", "bottom",
#'  or "none" as desired, or a two element numeric vector such as c(0.9, 0.1).
#' @param panel_borders `FALSE` by default. Set to `TRUE` to enable a black
#'   border around the plotting area.
#' @import ggrepel
#' @import ggplot2
#' @export

theme_djpr <- function(base_size = 14,
                       base_family = getOption("djprtheme.base_font_family", "sans"),
                       chart_type = "normal",
                       flipped = FALSE,
                       background = "white",
                       legend = "none",
                       panel_borders = FALSE) {
  if (!chart_type %in% c("normal", "scatter")) {
    warning(paste0(
      "Note: chart_type should be 'normal' or 'scatter', but you entered '",
      chart_type, "'. Reverting to 'normal'"
    ))
    chart_type <- "normal"
  }

  if (chart_type == "normal") {
    ret <- theme_djpr_normal(
      base_size = base_size,
      base_family = base_family,
      background = background,
      legend = legend,
      panel_borders = panel_borders,
      flipped = flipped
    )
  }

  if (chart_type == "scatter") {
    ret <- theme_djpr_scatter(
      base_size = base_size,
      base_family = base_family,
      background = background,
      legend = legend,
      panel_borders = panel_borders
    )
    if (flipped) message("Note that the 'flipped' argument is ignored for scatter plots.")
  }

  # Call a function that modifies various geom defaults
  djprify_geom_defaults()

  # Return
  return(ret)
}
