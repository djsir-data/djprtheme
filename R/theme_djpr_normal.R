# This function is called by `theme_djpr` but is not exported.

theme_djpr_normal <- function(base_size = 12,
                              base_family = "sans",
                              flipped = FALSE,
                              background = "white",
                              legend = "bottom",
                              panel_borders = FALSE) {
  ret <- theme_djpr_base(
    base_size = base_size,
    base_family = base_family,
    background = background,
    legend = legend,
    panel_borders = panel_borders
  )


  # reverse when flipped = TRUE; only if type = 'normal'
  if (isTRUE(flipped)) {
    ret <- ret %+replace%
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        #axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank()
      )
  } else {
    ret <- ret %+replace%
      ggplot2::theme(
        #axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank()
      )
  }

  ret

}
