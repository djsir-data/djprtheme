# This function is called by `theme_djpr` but is not exported.

theme_djpr_scatter <- function(base_size = 12,
                                  base_family = "sans",
                                  background = "white",
                                  legend = "bottom",
                                  panel_borders = FALSE) {

  ret <- theme_djpr_base(base_size = base_size,
                            base_family = base_family,
                            background = background,
                            legend = legend,
                            panel_borders = panel_borders)

  ret

}
