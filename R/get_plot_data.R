#' Extract data behind a ggplot
#'
#' Extract the data that was passed into the `ggplot2()` or `qplot()` call,
#' with only the columns that are mapped to aesthetics.
#'
#' If an expression is mapped to an aesthetic, it is added as a column in
#' the result, with the expression as the column name. You may need to use
#' backticks (e.g. \verb{`column expression`}) to refer to those columns.
#'
#' Mappings provided in subsequent layers are also added if those
#' layers don't have their own data. Layers which have their own data
#' (passed in as `geom_xxxx(data=...)`) are ignored, as these are often used to
#' add annotations or guides.
#'
#' @param plot The plot to extract data from. Currently only supports `ggplot`
#' objects. Defaults to the last ggplot modified or created.
#'
#' @details
#' @return A data frame with the extracted data. Each column has been mapped
#' to some aesthetic in the plot.
#' @export
#'
#'
#' @examples
#' library(ggplot2)
#'
#' # Extract the data that was passed into the plot
#' sample_plot <- ggplot(mtcars, aes(mpg, hp)) + geom_point()
#' get_plot_data(sample_plot) # -> data.frame with columns: mpg, hp
#'
#' # Also picks up mappings in subsequent layers
#' sample_plot <-
#'   ggplot(iris, aes(Petal.Width, Petal.Length)) +
#'   geom_density2d() +
#'   geom_point(aes(colour=Species))
#' get_plot_data(sample_plot)
#' # -> data.frame with columns: Petal.Width, Petal.Length, Species
#'
#' # Supports expression mappings
#' sample_plot <- qplot(mpg - mean(mpg), hp^2, data=mtcars)
#' get_plot_data(sample_plot)
#' # -> data.frame with columns: `mpg - mean(mpg)`, `hp^2`
#'
#' # Facet variables are included:
#' sample_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   facet_wrap(~cyl)
#'
#' get_plot_data(sample_plot)
#' # -> data.frame with columns: `wt`, `mpg`, `cyl`
#'
get_plot_data <- function(plot = ggplot2::last_plot()) {
  # Note: using `if` rather than `UseMethod` for now as patchwork plots
  # were defaulting to the .ggplot method. Will rectify later

  stopifnot(inherits(plot, "gg")) # Note: patchwork plots are also of class 'gg'

  if (inherits(plot, "patchwork")) {
    get_plot_data.patchwork(plot = plot)
  } else {
    get_plot_data.ggplot(plot = plot)
  }

}

#' @export
get_plot_data.ggplot <- function(plot = ggplot2::last_plot()) {
  # Layers to add to the base aes() mappings
  layers_to_check <- which(
    sapply(plot$layers, function(x) {
      inherits(x$data, "waiver")
    })
  )
  # aes() mappings across plot and layers
  layer_mappings <- lapply(plot$layers[layers_to_check], function(x) x$mapping)
  layer_mappings <- unlist(layer_mappings, recursive = FALSE)

  facet_params <- plot$facet$params
  facet_mappings <- c(
    # facet_wrap
    facet_params$facets,
    # facet_grid
    facet_params$rows, facet_params$cols
  )

  mappings <- c(plot$mapping, layer_mappings, facet_mappings)
  mappings <- unique(mappings)

  # Construct the mapped data frame
  mapped_data <- lapply(mappings, rlang::eval_tidy, data=plot$data)
  mapped_df <- as.data.frame(mapped_data, check.names=F)
  colnames(mapped_df) <- sapply(mappings, rlang::as_label)
  # Preserve row names without stringifying
  attr(mapped_df, "row.names") <- attr(plot$data, "row.names")
  mapped_df
}

#' @export
get_plot_data.patchwork <- function(plot = ggplot2::last_plot()) {

  # The number of subplots in a patchwork is the number of patches
  # plus 1 (for the main plot)
  n_subplots <- length(plot$patches$plots) + 1

  # For each subplot, get the plot data
  patch_dfs <- lapply(seq_len(n_subplots),
         function(x) get_plot_data.ggplot(plot[[x]]))

  # Combine the subplot data in one data frame (this is suboptimal,
  # will improve later)
  patch_dfs <- dplyr::bind_rows(patch_dfs)

  patch_dfs
}
