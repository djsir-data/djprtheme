# This file implements the `djpr_save_pptx` function which takes a ggplot
# and exports it as a PowerPoint slide.

#' Export a ggplot to a DJPR SPP PowerPoint slide.
#'
#' The exported slide will use the SPP template. All text is editable on the
#' slide. Users can choose from several slide formats.
#'
#' @param destination The destination for the slide. Can be one of:
#' - `NULL` (default): The slide will be kept in-memory and returned for further
#' manipulation.
#' - `character`: File path to save the slide to, including extension,
#' such as "output.pptx".
#' - A `rpptx` object: An in-memory slide pack to append to.
#' @param plot The ggplot to turn into a slide.
#' Default is `ggplot2::last_plot()`.
#' @param layout Slide layout to use. Options are "full", "half", and
#' "twothirds". The layouts refer to the proportion of the slide width taken
#' up by the chart.
#' @param signpost Signpost heading, if required. If `NULL` (the default), no
#' signpost will be included.
#'
#' @return If `destination` was `NULL` or a `pptx` object, returns the updated
#' presentation ready for further changes.
#' Otherwise, the return value is undefined (and may change in future) since
#' the function saves the slide to a file.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' # First, create a ggplot2 object
#' the_ggplot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   labs(
#'     title = "Chart title",
#'     subtitle = "Chart subtitle",
#'     caption = "Notes and source information."
#'   )
#'
#' # Export `the_ggplot` to file `output.pptx`
#' djpr_save_pptx("output.pptx", the_ggplot)
#'
#' # Specify layout and signpost
#' djpr_save_pptx("output.pptx", the_ggplot, layout = "half", signpost = "section")
#'
#' # Export multiple slides. Different plots can be passed in for each slide.
#' djpr_save_pptx(NULL, the_ggplot) %>%
#'   djpr_save_pptx(the_ggplot, layout="half") %>%
#'   print(target="slidepack.pptx")
#' }
djpr_save_pptx <- function(destination = NULL,
                           plot = ggplot2::last_plot(),
                           layout = c("full", "half", "twothirds"),
                           signpost = NULL) {
  UseMethod("djpr_save_pptx")
}

#' @export
djpr_save_pptx.NULL <- function(destination, ...) {
  djpr_save_pptx(get_template(), ...)
}

#' @export
djpr_save_pptx.character <- function(destination, ...) {
  slide <- djpr_save_pptx(NULL, ...)
  print(slide, target = destination)
}

#' @export
djpr_save_pptx.rpptx <- function(destination,
                                 plot = ggplot2::last_plot(),
                                 layout = c("full", "half", "twothirds"),
                                 signpost = NULL) {
  layout <- match.arg(layout)
  orig_plot <- plot

  # Don't override the layout argument because we later use it to check
  # for commentary
  layout_name <- paste0(layout, ifelse(is.null(signpost), "", "+signpost"))
  master <- "SPPcharts"

  slide <- officer::add_slide(destination, layout_name, master)

  if (!is.null(signpost)) {
    slide <- officer::ph_with(
      slide, signpost, officer::ph_location_label("signpost")
    )
  }

  # The following are defaulted to "" so the textboxes are preserved even
  # when there is no content to put in them

  chart_title <- default(plot$labels$title, "")
  slide <- officer::ph_with(
    slide, chart_title, officer::ph_location_label("title")
  )

  chart_subtitle <- default(plot$labels$subtitle, "")
  slide <- officer::ph_with(
    slide, chart_subtitle, officer::ph_location_label("subtitle")
  )

  chart_caption <- default(plot$labels$caption, "")
  slide <- officer::ph_with(
    slide, chart_caption, officer::ph_location_label("caption")
  )

  plot <- plot + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  slide <- officer::ph_with(
    slide, rvg::dml(ggobj = plot), officer::ph_location_label("chart")
  )

  if (layout != "full") {
    sidebar <- plot$labels$sidebar
    if(is.null(sidebar)) {
      sidebar <- "" # Keep space for user to type commentary
    } else {
      sidebar <- officer::unordered_list(sidebar, rep(1, length(sidebar)))
    }
    slide <- officer::ph_with(
      slide, sidebar, officer::ph_location_label("commentary")
    )
  }

  ggplot2::set_last_plot(orig_plot)
  slide
}

# Fetch the ppt template
get_template <- function() {
  # Template is modified by reference, so it must be re-loaded each time
  path <- system.file("extdata", "template.pptx", package = "djprtheme")
  officer::read_pptx(path)
}

# Substitute a default for empty values
default <- function(x, replacement, test = is.null) {
  ifelse(test(x), replacement, x)
}
