##
## This file implements the `djpr_save_pptx` function which takes a ggplot
## and exports it as a PowerPoint slide.
##

# Fetch the ppt template
get_template <- function() {
  # Template is modified by reference, so it must be re-loaded each time
  path <- system.file("extdata", "template.pptx", package="djprtheme")
  officer::read_pptx(path)
}

# Substitute a default for empty values
default <- function(x, replacement, test=is.null) {
  ifelse(test(x), replacement, x)
}

#' Export a ggplot to a slide
#'
#' The exported slide will be in the SPP template.
#'
#' @param plot The ggplot to insert
#' @param destination File path to save the slide to
#' @param layout Slide layout to use
#' @param signpost Whether there should be a signpost heading
#'
#' @return Since the function saves the slide to a file, the return value
#' is not defined and may change in future
#' @export
#'
#' @examples
#' \dontrun{
#' # Export `the_ggplot` to file `output.pptx`
#' djpr_save_pptx(the_ggplot, "output.pptx")
#' # Specify layout and signpost
#' djpr_save_pptx(the_ggplot, "output.pptx", layout="half", signpost="section")
#' }
djpr_save_pptx <- function(
  plot
  , destination
  , layout=c("full", "half", "twothirds")
  , signpost=NULL
) {
  layout <- match.arg(layout)

  # Don't override the layout argument because we later use it to check whether
  # to preserve the commentary box
  layout_name <- paste0(layout, ifelse(is.null(signpost), "", "+signpost"))
  master <- "SPPcharts"

  slide <- officer::add_slide(get_template(), layout_name, master)

  if(!is.null(signpost)) {
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

  plot <- plot + ggplot2::labs(title=NULL, subtitle=NULL, caption=NULL)
  slide <- officer::ph_with(
    slide, rvg::dml(ggobj=plot), officer::ph_location_label("chart")
  )

  if(layout != "full") {
    slide <- officer::ph_with(
      slide, "", officer::ph_location_label("commentary")
    )
  }

  print(slide, target=destination)
}
