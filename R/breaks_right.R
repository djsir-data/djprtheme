#' Generate date breaks that align with the max limit
#'
#' This function generates a vector of of 'pretty'
#' breaks (using `scales::breaks_pretty()`) that ends with
#' the upper limit provided and excludes any values that lie
#' outside the limits.
#'
#' @param limits Length-two numeric or date vector
#' @param n_breaks Number of breaks; passed to `scales::breaks_pretty()`
#' @param ... Passed to `scales::breaks_pretty()`
#' @export
#' @return  Vector of breaks
#' @examples
#'# Can be used with numeric vectors
#'breaks_right(c(10, 30))
#'
#'# Or date vectors
#'econ_dates <- c(min(ggplot2::economics$date),
#'                max(ggplot2::economics$date))
#'breaks_right(econ_dates)
#'
#'# Can be supplied directly to the `breaks` argument of
#'# `ggplot2::scale_*_continuous()`, but limits will include
#'# padding defined by `expand`
#'
#'ggplot(ggplot2::economics,
#'       aes(x = date, y = unemploy)) +
#'  geom_line() +
#'  scale_x_date(breaks = breaks_right)
#'
breaks_right <- function(limits,
                         n_breaks = 5,
                         ...) {

  min_date <- limits[1]
  max_date <- limits[2]
  pre_br <- scales::breaks_pretty(n = n_breaks,
                                  ...)(c(min_date, max_date))
  date_adj <- pre_br[length(pre_br)] - max_date
  adj_br <- pre_br - date_adj
  names(adj_br) <- NULL
  out_br <- adj_br[adj_br >= min_date & adj_br <= max_date]
  out_br
}
