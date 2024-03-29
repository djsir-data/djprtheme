% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/breaks_right.R
\name{breaks_right}
\alias{breaks_right}
\title{Generate date breaks that align with the max limit}
\usage{
breaks_right(limits, n_breaks = 5, ...)
}
\arguments{
\item{limits}{Length-two numeric or date vector}

\item{n_breaks}{Number of breaks; passed to \code{scales::breaks_pretty()}}

\item{...}{Passed to \code{scales::breaks_pretty()}}
}
\value{
Vector of breaks
}
\description{
This function generates a vector of of 'pretty'
breaks (using \code{scales::breaks_pretty()}) that ends with
the upper limit provided and excludes any values that lie
outside the limits.
}
\examples{
# Can be used with numeric vectors
breaks_right(c(10, 30))

# Or date vectors
econ_dates <- c(min(ggplot2::economics$date),
               max(ggplot2::economics$date))
breaks_right(econ_dates)

# Can be supplied directly to the `breaks` argument of
# `ggplot2::scale_*_continuous()`, but limits will include
# padding defined by `expand`
library(ggplot2)

ggplot(ggplot2::economics,
      aes(x = date, y = unemploy)) +
 geom_line() +
 scale_x_date(breaks = breaks_right)

}
