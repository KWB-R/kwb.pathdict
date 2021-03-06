% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{named_vector_to_data_frame}
\alias{named_vector_to_data_frame}
\title{Convert a Named Vector to a Data Frame}
\usage{
named_vector_to_data_frame(x)
}
\arguments{
\item{x}{named vector}
}
\value{
data frame with columns \code{name}, containing the names of \code{x}
  and \code{value}, containing the values of \code{x}
}
\description{
Convert a Named Vector to a Data Frame
}
\examples{
kwb.pathdict:::named_vector_to_data_frame(c(a = 1, b = 2, c = 3))
}
