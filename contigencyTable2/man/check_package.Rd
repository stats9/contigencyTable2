% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_contigency_table.r
\name{check_package}
\alias{check_package}
\title{This function is prepared to check whether the package is installed by
entering the name of a package as a string.}
\usage{
check_package(pak)
}
\arguments{
\item{pak}{name of package as string format}
}
\value{
return a string ("this package is not installed") or a vector with two element, name and version of vector
}
\description{
This function is prepared to check whether the package is installed by
entering the name of a package as a string.
}
\examples{
\dontrun{
    pak <- "ggplot2"
    check_package(pak)}
}
