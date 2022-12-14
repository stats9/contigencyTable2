% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_contigency_table.r
\name{uncertainty_get}
\alias{uncertainty_get}
\title{Uncertainty coefficient function}
\usage{
uncertainty_get(n11, n12, n21, n22, 
    varname1 = "Expose", varname2 = "Disease", 
    levels_var1 = c("Exposed", "UnExposed"), 
    levels_var2 = c("Disease", "UnDisease"))
}
\arguments{
\item{n11}{see also \code{\link{get_contigency_result}}}

\item{n12}{see also \code{\link{get_contigency_result}}}

\item{n21}{see also \code{\link{get_contigency_result}}}

\item{n22}{see also \code{\link{get_contigency_result}}}

\item{varname1}{see also \code{\link{get_contigency_result}}}

\item{varname2}{see also \code{\link{get_contigency_result}}}

\item{levels_var1}{see also \code{\link{get_contigency_result}}}

\item{levels_var2}{see also \code{\link{get_contigency_result}}}
}
\value{
table of uncertainty coefficienty results
}
\description{
Uncertainty coefficient function
}
\examples{
\dontrun{uncertainty_get(n11 = 475, n12 = 461, 
    n21 = 7, n22 = 61, varname1 = "Expose", 
    varname2 = "Disease", 
    levels_var1 = c("Exposed", "UnExposed"), 
    levels_var2 = c("Disease", "UnDisease"))}
}
