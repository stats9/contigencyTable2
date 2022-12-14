% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_contigency_table.r
\name{rr}
\alias{rr}
\title{define function for get relative risk results}
\usage{
rr(n11, n12, n21, n22, 
    varname1 = "Expose", varname2 = "Diseasee", levels_var1 = c("Exposed", 
    "UnExposed"), levels_var2 = c("Disease", "UnDisease"), 
    method = "wald", conf_level = 0.95, nboot = 1000)
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

\item{method}{It has two modes: \code{"wald", "boot"}
which is the \code{"boot"} mode based on resampling Method.}

\item{conf_level}{see \code{\link{odr}}}

\item{nboot}{when \code{method = "boot"} therefore nboot is number of
replicates that make resampling. \href{https://uc-r.github.io/resampling_methods}{resamplingMethods}.}
}
\value{
two table for RiskRatio results.
}
\description{
define function for get relative risk results
}
\examples{
\dontrun{rr(475, 461, 7, 61, "Expose", "Disease", c("Exposed", "UnExposed"), 
    c("Disease", "UnDisease"), method = "boot", conf_level = 0.95, nboot = 1000)}
}
