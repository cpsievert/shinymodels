% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore.R
\name{explore.default}
\alias{explore.default}
\alias{explore.tune_results}
\title{Launches shiny application based on data from \code{\link[tune:fit_resamples]{tune::fit_resamples()}}}
\usage{
\method{explore}{default}(x, ...)

\method{explore}{tune_results}(x, hover_cols = NULL, hover_only = FALSE, ...)
}
\arguments{
\item{x}{The \code{\link[=organize_data]{organize_data()}} result.}

\item{...}{Other parameters not currently used.}

\item{hover_cols}{The columns to display while hovering in the Shiny app.}

\item{hover_only}{A Boolean to turn on and off hovering in the interactive
plots; the default option is FALSE, it is recommended to use TRUE for a big data.}
}
\description{
This function takes the data based on \code{\link[tune:fit_resamples]{tune::fit_resamples()}} results to
launch a Shiny app. Also, note that the default configuration is based on the
optimal value of the first metric.
}
