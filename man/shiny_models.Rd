% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny_models.R, R/shiny_models_multicls.R,
%   R/shiny_models_reg.R, R/shiny_models_twocls.R
\name{shiny_models}
\alias{shiny_models}
\alias{shiny_models.default}
\alias{shiny_models.multi_cls_shiny_data}
\alias{shiny_models.reg_shiny_data}
\alias{shiny_models.two_cls_shiny_data}
\title{Internal function to run shiny application on an object of \code{shiny_data} class}
\usage{
shiny_models(x, hover_cols = NULL, hover_only = NULL, ...)

\method{shiny_models}{default}(x, hover_cols = NULL, hover_only = NULL, ...)

\method{shiny_models}{multi_cls_shiny_data}(x, hover_cols = NULL, hover_only = FALSE, ...)

\method{shiny_models}{reg_shiny_data}(x, hover_cols = NULL, hover_only = FALSE, ...)

\method{shiny_models}{two_cls_shiny_data}(x, hover_cols = NULL, hover_only = FALSE, ...)
}
\arguments{
\item{x}{The \code{\link[=organize_data]{organize_data()}} result.}

\item{hover_cols}{The columns to display while hovering in the Shiny app.}

\item{hover_only}{A Boolean to turn on and off hovering in the interactive
plots; the default option is FALSE, it is recommended to use TRUE for a big data.}

\item{...}{Other parameters not currently used.}

\item{original_data}{Original dataset.}
}
\description{
This function takes the \code{\link[=organize_data]{organize_data()}} result to shiny_models a Shiny app.
}
\keyword{internal}
