#' @importFrom generics explore
#' @export
generics::explore

#'  Launches shiny application based on data from [tune::fit_resamples()]
#'
#' This function takes the data based on [tune::fit_resamples()] results to
#' launch a Shiny app. Also, note that the default configuration is based on the
#' optimal value of the first metric.
#' @export
#' @rdname explore
explore.default <- function(x, ...) {
  rlang::abort("No `explore()` exists for this type of object.")
}

#' @export
#' @inheritParams shiny_models
#' @rdname explore
explore.tune_results <-
  function(x,
           hover_cols = NULL,
           hover_only = FALSE,
           ...) {
    dat <- organize_data(x, hover_cols = {{ hover_cols }}, ...)
    shiny_models(dat, hover_only = hover_only)
  }
