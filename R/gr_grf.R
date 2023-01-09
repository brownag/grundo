#' Local Linear Forest Model (`grf`)
#'
#' Wrapper method for fitting a `grf::ll_regression_forest()` model on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#' @param ... Additional arguments to [grf::ll_regression_forest()]
#' @return an object of class `ll_regression_forest` with attribute `"grundo_model"` `"grf::ll_regression_forest"`
#' @export
gr_grfllf <- function(mf, variable, predvars = NULL, ...) {
  if (!requireNamespace("grf")) {
    stop("Package 'grf' is required.", call. = FALSE)
  }

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  res <- grf::ll_regression_forest(mf[predvars], mf[[variable]], ...)
  attr(res, 'grundo_model') <- "grf::ll_regression_forest"
  return(res)
}

#' Regression Forest Model (`grf`)
#'
#' Wrapper method for fitting a `grf::regression_forest()` model on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#' @param ... Additional arguments to [grf::regression_forest()]
#' @return an object of class `regression_forest` with attribute `"grundo_model"` `"grf::regression_forest"`
#' @export
gr_grfrf <- function(mf, variable, predvars = NULL, ...) {
  if (!requireNamespace("grf")) {
    stop("Package 'grf' is required.", call. = FALSE)
  }

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  res <- grf::regression_forest(mf[predvars], mf[[variable]], ...)
  attr(res, 'grundo_model') <- "grf::regression_forest"
  return(res)
}

#' Multiple Regression Forest Model (`grf`)
#'
#' Wrapper method for fitting a `grf::multi_regression_forest()` model on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#' @param ... Additional arguments to [grf::multi_regression_forest()]
#' @return an object of class `multi_regression_forest` with attribute `"grundo_model"` `"grf::multi_regression_forest"`
#' @export
gr_grfmrf <- function(mf, variable, predvars = NULL, ...) {
  if (!requireNamespace("grf")) {
    stop("Package 'grf' is required.", call. = FALSE)
  }

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  res <- grf::multi_regression_forest(mf[predvars], mf[[variable]], ...)
  attr(res, 'grundo_model') <- "grf::multi_regression_forest"
  return(res)
}

#' Quantile Forest Model (`grf`)
#'
#' Wrapper method for fitting a `grf::quantile_forest()` model on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#' @param ... Additional arguments to [grf::quantile_forest()]
#' @return an object of class `quantile_forest` with attribute `"grundo_model"` `"grf::quantile_forest"`
#' @export
gr_grfqf <- function(mf, variable, predvars = NULL, ...) {
  if (!requireNamespace("grf")) {
    stop("Package 'grf' is required.", call. = FALSE)
  }

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  res <- grf::quantile_forest(mf[predvars], mf[[variable]], ...)
  attr(res, 'grundo_model') <- "grf::quantile_forest"
  return(res)
}

#' Probability Forest Model (`grf`)
#'
#' Wrapper method for fitting a `grf::probability_forest()` model on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#' @param ... Additional arguments to [grf::probability_forest()]
#' @return an object of class `probability_forest` with attribute `"grundo_model"` `"grf::probability_forest"`
#' @export
gr_grfpf <- function(mf, variable, predvars = NULL, ...) {
  if (!requireNamespace("grf")) {
    stop("Package 'grf' is required.", call. = FALSE)
  }

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  res <- grf::probability_forest(mf[predvars], mf[[variable]], ...)
  attr(res, 'grundo_model') <- "grf::quantile_forest"
  return(res)
}
