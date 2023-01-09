#' KNN Regression Model (`caret`)
#'
#' Wrapper method for fitting a `caret::knnreg()` model on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#' @param k integer Passed to [caret::knnreg()]
#' @param ... Additional arguments to [caret::knnreg()]
#' @return an object of class `knnreg` with attribute `"grundo_model"` `"caret::knnreg"`
#' @export
gr_knnreg <- function(mf, variable, predvars = NULL, k = 5, ...) {
  if (!requireNamespace("caret")) {
    stop("Package 'caret' is required.", call. = FALSE)
  }

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  res <- caret::knnreg(x = mf[predvars], y = mf[[variable]], k = k, ...)
  attr(res, 'grundo_model') <- "caret::knnreg"
  return(res)
}

