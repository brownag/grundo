#' Random Forest Model (`ranger`)
#'
#' Wrapper method for fitting a `ranger::ranger()` model on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#'
#' @return an object of class `ranger` with attribute `"grundo_model"` `"ranger::ranger"`
#' @export
gr_ranger <- function(mf, variable, predvars = NULL) {
  if (!requireNamespace("ranger")) {
    stop("Package 'ranger' is required.", call. = FALSE)
  }

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  fm <- as.formula(paste0(variable, " ~ ", paste0(predvars, collapse = " + ")))
  res <- ranger::ranger(fm, data = mf)
  attr(res, 'grundo_model') <- "ranger::ranger"
  return(res)
}

