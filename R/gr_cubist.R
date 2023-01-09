#' Cubist Model (`Cubist`)
#'
#' Wrapper method for fitting a `Cubist::cubist()` model on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#'
#' @return an object of class `cubist` with attribute `"grundo_model"` `"Cubist::cubist"`
#' @export
gr_cubist <- function(mf, variable, predvars = NULL) {
  if (!requireNamespace("Cubist")) {
    stop("Package 'Cubist' is required.", call. = FALSE)
  }

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  res <- Cubist::cubist(mf[predvars], mf[[variable]])
  attr(res, 'grundo_model') <- "Cubist::cubist"
  return(res)
}
