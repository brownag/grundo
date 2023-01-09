#' Multiple Linear Regression Model (`stats`)
#'
#' Wrapper method for fitting a `stats::lm()` model on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#' @param step Select the "best" based on AIC step-wise using `stats::step()` on standard model frame. Default: `FALSE`
#' @param ... Additional arguments to [FNN::knn.reg()]
#' @return an object of class `lm` with attribute `"grundo_model"` `"stats::lm"`
#' @seealso [gr_stpreg()]
#' @export
gr_mlreg <- function(mf, variable, predvars = NULL, step = FALSE, ...) {

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  fm <- as.formula(paste0(variable, " ~ ", paste0(predvars, collapse = " + ")))
  mf[predvars] <- lapply(mf[predvars], as.numeric)
  res <- lm(fm, data = mf)
  attr(res, 'grundo_model') <- "stats::lm"

  return(res)
}
