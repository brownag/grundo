#' Step-wise Regression (`stats`)
#'
#' Wrapper method for fitting a `stats::lm()` model, selecting the "best" based on AIC step-wise using `stats::step()` on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#'
#' @return an object of class `lm` with attribute `"grundo_model"` `"stats::lm"`
#' @importFrom stats step
#' @export
gr_stpreg <- function(mf, variable, predvars = NULL) {
  gr_mlreg(mf, variable, predvars, step = TRUE)
}
