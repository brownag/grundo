#' Gaussian Process Regression (`GauPro`)
#'
#' Wrapper method for fitting a `GauPro::GauPro()` Gaussian process model
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#'
#' @return an object of class `GauPro` with attribute `"grundo_model"` `"gstat::gstat (rkriging)"`
#' @export
gr_gaupro <- function(mf, variable, predvars = NULL) {
  if (!requireNamespace("GauPro")) {
    stop("Package 'GauPro' is required.", call. = FALSE)
  }

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  res <- GauPro::GauPro(as.matrix(mf[predvars]), mf[, variable], parallel = FALSE)

  attr(res, 'grundo_model') <- "GauPro::GauPro"
  return(res)
}
