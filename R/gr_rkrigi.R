#' Regression Kriging (`gstat`)
#'
#' Wrapper method for fitting a `gstat::gstat()` regression kriging model
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#' @param vgm_model Variogram model selection choices, see [gstat::vgm()] for details
#' @param coords Column names in `mf` containing spatial coordinates. Default: `c("x", "y")`
#'
#' @return an object of class `gstat` with attribute `"grundo_model"` `"gstat::gstat (rkriging)"`
#' @export
gr_rkrigi <- function(mf, variable, predvars = NULL, vgm_model = gstat::vgm(), coords = c("x", "y")) {

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% c(variable, coords)]
  }

  fm <- as.formula(paste0(variable, " ~ ", paste0(predvars, collapse = " + ")))

  # fit variogram
  var_vgm <- gstat::fit.variogram(grundo::variogram(vect(mf, geom = coords), fm), gstat::vgm(vgm_model))
  co <- mf[coords]
  res <- gstat::gstat(
    formula = fm,
    locations = as.formula(paste("~", paste0(colnames(co), collapse = " + "))),
    data = mf,
    model = var_vgm
  )
  attr(res, 'grundo_model') <- "gstat::gstat (rkriging)"
  return(res)
}
