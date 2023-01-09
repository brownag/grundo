#' grundo
#'
#' grundo mundo
#'
#' @param predictors `SpatRaster` (or coercible to one)
#' @param samples `SpatVector` (or coercible to one)
#' @param variable `character`. Observed variable name (in `samples`)
#' @param model Primary model selection choice. Includes: `"cubist"` (default), `"ranger"` ...
#' @param vgm_model Variogram model selection choices for ordinary kriging of primary model residuals, see [gstat::vgm()] for details
#' @param ... Additional arguments passed to `[gstat::gstat()]`
#'
#' @return an object of class `"grundo"`
#' @export
#' @importFrom terra rast vect crs extract interpolate crds
#' @importFrom gstat vgm variogram fit.variogram
#' @importFrom stats as.formula terms complete.cases model.frame na.pass lm predict
grundo <- function(predictors, samples, variable, model,
                   vgm_model = gstat::vgm(), ...) {

  if (!inherits(predictors, 'SpatRaster')) {
    predictors <- terra::rast(predictors)
  }

  if (!inherits(samples, 'SpatVector')) {
    samples <- terra::vect(samples)
  }

  # assume samples are in same CRS as predictors if undefined
  if (terra::crs(samples) == "") {
    terra::crs(samples) <- terra::crs(predictors)
  }

  # empty result template and var names
  result <- rast(predictors[[1]])
  var_pred <- paste0(variable, "_pred")
  var_resid <- paste0(variable, "_resid")

  # extract predictor variables
  x0 <- terra::extract(predictors, samples)[-1]

  # model and ordinary kriging formulas
  fm <- as.formula(paste0(variable, " ~ ", paste(colnames(x0), collapse = " + ")))
  rfm <- as.formula(paste0(var_resid, " ~ 1"))

  # construct model frame data.frame(y, x1, x2, ...)
  mf <- model.frame(fm, cbind(samples, x0), na.action = na.pass)

  # append coordinates for primary spatial models
  cr <- terra::crds(samples)
  mfsp <- cbind(mf, cr)

  # fit a model
  if (is.character(model)){
    model <- tolower(trimws(model))
    m0 <- switch(model,
                   "kriging" = gr_rkrigi(mfsp, variable,
                                         vgm_model = vgm_model,
                                         coords = colnames(cr)), # regression kriging
                   "cubist"  = gr_cubist(mf, variable),   # cubist
                   "ranger"  = gr_ranger(mf, variable),   # random forest
                   "gaupro"  = gr_gaupro(mf, variable),   # gaussian process
                   "mlreg"   = gr_mlreg(mf, variable),    # multiple linear regression
                   "stepreg" = gr_stpreg(mf, variable),   # stepwise regression
                   "knnreg"  = gr_knnreg(mfsp, variable), # k nearest neighbor regression
                   "svm"     = gr_svm(mf, variable),      # support vector regression
                   "grfrf"   = gr_grfrf(mfsp, variable),  # grf regression forest
                   "grfmrf"  = gr_grfmrf(mfsp, variable), # grf multiple regression forest
                   "grfqf"   = gr_grfqf(mfsp, variable),  # grf quantile forest
                   "grfpf"   = gr_grfpf(mfsp, variable),  # grf probability forest
                   "grfllf"  = gr_grfllf(mfsp, variable)  # grf local linear forest
    )
  } else {
    # TODO: check if it has a predict or pred method and error otherwise
    m0 <- model
  }

  # special handling as needed for funky model predict methods
  if (model == "gaupro") {
    # TODO: implement as custom predict() function to interpolate()
    result[[var_pred]] <- m0$pred(as.matrix(predictors))
  } else if (model == "knnreg") {
    # TODO: interpolate does not play well with caret predict method, so custom
    #       KNN needs the X and Y grids -> mismatch train v.s. test matrix
    #       this operation may not scale as well to larger rasters
    predictorssp <- c(predictors, rast(list(x = terra::init(predictors, 'x'),
                                            y = terra::init(predictors, 'y'))))
    dp <- as.data.frame(predictorssp)
    res <- predict(m0, dp[complete.cases(dp),])
    result[[var_pred]] <- NA
    result[[1]][terra::cells(predictors)] <- res
  } else {
    res <- terra::interpolate(predictors, m0, na.rm = TRUE)
    # interpolate() calls <package>::predict() on SpatRaster by default
    result[[var_pred]] <- res[[grep("lyr1|pred", names(res))]]
  }

  # NB: some predict methods produce additional values (e.g. RK)

  # construct calibration data.frame
  calibration <- mf[variable]

  # extract prediction
  calibration[[var_pred]] <- terra::extract(result[[var_pred]], samples)[[var_pred]]

  # calculate residuals
  calibration[[var_resid]] <- (calibration[[variable]] - calibration[[var_pred]])

  # combine calibration results from model with coordinates of samples for kriging
  samples <- cbind(samples, calibration)

  # fit variogram
  var_vgm <- gstat::fit.variogram(grundo::variogram(samples, rfm), gstat::vgm(vgm_model))
  co <- terra::crds(samples)

  # TODO: consider calling gr_rkrigi() with args for ordinary kringing
  # TODO: consider more generic options for error correction (e.g. IDW)
  g <- gstat::gstat(
    formula = rfm,
    locations = as.formula(paste("~", paste0(colnames(co), collapse = " + "))),
    data = cbind(as.data.frame(samples), co),
    model = var_vgm,
    ...
  )

  var_krig <- terra::interpolate(predictors, g)

  # kriging residuals
  result[[var_resid]] <- var_krig[[1]]

  # calculate sd as sqrt of variance
  result[[paste0(var_resid, "_sd")]] <- sqrt(var_krig[[2]])

  # calculate primary prediction + kriging residual
  result[[paste0(variable, "_grundo")]] <- sum(result[[1:2]])

  res <- list(
    result = result,
    calibration = calibration,
    model = m0,
    variogram = var_vgm,
    kriging = var_krig
  )
  class(res) <- c('grundo', 'list')
  res
}


#' Variogram Method for SpatVector Objects
#'
#' This function uses the `gstat::variogram()` generic to define a `variogram(<SpatVector>, <formula>, ...)` method.
#'
#' @param object `SpatVector`
#' @param formula `formula`, specifying the dependent variable and possible covariates
#' @param data Optional: A `data.frame` containing variables and regressors
#' @param ... Additional arguments passed to `gstat:::variogram.default()`
#' @details This method does not require that the `SpatVector` input first be converted to `sf` object, preventing dependency on the {sf} namespace being loaded.
#' @return `data.frame` result of [gstat::variogram()]
#' @export
setMethod("variogram", signature("SpatVector"), function(object, formula, data = as.data.frame(object), ...) {
  stopifnot(nrow(data) == nrow(object))
  trm <- terms(formula)
  vrs <- all.vars(formula)
  lbl <- attr(trm, "term.labels")
  obj <- list(sapply(vrs[!vrs %in% lbl], function(x) data[[x]]))
  X <- list(cbind(sapply(lbl, function(x) data[[x]]),
                  matrix(1, nrow = nrow(object), ncol = length(attr(trm, "intercept")))
                ))
  # this calls gstat:::variogram.default(list, list, list)
  gstat::variogram(object = obj, locations = list(terra::crds(object)), X = X, ...)
})

#' @export
#' @importFrom methods show
print.grundo <- function(x, ...) {
  show(x$result)
}
